use crate::parser::ast::{
    DeclarationType, ExpressionPatternType, IdentifierData, PatternType, StatementType,
    VariableDeclarationKind,
};

pub struct TriStateBool {
    flag: Option<bool>,
}
impl TriStateBool {
    pub(crate) fn new_true() -> Self {
        TriStateBool { flag: Some(true) }
    }

    pub(crate) fn new_false() -> Self {
        TriStateBool { flag: Some(false) }
    }

    pub(crate) fn new_unset() -> Self {
        TriStateBool { flag: None }
    }

    pub(crate) fn is_true(&self) -> bool {
        self.flag.is_some() && self.flag.unwrap()
    }

    pub(crate) fn is_false(&self) -> bool {
        self.flag.is_some() && !self.flag.unwrap()
    }

    pub(crate) fn is_unset(&self) -> bool {
        self.flag.is_none()
    }

    pub(crate) fn make_true(&mut self) {
        self.flag = Some(true);
    }

    pub(crate) fn make_false(&mut self) {
        self.flag = Some(false);
    }

    pub(crate) fn make_unset(&mut self) {
        self.flag = None;
    }

    pub(crate) fn merge<F>(&mut self, other: TriStateBool, conflict_resolver: F)
    where
        F: Fn(bool, bool) -> bool,
    {
        self.flag = if let Some(f) = self.flag {
            if let Some(f2) = other.flag {
                if f == f2 {
                    Some(f)
                } else {
                    Some(conflict_resolver(f, f2))
                }
            } else {
                Some(false)
            }
        } else {
            other.flag
        };
    }
}
impl PartialEq for TriStateBool {
    fn eq(&self, other: &Self) -> bool {
        if let Some(f) = &self.flag {
            if let Some(f2) = &other.flag {
                *f == *f2
            } else {
                false
            }
        } else {
            if let Some(_) = &other.flag {
                false
            } else {
                true
            }
        }
    }
}

pub struct Semantics {
    pub(crate) bound_names: Vec<IdentifierData>,
    pub(crate) lexically_declared_names: Vec<IdentifierData>,
    pub(crate) var_declared_names: Vec<IdentifierData>,
    pub(crate) top_level_lexically_declared_names: Vec<IdentifierData>,
    pub(crate) top_level_var_declared_names: Vec<IdentifierData>,
    pub(crate) has_direct_super: TriStateBool,
    pub(crate) is_valid_simple_assignment_target: TriStateBool,
    pub(crate) contains_yield_expression: TriStateBool,
    pub(crate) contains_unpaired_continue: TriStateBool,
    pub(crate) contains_unpaired_break: TriStateBool,
    pub(crate) contains_super_property: TriStateBool,
    pub(crate) contains_super_call: TriStateBool,
}
impl Semantics {
    pub(crate) fn new_empty() -> Self {
        Semantics {
            bound_names: vec![],
            lexically_declared_names: vec![],
            var_declared_names: vec![],
            top_level_lexically_declared_names: vec![],
            top_level_var_declared_names: vec![],
            has_direct_super: TriStateBool::new_unset(),
            is_valid_simple_assignment_target: TriStateBool::new_unset(),
            contains_yield_expression: TriStateBool::new_unset(),
            contains_unpaired_continue: TriStateBool::new_unset(),
            contains_unpaired_break: TriStateBool::new_unset(),
            contains_super_property: TriStateBool::new_unset(),
            contains_super_call: TriStateBool::new_unset(),
        }
    }

    pub(crate) fn merge(&mut self, mut other: Self) -> &mut Self {
        let panic_on_resolve = |f, f2| {
            panic!(format!(
                "Not sure how to resolve this conflict: (f, f2) = ({},{})",
                f, f2
            ));
        };
        let true_wins = |f, f2| f || f2;

        self.bound_names.append(&mut other.bound_names);
        self.lexically_declared_names
            .append(&mut other.lexically_declared_names);
        self.has_direct_super
            .merge(other.has_direct_super, panic_on_resolve);
        self.is_valid_simple_assignment_target
            .merge(other.is_valid_simple_assignment_target, panic_on_resolve);
        self.contains_yield_expression
            .merge(other.contains_yield_expression, true_wins);
        self.contains_unpaired_continue
            .merge(other.contains_unpaired_continue, true_wins);
        self.contains_unpaired_break
            .merge(other.contains_unpaired_break, true_wins);
        self.contains_super_property
            .merge(other.contains_super_property, true_wins);
        self.contains_super_call
            .merge(other.contains_super_call, true_wins);
        self
    }
}

/// Scan a pattern and extract bound names.
/// Returns: (bound_names, contains_expression, is_simple)
/// - bound_names: list of identifier names bound by this pattern
/// - contains_expression: true if the pattern contains any expressions (e.g., default values)
/// - is_simple: true if the pattern is a simple identifier binding
pub fn scan_pattern(pattern: &PatternType) -> (Vec<String>, bool, bool) {
    let mut bound_names = Vec::new();
    let mut contains_expression = false;
    let mut is_simple = false;

    match pattern {
        PatternType::PatternWhichCanBeExpression(expr_pattern) => match expr_pattern {
            ExpressionPatternType::Identifier(id) => {
                bound_names.push(id.name.clone());
                is_simple = true;
            }
        },
        PatternType::ObjectPattern { properties, .. } => {
            for prop in properties {
                let (mut names, has_expr, _) = scan_pattern(&prop.0.value);
                bound_names.append(&mut names);
                if has_expr {
                    contains_expression = true;
                }
            }
        }
        PatternType::ArrayPattern { elements, .. } => {
            for elem in elements.iter().flatten() {
                let (mut names, has_expr, _) = scan_pattern(elem);
                bound_names.append(&mut names);
                if has_expr {
                    contains_expression = true;
                }
            }
        }
        PatternType::RestElement { argument, .. } => {
            let (mut names, has_expr, _) = scan_pattern(argument);
            bound_names.append(&mut names);
            if has_expr {
                contains_expression = true;
            }
        }
        PatternType::AssignmentPattern { left, .. } => {
            let (mut names, _, _) = scan_pattern(left);
            bound_names.append(&mut names);
            contains_expression = true; // AssignmentPattern always contains an expression (the default)
        }
    }

    (bound_names, contains_expression, is_simple)
}

/// Scan a statement list for bound names from declarations.
/// Returns bound names from var, let, const, and function declarations.
pub fn scan_statement_list(statements: &[StatementType]) -> Vec<String> {
    let mut bound_names = Vec::new();

    for stmt in statements {
        scan_statement(&mut bound_names, stmt);
    }

    bound_names
}

/// Helper function to scan a single statement for var-declared names.
fn scan_statement(bound_names: &mut Vec<String>, stmt: &StatementType) {
    match stmt {
        StatementType::DeclarationStatement(decl) => match decl {
            DeclarationType::VariableDeclaration(var_decl) => {
                // Only collect var declarations at this level (let/const are block-scoped)
                if matches!(var_decl.kind, VariableDeclarationKind::Var) {
                    for declarator in &var_decl.declarations {
                        let (mut names, _, _) = scan_pattern(&declarator.id);
                        bound_names.append(&mut names);
                    }
                }
            }
            DeclarationType::FunctionOrGeneratorDeclaration(func) => {
                if let Some(id) = &func.id {
                    bound_names.push(id.name.clone());
                }
            }
            DeclarationType::ClassDeclaration(class) => {
                if let Some(id) = &class.id {
                    bound_names.push(id.name.clone());
                }
            }
        },
        StatementType::BlockStatement(block) => {
            // Recursively scan nested block statements for var declarations only
            for nested_stmt in &block.body {
                scan_statement(bound_names, nested_stmt);
            }
        }
        StatementType::ForStatement { init, body, .. } => {
            // Check init for var declarations
            if let Some(init) = init {
                if let crate::parser::ast::VariableDeclarationOrExpression::VariableDeclaration(
                    var_decl,
                ) = init
                {
                    if matches!(var_decl.kind, VariableDeclarationKind::Var) {
                        for declarator in &var_decl.declarations {
                            let (mut names, _, _) = scan_pattern(&declarator.id);
                            bound_names.append(&mut names);
                        }
                    }
                }
            }
            // Recursively scan body
            scan_statement(bound_names, body);
        }
        StatementType::ForInStatement(for_data) | StatementType::ForOfStatement(for_data) => {
            // Check left for var declarations
            if let crate::parser::ast::VariableDeclarationOrPattern::VariableDeclaration(var_decl) =
                &for_data.left
            {
                if matches!(var_decl.kind, VariableDeclarationKind::Var) {
                    for declarator in &var_decl.declarations {
                        let (mut names, _, _) = scan_pattern(&declarator.id);
                        bound_names.append(&mut names);
                    }
                }
            }
            // Recursively scan body
            scan_statement(bound_names, &for_data.body);
        }
        StatementType::WhileStatement { body, .. }
        | StatementType::DoWhileStatement { body, .. } => {
            scan_statement(bound_names, body);
        }
        StatementType::IfStatement {
            consequent,
            alternate,
            ..
        } => {
            scan_statement(bound_names, consequent);
            if let Some(alt) = alternate {
                scan_statement(bound_names, alt);
            }
        }
        StatementType::SwitchStatement { cases, .. } => {
            for case in cases {
                for case_stmt in &case.consequent {
                    scan_statement(bound_names, case_stmt);
                }
            }
        }
        StatementType::TryStatement {
            block,
            handler,
            finalizer,
            ..
        } => {
            for block_stmt in &block.body {
                scan_statement(bound_names, block_stmt);
            }
            if let Some(h) = handler {
                for handler_stmt in &h.body.body {
                    scan_statement(bound_names, handler_stmt);
                }
            }
            if let Some(f) = finalizer {
                for finalizer_stmt in &f.body {
                    scan_statement(bound_names, finalizer_stmt);
                }
            }
        }
        _ => {}
    }
}

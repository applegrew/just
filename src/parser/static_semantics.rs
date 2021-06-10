use crate::parser::ast::{
    AssignmentPropertyData, ExpressionPatternType, PatternType, VariableDeclarationData,
};

pub struct Semantics {
    pub(crate) bound_names: Vec<String>,
    pub(crate) is_valid_simple_assignment_target: Option<bool>,
}
impl Semantics {
    pub(crate) fn new_empty() -> Self {
        Semantics {
            bound_names: vec![],
            is_valid_simple_assignment_target: None,
        }
    }

    pub(crate) fn new_with_params(
        bound_names: Vec<String>,
        is_valid_simple_assignment_target: Option<bool>,
    ) -> Self {
        Semantics {
            bound_names,
            is_valid_simple_assignment_target,
        }
    }

    pub(crate) fn merge(&mut self, mut other: Self) -> &mut Self {
        self.bound_names.append(&mut other.bound_names);
        self.is_valid_simple_assignment_target =
            if let Some(f) = self.is_valid_simple_assignment_target {
                if let Some(f2) = other.is_valid_simple_assignment_target {
                    Some(f && f2)
                } else {
                    Some(f)
                }
            } else {
                if let Some(f2) = other.is_valid_simple_assignment_target {
                    Some(f2)
                } else {
                    None
                }
            };
        &mut self
    }
}

pub(crate) fn get_bound_names_from_pattern(pattern: &PatternType) -> Vec<String> {
    let mut bound_names = vec![];
    match pattern {
        PatternType::PatternWhichCanBeExpression(e) => match e {
            ExpressionPatternType::Identifier(i) => {
                bound_names.push(i.name.to_string());
            }
            ExpressionPatternType::MemberExpression(_) => {}
        },
        PatternType::ObjectPattern { properties, .. } => {
            for AssignmentPropertyData(p) in properties {
                let mut bn = get_bound_names_from_pattern(&p.value);
                bound_names.append(&mut bn);
            }
        }
        PatternType::ArrayPattern { elements, .. } => {
            for p in elements {
                if let Some(p) = p {
                    let mut bn = get_bound_names_from_pattern(p);
                    bound_names.append(&mut bn);
                }
            }
        }
        PatternType::RestElement { argument, .. } => {
            let mut bn = get_bound_names_from_pattern(argument);
            bound_names.append(&mut bn);
        }
        PatternType::AssignmentPattern { left, .. } => {
            let mut bn = get_bound_names_from_pattern(left);
            bound_names.append(&mut bn);
        }
    }
    bound_names
}

pub(crate) fn get_bound_names_from_variable_declaration_data(
    v: &VariableDeclarationData,
) -> Vec<String> {
    let mut bound_names = vec![];
    for d in &v.declarations {
        let (mut bn, _, _) = scan_pattern(&d.id);
        bound_names.append(&mut bn);
    }
    bound_names
}

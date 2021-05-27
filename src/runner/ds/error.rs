pub enum JErrorType {
    ReferenceError(String),
    TypeError(String),
    RangeError(String),
}
impl JErrorType {
    pub fn new_copy(other: &Self) -> Self {
        match other {
            JErrorType::ReferenceError(m) => JErrorType::ReferenceError(m.to_string()),
            JErrorType::TypeError(m) => JErrorType::TypeError(m.to_string()),
            JErrorType::RangeError(m) => JErrorType::TypeError(m.to_string()),
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            JErrorType::ReferenceError(m) => format!("Uncaught reference error: {}.", m),
            JErrorType::TypeError(m) => format!("Uncaught type error: {}.", m),
            JErrorType::RangeError(m) => format!("Uncaught range error: {}.", m),
        }
    }
}

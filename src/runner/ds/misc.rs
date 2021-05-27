use crate::runner::ds::env_record::EnvironmentRecordType;

pub struct IdentifierReference<'a> {
    pub name: String,
    pub value: Option<&'a EnvironmentRecordType>,
}

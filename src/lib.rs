pub mod parser;
pub mod reducer;
pub mod stdlib;

pub const STARTING_VARIABLE_ID: char = '`';

pub fn incr_identifier(id: char) -> char {
    char::from_u32(id as u32 + 1).unwrap_or(STARTING_VARIABLE_ID)
}

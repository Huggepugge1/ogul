pub const EXIT:         i32 = 0;
pub const WRITE:        i32 = 1;
pub const READ:         i32 = 2;
pub const ALLOCATE_INT: i32 = 3;
pub const ALLOCATE_STR: i32 = 4;

pub const EXIT_TOKEN:         &str = "exit";
pub const WRITE_TOKEN:        &str = "write";
pub const READ_TOKEN:         &str = "read";
pub const ALLOCATE_INT_TOKEN: &str = "=int";
pub const ALLOCATE_STR_TOKEN: &str = "=str";

pub const INSTRUCTIONS: [&str; 4] = [
    "exit",
    "write",
    "=int",
    "=str"
];

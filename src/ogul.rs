use std::fs::{File, OpenOptions};
use std::path::Path;
use std::io::{self, BufRead, BufReader, Write};

mod instructions;

struct Instruction {
    ip: i32,
    instruction_id: i32,
    value: i32,
    actual_instruction: String
}

fn setup(path: &str) -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .append(true)
        .open(path)?;

    write!(file,
"section .text
global _start
write:
    pop rax
    pop rsi
    pop rdx
    push rax
    mov rax, 1
    mov rbx, 1
    syscall
    ret

exit:
    pop rax
    pop rbx
    push rax
    mov rax, 60
    syscall

_start:
    jmp ins0\n")?;
    Ok(())
}

fn exit_file(path: &str) -> Result<String, String> {
    let file = OpenOptions::new()
        .append(true)
        .open(path);
    match file {
        Ok(mut v) => assert!(write!(v,
"stop:
    push 0
    call exit
section .data\n").is_ok(), "Failed to write to file \"{}\"", path),
        Err(_e) => return Err(format!("Failed to open file \"{}\"", path)),
    }
    Ok(String::new())
}

fn data(path: &str, strings: &Vec<String>) -> Result<String, String> {
    let mut file = OpenOptions::new()
        .append(true)
        .open(path)
        .expect("Failed to open file");

    let mut ptr: usize = 0;
    while ptr < strings.len() {
        match write!(file,
"    str{0}: db {1}, 10
    strlen{0}: EQU $- str{0}", ptr, strings[ptr]) {
            Ok(v)  => v,
            Err(_e) => panic!("Could not write to file {}", path),
        }
        ptr += 1;
    };
    Ok(String::new())
}

fn parse_instruction(ins: Instruction) -> Result<String, String> {
    if ins.instruction_id == instructions::WRITE {
        Ok(format!(
"ins{0}:
    push strlen{1}
    push str{1}
    call write\n", ins.ip, ins.value))
    } else {
        Err(String::from(format!("Invalid instruction : {}", ins.actual_instruction)))
    }
}

fn parse_line(line: String) -> (Result<Vec<String>, String>, Result<Vec<Instruction>, String>) {
    const NONE: i32        = 0;
    const INT: i32         = 1;
    const STRING: i32      = 2;
    const INSTRUCTION: i32 = 3;
    
    let mut tokens: Vec<&str> = line.split(" ").collect();
    let mut current_string: String = String::new();
    let mut strings: Vec<String> = Vec::new();
    let mut ins: i32 = NONE;

    for token in &mut tokens {
        if ins == STRING {
            if token.chars().nth(token.chars().count() - 1).unwrap() == '"' { 
                ins = NONE;
                strings.push(current_string + &" ".to_string() + token);
                current_string = String::new();
            } else {
                current_string += &(" ".to_string() + &token.to_string());
            }
        } else if token.chars().nth(0).unwrap() == '"' {
            ins = STRING;
            current_string = token.to_string();
        }
    }
    (Ok(strings), Ok(Vec::new()))
}

fn main() -> std::io::Result<()> {
    let mut instructions: Vec<Instruction> = Vec::new();
    let mut strings: Vec<String> = Vec::new();
    
    let file   = File::open("foo.ogul")?;
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let (mut current_strings, current_line) = match line {
            Ok(l)  => match parse_line(l) {
                (Ok(x), Ok(y)) => (x, y),
                (Ok(x), Err(e)) => panic!("{}", e),
                (Err(e), Ok(y)) => panic!("{}", e),
                (Err(e1), Err(e2)) => panic!("{0}, {1}", e1, e2),
            },
            Err(e) => panic!("{}", e),
        };
        strings.append(&mut current_strings);
    }

    let path: String = String::from("../compiled/foo.asm");
    File::create(&path)?;

    setup(&path)?;

    let ins: String = parse_instruction(Instruction {
        ip: 0,
        instruction_id: instructions::WRITE,
        value: 0,
        actual_instruction: String::from("Hello, World!")
    }).unwrap();
    
    let file = OpenOptions::new()
        .append(true)
        .open(&path);
    
    write!(file?, "{}", ins)?;

    match exit_file(&path) {
        Ok(s)  => s,
        Err(e) => panic!("{}", e),
    };
    match data(&path, &strings) {
        Ok(s)  => s,
        Err(e) => panic!("{}", e),
    };

    Ok(())
}

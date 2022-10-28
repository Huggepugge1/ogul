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
    let mut file = match OpenOptions::new()
        .append(true)
        .open(path) {
        Ok(v)   => v,
        Err(_e) => return Err(format!("Failed to open file \"{}\"", path))
    };

    let mut ptr: usize = 0;
    while ptr < strings.len() {
        match write!(file,
"    str{0}: db {1}, 10
    strlen{0}: EQU $- str{0}\n", ptr, strings[ptr]) {
            Ok(v)  => v,
            Err(_e) => return Err(format!("Could not write to file {}", path)),
        }
        ptr += 1;
    };
    Ok(String::new())
}

fn parse_instruction(ins: &Instruction) -> Result<String, String> {
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

fn parse_line(line: String, mut ip: i32, mut sp: i32) -> Result<(Vec<String>, Vec<Instruction>, i32, i32), String> {
    const NONE: i32        = 0;
    const INT: i32         = 1;
    const STRING: i32      = 2;
    const INSTRUCTION: i32 = 3;
    
    let mut tokens: Vec<&str> = line.split(" ").collect();
    let mut current_string: String = String::new();
    let mut strings: Vec<String> = Vec::new();
    let mut ins: i32 = NONE;

    let mut instructions: Vec<Instruction> = Vec::new();

    for token in &mut tokens {
        if token.chars().count() == 0 {
            continue;
        } else if ins == STRING {
            if token.chars().nth(token.chars().count() - 1).unwrap() == '"' { 
                ins = NONE;
                strings.push(current_string + &" ".to_string() + token);
                current_string = String::new();
                sp += 1;
            } else {
                current_string += &(" ".to_string() + &token.to_string());
            }
        } else if token.chars().nth(0).unwrap() == '"' {
            if token.chars().nth(token.chars().count() - 1).unwrap() == '"' {
                ins = NONE;
                strings.push(token.to_string());
                sp += 1;
            } else {
                ins = STRING;
                current_string = token.to_string();
            }
        } else if instructions::INSTRUCTIONS.contains(&token) {
            let token_copy: &str = token.clone();
            instructions.push(match token_copy {
                instructions::WRITE_TOKEN => Instruction {
                    ip: ip,
                    instruction_id: instructions::WRITE,
                    value: sp,
                    actual_instruction: token.to_string()
                },
                &_ => return Err(String::from("Unreachable")),
            });
            ip += 1;
        } 
    }
    Ok((strings, instructions, ip, sp))
}

fn write_instruction(path: &str, instruction: String) -> Result<String, String> {
    match OpenOptions::new()
        .append(true)
        .open(path) {
        Ok(mut f)  => write!(f, "{}", instruction),
        Err(_e)    => return Err(format!("Failed to open file \"{}\"", path)),
    };

    Ok(String::new())
}

fn main() -> std::io::Result<()> {
    let mut program: Vec<Instruction> = Vec::new();
    let mut strings: Vec<String> = Vec::new();
    
    let file   = File::open("foo.ogul")?;
    let reader = BufReader::new(file);
    
    let mut ip: i32 = 0;
    let mut sp: i32 = 0;

    for line in reader.lines() {
        let (mut current_strings, mut current_line, mut line_ip, mut line_sp) = match line {
            Ok(l)  => match parse_line(l, ip, sp) {
                Ok((a, b, c, d)) => (a, b, c, d),
                Err(e)           => panic!("{}", e),
            },
            Err(e) => panic!("{}", e),
        };
        ip = line_ip;
        sp = line_sp;
        program.append(&mut current_line);
        strings.append(&mut current_strings);
    }

    let path: String = String::from("../compiled/foo.asm");
    File::create(&path)?;

    setup(&path)?;
 
    for ins in program {
        match write_instruction(&path, match parse_instruction(&ins) {
            Ok(s)  => s,
            Err(e) => panic!("{}", e),
        }) {
            Ok(s)  => s,
            Err(e) => panic!("{}", e),
        };
    }

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

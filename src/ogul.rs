use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, Write};
use std::env;
use std::collections::HashMap;

mod instructions;

struct Instruction {
    ip: i32,
    instruction_id: i32,
    value: String,
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
    mov rax, 60
    pop rdi
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

fn data(path: &str, strings: &HashMap<String, String>) -> Result<String, String> {
    let mut file = match OpenOptions::new()
        .append(true)
        .open(path) {
        Ok(v)   => v,
        Err(e)  => return Err(format!("Failed to open file \"{0}\"  Error:{1}", path, e))
    };

    println!("{:?}", strings.keys());

    for string in strings.keys() {
        match write!(file,
"    {0}: db \"{1}\", 10
    len{0}: EQU $- {0}\n", string, strings[string]) {
            Ok(v)  => v,
            Err(_e) => return Err(format!("Could not write to file {}", path)),
        }
    };
    Ok(String::new())
}

fn is_var(strings: &HashMap<String, String>, ints: &HashMap<String, i64>, token: String) -> bool {
    if strings.keys().collect::<Vec<&String>>().contains(&&token) || ints.keys().collect::<Vec<&String>>().contains(&&token) {
        true
    } else {
        false
    }
}

fn parse_instruction(ins: &Instruction) -> Result<String, String> {
    if ins.instruction_id == instructions::WRITE {
        Ok(format!(
"ins{0}:
    push len{1}
    push {1}
    call write\n", ins.ip, ins.value))
    } else if ins.instruction_id == instructions::EXIT {
        Ok(format!(
"ins{0}:
    push {1}
    call exit\n", ins.ip, ins.value))
    } else {
        Err(String::from(format!("Invalid instruction : {}", ins.actual_instruction)))
    }
}

fn parse_line(line: String, mut strings: HashMap<String, String>, mut vars: HashMap<String, i64>, mut ip: i32, mut sp: i32) -> Result<(HashMap<String, String>, HashMap<String, i64>, Vec<Instruction>, i32, i32), String> {
    const NONE:        i32 = 0;
    const VAR:         i32 = 1;
    const STRING:      i32 = 2;
    const ASSIGN:      i32 = 3;
    
    let mut tokens: Vec<&str> = line.split(" ").collect();
    let mut current_string: String = String::new();
    let mut ins: i32 = NONE;
    let mut var: String = String::new();

    let mut instructions: Vec<Instruction> = Vec::new();
    
    let mut token = 0;

    while token < tokens.len() {
        println!("{}", token);
        if tokens[token].chars().count() == 0 {
            token += 1;
            continue;
        } else if instructions::INSTRUCTIONS.contains(&tokens[token]) {
            let token_copy: &str = tokens[token].clone();
            instructions.push(match token_copy {
                instructions::EXIT_TOKEN  => Instruction {
                    ip: ip,
                    instruction_id: instructions::EXIT,
                    value: tokens[token + 1].to_string(),
                    actual_instruction: tokens[token].to_string()
                },
                instructions::WRITE_TOKEN => Instruction {
                    ip: ip,
                    instruction_id: instructions::WRITE,
                    value: tokens[token + 1].to_string(),
                    actual_instruction: tokens[token].to_string()
                },
                &_ => return Err(String::from("Unreachable")),
            });
            ip += 1;
            token += match token_copy {
                instructions::EXIT_TOKEN  => 1,
                instructions::WRITE_TOKEN => 0,
                &_ => 0,
            }
        } else if ins == NONE {
            if is_var(&strings, &vars, tokens[token].to_string()) || (tokens.len() > token + 2 && tokens[token + 1] == "=") {
                ins = VAR;
                var = tokens[token].to_string();
                token += 1;
            } else {
                return Err(format!("{} is not a valid token", tokens[token]));
            }
        } else if ins == STRING {
            if tokens[token].chars().nth(tokens[token].chars().count() - 1).unwrap() == '"' { 
                ins = NONE;
                strings.insert(var.clone(), current_string + &" ".to_string() + &tokens[token][0..tokens[token].len()-1]);
                current_string = String::new();
                sp += 1;
            } else {
                current_string += &(" ".to_string() + &tokens[token].to_string());
            }
        } else if ins == VAR {
            if tokens[token].chars().nth(0).unwrap() == '"' {
                if tokens[token].chars().nth(tokens[token].chars().count() - 1).unwrap() == '"' {
                    ins = NONE;
                    strings.insert(var.clone(), tokens[token][1..tokens[token].len()-1].to_string());
                    sp += 1;
                } else {
                    ins = STRING;
                    current_string = tokens[token][1..tokens[token].len()].to_string();
                }
            } else {
                vars.insert(var.clone(), tokens[token].parse().unwrap());
            }
        } else { 
            return Err(format!("{} is not a valid token", tokens[token]));
        }
        token += 1;
    }
    Ok((strings, vars, instructions, ip, sp))
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
    let mut strings: HashMap<String, String> = HashMap::new();
    let mut vars: HashMap<String, i64> = HashMap::new();
    let mut program: Vec<Instruction> = Vec::new();
    
    let path:          String = String::from(&env::args().collect::<Vec<String>>()[1]);
    let compiled_path: String = String::from(&env::args().collect::<Vec<String>>()[2]);
    let asm_path:      String = String::from(compiled_path.clone() + &String::from(".asm"));
    
    let file   = File::open(&path)?;
    let reader = BufReader::new(file);
    
    let mut ip: i32 = 0;
    let mut sp: i32 = 0;

    for line in reader.lines() {
        let (mut current_strings, mut current_vars, mut current_line, mut line_ip, mut line_sp) = match line {
            Ok(l)  => match parse_line(l, strings, vars, ip, sp) {
                Ok((a, b, c, d, e)) => (a, b, c, d, e),
                Err(e)              => panic!("{}", e),
            },
            Err(_e) => panic!("Failed to read file \"{}\"", &path),
        };
        ip = line_ip;
        sp = line_sp;
        strings = current_strings;
        vars = current_vars;
        program.append(&mut current_line);
    }
    
    File::create(&asm_path)?;

    setup(&asm_path)?;
 
    for ins in program {
        match write_instruction(&asm_path, match parse_instruction(&ins) {
            Ok(s)  => s,
            Err(e) => panic!("{}", e),
        }) {
            Ok(s)  => s,
            Err(e) => panic!("{}", e),
        };
    }

    match exit_file(&asm_path) {
        Ok(s)  => s,
        Err(e) => panic!("{}", e),
    };
    match data(&asm_path, &strings) {
        Ok(s)  => s,
        Err(e) => panic!("{}", e),
    };

    Ok(())
}

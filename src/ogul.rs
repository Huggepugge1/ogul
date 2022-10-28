use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, Write};
use std::env;
use std::collections::HashMap;

mod instructions;
mod types;

struct Instruction {
    ip: i32,
    sp: i32,
    instruction_id: i32,
    name: String,
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

allocate_str:
    pop rax
    pop r8
    pop r9
    pop r10
    push r10
    push r9
    push r8
    push rax 
    mov rax, [counter]
    mov rbx, r9[rax]
    mov byte [r10 + rax], bl
    add qword [counter], 1
    mov rax, [counter]
    mov rbx, r8
    cmp rbx, rax
    jne allocate_str
    mov rax, [counter]
    mov word [r10 + rax], 0
    ret

write:
    pop rax
    pop rdx
    pop rsi
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
    jmp ins0\n\n")?;
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

fn data(path: &str, strings: &HashMap<i32, String>, vars: &HashMap<String, i32>) -> Result<String, String> {
    let mut file = match OpenOptions::new()
        .append(true)
        .open(path) {
        Ok(v)   => v,
        Err(e)  => return Err(format!("Failed to open file \"{0}\"  Error:{1}", path, e))
    };

    for string in strings.keys() {
        match write!(file,
"    str{0}: db \"{1}\"
    len{0}: EQU $- str{0}\n\n", string, strings[string]) {
            Ok(s)   => s,
            Err(_e) => return Err(format!("Could not write to file {}", path)),
        }
    };
    match write!(file,
"section .bss
    counter: resq 1\n") {
        Ok(s)   => s,
        Err(_e) => return Err(format!("Could not write to file {}", path)),
    };
    
    for var in vars.keys() {
        if vars[var] == types::INT {
            match add_int(path, var.to_string()) {
                Ok(s)  => s,
                Err(e) => return Err(e),
            };
        } else if vars[var] == types::STR {
            match add_str(path, var.to_string()) {
                Ok(s)  => s,
                Err(e) => return Err(e),
            };
        }
    }

    Ok(String::new())
}

fn add_int(path: &str, name: String) -> Result<String, String> {
    let _file = match OpenOptions::new()
        .append(true)
        .open(path) {
            Ok(mut file) => write!(file, "    {}: resq 1\n", name),
            Err(_e)      => return Err(format!("Failed to open file \"{}\"", path)),
    };
    Ok(String::new())
}

fn add_str(path: &str, name: String) -> Result<String, String> {
    let _file = match OpenOptions::new()
        .append(true)
        .open(path) {
            Ok(mut file) => write!(file, "    {}: resb 10000\n", name),
            Err(_e)      => return Err(format!("Failed to open file \"{}\"", path)),
    };
    Ok(String::new())
}

fn write_instruction(path: &str, instruction: String) -> Result<String, String> {
    let _file = match OpenOptions::new()
        .append(true)
        .open(path) {
        Ok(mut file)  => write!(file, "{}", instruction),
        Err(_e)    => return Err(format!("Failed to open file \"{}\"", path)),
    };

    Ok(String::new())
}

fn is_var(vars: &HashMap<String, i32>, token: String) -> bool {
    if vars.keys().collect::<Vec<&String>>().contains(&&token) {
        true
    } else {
        false
    }
}

fn parse_instruction(ins: &Instruction, mut allocated: HashMap<String, i32>) -> Result<(String, HashMap<String, i32>), String> {
    if ins.instruction_id == instructions::EXIT {
        Ok((format!(
"ins{0}:
    push qword [{1}]
    call exit\n\n", ins.ip, ins.value), allocated))
    } else if ins.instruction_id == instructions::WRITE {
        Ok((format!(
"ins{0}:
    push {1}
    push len{2}
    call write\n\n", ins.ip, ins.value, allocated[&ins.value]), allocated))
    } else if ins.instruction_id == instructions::ALLOCATE_INT {
        Ok((format!(
"ins{0}:
    mov qword [{1}], {2}\n\n", ins.ip, ins.name, ins.value), allocated))
    } else if ins.instruction_id == instructions::ALLOCATE_STR {
        allocated.insert(ins.name.to_string(), ins.sp);
        Ok((format!(
"ins{0}:
    mov qword [counter], 0
    push {1}
    push str{2}
    push len{2}
    call allocate_str\n\n", ins.ip, ins.name, ins.sp), allocated))
    } else {
        Err(String::from(format!("Invalid instruction : {}", ins.actual_instruction)))
    }
}

fn parse_line(line: String, mut strings: HashMap<i32, String>, mut vars: HashMap<String, i32>, mut ip: i32, mut sp: i32) -> Result<(HashMap<i32, String>, HashMap<String, i32>, Vec<Instruction>, i32, i32), String> {
    let mut tokens: Vec<&str> = line.split(" ").collect();
    let mut instructions: Vec<Instruction> = Vec::new();
    
    let mut token = 0;

    while token < tokens.len() {
        if tokens[token].chars().count() == 0 {
            tokens.remove(token);
        } else {
            token += 1;
        }
    }

    token = 0;

    while token < tokens.len() {
        if tokens[token].chars().count() == 0 {
            token += 1;
            continue;
        } else if instructions::INSTRUCTIONS.contains(&tokens[token]) {
            let token_copy: &str = tokens[token].clone();
            instructions.push(match token_copy {
                instructions::EXIT_TOKEN  => Instruction {
                    ip: ip,
                    sp: sp - 1,
                    instruction_id: instructions::EXIT,
                    name: "".to_string(),
                    value: tokens[token + 1].to_string(),
                    actual_instruction: format!("{0} : {1}", tokens[token], tokens.join(" "))
                },
                instructions::WRITE_TOKEN => Instruction {
                    ip: ip,
                    sp: sp - 1,
                    instruction_id: instructions::WRITE,
                    name: "".to_string(),
                    value: tokens[token + 1].to_string(),
                    actual_instruction: format!("{0} : {1}", tokens[token], tokens.join(" "))
                },
                instructions::ALLOCATE_INT_TOKEN => Instruction {
                    ip: ip,
                    sp: sp - 1,
                    instruction_id: instructions::ALLOCATE_INT,
                    name: tokens[token - 1].to_string(),
                    value: tokens[token + 1].to_string(),
                    actual_instruction: format!("{0} : {1}", tokens[token], tokens.join(" "))
                },
                instructions::ALLOCATE_STR_TOKEN => Instruction {
                    ip: ip,
                    sp: sp - 1,
                    instruction_id: instructions::ALLOCATE_STR,
                    name: tokens[token - 1].to_string(),
                    value: tokens[token + 1].to_string(),
                    actual_instruction: format!("{0} : {1}", tokens[token], tokens.join(" "))
                },
                &_ => return Err(String::from("Unreachable")),
            });
            ip += 1;
            token += match token_copy {
                instructions::EXIT_TOKEN         => 0,
                instructions::WRITE_TOKEN        => 0,
                instructions::ALLOCATE_INT_TOKEN => 1,
                instructions::ALLOCATE_STR_TOKEN => tokens[token..tokens.len()].join(" ").split("\"").collect::<Vec<&str>>()[1].to_string().matches(" ").count() + 1,
                &_ => 0,
            }
            
        } else {
            if is_var(&vars, tokens[token].to_string()) {
                if vars[tokens[token]] == types::STR && tokens.len() > token + 2 && tokens[token + 1] == instructions::ALLOCATE_STR_TOKEN {
                    match tokens[token+1..tokens.len()].join(" ").matches("\"").count() >= 2 {
                        true  => strings.insert(sp, tokens[token+1..tokens.len()].join(" ").split("\"").collect::<Vec<&str>>()[1].to_string().replace("\\n", "\", 10\"")),
                        false => return Err(format!("{} does not contain enough \"", tokens.join(" "))),
                    };
                    sp += 1;
                }
            } else if tokens.len() > token + 2 {
                if tokens[token + 1] == instructions::ALLOCATE_INT_TOKEN {
                    vars.insert(tokens[token].to_string(), types::INT);
                
                } else if tokens[token + 1] == instructions::ALLOCATE_STR_TOKEN {
                    vars.insert(tokens[token].to_string(), types::STR);
                    match tokens[token..tokens.len()].join(" ").matches("\"").count() >= 2 {
                        true  => strings.insert(sp, tokens[token..tokens.len()].join(" ").split("\"").collect::<Vec<&str>>()[1].to_string().replace("\\n", "\", 10\"")),
                        false => return Err(format!("{} does not contain enough \"", tokens.join(" "))),
                    };
                    sp += 1;
                }
            } else {
                return Err(format!("\"{}\" is not a valid token", tokens[token]));
            }
        }
        token += 1;
    }
    Ok((strings, vars, instructions, ip, sp))
}

fn main() -> std::io::Result<()> {
    let mut strings: HashMap<i32, String> = HashMap::new();
    let mut vars: HashMap<String, i32> = HashMap::new();
    let mut program: Vec<Instruction> = Vec::new();
    
    let path:          String = String::from(&env::args().collect::<Vec<String>>()[1]);
    let compiled_path: String = String::from(&env::args().collect::<Vec<String>>()[2]);
    let asm_path:      String = String::from(compiled_path.clone() + &String::from(".asm"));
    
    let file   = File::open(&path)?;
    let reader = BufReader::new(file);
    
    let mut ip: i32 = 0;
    let mut sp: i32 = 0;

    for line in reader.lines() {
        let (current_strings, current_vars, mut current_line, line_ip, line_sp) = match line {
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
 
    let mut allocated: HashMap<String, i32> = HashMap::new();

    for ins in program {
        let (s, current_allocated) = match parse_instruction(&ins, allocated) {
            Ok((s, a))  => (s, a),
            Err(e) => panic!("{}", e),
        };
        match write_instruction(&asm_path, s) {
            Ok(s)  => s,
            Err(e) => panic!("{}", e),
        };
        allocated = current_allocated;
    }

    match exit_file(&asm_path) {
        Ok(s)  => s,
        Err(e) => panic!("{}", e),
    };

    match data(&asm_path, &strings, &vars) {
        Ok(s)  => s,
        Err(e) => panic!("{}", e),
    };

    Ok(())
}

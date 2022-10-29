use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::collections::HashMap;
use std::env;
use std::process::{exit, Command};

mod instructions;
mod types;

struct Instruction {
    ip: i32,
    sp: i32,
    id: i32,
    name: String,
    value: String,
    error: String
}

fn exit_program<T>(e: String) -> T {
    println!("{}", e);
    exit(1);
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

fn data(path: &str, strings: &Vec<String>, vars: &HashMap<String, i32>) -> Result<String, String> {
    let mut file = match OpenOptions::new()
        .append(true)
        .open(path) {
        Ok(v)   => v,
        Err(e)  => return Err(format!("Failed to open file \"{0}\"  Error:{1}", path, e))
    };

    let mut string_num = 0;

    while string_num < strings.len() {
        let string = &strings[string_num];
        match write!(file,
"    str{0}: db \"{1}\"
    len{0}: EQU $- str{0}\n\n", string_num, string) {
            Ok(s)   => s,
            Err(_e) => return Err(format!("Could not write to file {}", path)),
        }
        string_num += 1;
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
    if ins.id == instructions::EXIT {
        Ok((format!(
"ins{0}:
    push qword [{1}]
    call exit\n\n", ins.ip, ins.value), allocated))
    } else if ins.id == instructions::WRITE {
        Ok((format!(
"ins{0}:
    push {1}
    push len{2}
    call write\n\n", ins.ip, ins.value, allocated[&ins.value]), allocated))
    } else if ins.id == instructions::ALLOCATE_INT {
        Ok((format!(
"ins{0}:
    mov qword [{1}], {2}\n\n", ins.ip, ins.name, ins.value), allocated))
    } else if ins.id == instructions::ALLOCATE_STR {
        allocated.insert(ins.name.to_string(), ins.sp);
        Ok((format!(
"ins{0}:
    mov qword [counter], 0
    push {1}
    push str{2}
    push len{2}
    call allocate_str\n\n", ins.ip, ins.name, ins.sp), allocated))
    } else {
        Err(String::from(format!("Invalid instruction : {}", ins.error)))
    }
}

fn parse_file(path: &str) -> Result<(Vec<Instruction>, HashMap<String, i32>, Vec<String>), String> {
    let mut program: Vec<Instruction>             = Vec::new();
    let mut strings: Vec<String>                  = Vec::new();
    let mut vars:    HashMap<String, i32>         = HashMap::new();
    let mut tokens:  Vec<(String, i32, i32, i32)> = Vec::new();
    
    tokens.push(("".to_string(), types::EMPTY, -1, -1));

    let mut ip:   i32 = 0;
    let mut sp:   i32 = 0;
    let mut line: i32 = 0;
    let mut pos:  i32 = 0;
    
    let mut string: bool = false;
    let mut token: String = String::new();

    let file = match File::open(path) {
        Ok(f)   => f,
        Err(_e) => return Err(format!("Failed to open File {}", path)),
    };

    for byte in file.bytes() {
        let byte = byte.unwrap();
        // "
        if byte == 34 {
            if string {
                string = false;
                tokens.push((token.replace("\n", "\", 10\"").replace("\\n", "\", 10\""), types::STR, line, pos));
                token = String::new();
            } else {
                string = true;
            }
        // Space or newline
        } else if byte == 32 || byte == 10 {
            if byte == 10 {
                line += 1;
                pos = 0;
            }
            if string {
                token.push(byte as char);
            } else if instructions::INTRINSICS.contains(&&*token) {
                tokens.push((token, types::INTRINSIC, line, pos));
                token = String::new()
            } else if token.matches(" ").count() != token.len() {
                match token.parse::<i64>() {
                    Ok(_i)  => tokens.push((token, types::INT, line, pos)),
                    Err(_e) => tokens.push((token, types::VAR, line, pos)),
                };
                token = String::new();
            } else {
                continue;
            }
        } else {
            token.push(byte as char);
        }
    }

    tokens.push(("".to_string(), types::EMPTY, -1, -1));

    let mut token_num: usize = 1;

    while token_num < tokens.len() - 1 {
        let token      = &tokens[token_num];
        let last_token = &tokens[token_num - 1];
        let next_token = &tokens[token_num + 1];
        
        if token.1 == 0 {
            if token.0 == instructions::EXIT_TOKEN {
                if next_token.1 == types::EMPTY {
                    return Err(format!("{1}:{2}:{3}  \"{0}\" did not recieve enough arguments", token.0, &path, token.2, token.3));
                }
                if next_token.1 != types::VAR {
                    return Err(format!("{1}:{2}:{3}  \"{0}\" is not a user defined variable", next_token.0, &path, next_token.2, next_token.3));
                }
                program.push(Instruction {
                    ip: ip,
                    sp: sp,
                    id: instructions::EXIT,
                    name: "".to_string(),
                    value: next_token.0.to_string(),
                    error: format!("{1}:{2}:{3}  \"{0}\"", token.0, &path, token.2, token.3)
                });
                token_num += 1

            } else if token.0 == instructions::WRITE_TOKEN {
                if next_token.1 == types::EMPTY {
                    return Err(format!("{1}:{2}:{3}  \"{0}\" did not recieve enough arguments", token.0, &path, token.2, token.3));
                }
                program.push(Instruction {
                    ip: ip,
                    sp: sp,
                    id: instructions::WRITE,
                    name: "".to_string(),
                    value: next_token.0.to_string(),
                    error: format!("{1}:{2}:{3}  \"{0}\"", token.0, &path, token.2, token.3)
                });
                token_num += 1

            } else if token.0 == instructions::ALLOCATE_INT_TOKEN {
                if last_token.1 == types::EMPTY {
                    return Err(format!("{1}:{2}:{3}  \"{0}\" =int need a variable name to be allocated to", token.0, &path, token.2, token.3));
                } else if next_token.1 == types::EMPTY {
                    return Err(format!("{1}:{2}:{3}  \"{0}\" =int can not allocate int to value NONE", token.0, &path, token.2, token.3));
                }
                
                 if vars.contains_key(&last_token.0) {
                    if vars[&last_token.0] != types::INT {
                        return Err(format!("{1}:{2}:{3}  \"{0}\" is previously defined as a non integer", token.0, &path, token.2, token.3));
                    }

                } else {
                    match next_token.0.parse::<i64>() {
                        Ok(i)   => i,
                        Err(_e) => return Err(format!("{1}:{2}:{3}  \"{0}\" is not an integer", next_token.0, &path, next_token.2, next_token.3)),
                    };
                    vars.insert(last_token.0.to_string(), types::INT);
                }
                program.push(Instruction {
                    ip: ip,
                    sp: sp,
                    id: instructions::ALLOCATE_INT,
                    name: last_token.0.to_string(),
                    value: next_token.0.to_string(),
                    error: format!("{1}:{2}:{3}  \"{0}\"", token.0, &path, token.2, token.3)
                });
                token_num += 1;

            } else if token.0 == instructions::ALLOCATE_STR_TOKEN {
                if last_token.1 == types::EMPTY {
                    return Err(format!("{1}:{2}:{3}  \"{0}\" =str need a variable name to be allocated to", token.0, &path, token.2, token.3));
                } else if next_token.1 == types::EMPTY {
                    return Err(format!("{1}:{2}:{3}  \"{0}\" can not allocate string to value NONE", token.0, &path, token.2, token.3));
                }
                if vars.contains_key(&last_token.0) {
                    if vars[&last_token.0] != types::STR {
                        return Err(format!("{1}:{2}:{3}  \"{0}\" is previously defined as a non string", token.0, &path, token.2, token.3));
                    }
                } else {
                    vars.insert(last_token.0.to_string(), types::STR);
                }
                program.push(Instruction {
                    ip: ip,
                    sp: sp,
                    id: instructions::ALLOCATE_STR,
                    name: last_token.0.to_string(),
                    value: next_token.0.to_string(),
                    error: format!("{1}:{2}:{3}  \"{0}\"", token.0, &path, token.2, token.3)
                });
                strings.push(next_token.0.to_string());
                token_num += 1;
                sp += 1;

            } else {
                return Err(format!("{1}:{2}:{3}  \"{0}\" is not implemented yet", token.0, &path, token.2, token.3));
            }
            ip += 1;
        }
        token_num += 1;
    }

    Ok((program, vars, strings))
}

fn main() -> std::io::Result<()> { 
    let path:          String = String::from(&env::args().collect::<Vec<String>>()[1]);
    let compiled_path: String = String::from(&env::args().collect::<Vec<String>>()[2]);
    let binary_path:   String = String::from(&env::args().collect::<Vec<String>>()[3]);
    let asm_path:      String = String::from(compiled_path.clone() + &String::from(".asm")); 
    let out_path:      String = String::from(compiled_path.clone() + &String::from(".o"));

    let (program, vars, strings) = match parse_file(&path) {
        Ok(v)  => v,
        Err(e) => exit_program(e),
    };

    File::create(&asm_path)?;

    setup(&asm_path)?;
 
    let mut allocated: HashMap<String, i32> = HashMap::new();

    for ins in program {
        let (s, current_allocated) = match parse_instruction(&ins, allocated) {
            Ok((s, a))  => (s, a),
            Err(e) => exit_program(e),
        };
        match write_instruction(&asm_path, s) {
            Ok(s)  => s,
            Err(e) => exit_program(e),
        };
        allocated = current_allocated;
    }

    match exit_file(&asm_path) {
        Ok(s)  => s,
        Err(e) => exit_program(e),
    };

    match data(&asm_path, &strings, &vars) {
        Ok(s)  => s,
        Err(e) => exit_program(e),
    };

    Command::new("nasm")
        .args(&["-f", "elf64"])
        .args(&["-o", &out_path])
        .arg(&asm_path)
        .spawn()?;

    Command::new("ld")
        .args(&["-o", &binary_path])
        .arg(&out_path)
        .spawn()?;

    Ok(())
}

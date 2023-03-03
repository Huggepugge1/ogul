use std::fs::OpenOptions;
use std::io::Result;
use std::process::{exit, Command};
use std::io::Write;
use std::env;

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
enum OpType {
    PushInt,
    Plus,
    Minus,
    Dump,
    Jump,
    Exit,
    Else,
    BlockStart,
    BlockEnd,
    IsEq,
    LT,
    GT,
}

#[derive(Debug)]
#[derive(Clone)]
struct Token {
    typ: OpType,
    value: String,
    pos: [usize; 2],
}

#[derive(Debug)]
#[derive(Clone)]
struct Op {
    typ: OpType,
    value: i64,
    string_value: String,
    line: usize,
}

impl Token {
    pub fn new(typ: OpType, value: String, pos: [usize; 2]) -> Token {
        Token {
            typ: typ,
            value: value,
            pos: pos,
        }
    }
}

impl Op {
    pub fn new(typ: OpType, value: i64, string_value: String, line: usize) -> Op {
        Op {
            typ: typ,
            value: value,
            string_value: string_value,
            line: line,
        }
    }
}

fn lexer(path: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let content: String = String::from_utf8(std::fs::read(path).expect("Could not read file {path}")).unwrap();

    let mut token: String = String::new();
    // pos, line, col
    let mut pos: [usize; 3] = [0, 1, 1];
    while pos[0] < content.len() {
        let mut c = content.chars().nth(pos[0]).unwrap();
        if c.is_numeric() {
            while c.is_numeric() {
                token += &c.to_string();
                pos[0] += 1;
                c = content.chars().nth(pos[0]).unwrap();
            }
            tokens.push(Token::new(OpType::PushInt, token.clone(), [pos[1], pos[2]]));
            pos[2] += token.len();
            token = String::new();
            
        } else if !c.is_whitespace() {
            while !c.is_whitespace() {
                token += &c.to_string();
                pos[0] += 1;
                c = content.chars().nth(pos[0]).unwrap();
            }
            match &token[..] {
                "+"    => tokens.push(Token::new(OpType::Plus, token.clone(), [pos[1], pos[2]])),
                "-"    => tokens.push(Token::new(OpType::Minus, token.clone(), [pos[1], pos[2]])),
                "dump" => tokens.push(Token::new(OpType::Dump, token.clone(), [pos[1], pos[2]])),
                "jump" => tokens.push(Token::new(OpType::Jump, token.clone(), [pos[1], pos[2]])),
                "exit" => tokens.push(Token::new(OpType::Exit, token.clone(), [pos[1], pos[2]])),
                "{"    => tokens.push(Token::new(OpType::BlockStart, token.clone(), [pos[1], pos[2]])),
                "}"    => tokens.push(Token::new(OpType::BlockEnd, token.clone(), [pos[1], pos[2]])),
                "if"   => (),
                "else" => tokens.push(Token::new(OpType::Else, token.clone(), [pos[1], pos[2]])),
                "=="   => tokens.push(Token::new(OpType::IsEq, token.clone(), [pos[1], pos[2]])),
                "<"    => tokens.push(Token::new(OpType::LT, token.clone(), [pos[1], pos[2]])),
                ">"    => tokens.push(Token::new(OpType::GT, token.clone(), [pos[1], pos[2]])),
                _      => {
                    eprintln!("{token} is not defined\n");
                    exit(1);
                },
            }
            token = String::new();
        } else {
            pos[0] += 1;
            if c == '\n' {
                pos[2]  = 0;
                pos[1] += 1;
            }
        }
    }
    tokens
}

fn get_block_end(mut tokens: Vec<Token>) -> Vec<Token> {
    let mut stack: Vec<(Token, usize)> = Vec::new();

    for (index, token) in tokens.clone().into_iter().enumerate() {
        let pos = format!("{0}:{1}", token.pos[0], token.pos[1]);
        if token.typ == OpType::BlockStart {
            stack.push((token, index));
        } else if token.typ == OpType::BlockEnd {
            if stack.len() == 0 {
                eprintln!("{pos}: Block end (\"}}\") does not have a corresponding start");
                exit(1);
            } else {
                let (mut curr_token, curr_token_index) = stack.pop().unwrap();
                curr_token.value = index.to_string();
                tokens[curr_token_index] = curr_token;
            }
        }
    }

    for (token, _) in stack {
        let pos = format!("{0}:{1}", token.pos[0], token.pos[1]);
        eprintln!("{pos}: Block start (\"{{\") does not have a corresponding end");
        exit(1);
    }

    tokens
}

fn parse(tokens: Vec<Token>) -> Vec<Op> {
    let mut operations: Vec<Op> = Vec::new();

    for (index, token) in tokens.clone().into_iter().enumerate() {
        let pos = format!("{0}:{1}", token.pos[0], token.pos[1]);
        let value = &token.value;
        let mut val = 0;

        if &token.typ == &OpType::Jump && &tokens[index - 1].typ == &OpType::PushInt {
            val = operations.pop().unwrap().value;
        } else if &token.typ == &OpType::Jump {
            eprintln!("{pos}: \"jump\" requires a line number as a positive integer");
        }
        
        operations.push(match token.typ {
            OpType::PushInt    => Op::new(token.typ, token.value
                                       .parse::<i64>()
                                       .expect(&format!("{pos}: {value} is not a valid integer"))
                                       , String::new(), token.pos[0]),

            OpType::Plus       => Op::new(token.typ, 0, String::new(), token.pos[0]),
            OpType::Minus      => Op::new(token.typ, 0, String::new(), token.pos[0]),
            OpType::Dump       => Op::new(token.typ, 0, String::new(), token.pos[0]),
            OpType::Jump       => Op::new(token.typ, val, String::new(), token.pos[0]),
            OpType::Exit       => Op::new(token.typ, 0, String::new(), token.pos[0]),
            OpType::Else       => Op::new(token.typ, 0, String::new(), token.pos[0]),
            OpType::BlockStart => Op::new(token.typ, token.value
                                          .parse::<i64>()
                                          .unwrap()
                                          , String::new(), token.pos[0]),

            OpType::BlockEnd   => Op::new(token.typ, 0, String::new(), token.pos[0]),
            OpType::IsEq       => Op::new(token.typ, 0, String::new(), token.pos[0]),
            OpType::LT         => Op::new(token.typ, 0, String::new(), token.pos[0]),
            OpType::GT         => Op::new(token.typ, 0, String::new(), token.pos[0]),
            
        });
    }

    operations
}

fn write(path: &str, content: &str, start: bool) {
    if start {
        match std::fs::write(path, content) {
            Ok(_) => return,
            Err(_) => {
                eprintln!("could not write to file");
                exit(1);
            },
        }
    }
    else {
        let mut file_ref = OpenOptions::new().append(true).open(path).expect(&format!("Could not open file \"{path}\""));
        file_ref.write_all(content.as_bytes()).expect(&format!("Could not write to file \"{path}\""));
    };
}

fn compile(path: &str, program: Vec<Op>) -> Result<String> {
    let start =  
"\
segment .text
dump:
\tmov r9, -3689348814741910323
\tsub rsp, 40
\tmov BYTE [rsp+31], 10
\tlea rcx, [rsp+30]
.L2:
\tmov rax, rdi
\tlea r8, [rsp+32]
\tmul r9
\tmov rax, rdi
\tsub r8, rcx
\tshr rdx, 3
\tlea rsi, [rdx+rdx*4]
\tadd rsi, rsi
\tsub rax, rsi
\tadd eax, 48
\tmov BYTE [rcx], al
\tmov rax, rdi
\tmov rdi, rdx
\tmov rdx, rcx
\tsub rcx, 1
\tcmp rax, 9
\tja  .L2
\tlea rax, [rsp+32]
\tmov edi, 1
\tsub rdx, rax
\txor eax, eax
\tlea rsi, [rsp+32+rdx]
\tmov rdx, r8
\tmov rax, 1
\tsyscall
\tadd rsp, 40
\tret
global _start
_start:
";

    write(path, start, true);

    let mut line: usize = 0;
    for (index, op) in program.clone().into_iter().enumerate() {
        while line != op.line {
            write(path, &format!("line{line}:\n"), false);
            line += 1;
        }

        write(path, &format!("op{index}:\n"), false);
        match op.typ {
            OpType::PushInt    => write(path, &format!("\tpush {}\n", op.value), false),

            OpType::Plus       => write(path, &format!("\tpop rbx\n\
                                                      \tpop rax\n\
                                                      \tadd rax, rbx\n\
                                                      \tpush rax\n"), false),

            OpType::Minus      => write(path, &format!("\tpop rbx\n\
                                                      \tpop rax\n\
                                                      \tsub rax, rbx\n\
                                                      \tpush rax\n"), false),
            
            OpType::Dump       => write(path, &format!("\tpop rdi\n\
                                                        \tcall dump\n"), false),

            OpType::Jump       => write(path, &format!("\tjmp line{}\n", op.value), false),

            OpType::Exit       => write(path, &format!("\tjmp exit\n"), false),

            OpType::Else       => write(path, &format!("\ttest rax, rax\n\
                                                        \tjz else{0}\n\
                                                        \tpush 0\n\
                                                        \tjmp op{1}\n\
                                                        else{0}:\n\
                                                        \tpush 1\n", index, program[index + 1].value), false),

            OpType::BlockStart => write(path, &format!("\tpop rax\n\
                                                        \tpush rax\n\
                                                        \ttest rax, rax\n\
                                                        \tjz op{}\n", op.value), false),

            OpType::BlockEnd   => write(path, &format!("\tpop rax\n"), false),
            
            OpType::IsEq       => write(path, &format!("\tpop rbx\n\
                                                        \tpop rax\n\
                                                        \tcmp rax, rbx\n\
                                                        \tjne false{index}\n\
                                                        \tpush 1\n\
                                                        \tjmp end{index}\n\
                                                        false{index}:\n\
                                                        \tpush 0\n\
                                                        end{index}:\n"), false),
 
            OpType::LT         => write(path, &format!("\tpop rbx\n\
                                                        \tpop rax\n\
                                                        \tcmp rax, rbx\n\
                                                        \tjge false{index}\n\
                                                        \tpush 1\n\
                                                        \tjmp end{index}\n\
                                                        false{index}:\n\
                                                        \tpush 0\n\
                                                        end{index}:\n"), false),
                                                    
            OpType::GT         => write(path, &format!("\tpop rbx\n\
                                                        \tpop rax\n\
                                                        \tcmp rax, rbx\n\
                                                        \tjle false{index}\n\
                                                        \tpush 1\n\
                                                        \tjmp end{index}\n\
                                                        false{index}:\n\
                                                        \tpush 0\n\
                                                        end{index}:\n"), false),
        };
    }
    let end = 
"
\tpush 0
exit:
\tmov rax, 60
\tpop rbx
\tsyscall
";

    write(path, end, false);
    run_command("nasm out.asm -felf64");
    run_command("ld out.o -o out");
    Ok(String::new())
}

fn run_command(command: &str) {
    println!("command: {command}");
    let output = Command::new("sh")
        .arg("-c")
        .arg(command)
        .output()
        .expect(&format!("failed to execute command {command}"));
    println!("output: {output:?}");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    
    let tokens = get_block_end(lexer(&args[1]));
    let program = parse(tokens);

    compile("out.asm", program).expect("Something went wrong");
}

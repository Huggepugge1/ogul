use std::fs::OpenOptions;
use std::io::Result;
use std::process::{exit, Command};
use std::io::Write;

#[derive(Debug)]
enum OpType {
    PushInt,
    Plus,
    Minus,
    Dump,
    Jump,
    Label,
}

#[derive(Debug)]
struct Token {
    typ: OpType,
    value: String,
    pos: [usize; 2],
}

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
            pos[1] += token.len();
            token = String::new();
            
        } else if c == '+' {
            tokens.push(Token::new(OpType::Plus, token.clone(), [pos[1], pos[2]]));
            pos[0] += 1;
            pos[2] += 1;

        } else if c == '-' {
            tokens.push(Token::new(OpType::Minus, token.clone(), [pos[1], pos[2]]));
            pos[0] += 1;
            pos[2] += 1;

        } else if !c.is_whitespace() {
            while !c.is_whitespace() {
                token += &c.to_string();
                pos[0] += 1;
                c = content.chars().nth(pos[0]).unwrap();
            }
            match &token[..] {
                "dump" => tokens.push(Token::new(OpType::Dump, token.clone(), [pos[1], pos[2]])),
                "jmp"  => tokens.push(Token::new(OpType::Jump, token.clone(), [pos[1], pos[2]])),
                _      => {
                    eprint!("{token} not defined");
                    exit(1);
                }
            }
            token = String::new();
        } else {
            pos[0] += 1;
            if c == '\n' {
                pos[1]  = 0;
                pos[2] += 1;
            }
        }
    }
    tokens
}

fn parse(tokens: Vec<Token>) -> Vec<Op> {
    let mut operations: Vec<Op> = Vec::new();

    for (index, token) in tokens.into_iter().enumerate() {
        let pos = format!("{0}:{1}", token.pos[0], token.pos[1]);
        let value = &token.value;
        operations.push(match token.typ {
            OpType::PushInt => Op::new(token.typ, token.value
                                       .parse::<i64>()
                                       .expect(&format!("{pos}: {value} is not a valid integer"))
                                       , String::new()),

            OpType::Plus    => Op::new(token.typ, 0, String::new()),
            OpType::Minus   => Op::new(token.typ, 0, String::new()),
            OpType::Dump    => Op::new(token.typ, 0, String::new()),
            OpType::Jump    => Op::new(token.typ, 0, String::new()),
        });
    }

    operations
}

fn write(path: &str, content: &str, start: bool) {
    if start {
        match std::fs::write(path, content) {
            Ok(_) => return,
            Err(_) => {
                eprint!("could not write to file");
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
        mov r9, -3689348814741910323
        sub rsp, 40
        mov BYTE [rsp+31], 10
        lea rcx, [rsp+30]
.L2:
        mov rax, rdi
        lea r8, [rsp+32]
        mul r9
        mov rax, rdi
        sub r8, rcx
        shr rdx, 3
        lea rsi, [rdx+rdx*4]
        add rsi, rsi
        sub rax, rsi
        add eax, 48
        mov BYTE [rcx], al
        mov rax, rdi
        mov rdi, rdx
        mov rdx, rcx
        sub rcx, 1
        cmp rax, 9
        ja  .L2
        lea rax, [rsp+32]
        mov edi, 1
        sub rdx, rax
        xor eax, eax
        lea rsi, [rsp+32+rdx]
        mov rdx, r8
        mov rax, 1
        syscall
        add rsp, 40
        ret
global _start
_start:
";

    write(path, start, true);

    for (index, op) in program.into_iter().enumerate() {
        write(path, &format!("op{index}:\n"), false);
        match op.typ {
            OpType::PushInt => write(path, &format!("\tpush {}\n", op.value), false),

            OpType::Plus     => write(path, &format!("\tpop rbx\n\
                                                      \tpop rax\n\
                                                      \tadd rax, rbx\n\
                                                      \tpush rax\n"), false),

            OpType::Minus    => write(path, &format!("\tpop rbx\n\
                                                      \tpop rax\n\
                                                      \tsub rax, rbx\n\
                                                      \tpush rax\n"), false),
            
            OpType::Dump     => write(path, &format!("\tpop rdi\n\
                                                      \tcall dump\n"), false),

            OpType::Jump     => write(path, &format!("\tpop rdi\n\
                                                      \tjmp {}", op.value), false),
                                                    
        };
    }
    let end = 
"
        mov rax, 60
        mov rbx, 0
        syscall
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
    println!("output: {output:?}", output=output.stdout);
}

fn main() {
    let tokens = lexer("examples/dump.ogul");
    let program = parse(tokens);

    compile("out.asm", program).expect("Something went wrong");
}

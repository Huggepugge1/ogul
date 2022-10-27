use std::fs::File;
use std::path::Path;
use std::io::{self, BufRead};

mod instructions;

struct Instruction {
    ip: i32,
    instruction_id: i32,
    value: i32,
    actual_instruction: String
}

const DEFAULT_LEN: usize = 100000;

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn setup(path: String) {
    let f = File::open("path")?;
    f.write_all(
b"write:
    pop rax
    pop rsi
    pop rdx
    push rax
    mov rax, 1
    mov rbx, 1
    syscall
    ret")
}

fn parse_instruction(ins: Instruction) -> Result<String, String> {
    if ins.instruction_id == instructions::WRITE {
        Ok(format!(
"push strlen{0}
push str{1}
call write", ins.value, ins.value))
    } else {
        Err(format!("Invalid instruction : {}", ins.actual_instruction))
    }
}

fn main() -> std::io::Result<()> {
    if let Ok(lines) = read_lines("./foo.ogul") {
        for line in lines {
            if let Ok(ip) = line {
                println!("{}", ip);
                // parse_line(ip);
            }
        }
    }

    let mut strings: [&str; DEFAULT_LEN] = [""; DEFAULT_LEN];

    let path: String = String::from("../compiled/foo.asm");
    let file = File::create(path)?;

    setup(path);

    let ins: String = parse_instruction(Instruction {
        ip: 0,
        instruction_id: instructions::WRITE,
        value: 0,
        actual_instruction: String::from("Hello, World!")
    }).unwrap();
    
    println!("{}", ins);

    Ok(())
}

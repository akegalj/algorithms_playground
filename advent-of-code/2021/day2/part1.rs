use std::fs::File;
use std::io::{BufRead, BufReader};

enum Instruction {
    Forward(i32),
    Down(i32),
    Up(i32),
}

fn parse_instruction(instr: &str) -> Instruction {
    let mut split = instr.split_whitespace();
    let instr_name = split.next().unwrap();
    let instr_step = split.next().unwrap().parse::<i32>().unwrap();
    match instr_name {
        "forward" => Instruction::Forward(instr_step),
        "down" => Instruction::Down(instr_step),
        "up" => Instruction::Up(instr_step),
        _ => panic!("wooo"),
    }
}

fn position(instrs: Vec<Instruction>) -> i32 {
    let mut hor = 0;
    let mut ver = 0;
    instrs.iter().for_each(|instr| match instr {
        Instruction::Forward(s) => hor += s,
        Instruction::Down(s) => ver += s,
        Instruction::Up(s) => ver -= s,
    });
    hor * ver
}

fn main() {
    let reader = BufReader::new(File::open("input").unwrap());
    let instructions = reader
        .lines()
        .map(|l| parse_instruction(&l.unwrap()))
        .collect();
    println!("{}", position(instructions));
}

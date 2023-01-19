use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
struct BinaryCount {
    one: u32,
    zero: u32,
}

#[derive(Debug)]
enum Bin {
    Zero,
    One,
}

fn bin2digit(bin: &Bin) -> u32 {
    match bin {
        Bin::Zero => 0,
        Bin::One => 1,
    }
}

fn accumulate_diagnostic(acc: &mut Vec<BinaryCount>, binary: Vec<Bin>) {
    let extend_counter = binary.len() - acc.len();
    (0..extend_counter).for_each(|_| acc.push(BinaryCount { one: 0, zero: 0 }));
    binary.iter().enumerate().for_each(|(ind, b)| match b {
        Bin::Zero => acc[ind].zero += 1,
        Bin::One => acc[ind].one += 1,
    });
}
fn power_consumption(acc: Vec<BinaryCount>) -> u32 {
    let gama = acc
        .iter()
        .map(|b| if b.zero > b.one { Bin::Zero } else { Bin::One });
    let epsilon = acc
        .iter()
        .map(|b| if b.zero < b.one { Bin::Zero } else { Bin::One });

    bin2int(gama.collect()) * bin2int(epsilon.collect())
}
fn bin2int(binary: Vec<Bin>) -> u32 {
    binary
        .iter()
        .zip((0..binary.len() as u32).rev())
        .map(|(bin, exp)| bin2digit(bin) * 2_u32.pow(exp))
        .sum()
}

fn parse_binary(input: &str) -> Vec<Bin> {
    let bin = input
        .chars()
        .map(|bin| match bin {
            '0' => Bin::Zero,
            '1' => Bin::One,
            _ => panic!("Banana"),
        })
        .collect();
    bin
}

fn main() {
    let reader = BufReader::new(File::open("input").unwrap());
    let mut res = vec![];
    let line = reader
        .lines()
        .map(|l| parse_binary(&l.unwrap()))
        .for_each(|l| accumulate_diagnostic(&mut res, l));
    println!("{:?}", power_consumption(res));
}

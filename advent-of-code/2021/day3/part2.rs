use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
struct BinaryCount {
    one: u32,
    zero: u32,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Bin {
    Zero,
    One,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Diagnostic {
    Gama,
    Epsilon,
}

fn bin2digit(bin: &Bin) -> u32 {
    match bin {
        Bin::Zero => 0,
        Bin::One => 1,
    }
}

fn accumulate_diagnostic(binary: &Vec<Bin>) -> BinaryCount {
    let mut count = BinaryCount { one: 0, zero: 0 };
    binary.iter().enumerate().for_each(|(ind, b)| match b {
        Bin::Zero => count.zero += 1,
        Bin::One => count.one += 1,
    });
    count
}

fn bin2int(binary: &Vec<Bin>) -> u32 {
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

fn gama_epsilon(bin_count: BinaryCount, diagnostic: Diagnostic) -> Bin {
    let mut res;
    // gama
    if bin_count.one >= bin_count.zero {
        res = Bin::One;
    } else {
        res = Bin::Zero;
    };
    // epsilon
    if diagnostic == Diagnostic::Epsilon {
        if res == Bin::One {
            res = Bin::Zero;
        } else {
            res = Bin::One;
        }
    }
    res
}

fn calc_diagnostic(mut binaries: Vec<Vec<Bin>>, diagnostic: Diagnostic) -> u32 {
    let mut index = 0;

    while binaries.len() > 1 {
        let bins = binaries.iter().map(|l| l[index]).collect();
        let bin_count = accumulate_diagnostic(&bins);
        let win_bin = gama_epsilon(bin_count, diagnostic);
        binaries = binaries
            .into_iter()
            .filter(|b| b[index] == win_bin)
            .collect();
        index += 1;
    }

    bin2int(&binaries[0])
}

fn main() {
    let reader = BufReader::new(File::open("input").unwrap());
    let binaries: Vec<Vec<Bin>> = reader.lines().map(|l| parse_binary(&l.unwrap())).collect();

    println!(
        "{:?}",
        calc_diagnostic(binaries.clone(), Diagnostic::Gama)
            * calc_diagnostic(binaries, Diagnostic::Epsilon)
    );
}

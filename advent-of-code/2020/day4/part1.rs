use std::fs;

#[derive(Ord, Eq, PartialOrd, PartialEq, Clone)]
enum Field {
    Byr,
    Iyr,
    Eyr,
    Hgt,
    Hcl,
    Ecl,
    Pid,
    Cid,
}

use Field::*;

type Passport = Vec<Field>;

fn parse_input(input: &str) -> Vec<Passport> {
    input
        .split("\n\n")
        .map(|p| {
            p.replace('\n', " ")
                .split_whitespace()
                .map(|f| match f.split(':').next().unwrap() {
                    "byr" => Byr,
                    "iyr" => Iyr,
                    "eyr" => Eyr,
                    "hgt" => Hgt,
                    "hcl" => Hcl,
                    "ecl" => Ecl,
                    "pid" => Pid,
                    "cid" => Cid,
                    _ => panic!("no such field"),
                })
                .collect()
        })
        .collect()
}

fn is_valid(pass: &Passport) -> bool {
    let mut loc_pass = pass.to_vec();
    loc_pass.push(Cid);
    loc_pass.sort();
    loc_pass.dedup();
    loc_pass.len() == 8
}

fn main() {
    let input = fs::read_to_string("input").unwrap();
    println!(
        "{}",
        parse_input(&input)
            .iter()
            .filter(|&p| is_valid(&p))
            .collect::<Vec<_>>()
            .len()
    );
}

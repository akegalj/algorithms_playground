use std::fs;

struct PasswordPolicy<'a> {
    min: usize,
    max: usize,
    letter: char,
    pass: &'a str,
}

fn parse_input(input: &str) -> Vec<PasswordPolicy> {
    let policies = input
        .lines()
        .map(|l| {
            let mut iter = l.split_whitespace();
            let mut bounds = iter.next().unwrap().split("-");
            let letter = iter.next().unwrap().chars().next().unwrap();
            let pass = iter.next().unwrap();
            PasswordPolicy {
                min: bounds.next().unwrap().parse().unwrap(),
                max: bounds.next().unwrap().parse().unwrap(),
                letter,
                pass,
            }
        })
        .collect();
    policies
}

fn is_valid(policy: &PasswordPolicy) -> bool {
    let mut pass = policy.pass.chars();
    (pass.nth(policy.min - 1) == Some(policy.letter))
        ^ (pass.nth(policy.max - policy.min - 1) == Some(policy.letter))
}

fn main() {
    let input = fs::read_to_string("input").unwrap();
    println!(
        "{}",
        parse_input(&input)
            .iter()
            .filter(|p| is_valid(p))
            .collect::<Vec<_>>()
            .len()
    );
}

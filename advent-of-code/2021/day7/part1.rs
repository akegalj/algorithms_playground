use std::cmp;
use std::fs;
use std::iter;

fn main() {
    let file = fs::read_to_string("input").unwrap();

    let input: Vec<_> = file.split(",").map(|f| f.parse::<i32>().unwrap()).collect();

    let res = input
        .iter()
        .map(|n| input.iter().fold(0, |acc, b| acc + (b - n).abs()))
        .min()
        .unwrap();

    println!("{}", res);
}

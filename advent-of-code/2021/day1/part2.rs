use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let reader = BufReader::new(File::open("input").unwrap());
    let nums = reader
        .lines()
        .map(|n| n.unwrap().parse::<i32>().unwrap())
        .collect::<Vec<i32>>();
    let inc_count = nums
        .iter()
        .zip(nums.iter().skip(3))
        .filter(|(f, s)| f < s)
        .count();
    println!("{}", inc_count);
}

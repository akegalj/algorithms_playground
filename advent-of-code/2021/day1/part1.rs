use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let reader = BufReader::new(File::open("input").unwrap());
    let num_inc = reader
        .lines()
        .map(|n| n.unwrap().parse::<i32>().unwrap())
        .fold((None, 0), |acc, n| {
            (
                Some(n),
                match acc.0 {
                    Some(prev) if prev < n => acc.1 + 1,
                    _ => acc.1,
                },
            )
        });
    println!("{}", num_inc.1)
}

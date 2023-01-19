use std::cmp::{max, min};
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let reader = BufReader::new(File::open("input").unwrap());
    let coords = reader.lines().map(|l| {
        let s = l.unwrap();
        let (left, right) = s.trim().split_once("|").unwrap();
        let c1 = left
            .split(" ")
            .map(|s| String::from(s.trim()))
            .collect::<Vec<String>>();
        let c2 = right
            .split(" ")
            .map(|s| String::from(s.trim()))
            .collect::<Vec<String>>();
        (c1, c2)
    });

    println!(
        "{:?}",
        coords
            .flat_map(|(_, r)| r.into_iter().map(|s| s.len()))
            .filter(|l| *l == 2 || *l == 4 || *l == 3 || *l == 7)
            .count() //.collect::<Vec<_>>()
    );
}

use std::cmp::{max, min};
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let reader = BufReader::new(File::open("input").unwrap());
    let coords = reader
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut res = 0;
    for y in 0..coords.len() {
        for x in 0..coords.get(y).unwrap().len() {
            let left = if x == 0 {
                u32::MAX
            } else {
                *coords.get(y).unwrap().get(x - 1).unwrap()
            };
            let right = if x == coords.get(y).unwrap().len() - 1 {
                u32::MAX
            } else {
                *coords.get(y).unwrap().get(x + 1).unwrap()
            };
            let up = if y == 0 {
                u32::MAX
            } else {
                *coords.get(y - 1).unwrap().get(x).unwrap()
            };
            let down = if y == coords.len() - 1 {
                u32::MAX
            } else {
                *coords.get(y + 1).unwrap().get(x).unwrap()
            };

            let coord = coords.get(y).unwrap().get(x).unwrap();

            if coord < &up && coord < &down && coord < &left && coord < &right {
                res += (1 + coord);
            }
        }
    }
    println!("{}", res);
}

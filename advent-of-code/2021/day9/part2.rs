use std::cmp::{max, min};
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn basin(mut cave: Vec<Vec<(u32, bool)>>, pos: (usize, usize)) -> u32 {
    let mut visit = vec![pos];
    let mut res = 1;
    while visit.len() > 0 {
        let c = visit.pop().unwrap();
        let num = cave[c.0][c.1].0;
        let up = if c.0 == 0 { 9 } else { cave[c.0 - 1][c.1].0 };
        let down = if c.0 == cave.len() - 1 {
            9
        } else {
            cave[c.0 + 1][c.1].0
        };
        let left = if c.1 == 0 { 9 } else { cave[c.0][c.1 - 1].0 };
        let right = if c.1 == cave[c.0].len() - 1 {
            9
        } else {
            cave[c.0][c.1 + 1].0
        };

        if up != 9 && !cave[c.0 - 1][c.1].1 && up > num {
            visit.push((c.0 - 1, c.1));
            cave[c.0 - 1][c.1].1 = true;
            res += 1;
        }
        if down != 9 && !cave[c.0 + 1][c.1].1 && down > num {
            visit.push((c.0 + 1, c.1));
            cave[c.0 + 1][c.1].1 = true;
            res += 1;
        }
        if left != 9 && !cave[c.0][c.1 - 1].1 && left > num {
            visit.push((c.0, c.1 - 1));
            cave[c.0][c.1 - 1].1 = true;
            res += 1;
        }
        if right != 9 && !cave[c.0][c.1 + 1].1 && right > num {
            visit.push((c.0, c.1 + 1));
            cave[c.0][c.1 + 1].1 = true;
            res += 1;
        }
    }
    res
}

fn main() {
    let reader = BufReader::new(File::open("input").unwrap());
    let coords = reader
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|c| (c.to_digit(10).unwrap(), false))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut res = vec![];

    for y in 0..coords.len() {
        for x in 0..coords[y].len() {
            let left = if x == 0 { u32::MAX } else { coords[y][x - 1].0 };
            let right = if x == coords[y].len() - 1 {
                u32::MAX
            } else {
                coords[y][x + 1].0
            };
            let up = if y == 0 { u32::MAX } else { coords[y - 1][x].0 };
            let down = if y == coords.len() - 1 {
                u32::MAX
            } else {
                coords[y + 1][x].0
            };

            let coord = coords[y][x].0;

            if coord < up && coord < down && coord < left && coord < right {
                res.push(basin(coords.clone(), (y, x)));
            }
        }
    }
    res.sort();

    println!("{:?}", res.iter().rev().take(3).product::<u32>());
}

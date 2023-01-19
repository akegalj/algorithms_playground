use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let reader = BufReader::new(File::open("input").unwrap());
    let mut coords = reader
        .lines()
        .map(|l| {
            let s = l.unwrap();
            let (left, right) = s.trim().split_once("|").unwrap();
            let c1 = left
                .trim()
                .split(" ")
                .map(|s| s.trim().chars().collect::<HashSet<_>>())
                .collect::<Vec<_>>();
            let c2 = right
                .trim()
                .split(" ")
                .map(|s| s.trim().chars().collect::<HashSet<_>>())
                .collect::<Vec<_>>();
            (c1, c2)
        })
        .peekable();

    let mut res = 0;

    while coords.peek().is_some() {
        let mut digit = HashMap::new();
        let (left, right) = coords.next().unwrap();

        // find index for 1,4,7,8
        //                2 4 3 7
        left.iter().for_each(|s| {
            match s.len() {
                2 => digit.insert(1, s),
                4 => digit.insert(4, s),
                3 => digit.insert(7, s),
                7 => digit.insert(8, s),
                _ => None,
            };
            ()
        });

        // find index for 6
        // x.len = 6 && x union 1 len is 7
        left.iter().for_each(|s| {
            if s.len() == 6 && !s.is_superset(digit[&1]) {
                digit.insert(6, s);
            }
        });

        // find index for 9
        // x.len = 6 && x is superset of 4
        left.iter().for_each(|s| {
            if s.len() == 6 && s.is_superset(digit[&4]) {
                digit.insert(9, s);
            }
        });

        // find index for 0
        // x.len = 6 && x is not 6 or 9
        left.iter().for_each(|s| {
            if s.len() == 6 && s != digit[&6] && s != digit[&9] {
                digit.insert(0, s);
            }
        });

        // find index for 5
        // x.len = 5 && x is subset of 6
        left.iter().for_each(|s| {
            if s.len() == 5 && s.is_subset(digit[&6]) {
                digit.insert(5, s);
            }
        });

        // find index for 3
        // x.len = 5 && 1 is subset of 3
        left.iter().for_each(|s| {
            if s.len() == 5 && s.is_superset(digit[&1]) {
                digit.insert(3, s);
            }
        });

        // find index for 2
        // x.len = 5 && x is not 3 or 5
        left.iter().for_each(|s| {
            if s.len() == 5 && s != digit[&3] && s != digit[&5] {
                digit.insert(2, s);
            }
        });

        res += right
            .iter()
            .map(|r| digit.iter().filter(|(_, num)| **num == r).nth(0).unwrap().0)
            .rev()
            .zip(0..)
            .fold(0, |acc, (n, e)| acc + n * 10_i32.pow(e));
    }
    println!("{}", res);
}

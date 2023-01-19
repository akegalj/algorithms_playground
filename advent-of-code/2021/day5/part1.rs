use std::cmp::{max, min};
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let reader = BufReader::new(File::open("input").unwrap());
    let coords = reader.lines().map(|l| {
        let cc = l.unwrap();
        let (left, right) = cc.trim().split_once(" -> ").unwrap();
        let (l_x, l_y) = left.split_once(",").unwrap();
        let (r_x, r_y) = right.split_once(",").unwrap();
        (
            (l_x.parse::<u32>().unwrap(), l_y.parse::<u32>().unwrap()),
            (r_x.parse::<u32>().unwrap(), r_y.parse::<u32>().unwrap()),
        )
    });

    let ho_ve = coords.filter(|c| c.0 .0 == c.1 .0 || c.0 .1 == c.1 .1);

    let mut clouds = ho_ve
        .flat_map(|c| {
            let mut v = vec![];
            for x in min(c.0 .0, c.1 .0)..max(c.0 .0, c.1 .0) + 1 {
                for y in min(c.0 .1, c.1 .1)..max(c.0 .1, c.1 .1) + 1 {
                    v.push((x, y));
                }
            }
            v
        })
        .collect::<Vec<(u32, u32)>>();
    clouds.sort();
    let res = clouds
        .iter()
        .fold(vec![vec![]], |mut v, c| match v.last_mut() {
            None => {
                v.push(vec![c]);
                v
            }
            Some(l) => match l.last_mut() {
                None => {
                    l.push(c);
                    v
                }
                Some(prev) => {
                    if *prev == c {
                        l.push(c);
                        v
                    } else {
                        v.push(vec![c]);
                        v
                    }
                }
            },
        })
        .iter()
        .filter(|c| c.len() > 1)
        .count();

    println!("{}", res);
}

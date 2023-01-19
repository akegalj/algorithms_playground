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

    let ho_ve = coords.filter(|c| {
        c.0 .0 == c.1 .0
            || c.0 .1 == c.1 .1
            || (c.0 .0 as i64 - c.1 .0 as i64).abs() == (c.0 .1 as i64 - c.1 .1 as i64).abs()
    });

    let mut clouds = ho_ve
        .flat_map(|c| {
            let mut v = vec![];
            let xstep_p = c.1 .0 as i32 - c.0 .0 as i32;
            let xstep = xstep_p.signum();
            let ystep_p = c.1 .1 as i32 - c.0 .1 as i32;
            let ystep = ystep_p.signum();

            let mut x = c.0 .0 as i32;
            let mut y = c.0 .1 as i32;
            for _ in 0..=max(xstep_p.abs(), ystep_p.abs()) {
                v.push((x as u32, y as u32));
                x += xstep;
                y += ystep;
            }
            v
        })
        .collect::<Vec<(u32, u32)>>();
    clouds.sort();
    let res = clouds
        .iter()
        // this is .group()
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

use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn sum_unmarked(board: (&[[i8; 5]; 5], &[[bool; 5]; 5])) -> i32 {
    let b = board.0.iter().map(|i| i.iter()).flatten();
    let r = board.1.iter().map(|i| i.iter()).flatten();
    b.zip(r).filter(|(_, r)| !**r).map(|(b, _)| *b as i32).sum()
}

fn main() {
    let reader = BufReader::new(File::open("input").unwrap());
    let mut lines = reader.lines().peekable();
    let rand_temp = lines.next().unwrap().unwrap();
    let rand = rand_temp.split(',').map(|r| r.parse::<i8>().unwrap());
    let mut boards: Vec<_> = vec![];

    while !lines.peek().is_none() {
        lines.next();
        let mut bingo = [[0i8; 5]; 5];
        let res = [[false; 5]; 5];

        for y in 0..=4 {
            let l = lines.next().unwrap().unwrap();
            l.split_whitespace()
                .enumerate()
                .for_each(|(x, n)| bingo[x][y] = n.parse().unwrap());
        }
        boards.push((bingo, res));
    }

    let mut finished: Vec<_> = vec![];

    'outer: for r in rand {
        let fin = finished.clone();
        'inner: for (ind, (b, res)) in boards
            .iter_mut()
            .enumerate()
            .filter(|(i, _)| !fin.iter().map(|(i, _)| i).collect::<Vec<_>>().contains(&i))
        {
            for x in 0..5 {
                for y in 0..5 {
                    if b[x][y] == r {
                        res[x][y] = true;
                        if res[x][0] && res[x][1] && res[x][2] && res[x][3] && res[x][4] {
                            finished.push((ind, r));
                            continue 'inner;
                        }
                        if res[0][y] && res[1][y] && res[2][y] && res[3][y] && res[4][y] {
                            finished.push((ind, r));
                            continue 'inner;
                        }
                    }
                }
            }
        }
    }

    let win = finished.last().unwrap();
    let b = boards[win.0];
    println!(
        "{} {} {}",
        win.1,
        sum_unmarked((&b.0, &b.1)),
        win.1 as i32 * sum_unmarked((&b.0, &b.1))
    );
}

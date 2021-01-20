use std::fs;

#[derive(PartialEq)]
enum Square {
    Open,
    Tree,
}

struct Forest {
    pattern: Vec<Vec<Square>>,
}

impl Forest {
    fn is_tree(&self, x: usize, y: usize) -> Option<bool> {
        if y >= self.pattern.len() {
            None
        } else {
            Some(self.pattern[y][x % self.pattern[0].len()] == Square::Tree)
        }
    }
}

fn parse_forest(input: &str) -> Forest {
    let pattern = input
        .lines()
        .map(|y| {
            y.chars()
                .map(|x| match x {
                    '#' => Square::Tree,
                    '.' => Square::Open,
                    _ => panic!("parse error"),
                })
                .collect()
        })
        .collect();
    Forest { pattern }
}

fn count_trees(forest: &Forest, right: usize, down: usize) -> usize {
    let mut y = 0;
    let mut x = 0;
    let mut count = 0;
    loop {
        x += right;
        y += down;
        match forest.is_tree(x, y) {
            None => break,
            Some(true) => count += 1,
            Some(false) => continue,
        };
    }
    count
}

fn main() {
    let input = fs::read_to_string("input").unwrap();
    let forest = parse_forest(&input);
    let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
    println!(
        "{}",
        slopes
            .iter()
            .map(|&(r, d)| count_trees(&forest, r, d))
            .fold(1, |acc, s| acc * s)
    );
}

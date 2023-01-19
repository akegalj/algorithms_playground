use std::fs;
use std::iter;

fn main() {
    let mut fish: [u64; 9] = [0; 9];
    let file = fs::read_to_string("input").unwrap();

    file.split(",")
        .for_each(|f| fish[f.trim().parse::<usize>().unwrap()] += 1);

    for _ in 0..256 {
        fish[7] += fish[0];
        fish.rotate_left(1);
    }

    println!("{}", fish.iter().sum::<u64>());
}

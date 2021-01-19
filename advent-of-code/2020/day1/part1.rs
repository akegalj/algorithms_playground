use std::fs;

fn main() {
    let input = fs::read_to_string("input").unwrap();
    let lines = input.lines().map(|x| x.parse::<i32>().unwrap());
    let ind_lines = lines.zip(1..).collect::<Vec<_>>();
    for (first, first_ind) in &ind_lines {
        for (second, second_ind) in &ind_lines {
            if first_ind != second_ind && first + second == 2020 {
                println!(
                    "({},{})*({},{})=={}",
                    first,
                    first_ind,
                    second,
                    second_ind,
                    first * second
                );
            }
        }
    }
}

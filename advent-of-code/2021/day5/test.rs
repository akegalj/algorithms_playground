fn main() {
    let mut v = vec![(0, 0), (0, 1)];

    for (_, x) in v.iter_mut() {
        *x = 0;
    }
    println!("{:?}", v);
}
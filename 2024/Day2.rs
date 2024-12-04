use std::error::Error;
use std::io::{self, BufRead, BufReader};

fn ok(a: i32, b: i32, dir: i32) -> bool {
    let diff = a.abs_diff(b);
    let range_cond = (1..=3).contains(&diff);
    let dir_cond = (a - b).signum() == dir;

    range_cond && dir_cond
}

fn ok_line(ints: &[i32], n: usize) -> bool {
    let ints = ints
        .iter()
        .enumerate()
        .filter_map(|(i, x)| if i != n { Some(*x) } else { None })
        .collect::<Vec<i32>>();

    let dir = (ints[0] - ints[1]).signum();
    ints.iter()
        .zip(ints[1..].iter())
        .all(|(a, b)| ok(*a, *b, dir))
}

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());

    let mut n_safe_1 = 0;
    let mut n_safe_2 = 0;

    for line in inp.lines() {
        let line = line?;
        let ints: Vec<i32> = line.split(" ").map(|s| s.parse::<i32>().unwrap()).collect();
        let n = ints.len();

        let safe_1 = ok_line(&ints, n);
        let safe_2 = (0..n).any(|i| {
            let r = ok_line(&ints, i);
            r
        });

        if safe_1 {
            n_safe_1 += 1;
        }

        if safe_1 || safe_2 {
            n_safe_2 += 1
        }
    }

    println!("Part1: {}", n_safe_1);
    println!("Part2: {}", n_safe_2);

    Ok(())
}

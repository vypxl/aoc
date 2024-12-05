use std::error::Error;
use std::io::{self, BufRead, BufReader};

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());
    let mut p1 = 0;
    let mut p2 = 0;

    for line in inp.lines() {
        let line = line?;
        let ints: Vec<i32> = line.split(" ").map(|s| s.parse::<i32>().unwrap()).collect();
    }

    println!("Part1: {}", p1);
    // println!("Part2: {}", p2);

    Ok(())
}


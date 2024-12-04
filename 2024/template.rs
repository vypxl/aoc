use std::error::Error;
use std::io::{self, BufRead, BufReader};

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());
    let p1 = 0;
    let p2 = 0;

    for line in inp.lines() {
        let line = line?;
        let ints: Vec<i32> = line.split(" ").map(|s| s.parse::<i32>().unwrap()).collect();
    }

    println!("Part-1: {}", p1);
    // println!("Part-2: {}", p2);

    Ok(())
}


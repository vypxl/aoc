use std::cmp::Ordering;
use std::error::Error;
use std::io::{self, BufRead, BufReader};

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());
    let mut p1 = 0;
    let mut p2 = 0;

    let mut lookup = [[Ordering::Equal; 100]; 100];

    let mut parsing_order = true;
    for line in inp.lines() {
        let line = line?;
        if line == "" {
            parsing_order = false;
            continue;
        }

        if parsing_order {
            let a: usize = unsafe { line.get_unchecked(0..2).parse().unwrap_unchecked() };
            let b: usize = unsafe { line.get_unchecked(3..5).parse().unwrap_unchecked() };
            lookup[a][b] = Ordering::Less;
            lookup[b][a] = Ordering::Greater;
        } else {
            let n = (line.len() + 1) / 3;
            let mid = n / 2;
            let mut mid_v = 0;
            let mut last = 0;
            let mut ordered = true;

            let mut xs = vec![0; n];

            for i in 0..n {
                let x: usize = unsafe {
                    line.get_unchecked(i * 3..i * 3 + 2)
                        .parse()
                        .unwrap_unchecked()
                };

                if i == mid {
                    mid_v = x;
                }

                if lookup[last][x] == Ordering::Greater {
                    ordered = false;
                }

                last = x;
                xs[i] = x;
            }

            if ordered {
                p1 += mid_v;
            } else {
                xs.sort_unstable_by(|a, b| lookup[*a][*b]);
                p2 += xs[mid];
            }
        }
    }

    println!("Part1: {}", p1);
    println!("Part2: {}", p2);

    Ok(())
}

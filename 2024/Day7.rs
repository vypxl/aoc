use std::error::Error;
use std::io::{self, BufRead, BufReader};

fn check(result: usize, xs: &Vec<usize>, p2: bool) -> bool {
    let mults: Vec<usize> = xs.iter().map(|x| 10_usize.pow(1 + x.ilog10())).collect();
    let mut stack: Vec<(usize, usize)> = Vec::with_capacity(xs.len() * xs.len());

    stack.push((xs[0], 1));
    if p2 {
        stack.push((xs[0] * mults[1] + xs[1], 2));
    }

    while !stack.is_empty() {
        let (acc, pos) = unsafe { stack.pop().unwrap_unchecked() };
        if acc > result {
            continue;
        }

        if acc == result && pos == xs.len() {
            return true;
        }

        if pos >= xs.len() {
            continue;
        }

        let val = unsafe { *xs.get_unchecked(pos) };

        stack.push((acc * val, pos + 1));
        stack.push((acc + val, pos + 1));

        if p2 {
            let c = acc * unsafe { mults.get_unchecked(pos) } + val;
            stack.push((c, pos + 1));
        }
    }

    false
}

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());
    let mut p1 = 0;
    let mut p2 = 0;

    for line in inp.lines() {
        let line = line?;
        let mut s = 0;
        let mut v = 0;
        let mut ints: Vec<usize> = Vec::with_capacity(16);

        for c in line.bytes() {
            match c {
                b'0'..=b'9' => {
                    v = v * 10 + (c - b'0') as usize;
                }
                b':' => {
                    s = v;
                    v = 0;
                }
                b' ' => {
                    if v != 0 {
                        ints.push(v);
                        v = 0;
                    };
                }
                _ => {}
            }
        }
        if s == 0 {
            continue;
        }
        ints.push(v);

        if check(s, &ints, false) {
            p1 += s;
            p2 += s;
        } else if check(s, &ints, true) {
            p2 += s;
        }
    }

    println!("Part1: {}", p1);
    println!("Part2: {}", p2);

    Ok(())
}

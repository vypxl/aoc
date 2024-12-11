use std::collections::HashMap;
use std::error::Error;
use std::io::{self, BufRead, BufReader};

fn parse(s: &[u8]) -> usize {
    let mut n = 0;
    for c in s.iter() {
        n = n * 10 + (c - b'0') as usize;
    }

    n
}

type M = HashMap<usize, (Vec<u8>, usize)>;

fn inc(stones: &mut M, which: usize, n: usize) {
    if let Some(stone) = stones.get_mut(&which) {
        stone.1 += n;
    } else {
        stones.insert(which, (Vec::from(which.to_string().as_bytes()), n));
    }
}

fn dec(stones: &mut M, which: usize, n: usize) {
    let after;
    if let Some(stone) = stones.get_mut(&which) {
        after = stone.1 - n;
        if after > stone.1 {
            panic!("Negative amount");
        }
        stone.1 = after;
    } else {
        return;
    }
}

fn sum(stones: &M) -> usize {
    stones.values().map(|(_, n)| n).sum()
}

fn step(stones: &mut M) {
    let stones_current = stones
        .iter()
        .map(|(stone, (_, amount))| (*stone, *amount))
        .collect::<Vec<(usize, usize)>>();

    for (stone, m) in stones_current {
        if m == 0 {
            continue;
        }

        if stone == 0 {
            inc(stones, 1, m);
        } else if &stones[&stone].0.len() & 1 == 1 {
            let n = stone.checked_mul(2024).unwrap();
            inc(stones, n, m);
        } else {
            let st = &stones[&stone].0;
            let h = st.len() / 2;
            let a = parse(&st[..h]);
            let b = parse(&st[h..]);

            inc(stones, a, m);
            inc(stones, b, m);
        }

        dec(stones, stone, m);
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());

    let line = inp.lines().next().unwrap()?;
    let initial: Vec<usize> = line.split(" ").map(|s| parse(s.as_bytes())).collect();
    let mut stones = M::new();
    for n in initial {
        inc(&mut stones, n, 1);
    }

    for _ in 1..=25 {
        step(&mut stones);
    }

    println!("Part1: {}", sum(&stones));

    for _ in 26..=75 {
        step(&mut stones);
    }

    println!("Part2: {}", sum(&stones));

    Ok(())
}

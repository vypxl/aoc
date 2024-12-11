use std::collections::VecDeque;
use std::error::Error;
use std::io::{self, BufRead, BufReader};

use util::Pos;

mod util;

type Grid = util::Grid<u8>;

fn calc(g: &Grid) -> (usize, usize) {
    let mut p1 = 0;
    let mut p2 = 0;
    let mut q: VecDeque<Pos> = VecDeque::with_capacity(1024);
    let seen_blueprint = util::Grid::new(g.width, g.height, false);
    let mut seen = seen_blueprint.clone();

    for (p, v) in g.items() {
        if *v == b'0' {
            q.push_back(p);

            while !q.is_empty() {
                let pos = unsafe { q.pop_front().unwrap_unchecked() };
                let v = g[&pos];

                for (n, nv) in g.neighbours(&pos) {
                    if nv - 1 != v {
                        continue;
                    }

                    if g[&n] == b'9' {
                        p2 += 1;
                        let was_seen = unsafe { seen.get_unchecked_mut(&n) };
                        if !*was_seen {
                            p1 += 1;
                            *was_seen = true;
                        }
                    } else {
                        q.push_back(n);
                    }
                }
            }

            seen.clone_from(&seen_blueprint);
        }
    }

    (p1, p2)
}

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());

    let mut g: Vec<Vec<u8>> = Vec::new();
    for line in inp.lines() {
        let line = line?;
        g.push(line.bytes().collect());
    }

    let g = Grid::from_vec2d(g);

    let result = calc(&g);
    println!("Part1: {}", result.0);
    println!("Part2: {}", result.1);

    Ok(())
}

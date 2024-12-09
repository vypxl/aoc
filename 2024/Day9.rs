use std::array;
use std::error::Error;
use std::io::{self, BufRead, BufReader};

const FREE: usize = 0xffff_ffff_ffff_ffff;

fn p1(disk: &[usize]) -> usize {
    let mut p1 = 0;

    let mut left = 0;
    let mut right = disk.len() - 1;

    while left <= right {
        let mut id = disk[left];
        if id == FREE {
            id = disk[right];
            right -= 1;
        }
        p1 += left * id;
        left += 1;

        while disk[right] == FREE {
            right -= 1;
        }
    }

    p1
}

fn p2(blocks: &[(usize, usize, usize)]) -> usize {
    let mut free_blocks: [Vec<(usize, usize, usize)>; 10] =
        array::from_fn(|_| Vec::with_capacity(blocks.len() / 2));
    let mut blocks_df: Vec<(usize, usize, usize)> = Vec::with_capacity(blocks.len());

    for (pos, id, len) in blocks.iter().rev() {
        if *id == FREE {
            free_blocks[*len].push((*pos, *id, *len));
        }
    }

    for (pos, id, len) in blocks.iter().rev() {
        if *id == FREE {
            continue;
        }

        let mut choice = (FREE, 0, 0);
        for k in *len..=9 {
            let f = &free_blocks[k];
            if f.is_empty() {
                continue;
            }

            let candidate = f[f.len() - 1];
            if candidate.0 < choice.0 {
                choice = candidate;
            }
        }

        if choice.0 > *pos {
            // No free block available
            blocks_df.push((*pos, *id, *len));
        } else {
            // Move into leftmost space
            free_blocks[choice.2].pop();
            blocks_df.push((choice.0, *id, *len));

            let rest_len = choice.2 - *len;
            if rest_len > 0 {
                free_blocks[rest_len].push((choice.0 + *len, FREE, rest_len));
                free_blocks[rest_len].sort_by_key(|(pos, _, _)| FREE - pos);
            }
        }
    }

    let p2: usize = blocks_df
        .into_iter()
        .map(|(pos, id, len)| ((pos..pos + len).map(|p| p * id)).sum::<usize>())
        .sum();

    p2
}

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());

    let line = inp.lines().next().unwrap()?;
    let mut disk: Vec<usize> = vec![FREE; 9 * line.len()];
    let mut blocks: Vec<(usize, usize, usize)> = Vec::with_capacity(line.len());

    let mut is_block = true;
    let mut id = 0;
    let mut i = 0_usize;

    for c in line.bytes() {
        let len = (c - b'0') as u8;
        if is_block {
            blocks.push((i, id, len as usize));
            disk[i..i+len as usize].fill(id);
            id += 1;
        } else {
            blocks.push((i, FREE, len as usize));
            disk[i..i+len as usize].fill(FREE);
        }

        i += len as usize;
        is_block = !is_block;
    }

    println!("Part1: {}", p1(&disk));
    println!("Part2: {}", p2(&blocks));

    Ok(())
}

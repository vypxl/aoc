use std::error::Error;
use std::io::{self, BufRead, BufReader};

use crate::State::*;
use crate::Token::*;

#[derive(Copy, Clone, Debug)]
enum State {
    NUL,
    _X,
    XM,
    XMA,
    _S,
    SA,
    SAM,
}

#[derive(Copy, Clone, Debug)]
enum Token {
    X,
    M,
    A,
    S,
}

fn next(s: State, t: Token) -> (bool, State) {
    match (s, t) {
        (_X, M) => (false, XM),
        (XM, A) => (false, XMA),
        (XMA, S) => (true, _S),
        (_S, A) => (false, SA),
        (SA, M) => (false, SAM),
        (SAM, X) => (true, _X),
        (_, X) => (false, _X),
        (_, S) => (false, _S),
        _ => (false, NUL),
    }
}

fn parse_char(c: char) -> Token {
    match c {
        'X' => X,
        'M' => M,
        'A' => A,
        'S' => S,
        _ => panic!("invalid char {c}"),
    }
}

fn p1(mat: &[Vec<Token>]) -> usize {
    let mut p1 = 0;
    let w = mat[0].len();
    let h = mat.len();

    let mut state = NUL;

    let mut feed = |o: Option<(usize, usize)>| {
        if let Some((x, y)) = o {
            let (hit, nstate) = next(state, mat[y][x]);
            if hit {
                p1 += 1
            }
            state = nstate
        } else {
            state = NUL
        }
    };

    // Normal
    for y in 0..h {
        for x in 0..w {
            feed(Some((x, y)));
        }
        feed(None);
    }

    // Transposed
    for x in 0..w {
        for y in 0..h {
            feed(Some((x, y)));
        }
        feed(None);
    }

    // Diag
    for offset in 0..(h + w) {
        // Up
        for x in 0..w {
            if (0..h).contains(&(offset - x)) {
                feed(Some((x, offset - x)));
            }
        }
        feed(None);

        // Down
        for x in 0..w {
            if (0..h).contains(&(offset - h + x)) {
                feed(Some((x, offset - h + x)));
            }
        }
        feed(None);
    }

    p1
}

fn p2(mat: &[Vec<Token>]) -> usize {
    let mut p2 = 0;
    let w = mat[0].len();
    let h = mat.len();

    for sx in 0..w - 2 {
        for sy in 0..h - 2 {
            let x1 = (mat[sy][sx], mat[sy + 1][sx + 1], mat[sy + 2][sx + 2]);
            let x2 = (mat[sy][sx + 2], mat[sy + 1][sx + 1], mat[sy + 2][sx]);
            match (x1, x2) {
                ((M, A, S), (M, A, S))
                | ((S, A, M), (M, A, S))
                | ((M, A, S), (S, A, M))
                | ((S, A, M), (S, A, M)) => p2 += 1,
                _ => {}
            }
        }
    }

    p2
}

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());

    let mat = inp
        .lines()
        .map_while(Result::ok)
        .map(|l| l.chars().map(parse_char).collect())
        .collect::<Vec<Vec<Token>>>();

    println!("Part1: {}", p1(&mat));
    println!("Part2: {}", p2(&mat));

    Ok(())
}

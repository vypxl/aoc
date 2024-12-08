use std::array;
use std::error::Error;
use std::fmt::{self, Display};
use std::io::{self, BufRead, BufReader};
use std::ops::{Add, Sub};

#[derive(Clone, Debug)]
struct Grid<T> {
    width: usize,
    height: usize,
    inner: Vec<T>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
struct Pos {
    x: usize,
    y: usize,
}

fn gcd(a: i64, b: i64) -> i64 {
    let mut a = a;
    let mut b = b;

    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }

    a.abs()
}

impl Pos {
    fn simplify(&self) -> Pos {
        let div = gcd(self.x as i64, self.y as i64);
        Self {
            x: ((self.x as i64) / div) as usize,
            y: ((self.y as i64) / div) as usize,
        }
    }

    fn scale(&self, m: usize) -> Pos {
        Self {
            x: self.x * m,
            y: self.y * m,
        }
    }
}

impl Add for Pos {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Add for &Pos {
    type Output = Pos;

    fn add(self, other: &Pos) -> Pos {
        Pos {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub for Pos {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl Sub for &Pos {
    type Output = Pos;

    fn sub(self, other: &Pos) -> Pos {
        Pos {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl Display for Grid<u8> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in self.inner.chunks(self.width) {
            for tile in row {
                write!(f, "{}", *tile as char)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.x as i64, self.y as i64)
    }
}

impl<T: Copy> Grid<T> {
    fn new(width: usize, height: usize, default: T) -> Self {
        let mut inner = Vec::with_capacity(width * height);
        for _ in 0..width * height {
            inner.push(default);
        }

        Self {
            width,
            height,
            inner,
        }
    }
}

impl<T> Grid<T> {
    fn from_vec2d(src: Vec<Vec<T>>) -> Self {
        let height = src.len();
        let width = src[0].len();

        let mut inner = Vec::with_capacity(width * height);

        for row in src {
            inner.extend(row.into_iter());
        }

        Self {
            width,
            height,
            inner,
        }
    }

    #[inline]
    fn is_valid(&self, pos: &Pos) -> bool {
        return pos.x < self.width && pos.y < self.height;
    }

    #[inline]
    unsafe fn get_unchecked(&self, pos: &Pos) -> &T {
        self.inner.get_unchecked(pos.y * self.width + pos.x)
    }

    #[inline]
    unsafe fn set_unchecked(&mut self, pos: &Pos, val: T) {
        let ptr = self.inner.get_unchecked_mut(pos.y * self.width + pos.x);
        *ptr = val;
    }
}

#[inline]
fn check(lookup: &mut Grid<bool>, pos: &Pos) -> usize {
    if lookup.is_valid(&pos) {
        let x = unsafe { *lookup.get_unchecked(&pos) };
        if !x {
            unsafe { lookup.set_unchecked(&pos, true) };
            return 1;
        }
    }

    0
}

fn calc(grid: &Grid<u8>, part2: bool) -> usize {
    let mut antennas: [Vec<Pos>; 256] = array::from_fn(|_| Vec::new());

    for y in 0..grid.height {
        for x in 0..grid.width {
            let v = unsafe { *grid.get_unchecked(&Pos { x, y }) };
            if v != b'.' {
                let positions = unsafe { antennas.get_unchecked_mut(v as usize) };
                positions.push(Pos { x, y });
            }
        }
    }

    let mut res: usize = 0;
    let mut lookup = Grid::new(grid.width, grid.height, false);
    for (_, positions) in antennas.iter().enumerate() {
        if positions.is_empty() {
            continue;
        }

        for p1 in positions {
            for p2 in positions {
                if p1 == p2 {
                    continue;
                }

                let atob = p2 - p1;

                if !part2 {
                    let anti1 = p1 - &atob;
                    let anti2 = p2 + &atob;

                    res += check(&mut lookup, &anti1);
                    res += check(&mut lookup, &anti2);
                } else {
                    let dir = atob.simplify();

                    for i in 1_usize.. {
                        let scaled = dir.scale(i);
                        let up = p1 + &scaled;
                        let down = p1 - &scaled;

                        res += check(&mut lookup, &up) + check(&mut lookup, &down);

                        if (!grid.is_valid(&down)) && (!grid.is_valid(&up)) {
                            break;
                        }
                    }
                }
            }
        }
    }

    res
}

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());

    let mut g: Vec<Vec<u8>> = vec![];
    for line in inp.lines() {
        let line = line?;
        g.push(line.as_bytes().into());
    }

    let grid = Grid::from_vec2d(g);

    println!("Part1: {}", calc(&grid, false));
    println!("Part2: {}", calc(&grid, true));

    Ok(())
}

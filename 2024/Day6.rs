use std::error::Error;
use std::fmt::{self, Display};
use std::io::{self, BufRead, BufReader};

#[derive(Debug, Clone, Copy, PartialEq)]
struct Tile(u8);

const FREE: Tile = Tile(0);
const OBSTACLE: Tile = Tile(0x10);

impl Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FREE => write!(f, " "),
            OBSTACLE => write!(f, "#"),
            Tile(which) => {
                let has_up = which & Dir::Up as u8 != 0;
                let has_down = which & Dir::Down as u8 != 0;
                let has_left = which & Dir::Left as u8 != 0;
                let has_right = which & Dir::Right as u8 != 0;

                let has_horizontal = has_left || has_right;
                let has_vertical = has_up || has_down;

                if has_horizontal && has_vertical {
                    write!(f, "┼")
                } else if has_horizontal {
                    write!(f, "─")
                } else if has_vertical {
                    write!(f, "│")
                } else {
                    write!(f, "!")
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
enum Dir {
    Up = 1,
    Down = 2,
    Left = 4,
    Right = 8,
}

impl Display for Dir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Dir::Up => write!(f, "^"),
            Dir::Down => write!(f, "v"),
            Dir::Left => write!(f, "<"),
            Dir::Right => write!(f, ">"),
        }
    }
}

impl Dir {
    fn turn(&self) -> Dir {
        match self {
            Dir::Up => Dir::Right,
            Dir::Right => Dir::Down,
            Dir::Down => Dir::Left,
            Dir::Left => Dir::Up,
        }
    }
}

#[derive(Clone, Debug)]
struct Grid {
    width: usize,
    height: usize,
    inner: Vec<Tile>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
struct Pos {
    x: usize,
    y: usize,
    dir: Dir,
}

impl Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in self.inner.chunks(self.width) {
            for tile in row {
                write!(f, "{}", tile)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {}) {}", self.x, self.y, self.dir)
    }
}

impl Grid {
    fn from_vec2d(src: Vec<Vec<Tile>>) -> Self {
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
    unsafe fn get_unchecked(&self, pos: &Pos) -> &Tile {
        self.inner.get_unchecked(pos.y * self.width + pos.x)
    }

    #[inline]
    unsafe fn set_unchecked(&mut self, pos: &Pos, val: Tile) {
        let ptr = self.inner.get_unchecked_mut(pos.y * self.width + pos.x);
        *ptr = val;
    }

    unsafe fn visit(&mut self, pos: &Pos) -> bool {
        let v = *self.get_unchecked(pos);
        self.set_unchecked(pos, Tile(v.0 | pos.dir as u8));
        v.0 & pos.dir as u8 != 0
    }
}

impl Pos {
    fn new(x: usize, y: usize, dir: Dir) -> Self {
        Self { x, y, dir }
    }

    fn step(&self) -> Self {
        match self.dir {
            Dir::Up => Pos::new(self.x, self.y - 1, self.dir),
            Dir::Down => Pos::new(self.x, self.y + 1, self.dir),
            Dir::Left => Pos::new(self.x - 1, self.y, self.dir),
            Dir::Right => Pos::new(self.x + 1, self.y, self.dir),
        }
    }

    fn turn(&self) -> Self {
        Pos::new(self.x, self.y, self.dir.turn())
    }
}

fn sim(grid: &mut Grid, p: Pos) -> (usize, bool) {
    let mut pos = p;

    let mut n_visits = 0;

    while grid.is_valid(&pos) {
        let t = unsafe { grid.get_unchecked(&pos) };
        if *t == FREE {
            n_visits += 1;
        }

        if unsafe { grid.visit(&pos) } {
            return (n_visits, true);
        }

        while unsafe { *grid.get_unchecked(&pos.step()) } == OBSTACLE {
            pos = pos.turn();
        }

        pos = pos.step()
    }

    (n_visits, false)
}

fn p1(mut g: Grid, p: Pos) -> usize {
    sim(&mut g, p).0
}

fn p2(mut grid: Grid, p: Pos) -> usize {
    let mut res = 0;

    let mut pos = p;
    let mut g2 = grid.clone();
    
    while grid.is_valid(&pos) {
        let mut next = pos.step();
        let mut next_tile = unsafe { *grid.get_unchecked(&next) };
        
        while next_tile == OBSTACLE {
            pos = pos.turn();
            next = pos.step();
            next_tile = unsafe { *grid.get_unchecked(&next) };
        }

        if next_tile == FREE {
            g2.clone_from(&grid);
            unsafe { g2.set_unchecked(&next, OBSTACLE) };

            if sim(&mut g2, pos).1 {
                res += 1;
            }
        }

        unsafe { grid.visit(&pos) };
        pos = next;
    }

    res
}

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());

    let mut grid: Vec<Vec<Tile>> = Vec::new();

    let mut pos = Pos::new(0, 0, Dir::Up);

    let mut y = 0;
    for line in inp.lines() {
        let line = line?;
        let ints: Vec<Tile> = line
            .bytes()
            .enumerate()
            .map(|(x, c)| match c {
                b'.' => FREE,
                b'#' => OBSTACLE,
                b'^' => {
                    pos = Pos::new(x, y, Dir::Up);
                    FREE
                }
                _ => FREE,
            })
            .collect();
        grid.push(ints);
        y += 1;
    }

    let grid = Grid::from_vec2d(grid);

    println!("Part1: {}", p1(grid.clone(), pos));
    println!("Part2: {}", p2(grid, pos));

    Ok(())
}

use std::error::Error;
use std::fmt::{self, Display};
use std::io::{self, BufRead, BufReader};

#[derive(Debug, Clone, Copy, PartialEq)]
enum Tile {
    Free,
    Obstacle,
    Visited(u8),
}

impl Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Tile::Free => write!(f, " "),
            Tile::Obstacle => write!(f, "#"),
            Tile::Visited(which) => {
                // write!(
                //     f,
                //     "{}",
                //     [
                //         "*", "╵", "╷", "│", "╴", "╯", "╮", "┤", "╶", "╰", "╭", "├", "─", "┴", "┬",
                //         "┼",
                //     ][*which as usize]
                // )
                let has_up = *which & Dir::Up as u8 != 0;
                let has_down = *which & Dir::Down as u8 != 0;
                let has_left = *which & Dir::Left as u8 != 0;
                let has_right = *which & Dir::Right as u8 != 0;

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
struct Grid(Vec<Vec<Tile>>);
#[derive(Clone, Copy, PartialEq, Debug)]
struct Pos {
    x: usize,
    y: usize,
    dir: Dir,
}

impl Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in &self.0 {
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
    fn get(&self, pos: &Pos) -> Option<Tile> {
        self.0
            .get(pos.y)
            .and_then(|row| row.get(pos.x).and_then(|tile| Some(*tile)))
    }

    fn set(&mut self, pos: &Pos, val: Tile) {
        self.0[pos.y][pos.x] = val;
    }

    fn visit(&mut self, pos: &Pos) -> bool {
        let v = self.get(pos).unwrap();
        if let Tile::Visited(which) = v {
            self.set(pos, Tile::Visited(which as u8 | pos.dir as u8));
            return which & pos.dir as u8 != 0;
        } else {
            self.set(pos, Tile::Visited(pos.dir as u8));
            return false;
        }
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

    loop {
        if let Some(t) = grid.get(&pos) {
            if t == Tile::Free {
                n_visits += 1;
            }

            if grid.visit(&pos) {
                return (n_visits, true);
            }

            while grid.get(&pos.step()) == Some(Tile::Obstacle) {
                pos = pos.turn();
            }

            pos = pos.step()
        } else {
            break;
        }
    }

    (n_visits, false)
}

fn p1(g: Grid, p: Pos) -> usize {
    let mut g = g;
    sim(&mut g, p).0
}

fn p2(g: Grid, p: Pos) -> usize {
    let mut res = 0;
    let mut grid = g.clone();

    let _ = sim(&mut grid, p);

    for y in 0..g.0.len() {
        for x in 0..g.0[y].len() {
            if p.x == x && p.y == y {
                continue;
            }
            if let Tile::Visited(_) = grid.0[y][x] {
                // check this tile for a possible obstacle position
                let mut g2 = g.clone();
                let p2 = Pos::new(x, y, Dir::Up);
                g2.set(&p2, Tile::Obstacle);

                if sim(&mut g2, p).1 {
                    res += 1;
                }
            }
        }
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
                b'.' => Tile::Free,
                b'#' => Tile::Obstacle,
                b'^' => {
                    pos = Pos::new(x, y, Dir::Up);
                    Tile::Free
                }
                _ => Tile::Free,
            })
            .collect();
        grid.push(ints);
        y += 1;
    }

    let grid = Grid(grid);

    println!("Part1: {}", p1(grid.clone(), pos));
    println!("Part2: {}", p2(grid, pos));

    Ok(())
}

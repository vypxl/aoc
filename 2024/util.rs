use core::fmt;
use std::{
    fmt::Display,
    ops::{Add, Index, IndexMut, Sub},
};

#[derive(Clone, Debug)]
pub struct Grid<T> {
    pub width: usize,
    pub height: usize,
    inner: Vec<T>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Pos {
    pub x: usize,
    pub y: usize,
}

pub const UP: Pos = Pos {
    x: 0,
    y: (-1isize) as usize,
};
pub const DOWN: Pos = Pos { x: 0, y: 1 };
pub const LEFT: Pos = Pos {
    x: (-1isize) as usize,
    y: 0,
};
pub const RIGHT: Pos = Pos { x: 1, y: 0 };

pub const DIRS: [Pos; 4] = [UP, DOWN, LEFT, RIGHT];

pub fn gcd(a: i64, b: i64) -> i64 {
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
    pub fn simplify(&self) -> Pos {
        let div = gcd(self.x as i64, self.y as i64);
        Self {
            x: ((self.x as i64) / div) as usize,
            y: ((self.y as i64) / div) as usize,
        }
    }

    pub fn scale(&self, m: usize) -> Pos {
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

impl<T> Index<&Pos> for Grid<T> {
    type Output = T;

    fn index(&self, pos: &Pos) -> &T {
        unsafe { self.get_unchecked(pos) }
    }
}

impl<T> IndexMut<&Pos> for Grid<T> {
    fn index_mut(&mut self, pos: &Pos) -> &mut T {
        unsafe { self.get_unchecked_mut(pos) }
    }
}

impl<T: Copy> Grid<T> {
    pub fn new(width: usize, height: usize, default: T) -> Self {
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

    #[inline]
    pub fn items<'a>(&'a self) -> impl Iterator<Item = (Pos, &'a T)> + 'a {
        self.indices()
            .map(move |p| (p, unsafe { self.get_unchecked(&p) }))
    }
}

impl<T> Grid<T> {
    pub fn from_vec2d(src: Vec<Vec<T>>) -> Self {
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
    pub fn is_valid(&self, pos: &Pos) -> bool {
        return pos.x < self.width && pos.y < self.height;
    }

    #[inline]
    pub fn indices(&self) -> impl Iterator<Item = Pos> {
        let w = self.width;
        let h = self.height;
        (0..w).flat_map(move |x| (0..h).map(move |y| Pos { x, y }))
    }

    #[inline]
    pub fn neighbours<'a>(&'a self, pos: &'a Pos) -> impl Iterator<Item = (Pos, &'a T)> + 'a {
        DIRS.iter()
            .map(move |dir| pos + dir)
            .filter(|p| self.is_valid(p))
            .map(|p| (p, unsafe { self.get_unchecked(&p) }))
    }

    #[inline]
    pub unsafe fn get_unchecked(&self, pos: &Pos) -> &T {
        self.inner.get_unchecked(pos.y * self.width + pos.x)
    }

    #[inline]
    pub unsafe fn get_unchecked_mut(&mut self, pos: &Pos) -> &mut T {
        self.inner.get_unchecked_mut(pos.y * self.width + pos.x)
    }

    #[inline]
    pub unsafe fn set_unchecked(&mut self, pos: &Pos, val: T) {
        let ptr = self.inner.get_unchecked_mut(pos.y * self.width + pos.x);
        *ptr = val;
    }
}

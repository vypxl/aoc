use std::error::Error;
use std::io::{self, BufRead, BufReader, Read};

#[derive(Debug, Copy, Clone)]
enum State {
    NUL,
    D,
    DO,
    DOl,
    DON,
    DON_,
    DON_T,
    DON_Tl,
    M,
    MU,
    MUL,
    MUL1,
    MUL2,
}

fn main() -> Result<(), Box<dyn Error>> {
    let inp = BufReader::new(io::stdin());
    let mut p1 = 0;
    let mut p2 = 0;

    let mut state = State::NUL;
    let mut active = true;
    let mut num1 = 0;
    let mut num2 = 0;

    for c in inp.bytes() {
        let c = c?;

        state = match state {
            State::NUL => match c {
                b'd' => State::D,
                b'm' => State::M,
                _ => State::NUL,
            },
            State::D => match c {
                b'o' => State::DO,
                b'd' => State::D,
                b'm' => State::M,
                _ => State::NUL,
            },
            State::DO => match c {
                b'(' => State::DOl,
                b'n' => State::DON,
                b'd' => State::D,
                b'm' => State::M,
                _ => State::NUL,
            },
            State::DOl => match c {
                b')' => {
                    active = true;
                    State::NUL
                }
                b'd' => State::D,
                b'm' => State::M,
                _ => State::DOl,
            },
            State::DON => match c {
                b'\'' => State::DON_,
                b'd' => State::D,
                b'm' => State::M,
                _ => State::NUL,
            },
            State::DON_ => match c {
                b't' => State::DON_T,
                b'd' => State::D,
                b'm' => State::M,
                _ => State::NUL,
            },
            State::DON_T => match c {
                b'(' => State::DON_Tl,
                b'd' => State::D,
                b'm' => State::M,
                _ => State::NUL,
            },
            State::DON_Tl => match c {
                b')' => {
                    active = false;
                    State::NUL
                }
                b'd' => State::D,
                b'm' => State::M,
                _ => State::DON_Tl,
            },
            State::M => match c {
                b'u' => State::MU,
                b'd' => State::D,
                b'm' => State::M,
                _ => State::NUL,
            },
            State::MU => match c {
                b'l' => State::MUL,
                b'd' => State::D,
                b'm' => State::M,
                _ => State::NUL,
            },
            State::MUL => match c {
                b'(' => {
                    num1 = 0;
                    State::MUL1
                }
                b'd' => State::D,
                b'm' => State::M,
                _ => State::NUL,
            },
            State::MUL1 => match c {
                b'0'..=b'9' => {
                    num1 *= 10;
                    num1 += (c - b'0') as i32;
                    State::MUL1
                }
                b',' => {
                    num2 = 0;
                    State::MUL2
                }
                b'd' => State::D,
                b'm' => State::M,
                _ => State::NUL,
            },
            State::MUL2 => match c {
                b'0'..=b'9' => {
                    num2 *= 10;
                    num2 += (c - b'0') as i32;
                    State::MUL2
                }
                b')' => {
                    let prod = num1 * num2;

                    p1 += prod;

                    if active {
                        p2 += prod;
                    }

                    State::NUL
                }
                b'd' => State::D,
                b'm' => State::M,
                _ => State::NUL,
            },
        };
    }

    println!("Part1: {}", p1);
    println!("Part2: {}", p2);

    Ok(())
}

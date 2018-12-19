fn main() {
    // Reverse engineered from input, sums up all factors of the inputs 860 (part 1) and 10551260 (part 2)
    let factorsum = |n|(1..(n as f64).sqrt() as i32+1).fold(0,|a,x|if n%x==0{a+x+n/x}else{a});
    println!("Solution for part 1:\n{}", factorsum(860));
    println!("Solution for part 2:\n{}", factorsum(10551260));
}

// Solution part 1: 1848
// Solution part 2: 22157688

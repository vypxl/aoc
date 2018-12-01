package main

import (
    "fmt"
    "time"
)

func main() {
    start := time.Now()
    fmt.Println(p1(40000000))
    el := time.Since(start)
    fmt.Println("took", el)
    start = time.Now()
    fmt.Println(p2(5000000))
    el = time.Since(start)
    fmt.Println("took", el)
}

func p1(iters int) int {
    startA := 289
    startB := 629
    facA := 16807
    facB := 48271
    divBy := 2147483647

    a := startA
    b := startB
    counter := 0

    for i := 0; i < iters; i++ {
        a = (a * facA) % divBy
        b = (b * facB) % divBy
        if a & 0xffff == b & 0xffff {
            counter++
        }
    }

    return counter
}

func p2(iters int) int {

    startA := 289
    startB := 629
    //startA := 65
    //startB := 8921
    facA := 16807
    facB := 48271
    divBy := 2147483647

    a := startA
    b := startB

    as := make([]int, 0)
    bs := make([]int, 0)

    for len(as) < iters {
        a = (a * facA) % divBy
        if a % 4 == 0 {
            as = append(as, a)
        }
    }
    for len(bs) < iters {
       b = (b * facB) % divBy
       if b % 8 == 0 {
           bs = append(bs, b)
       }
    }

    counter := 0

    for i := 0; i < iters; i++ {
        if as[i] & 0xffff == bs[i] & 0xffff {
            counter++
        }
    }

    return counter
}
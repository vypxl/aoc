package main

import (
    "fmt"
    "io/ioutil"
    "strings"
    "strconv"
    "sync"
)

func p1(input []string) {
    registers := map[byte]int{}
        str := "abcdefghijklmnopqrstuvwxyz"

        for i, _ := range str {
            registers[str[i]] = 0
        }

        lastsound := 0
        i := 0;
        for i < len(input) && i > -1 {
            l := input[i]
            split := strings.Split(l, " ")
            offset := 1

            var v1 int
            fst, err1 := strconv.Atoi(split[1])
            if err1 == nil {
                v1 = fst
            } else {
                v1 = registers[split[1][0]]
            }

            var v2 int
            if len(split) > 2 {
                snd, err2 := strconv.Atoi(split[2])
                if err2 == nil {
                    v2 = snd
                } else {
                    v2 = registers[split[2][0]]
                }
            }

            switch split[0] {
            case "snd":
                //playsound..
                lastsound = v1
            case "set":
                r := split[1][0]
                registers[r] = v2
            case "add":
                r := split[1][0]
                registers[r] += v2
            case "mul":
                r := split[1][0]
                registers[r] *= v2
            case "mod":
                r := split[1][0]
                registers[r] %= v2
            case "rcv":
                if v1 != 0 {
                    fmt.Printf("rcv: %d\n", lastsound)
                    i = -99999
                }
            case "jgz":
                if v1 > 0 {
                    offset = v2
                }
            }
            i += offset
            //fmt.Println(i, v1, v2, registers['a'])

        }

        fmt.Printf("i: %d, regs: %v\n", i, registers)
}

func run2(input []string, id int, c0 chan int64, c1 chan int64, wg *sync.WaitGroup) {
    defer wg.Done()
    registers := map[byte]int64{}
    str := "abcdefghijklmnopqrstuvwxyz"
    for i, _ := range str {
        registers[str[i]] = 0
    }
    registers['p'] = int64(id)

    counter := 0

    i := int64(0)
    for i < int64(len(input)) && i > -1 {
        l := input[i]
        split := strings.Split(l, " ")
        offset := int64(1)

        var v1 int64
        fst, err1 := strconv.Atoi(split[1])
        if err1 == nil {
            v1 = int64(fst)
        } else {
            v1 = registers[split[1][0]]
        }

        var v2 int64
        if len(split) > 2 {
            snd, err2 := strconv.Atoi(split[2])
            if err2 == nil {
                v2 = int64(snd)
            } else {
                v2 = registers[split[2][0]]
            }
        }

        switch split[0] {
        case "snd":

            if id == 1 {
                counter++
                fmt.Printf("id %d: %d\n", id, counter)
            }
            if id == 0 {
                c1 <- v1
            } else {
                c0 <- v1
            }
        case "set":
            r := split[1][0]
            registers[r] = v2
        case "add":
            r := split[1][0]
            registers[r] += v2
        case "mul":
            r := split[1][0]
            registers[r] *= v2
        case "mod":
            r := split[1][0]
            registers[r] %= v2
        case "rcv":
            r := split[1][0]
            var received int64
            if id == 0 {
                received = <- c0
            } else {
                received = <- c1
            }
            registers[r] = received
        case "jgz":
            if v1 > 0 {
                offset = v2
            }
        }
        i += offset
        //fmt.Println(i, v1, v2, registers['a'])
    }
    fmt.Printf("id: %d, i: %d, regs: %v\n", id, i, registers)
}

func p2(input []string) {

    c0 := make(chan int64, 8192)
    c1 := make(chan int64, 8192)
    var wg sync.WaitGroup

    wg.Add(2)

    go run2(input, 0, c0, c1, &wg)
    go run2(input, 1, c0, c1, &wg)
    wg.Wait()
}

func main() {

    r, _ := ioutil.ReadFile("input18.txt")
    input := strings.Split(string(r), "\n")
    //input := strings.Split("snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d", "\n")
    fmt.Println(input)

    p1(input)
    fmt.Println("           ----------          ")
    p2(input)

    //7367 too high
}
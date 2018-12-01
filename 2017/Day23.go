package main

import (
    "fmt"
    "io/ioutil"
    "strconv"
    "strings"
)

func p1(input []string) {
    registers := map[byte]int{}
        str := "abcdefgh"

        for i := range str {
            registers[str[i]] = 0
        }

        mulCounter := 0
        i := 0
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
            case "set":
                r := split[1][0]
                registers[r] = v2
            case "sub":
                r := split[1][0]
                registers[r] -= v2
            case "mul":
                mulCounter++
                r := split[1][0]
                registers[r] *= v2
            case "jnz":
                if v1 != 0 {
                    offset = v2
                }
            }
            i += offset
            //fmt.Println(i, v1, v2, split, registers)

        }

        fmt.Printf("i: %d, regs: %v, counter: %d\n", i, registers, mulCounter)
}

func p2_() {
    b, c, d, f, h := int64(0), int64(0), int64(0), int64(0), int64(0)
    // /^...^/
    b = 107900
    c = 124900
    for {//l1 := true; l1; l1 = b != c {
        f = 1
        d = 2
        for l2 := true; l2; l2 = d != b {
            if b % d == 0 {
                f = 0
                break
            }
            d++
        }
        if f == 0 {
            h++
        }

        if b == c {
            fmt.Println(h)
            break
        }

        b += 17
        fmt.Printf("b: %d, d: %d, f: %d, h: %d\n", b, d, f, h)
    }
}

func main() {

    r, _ := ioutil.ReadFile("input23.txt")
    input := strings.Split(string(r), "\n")
    //input := strings.Split("snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d", "\n")
    //fmt.Println(input)

    p1(input)
    fmt.Println("           ----------          ")
    p2_()
}

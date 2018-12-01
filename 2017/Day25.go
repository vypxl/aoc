package main

import (
	"fmt"
)

type tape map[int]byte

const (
	A = 0
	B = 1
	C = 2
	D = 3
	E = 4
	F = 5
)

const OPS = 12586542

type status struct {
	state int
	pos int
	t tape
}

func action(s *status) {

	write := func(v byte) { (*s).t[s.pos] = v }
	moveR := func() { s.pos++ }
	moveL := func() { s.pos-- }
	set := func(v int) { s.state = v }

	val, ok := (*s).t[s.pos]
	if !ok {
		write(0)
		val = 0
	}
	zero := val == 0

	switch s.state {

	case A:
		if zero {
			write(1)
			moveR()
			set(B)
		} else {
			write(0)
			moveL()
			set(B)
		}
	case B:
		if zero {
			write(0)
			moveR()
			set(C)
		} else {
			write(1)
			moveL()
			set(B)
		}
	case C:
		if zero {
			write(1)
			moveR()
			set(D)
		} else {
			write(0)
			moveL()
			set(A)
		}
	case D:
		if zero {
			write(1)
			moveL()
			set(E)
		} else {
			write(1)
			moveL()
			set(F)
		}
	case E:
		if zero {
			write(1)
			moveL()
			set(A)
		} else {
			write(0)
			moveL()
			set(D)
		}
	case F:
		if zero {
			write(1)
			moveR()
			set(A)
		} else {
			write(1)
			moveL()
			set(E)
		}
	}
}

func main() {
	s := status{ A, 0, map[int]byte{} }

	for i := 0; i < OPS; i++ {
		action(&s)
	}

	sum := 0

	for _, v := range s.t {
		sum += int(v)
	}

	fmt.Printf("Checksum: %d", sum)
}

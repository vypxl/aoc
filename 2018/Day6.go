package main

import (
	"fmt"
	"io/ioutil"
	"strings"
	"strconv"
)

type coord struct {
	x, y, size int
	invalid bool
}

func (self *coord) dist(other coord) int {
	return abs(self.x - other.x) + abs(self.y - other.y)
}

type point struct {
	nearest *coord
	dist int
}

func main() {
	// Input parsing

	file, _ := ioutil.ReadFile("6.in")
	in := strings.Split(string(file), "\n")

	coords := make([]coord, 0)
	for _, l := range in {
		if len(l) == 0 {
			continue;
		}
		s := strings.Split(l, ", ")
		x, _ := strconv.Atoi(s[0])
		y, _ := strconv.Atoi(s[1])
		coords = append(coords, coord { x, y, 0, false })
	}

	// Part 1

	// Get the minimum and maximum x's and y's
	min := findminmax(coords, func(a int, b int) bool { return a < b })
	max := findminmax(coords, func(a int, b int) bool { return a > b })
	max.x += 1
	max.y += 1

	grid := make([][]point, max.x)

	// Fill the grid with dummy data
	for x := 0; x < max.x; x++ {
		grid[x] = make([]point, max.y)
		for y := 0; y < max.y; y++ {
			grid[x][y].dist = (max.x + max.y) * 10
		}
	}

	// Assign each point its nearest coordinate or a dummy one if two or more are competing
	for i, c := range coords {
		for x := 0; x < max.x; x++ {
			for y := 0; y < max.y; y++ {
				d := c.dist(coord{x, y, 0, false})

				if grid[x][y].dist > d {
					grid[x][y].nearest = &coords[i]
					grid[x][y].dist = d
				} else if grid[x][y].dist == d {
					grid[x][y].nearest = &coord{0, 0, 0, true}
				}
			}
		}
	}

	// Filter out invalids and count the size of each area
	for x := 0; x < max.x; x++ {
		for y := 0; y < max.y; y++ {
			if x < min.x || y < min.y || x > (max.x - 2) || (y > max.y - 2) {
				grid[x][y].nearest.invalid = true
			} else {
				grid[x][y].nearest.size++
			}
		}
	}

	// Select the largest area
	sol1 := coords[0]
	for _, c := range coords {
		if sol1.size < c.size && !c.invalid {
			sol1 = c
		}
	}

	fmt.Println("Solution to part 1:")
	fmt.Println(sol1.size)

	// Part 2 - Just sum up the distances and count the valid points
	sol2 := 0;
	for x := min.x; x < max.x-1; x++ {
		for y := min.y; y < max.y-1; y++ {
			sum := 0;
			for _, c := range coords {
				sum += c.dist(coord{ x, y, 0, false })
			}
			if sum < 10000 {
				sol2++
			}
		}
	}
	fmt.Println("Solution to part 2:")
	fmt.Println(sol2)

}

func findminmax(coords []coord, compare func(int, int) bool) coord {
	res := coords[0]
	for _, c := range coords {
		if compare(c.x, res.x) {
			res.x = c.x
		}
		if compare(c.y, res.y) {
			res.y = c.y
		}
	}
	return res
}

func abs(n int) int {
	y := n >> (strconv.IntSize - 1)
	return (n ^ y) - y
}

// Solution part 1: 4342
// Solution part 2: 42966

package day7

import (
	"sleepyswords/advent/day"
	"strings"
)

type Block int

const (
	Start Block = iota
	Empty
	Splitter
)

func Subscribe(m map[int]func() day.Day) {
	m[len(m)] = func() day.Day {
		return &Day7{}
	}
}

type Day7 struct {
	grid [][]Block
}

func (d *Day7) Part1() any {
	size := len(d.grid[0])
	total := 0
	state := make([]bool, size)

	for _, l := range d.grid {
		for i, c := range l {
			switch c {
			case Start:
				state[i] = true
			case Splitter:
				if state[i] == true {
					total++
					if i-1 >= 0 {
						state[i-1] = true
					}
					state[i] = false
					if i+1 < len(state) {
						state[i+1] = true
					}
				}
			}
		}
	}
	return total
}

func (d *Day7) Part2() any {
	total := 0
	size := len(d.grid[0])
	// If a light originates at x, how many paths can this light take
	// Initialise at 1, as originally all lights can take one path.
	dp := make([]int, size)
	for i := range len(dp) {
		dp[i] = 1
	}

	for i := size - 1; i >= 0; i-- {
		// Copy over the original array, if light originates at x, it would be the same,
		// unless a splitter was there, so must copy over.
		n_dp := dp
		l := d.grid[i]

		for i, c := range l {
			switch c {
			case Start:
				total = n_dp[i]
			case Splitter:
				// If we hit a splitter, get the paths the light takes for the left and right side
				// and update the dp array.
				n_dp[i] = dp[i-1] + dp[i+1]
			}
		}
		dp = n_dp
	}

	return total
}

func (d *Day7) Parse(buf []byte) {
	st := string(buf)
	input := strings.Split(st, "\n")

	grid := [][]Block{}

	for _, line := range input {
		if 0 == len(line) {
			continue
		}
		l := []Block{}
		for _, x := range line {
			switch x {
			case '.':
				l = append(l, Empty)
			case 'S':
				l = append(l, Start)
			case '^':
				l = append(l, Splitter)

			}
		}
		grid = append(grid, l)
	}

	d.grid = grid
	// fmt.Println(d.grid)
}

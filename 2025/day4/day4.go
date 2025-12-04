package day4

import (
	"sleepyswords/advent/day"
	"strings"
)

const (
	Empty = iota
	Roll
)

func Subscribe(m map[int]func() day.Day) {
	m[len(m)] = func() day.Day {
		return &Day4{}
	}
}

type Day4 struct {
	grid [][]byte
}

func (d *Day4) isValid(x int, y int) int {
	count := 0
	for i := -1; i <= 1; i++ {
		for j := -1; j <= 1; j++ {
			if i == 0 && j == 0 {
				continue
			}
			if x+i < 0 || x+i >= len(d.grid[0]) {
				continue
			}
			if y+j < 0 || y+j >= len(d.grid) {
				continue
			}

			if d.grid[y+j][x+i] == Roll {
				count++
			}
		}
	}
	return count
}

func (d *Day4) Part1() any {
	accessibleRolls := 0
	for y, row := range d.grid {
		for x, c := range row {
			if c == Roll {
				if d.isValid(x, y) < 4 {
					accessibleRolls += 1
				}
			}
		}
	}
	return accessibleRolls
}

func (d *Day4) Part2() any {
	removedRolls := 0
	for {
		total := 0
		for y, row := range d.grid {
			for x, c := range row {
				if c == Roll {
					if d.isValid(x, y) < 4 {
						total += 1
						// Yea probably should make a copy at the start
						// but whatever it is part 2
						d.grid[y][x] = Empty
					}
				}
			}
		}
		removedRolls += total
		if total == 0 {
			break
		}
	}
	return removedRolls
}

func (d *Day4) Parse(buf []byte) {
	st := string(buf)
	banks := strings.Split(st, "\n")

	parsedGrid := [][]byte{}

	for _, line := range banks {
		grid := []byte{}
		line := strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		for i := 0; i < len(line); i++ {
			if line[i] == '.' {
				grid = append(grid, Empty)
			} else {
				grid = append(grid, Roll)
			}
		}
		parsedGrid = append(parsedGrid, grid)
	}

	d.grid = parsedGrid
}

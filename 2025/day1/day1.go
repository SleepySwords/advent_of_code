package day1

import (
	"fmt"
	"sleepyswords/advent/day"
	"strconv"
	"strings"
)

func Subscribe(m map[int]func() day.Day) {
	m[len(m)] = func() day.Day {
		return &Day1{}
	}
}

type Day1 struct {
	part1 int
	part2 int
}

func (d *Day1) Part1() any {
	return d.part1
}

func (d *Day1) Part2() any {
	return d.part2
}

func (d *Day1) Parse(buf []byte) {

	st := string(buf)
	lines := strings.Split(st, "\n")

	dial := 50
	pass_p1 := 0
	pass_p2 := 0

	for _, line := range lines {
		if len(line) == 0 {
			continue
		}
		rotation := 0
		if line[0] == 'L' {
			rotation = -1
		} else {
			rotation = 1
		}

		amount, err := strconv.Atoi(line[1:])
		if err != nil {
			panic(fmt.Sprintf("Could not parse number: %s", line[1:]))
		}
		var hits0 int
		dial, hits0 = rotate(dial, amount, rotation)
		if dial == 0 {
			pass_p1 += 1
		}
		pass_p2 += hits0
	}

	d.part1 = pass_p1
	d.part2 = pass_p2
}

func rotate(dial int, amount int, direction int) (int, int) {
	a := 0
	for range amount {
		dial += direction
		if dial == -1 {
			dial = 99
		}
		if dial == 100 {
			dial = 0
		}
		if dial == 0 {
			a += 1
		}
	}
	return dial, a
}

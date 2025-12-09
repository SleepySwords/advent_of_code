package day06

import (
	"fmt"
	"sleepyswords/advent/day"
	"strconv"
	"strings"
)

type Operation int

const (
	Plus Operation = iota
	Multiply
)

func Subscribe(m map[int]func() day.Day) {
	m[len(m)] = func() day.Day {
		return &Day6{}
	}
}

type Day6 struct {
	hw         [][]int
	hw_rl      [][]int
	operations []Operation
}

func (d *Day6) Part1() any {
	total := 0
	for i, op := range d.operations {
		var solution int
		switch op {
		case Plus:
			solution = 0
		case Multiply:
			solution = 1
		}
		for _, h := range d.hw {
			switch op {
			case Plus:
				solution += h[i]
			case Multiply:
				solution *= h[i]
			}
		}
		total += solution
	}
	return total
}

func (d *Day6) Part2() any {
	total := 0
	for i, op := range d.operations {
		var solution int
		switch op {
		case Plus:
			solution = 0
		case Multiply:
			solution = 1
		}
		for _, n := range d.hw_rl[i] {
			switch op {
			case Plus:
				solution += n
			case Multiply:
				solution *= n
			}
		}
		total += solution
	}
	return total
}

func (d *Day6) Parse(buf []byte) {
	st := string(buf)
	input := strings.Split(st, "\n")

	hw := [][]int{}
	operations := []Operation{}

	for _, line := range input {
		if 0 == len(line) {
			continue
		}
		if line[0] == '*' || line[0] == '+' {
			for n := range strings.SplitSeq(line, " ") {
				n = strings.TrimSpace(n)
				if 0 == len(n) {
					continue
				}
				if n[0] == '*' {
					operations = append(operations, Multiply)
				} else {
					operations = append(operations, Plus)
				}
			}
		} else {
			l := []int{}
			for n := range strings.SplitSeq(line, " ") {
				n = strings.TrimSpace(n)
				if 0 == len(n) {
					continue
				}
				n, err := strconv.Atoi(n)
				if err != nil {
					fmt.Println(n)
					panic("Wow")
				}
				l = append(l, n)
			}
			hw = append(hw, l)
		}
	}

	hw_rl := [][]int{}
	current := []int{}
	for i := range len(input[0]) {
		num := 0
		for _, line := range input {
			if len(line) == 0 || line[0] == '*' || line[0] == '+' {
				continue
			}
			if line[i] == ' ' {
				continue
			}
			num *= 10
			num += int(line[i] - '0')
		}
		// the number 0 is not in the input
		if num == 0 {
			hw_rl = append(hw_rl, current)
			current = []int{}
		} else {
			current = append(current, num)
		}
	}

	hw_rl = append(hw_rl, current)

	d.hw = hw
	d.hw_rl = hw_rl
	d.operations = operations
}

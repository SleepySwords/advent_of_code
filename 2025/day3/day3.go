package day3

import (
	"math"
	"sleepyswords/advent/day"
	"strings"
)

func Subscribe(m map[int]func() day.Day) {
	m[len(m)] = func() day.Day {
		return &Day3{}
	}
}

type Day3 struct {
	part1 int
	part2 int

	banks [][]byte
}

func (d *Day3) Part1() any {
	totalJoltage := 0
	for _, bank := range d.banks {
		largest := byte(0)
		secondLargest := byte(0)
		for i := 0; i < len(bank)-1; i++ {
			if bank[i] > byte(largest) {
				largest = bank[i]
				secondLargest = 0
			} else if bank[i] > secondLargest {
				secondLargest = bank[i]
			}
		}

		if bank[len(bank)-1] > secondLargest {
			secondLargest = bank[len(bank)-1]
		}

		joltage := largest*10 + secondLargest
		totalJoltage += int(joltage)
	}
	return totalJoltage
}

func (d *Day3) Part2() any {
	totalJoltage := 0
	for _, bank := range d.banks {
		stack := []byte{}
		for i := range bank {
			remaining := (len(bank) - i)
			value := bank[i]

			for j := 0; j < len(stack); j++ {
				if stack[j] >= value {
					continue
				}

				// Verify boundry conditions
				if remaining >= 12-j {
					stack = stack[:j]
				}
			}

			if len(stack) < 12 {
				stack = append(stack, value)
			}
		}

		joltage := 0

		for i := 0; i < len(stack); i++ {
			b := math.Pow10(12 - (i + 1))
			joltage += int(b) * int(stack[i])
		}

		totalJoltage += joltage
	}
	return totalJoltage
}

func (d *Day3) Parse(buf []byte) {
	st := string(buf)
	banks := strings.Split(st, "\n")

	parsedBanks := [][]byte{}

	for _, line := range banks {
		bank := []byte{}
		line := strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		for i := 0; i < len(line); i++ {
			lbl := line[i] - '0'
			bank = append(bank, lbl)
		}
		parsedBanks = append(parsedBanks, bank)
	}

	d.banks = parsedBanks
}

package day10

import (
	"fmt"
	"sleepyswords/advent/day"
	"strconv"
	"strings"
)

type Point struct {
	x int
	y int
}

func Subscribe(m map[int]func() day.Day) {
	m[len(m)] = func() day.Day {
		return &Day10{}
	}
}

type Day10 struct {
	manuals []Manual
}

func bfs(manual Manual) int {
	queue := make([]struct {
		steps   int
		current int
	}, 0)

	queue = append(queue, struct {
		steps   int
		current int
	}{steps: 0, current: 0})

	for len(queue) != 0 {
		top := queue[0]
		queue = queue[1:]

		for _, button := range manual.buttons {
			step := top
			// fmt.Printf("%b %b %b\n", step.current, button, step.current ^ button)
			step.current ^= button

			step.steps += 1
			if step.current == manual.goal {
				return step.steps
			}

			queue = append(queue, step)
		}
	}
	return -1
}

func (d *Day10) Part1() any {
	total := 0
	for _, manual := range d.manuals {
		total += bfs(manual)
	}

	return total
}

// Got lucky that the box i found was on the inside in my input, have to check if a point on the box is in
// the inside before adding to the array using probably raycasting
func (d *Day10) Part2() any {
	largestArea := 0
	return largestArea
}

type Manual struct {
	goal    int
	buttons []int
}

func (d *Day10) Parse(buf []byte) {
	st := string(buf)
	input := strings.SplitSeq(st, "\n")
	manuals := []Manual{}

	for line := range input {
		if len(line) == 0 {
			continue
		}
		// Least significant bit is going to be the last character
		goal := 0
		buttons := []int{}

		i := 0
		i++

		for line[i] != ']' {
			st := 0
			if line[i] == '#' {
				st = 1
			}
			goal |= (st << (i - 1))
			i++
		}
		i++

		buttonsSt := strings.SplitSeq(line[(i+1):], " ")
		for button := range buttonsSt {
			if button[0] == '(' {
				btn := 0
				for state := range strings.SplitSeq(button[1:len(button)-1], ",") {
					n, _ := strconv.Atoi(state)
					btn |= (1 << n)
				}
				buttons = append(buttons, btn)
			}
		}

		fmt.Println(goal, buttons)
		manuals = append(manuals, Manual{
			goal:    goal,
			buttons: buttons,
		})
	}
	d.manuals = manuals
}

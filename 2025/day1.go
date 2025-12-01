package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

func main() {
	wd, _ := os.Getwd()
	path := filepath.Join(wd, "advent/day1.txt")
	file, err := os.Open(path)
	if err != nil {
		panic("Could not open file")
	}

	buffer := make([]byte, 32768)
	n, err := file.Read(buffer)
	if err != nil {
		panic("Could not read file")
	}

	st := string(buffer[0:n])
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

	fmt.Println("Part 1:", pass_p1)
	fmt.Println("Part 1:", pass_p2)
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

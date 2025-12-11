package main

import (
	"fmt"
	"os"
	"path/filepath"
	"sleepyswords/advent/day"
	"sleepyswords/advent/day01"
	"sleepyswords/advent/day02"
	"sleepyswords/advent/day03"
	"sleepyswords/advent/day04"
	"sleepyswords/advent/day05"
	"sleepyswords/advent/day06"
	"sleepyswords/advent/day07"
	"sleepyswords/advent/day08"
	"sleepyswords/advent/day09"
	"sleepyswords/advent/day10"
	"sleepyswords/advent/day11"
	"strconv"
)

// Convert to a list
var DAYS = map[int]func() day.Day{}

func main() {
	day, err := strconv.Atoi(os.Args[1])
	if err != nil {
		panic("Invalid day")
	}

	day01.Subscribe(DAYS)
	day02.Subscribe(DAYS)
	day03.Subscribe(DAYS)
	day04.Subscribe(DAYS)
	day05.Subscribe(DAYS)
	day06.Subscribe(DAYS)
	day07.Subscribe(DAYS)
	day08.Subscribe(DAYS)
	day09.Subscribe(DAYS)
	day10.Subscribe(DAYS)
	day11.Subscribe(DAYS)

	wd, _ := os.Getwd()
	path := filepath.Join(wd, fmt.Sprintf("advent/day%d.txt", day))
	buf, err := os.ReadFile(path)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	d := DAYS[day - 1]()
	d.Parse(buf)

	fmt.Println("Part 1:", d.Part1())
	fmt.Println("Part 2:", d.Part2())
}

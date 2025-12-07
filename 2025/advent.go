package main

import (
	"fmt"
	"os"
	"path/filepath"
	"sleepyswords/advent/day"
	"sleepyswords/advent/day1"
	"sleepyswords/advent/day2"
	"sleepyswords/advent/day3"
	"sleepyswords/advent/day4"
	"sleepyswords/advent/day5"
	"sleepyswords/advent/day6"
	"sleepyswords/advent/day7"
	"strconv"
)

// Convert to a list
var DAYS = map[int]func() day.Day{}

func main() {
	day, err := strconv.Atoi(os.Args[1])
	if err != nil {
		panic("Invalid day")
	}

	day1.Subscribe(DAYS)
	day2.Subscribe(DAYS)
	day3.Subscribe(DAYS)
	day4.Subscribe(DAYS)
	day5.Subscribe(DAYS)
	day6.Subscribe(DAYS)
	day7.Subscribe(DAYS)

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

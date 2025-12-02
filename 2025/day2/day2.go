package day2

import (
	"fmt"
	"sleepyswords/advent/day"
	"strconv"
	"strings"
)

func Subscribe(m map[int]func() day.Day) {
	m[len(m)] = func() day.Day {
		return &Day2{}
	}
}

type Day2 struct {
	part1 int
	part2 int

	ranges []Range
}

func (d *Day2) Part1() any {
	invalidID := 0
	for _, r := range d.ranges {
		for id := r.begin; id <= r.end; id++ {
			s := fmt.Sprint(id)
			if len(s)%2 == 0 {
				if s[(len(s)/2):] == s[:len(s)/2] {
					invalidID += id
				}
			}
		}
	}
	return invalidID
}

func (d *Day2) Part2() any {
	invalidID := 0
	for _, r := range d.ranges {
		for id := r.begin; id <= r.end; id++ {
			s := fmt.Sprint(id)
			for patternLength := 1; patternLength <= len(s)/2; patternLength++ {
				patternIndex := 0
				repeated := false
				for i := 0; i < len(s); i++ {
					ch := s[i]
					p := s[patternIndex]
					repeated = false
					if p != ch {
						break
					}
					patternIndex = (patternIndex + 1) % patternLength
					if patternIndex == 0 {
						repeated = true
					}
				}
				if repeated {
					invalidID += id
					break
				}
			}
		}
	}
	return invalidID
}

func (d *Day2) Parse(buf []byte) {
	st := string(buf)
	ranges := strings.Split(st, ",")

	parsedRanges := []Range{}

	for _, line := range ranges {
		line := strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		ra := strings.Split(line, "-")
		begin, err := strconv.Atoi(ra[0])
		if err != nil {
			panic("")
		}
		end, err := strconv.Atoi(ra[1])
		if err != nil {
			panic("Could not find number: " + ra[1])
		}
		r := Range{
			begin: begin,
			end:   end,
		}
		parsedRanges = append(parsedRanges, r)
	}

	d.ranges = parsedRanges
}

type Range struct {
	begin int
	end   int
}

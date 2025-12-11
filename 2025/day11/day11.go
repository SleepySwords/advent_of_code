package day11

import (
	"sleepyswords/advent/day"
	"slices"
	"strings"
)

type Point struct {
	x int
	y int
}

func Subscribe(m map[int]func() day.Day) {
	m[len(m)] = func() day.Day {
		return &Day11{}
	}
}

type Day11 struct {
	nodes []string

	edges map[string][]string
}

func (d *Day11) countPaths(node string) int {
	if node == "out" {
		return 1
	}
	total := 0
	for _, edge := range d.edges[node] {
		total += d.countPaths(edge)
	}
	return total
}

type State struct {
	node  string
	found int
}

func (d *Day11) countDacPaths(node string, found int, cache map[State]int) int {
	if node == "out" {
		if found&0b11 == 0b11 {
			return 1
		}
		return 0
	}
	if node == "fft" {
		found |= 1
	}
	if node == "dac" {
		found |= 0b10
	}
	cached_value, ok := cache[State{node, found}]
	if ok {
		return cached_value
	}
	total := 0
	for _, edge := range d.edges[node] {
		total += d.countDacPaths(edge, found, cache)
	}
	cache[State{node, found}] = total
	return total
}

func (d *Day11) Part1() any {
	return d.countPaths("you")
}

func (d *Day11) Part2() any {
	return d.countDacPaths("svr", 0, map[State]int{})
}

type Edge struct {
	head int
	tail int
}

func (d *Day11) Parse(buf []byte) {
	st := string(buf)
	input := strings.SplitSeq(st, "\n")

	nodes := []string{}
	edges := map[string][]string{}

	for line := range input {
		if len(line) == 0 {
			continue
		}

		parts := strings.Split(line, ": ")
		tail := parts[0]

		tailIndex := slices.Index(nodes, tail)
		if tailIndex == -1 {
			nodes = append(nodes, tail)
			tailIndex = len(nodes) - 1
		}

		heads := strings.Split(parts[1], " ")
		edges[tail] = heads
	}
	d.nodes = nodes
	d.edges = edges
}

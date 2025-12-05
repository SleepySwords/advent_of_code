package day5

import (
	"sleepyswords/advent/day"
	"strconv"
	"strings"
)

const (
	Empty = iota
	Roll
)

func Subscribe(m map[int]func() day.Day) {
	m[len(m)] = func() day.Day {
		return &Day5{}
	}
}

type Day5 struct {
	ranges []Range
	values []int
}

type Btree struct {
	degree int
	root   *BtreeInternalNode
}

type BtreeInternalNode struct {
	payloads []BtreeNode
}

type BtreeLeafNode struct {
	values []int
	next   *BtreeLeafNode
}

type BtreeNode interface {
	exists(value int) bool
}

func (b *BtreeInternalNode) exists(value int) bool {
	for _, payload := range b.payloads {
		if payload.exists(value) {
			return true
		}
	}
	return false
}

func (*BtreeLeafNode) exists(value int) bool {
	return false
}

func isMatch(id int, r Range) bool {
	return id >= r.begin && id <= r.end
}

func (d *Day5) Part1() any {
	matches := 0

	for _, id := range d.values {
		for _, r := range d.ranges {
			if isMatch(id, r) {
				matches++
				break
			}
		}
	}

	return matches
}

func (d *Day5) Part2() any {
	matches := 0

	for i, init := range d.ranges {
		ourRanges := []Range{init}

		for j := range i {
			accountedRange := d.ranges[j]
			l := len(ourRanges)
			for k := range l {
				r := &ourRanges[k]

				if r.begin == -1 && r.end == -1 {
					continue
				}

				// 4 cases,
				// Case 1: The beginning is in the range and the end is outside the range
				if isMatch(accountedRange.begin, *r) && !isMatch(accountedRange.end, *r) {
					// The count is then from the beginning of our range to the beginning of the found range.
					r.end = accountedRange.begin - 1
				}
				// Case 2: The beginning is outside the range and the end is inside the range
				if !isMatch(accountedRange.begin, *r) && isMatch(accountedRange.end, *r) {
					// The count is then from the beginning of our range to the beginning of the found range.
					r.begin = accountedRange.end + 1
				}
				// Case 3: The beginning is inside the range and the end is inside the range
				if isMatch(accountedRange.begin, *r) && isMatch(accountedRange.end, *r) {
					// Splits the range into two ranged
					old := r.end
					// This bit me, since append could reslice, all of our references to an existing array are invalid
					// We have to refetch those references or update before.
					r.end = accountedRange.begin - 1
					ourRanges = append(ourRanges, Range{
						begin: accountedRange.end + 1,
						end:   old,
					})
				}
				// Case 4: The beginning is outside the range and the end is outside the range, coverring the entire range
				if accountedRange.begin <= r.begin && accountedRange.end >= r.end {
					r.begin = -1
					r.end = -1
				}
			}
		}
		for _, r := range ourRanges {
			if r.begin == -1 && r.end == -1 {
				continue
			}
			if r.begin > r.end {
				continue
			}
			matches += r.end - r.begin + 1
		}

	}

	return matches
}

func (d *Day5) Parse(buf []byte) {
	st := string(buf)
	input := strings.Split(st, "\n\n")

	ranges := input[0]
	ids := input[1]

	parsedRanges := []Range{}

	for line := range strings.SplitSeq(ranges, "\n") {
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

	parsedIds := []int{}
	for line := range strings.SplitSeq(ids, "\n") {
		line := strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		id, err := strconv.Atoi(line)
		if err != nil {
			panic("")
		}
		parsedIds = append(parsedIds, id)
	}

	d.values = parsedIds
}

type Range struct {
	begin int
	end   int
}

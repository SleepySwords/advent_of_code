package day8

import (
	"cmp"
	"container/heap"
	"sleepyswords/advent/day"
	"slices"
	"strconv"
	"strings"
)

type JunctionBox struct {
	x int
	y int
	z int
}

func Subscribe(m map[int]func() day.Day) {
	m[len(m)] = func() day.Day {
		return &Day8{}
	}
}

type Day8 struct {
	boxes []JunctionBox
}

func (a *JunctionBox) distance(b *JunctionBox) int {
	return (a.x-b.x)*(a.x-b.x) + (a.y-b.y)*(a.y-b.y) + (a.z-b.z)*(a.z-b.z)
}

func (d *Day8) Part1() any {
	hp := MinHeap{}
	indexes := map[JunctionBox]int{}
	uf := Init(len(d.boxes))
	in := 0
	for i, a := range d.boxes {
		indexes[a] = i
		for j := i + 1; j < len(d.boxes); j++ {
			b := d.boxes[j]
			hp = append(hp, &Edge{
				a:        a,
				b:        b,
				distance: a.distance(&b),
				index:    in,
			})
			in++
		}
	}
	heap.Init(&hp)
	for range 1000 {
		item := heap.Pop(&hp).(*Edge)
		uf.Union(indexes[item.a], indexes[item.b])
	}
	sizes := uf.Sizes()
	slices.SortFunc(sizes, func(a, b int) int { return cmp.Compare(b, a) })
	return sizes[0] * sizes[1] * sizes[2]
}

func (d *Day8) Part2() any {
	hp := MinHeap{}
	indexes := map[JunctionBox]int{}
	uf := Init(len(d.boxes))
	in := 0
	for i, a := range d.boxes {
		indexes[a] = i
		for j := i + 1; j < len(d.boxes); j++ {
			b := d.boxes[j]
			hp = append(hp, &Edge{
				a:        a,
				b:        b,
				distance: a.distance(&b),
				index:    in,
			})
			in++
		}
	}
	heap.Init(&hp)
	for {
		item := heap.Pop(&hp).(*Edge)
		uf.Union(indexes[item.a], indexes[item.b])
		foundRep := uf.Find(0)
		allMatch := true
		for i := range len(d.boxes) {
			if uf.Find(i) != foundRep {
				allMatch = false
				break
			}
		}
		if allMatch {
			return item.a.x * item.b.x
		}
	}
}

func (d *Day8) Parse(buf []byte) {
	st := string(buf)
	input := strings.Split(st, "\n")

	boxes := []JunctionBox{}

	for _, line := range input {
		if len(line) == 0 {
			continue
		}
		positions := strings.Split(line, ",")
		x, e_x := strconv.Atoi(positions[0])
		y, e_y := strconv.Atoi(positions[1])
		z, e_z := strconv.Atoi(positions[2])

		if e_x != nil || e_y != nil || e_z != nil {
			panic(":(")
		}

		boxes = append(boxes, JunctionBox{
			x: x,
			y: y,
			z: z,
		})
	}

	d.boxes = boxes
}

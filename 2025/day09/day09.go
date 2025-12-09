package day09

import (
	"iter"
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
		return &Day9{}
	}
}

type Day9 struct {
	points []Point
}

func area(a, b Point) int {
	width := a.x - b.x
	if width < 0 {
		width = -width
	}
	height := a.y - b.y
	if height < 0 {
		height = -height
	}
	return (width + 1) * (height + 1)
}

func (d *Day9) Part1() any {
	largestArea := 0
	for i, a := range d.points {
		for j := range i - 1 {
			b := d.points[j]
			area := area(a, b)
			if largestArea < area {
				largestArea = area
			}
		}
	}
	return largestArea
}

func (d *Day9) findBB() (int, int, int, int) {
	minX, minY, maxX, maxY := d.points[0].x, d.points[0].y, d.points[0].x, d.points[0].x
	for _, p := range d.points {
		if minX > p.x {
			minX = p.x
		}
		if minY > p.y {
			minY = p.y
		}
		if maxX < p.x {
			maxX = p.x
		}
		if maxY < p.y {
			maxY = p.y
		}
	}

	return minX, minY, maxX, maxY
}

func (d *Day9) isLineInside(a, b Point) bool {
	minX := min(a.x, b.x)
	minY := min(a.y, b.y)

	maxX := max(a.x, b.x)
	maxY := max(a.y, b.y)
	for l := range d.lines() {
		// Vertical line
		if l.a.x == l.b.x {
			minLineY := min(l.a.y, l.b.y)
			maxLineY := max(l.a.y, l.b.y)
			if minX < l.a.x && l.a.x < maxX {
				if minY <= minLineY && maxY <= minLineY {
					continue
				}
				if minY >= maxLineY && maxY >= maxLineY {
					continue
				}
				return true
			}
		}
		// Horizontal line
		if l.a.y == l.b.y {
			minLineX := min(l.a.x, l.b.x)
			maxLineX := max(l.a.x, l.b.x)
			if minY < l.a.y && l.a.y < maxY {
				if minX <= minLineX && maxX <= minLineX {
					continue
				}
				if minX >= maxLineX && maxX >= maxLineX {
					continue
				}
				return true
			}
		}
	}
	return false
}

// func (d *Day9) isInside(a Point) {
// 	minX, _, _, _ := d.findBB()
// 	x := a.x
// }

// Got lucky that the box i found was on the inside in my input, have to check if a point on the box is in
// the inside before adding to the array using probably raycasting
func (d *Day9) Part2() any {
	largestArea := 0
	for i, a := range d.points {
		for j := range i - 1 {
			b := d.points[j]
			area := area(a, b)
			if largestArea < area && !d.isLineInside(a, b) {
				largestArea = area
			}
		}
	}
	return largestArea
}

type Edge struct {
	a, b Point
}

func (d *Day9) lines() iter.Seq[Edge] {
	return func(yield func(Edge) bool) {
		for i, a := range d.points {
			var b Point
			if i == len(d.points)-1 {
				b = d.points[0]
			} else {
				b = d.points[i+1]
			}

			if !yield(Edge{a, b}) {
				break
			}
		}
	}
}

func (d *Day9) Parse(buf []byte) {
	st := string(buf)
	input := strings.Split(st, "\n")

	points := []Point{}

	for _, line := range input {
		if len(line) == 0 {
			continue
		}
		positions := strings.Split(line, ",")
		x, e_x := strconv.Atoi(positions[0])
		y, e_y := strconv.Atoi(positions[1])

		if e_x != nil || e_y != nil {
			panic(":(")
		}

		points = append(points, Point{
			x: x,
			y: y,
		})
	}

	d.points = points
}

package day

type Day interface {
	Part1() any

	Part2() any

	Parse(buf []byte)
}

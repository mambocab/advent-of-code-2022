package main

import (
	_ "embed"
	"fmt"
	"strconv"
	"strings"
)

func main() {
	var part1, part2 int

	for _, line := range strings.Split(input, "\n") {
		elves := strings.Split(line, ",")
		if len(elves) != 2 {
			panic(fmt.Sprintf("bad line: %s", line))
		}
		lE, rE := parseElfRange(elves[0]), parseElfRange(elves[1])
		if lE.contains(rE) || rE.contains(lE) {
			part1++
			part2++
		} else if lE.overlaps(rE) {
			part2++
		}
	}
	println(part1)
	println(part2)
}

type elfRange struct {
	bottom, top int
}

func parseElfRange(s string) elfRange {
	split := strings.Split(s, "-")
	if len(split) != 2 {
		panic(fmt.Sprintf("bad elf spec string: %s", s))
	}
	bI, err := strconv.Atoi(split[0])
	if err != nil {
		panic(err)
	}
	tI, err := strconv.Atoi(split[1])
	if err != nil {
		panic(err)
	}
	return elfRange{
		bottom: bI,
		top:    tI,
	}
}

func (r elfRange) contains(other elfRange) bool {
	return r.bottom <= other.bottom && r.top >= other.top
}

func (r elfRange) overlaps(other elfRange) bool {
	return (r.bottom <= other.bottom && r.top >= other.bottom) || (other.bottom <= r.bottom && other.top >= r.bottom)
}

//go:embed input.txt
var input string

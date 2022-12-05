package main

import (
	_ "embed"
	"fmt"
	"os"
	"strconv"
	"strings"
	"text/tabwriter"
)

func main() {
	var part1, part2 int
	tw := tabwriter.NewWriter(os.Stdout, 0, 8, 0, '\t', 0)

	for _, line := range strings.Split(input, "\n") {
		fmt.Fprint(tw, line, "\t")
		elves := strings.Split(line, ",")
		if len(elves) != 2 {
			panic(fmt.Sprintf("bad line: %s", line))
		}
		lE, rE := parseElfRange(elves[0]), parseElfRange(elves[1])
		if lE.contains(rE) || rE.contains(lE) {
			part1++
			part2++
			fmt.Fprint(tw, "*", part1, "\t", "*", part2, "\n")
		} else if lE.overlaps(rE) {
			part2++
			fmt.Fprint(tw, part1, "\t", "*", part2, "\n")
		} else {
			fmt.Fprint(tw, part1, "\t", part2, "\n")
		}
	}
	tw.Flush()
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

func part1() (count int) {
	for _, line := range strings.Split(input, "\n") {
		elves := strings.Split(line, ",")
		if len(elves) != 2 {
			panic(fmt.Sprintf("bad line: %s", line))
		}
		lE, rE := parseElfRange(elves[0]), parseElfRange(elves[1])
		if lE.contains(rE) || rE.contains(lE) {
			count++
		}
	}
	return
}

//go:embed input.txt
var input string

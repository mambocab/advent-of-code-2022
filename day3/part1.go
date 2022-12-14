package main

import (
	"bytes"
	_ "embed"
	"fmt"
	"os"
	"strings"
	"text/tabwriter"
)

func main() {
	tw := tabwriter.NewWriter(os.Stdout, 0, 8, 0, '\t', 0)

	// priorityMap := initPriorityMap()

	sum := 0

	for _, line := range strings.Split(input, "\n") {
		left, right, err := halves(line)
		if err != nil {
			panic(err)
		}

		priorityMap := initPriorityMap()

		leftS := runeSet{}
		rightS := runeSet{}
		leftS.Add([]rune(left)...)
		rightS.Add([]rune(right)...)

		intersection := leftS.Intersection(rightS)
		for r := range intersection {
			sum += priorityMap[r]
		}
		fmt.Fprintln(tw, left, "\t", right, "\t", intersection, "\t", sum)
	}

	tw.Flush()
	fmt.Printf("%v\n", sum)
}

//go:embed input.txt
var input string

type runeSet map[rune]struct{}

func (s runeSet) String() string {
	b := bytes.Buffer{}
	for r := range s {
		b.WriteRune(r)
	}
	return b.String()
}

func (s runeSet) Has(r rune) bool {
	_, ok := s[r]
	return ok
}

func (s runeSet) Add(rs ...rune) {
	for _, r := range rs {
		s[r] = struct{}{}
	}
}

func (s runeSet) Intersection(other runeSet) runeSet {
	var shorter, longer runeSet
	if len(s) > len(other) {
		longer, shorter = s, other
	} else {
		longer, shorter = other, s
	}

	rv := runeSet{}
	for r := range shorter {
		if longer.Has(r) {
			rv.Add(r)
		}
	}
	return rv
}

func initPriorityMap() map[rune]int {
	m := map[rune]int{}
	ch := 'a'
	pr := 1
	for ch <= 'z' {
		m[ch] = pr
		ch++
		pr++
	}
	ch = 'A'
	for ch <= 'Z' {
		m[ch] = pr
		ch++
		pr++
	}

	if m['a'] != 1 {
		panic('a')
	}
	if m['d'] != 4 {
		panic('d')
	}
	if m['A'] != 27 {
		panic('A')
	}
	if m['Z'] != 52 {
		panic('Z')
	}

	return m
}

func halves(s string) (string, string, error) {
	l := len(s)
	if l%2 != 0 {
		return "", "", fmt.Errorf(`input %v has odd length`, s)
	}

	left, right := s[:l/2], s[l/2:]
	// I'm doing this instead of writing tests; don't judge me
	if len(left) != len(right) {
		panic(fmt.Sprintf("%v incorrectly split into %v and %v", s, left, right))
	}
	return left, right, nil
}

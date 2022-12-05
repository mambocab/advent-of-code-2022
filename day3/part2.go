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
	priorityMap := initPriorityMap()

	sum := 0
	lines := strings.Split(input, "\n")
	for i := 0; i < len(lines); i += 3 {
		e1, e2, e3 := lines[i], lines[i+1], lines[i+2]
		e1S, e2S, e3S := runeSet{}.AddString(e1), runeSet{}.AddString(e2), runeSet{}.AddString(e3)

		intersection := e1S.Intersection(e2S).Intersection(e3S)
		if len(intersection) != 1 {
			panic(fmt.Sprintf("%s, %s, and %s share multiple letters: %s", e1, e2, e3, intersection))
		}

		var common rune
		for common = range intersection {
			sum += priorityMap[common]
		}

		fmt.Fprintln(tw, e1, "\t", e2, "\t", e3, "\t", intersection, "\t", sum)
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

func (s runeSet) Add(rs ...rune) runeSet {
	for _, r := range rs {
		s[r] = struct{}{}
	}
	return s
}

func (s runeSet) AddString(str string) runeSet {
	for _, r := range []rune(str) {
		s[r] = struct{}{}
	}
	return s
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

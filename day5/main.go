package main

import (
	_ "embed"
	"fmt"
	"os"
	"reflect"
	"strconv"
	"strings"
	"text/tabwriter"
)

//go:embed input.txt
var input string

func main() {
	lines := strings.Split(input, "\n")
	var emptyLineIdx int
	for i, e := range lines {
		if e == "" {
			emptyLineIdx = i
			break
		}
	}

	crateStackSpec, _ := lines[:emptyLineIdx], lines[emptyLineIdx:]

	parseCrateStackSpec(crateStackSpec)

	// fmt.Println(crateStackSpec)
	// fmt.Println()
	// fmt.Println(movementSpec)
}

// assume all ASCII -- bytes not runes
type crate = stack[byte]

func parseCrateStackSpec(css []string) [10]*crate {
	rv := [10]*crate{
		// assume 9 crates, and we're leaving 0 empty since the spec is 1-indexed
		nil,
		newStack[byte](), newStack[byte](), newStack[byte](),
		newStack[byte](), newStack[byte](), newStack[byte](),
		newStack[byte](), newStack[byte](), newStack[byte](),
	}
	// easiest to deal with the spec backwards
	layers := stack[string]{}
	for _, line := range css {
		layers.push(line)
	}

	crateIndexLine, err := layers.pop()
	if err != nil {
		panic(err)
	}

	crateIndexes := [10]int{}
	for i, r := range crateIndexLine {
		if r != ' ' {
			crateNo, err := strconv.Atoi(string(r))
			if err != nil {
				panic(err)
			}
			crateIndexes[crateNo] = i
		}
	}

	for !layers.empty() {
		layer, _ := layers.pop()
		for crateNo, i := range crateIndexes {
			if crateNo == 0 {
				continue
			}
			b := layer[i]
			if b != ' ' {
				rv[crateNo].push(b)
			}
		}
	}

	tw := tabwriter.NewWriter(os.Stdout, 0, 8, 0, '\t', 0)
	for i, crate := range rv {
		if i == 0 {
			continue
		}
		fmt.Fprintln(tw, i, "\t", crate)
	}
	tw.Flush()

	return rv
}

type stack[T any] struct {
	vs []T
}

func newStack[T any]() *stack[T] {
	return &stack[T]{vs: []T{}}
}

func (s *stack[T]) push(t T) {
	s.vs = append(s.vs, t)
}

func (s *stack[T]) pop() (T, error) {
	if len(s.vs) == 0 {
		return *new(T), fmt.Errorf("stack is empty")
	}
	i := len(s.vs) - 1
	rv := s.vs[i]
	s.vs = s.vs[:i]
	return rv, nil
}

func (s *stack[T]) empty() bool {
	return len(s.vs) == 0
}

func (s *stack[T]) String() string {
	reversed := []string{}
	if !s.empty() {
		tname := reflect.TypeOf(s.vs[0]).String()
		fstr := "%v"
		if tname == "uint8" { // "byte"'s real name
			fstr = "%c"
		}
		for i := len(s.vs) - 1; i >= 0; i-- {
			reversed = append(reversed, fmt.Sprintf(fstr, s.vs[i]))
		}
	}
	return fmt.Sprintf("%v", reversed)
}

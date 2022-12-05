package main

import (
	_ "embed"
	"fmt"
	"os"
	"reflect"
	"regexp"
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

	crateStackSpec, movementsSpec := lines[:emptyLineIdx], lines[emptyLineIdx:]

	p1Crates := parseCrateStackSpec(crateStackSpec)
	p2Crates := [10]*crate{}
	for i, c := range p1Crates {
		if c != nil {
			p2Crates[i] = c.clone()
		}
	}
	moves := parseMovementSpec(movementsSpec)

	for _, m := range moves {
		for i := 0; i < m.n; i++ {
			if v, err := p1Crates[m.from].pop(); err != nil {
				panic(err)
			} else {
				p1Crates[m.to].push(v)
			}
		}
		if chopt, err := p2Crates[m.from].chop(m.n); err != nil {
			panic(err)
		} else {
			p2Crates[m.to].plop(chopt...)
		}
	}

	fmt.Print("part 1: ")
	for _, c := range p1Crates[1:] {
		fmt.Printf("%c", *c.peek())
	}
	fmt.Println()
	fmt.Print("part 2: ")
	for _, c := range p2Crates[1:] {
		fmt.Printf("%c", *c.peek())
	}
	fmt.Println()
}

// assume all ASCII -- bytes not runes
type crate = stack[byte]

type move struct{ n, from, to int }

var MOVE_REGEX = regexp.MustCompile(`\d+`)

func atoiOrPanic(s string) (i int) {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return
}

func parseMovementSpec(mss []string) (moves []move) {
	for _, line := range mss {
		if line == "" {
			continue
		}
		matches := MOVE_REGEX.FindAllString(line, 4) // 4 helps us check for bad lines
		if len(matches) != 3 {
			panic("bad line")
		}
		// matches[0], matches[1], matches[2]
		moves = append(moves, move{
			n:    atoiOrPanic(matches[0]),
			from: atoiOrPanic(matches[1]),
			to:   atoiOrPanic(matches[2]),
		})
	}
	return
}

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

func (s *stack[T]) plop(ts ...T) {
	s.vs = append(s.vs, ts...)
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

func (s *stack[T]) chop(n int) ([]T, error) {
	if len(s.vs) < n {
		return nil, fmt.Errorf("stack too short to chop(%d): %v", n, s)
	}
	i := len(s.vs) - n
	newVs, rv := s.vs[:i], s.vs[i:]
	if len(rv) != n {
		panic("oh no")
	}
	s.vs = newVs
	return rv, nil
}

func (s *stack[T]) peek() *T {
	if s.empty() {
		return nil
	}
	return &s.vs[len(s.vs)-1]
}

func (s *stack[T]) empty() bool {
	return len(s.vs) == 0
}

func (s *stack[T]) clone() *stack[T] {
	rv := newStack[T]()
	rv.vs = make([]T, len(s.vs))
	copy(rv.vs, s.vs)
	return rv
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

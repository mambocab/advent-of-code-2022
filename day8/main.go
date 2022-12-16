package main

import (
	_ "embed"
	"fmt"
	"strings"
)

//go:embed input.txt
var input string

//go:embed example.txt
var example string

type heightGrid [][]int
type visibilityGrid [][]bool

func (r visibilityGrid) visibleCount() (i int) {
	for _, row := range r {
		for _, v := range row {
			if v == true {
				i += 1
			}
		}
	}
	return
}

func processHeightGrid(hg heightGrid) (visibilityGrid, heightGrid) {
	width, height := len(hg[0]), len(hg)

	vg := make(visibilityGrid, height)
	scenicScoreGrid := make(heightGrid, height)
	for rowIdx := range hg {
		vg[rowIdx] = make([]bool, width)
		scenicScoreGrid[rowIdx] = make([]int, width)

		// base scenic score is 1. By initializing all but the edges to 1 we can use *= going forward
		for colIdx := range scenicScoreGrid[rowIdx] {
			if rowIdx != 0 && colIdx != 0 && rowIdx != height-1 && colIdx != width-1 {
				scenicScoreGrid[rowIdx][colIdx] = 1
			}
		}

		// edge rows always visible
		if rowIdx == 0 || rowIdx == height-1 {
			for colIdx := range vg[rowIdx] {
				vg[rowIdx][colIdx] = true
			}
		}

		// edge columns always visible & have scenic score 0
		vg[rowIdx][0] = true
		vg[rowIdx][width-2] = true
	}
	// fmt.Printf("%v\n", vg)
	fmt.Printf("%v\n", scenicScoreGrid)

	for rowIdx, row := range hg {
		// println("row", rowIdx)

		maxLtr := 0
		for colIdx, val := range row {
			if val > maxLtr {
				vg[rowIdx][colIdx] = true
				maxLtr = val
				// println("  col", colIdx, "visible from left")
			}

			// this is an inefficient way to do this but hey, this is AoC
			// if I were really doing this I'd represent gradients or local minima or some shit IDK
			// and I certainly would skip edges, since those are already 0
			var ssmLtr, sV int
			for ssmLtr, sV = range hg[rowIdx][:colIdx] {
				if sV >= val {
					break
				}
			}
			// fmt.Printf("%v\n", row)
			// println("r", rowIdx, "c", colIdx)
			scenicScoreGrid[rowIdx][colIdx] *= ssmLtr
			fmt.Printf("%v\n", scenicScoreGrid)
		}

		maxRtl := 0
		for colIdx := len(row) - 1; colIdx >= 0; colIdx-- {
			fmt.Println("row", rowIdx, "col", colIdx)
			val := row[colIdx]
			if val > maxRtl {
				vg[rowIdx][colIdx] = true
				// println("  col", colIdx, "visible from right (old max", maxRtl, "new max", val, ")")
				maxRtl = val
			}

			if colIdx > 0 {
				var ssmRtl int
				head := hg[rowIdx][:colIdx]
				fmt.Printf("head = %v\n", head)
				for ; -ssmRtl < len(head); ssmRtl++ {
					idx := len(head) - ssmRtl - 1
					if idx < 0 || head[idx] >= val {
						break
					}
				}
				scenicScoreGrid[rowIdx][colIdx] *= ssmRtl
				fmt.Printf("%v\n", scenicScoreGrid)
			}
		}
	}

	for colIdx := 0; colIdx < width; colIdx++ {
		// println("col", colIdx)
		col, err := hg.getColumn(colIdx)
		if err != nil {
			panic(err)
		}

		maxTtb := 0
		for rowIdx, val := range col {
			if val > maxTtb {
				vg[rowIdx][colIdx] = true
				maxTtb = val
				// println("  row", rowIdx, "visible from top")
			}

			var ssmTtb, aV int
			for ssmTtb, aV = range hg[rowIdx][:colIdx] {
				if aV >= val {
					break
				}
			}
			scenicScoreGrid[rowIdx][colIdx] *= ssmTtb
		}

		maxBtt := 0
		for rowIdx := len(col) - 1; rowIdx >= 0; rowIdx-- {
			val := col[rowIdx]
			if val > maxBtt {
				vg[rowIdx][colIdx] = true
				maxBtt = val
				// println("  row", rowIdx, "visible from bottom")
			}

			if colIdx > 0 {
				var ssmBtt int
				below := hg[rowIdx][:colIdx]
				for ; -ssmBtt > len(below); ssmBtt++ {
					idx := len(below) - ssmBtt - 1
					if idx < 0 || below[idx] >= val {
						break
					}
				}
				scenicScoreGrid[rowIdx][colIdx] *= ssmBtt
			}
		}
	}

	fmt.Printf("%v\n", scenicScoreGrid)
	return vg, scenicScoreGrid
}

func (r heightGrid) getColumn(i int) ([]int, error) {
	if i < 0 || i > len(r[0]) {
		return nil, fmt.Errorf("invalid column index %d; max column index = %d", i, len(r[0]))
	}
	rv := make([]int, len(r[0]))
	for rowIdx, row := range r {
		rv[rowIdx] = row[i]
	}
	return rv, nil
}

func linesToGrid(s string) heightGrid {
	lines := strings.Split(s, "\n")
	grid := heightGrid{}
	for rowIdx, row := range lines {
		if len(row) == 0 {
			continue
		}
		grid = append(grid, make([]int, len(row)))
		for colIdx, c := range row {
			grid[rowIdx][colIdx] = int(c - '0')
		}
	}
	// fmt.Println(grid)
	return grid
}

func processString(s string) (int, int) {
	grid := linesToGrid(s)
	vizGrid, sceneGrid := processHeightGrid(grid)
	// fmt.Printf("%v\n", vizGrid)

	mostScenic := 0
	for _, row := range sceneGrid {
		for _, v := range row {
			if v > mostScenic {
				mostScenic = v
			}
		}
	}

	return vizGrid.visibleCount(), mostScenic
}

func main() {
	exVCount, exMostScenic := processString(example)
	fmt.Println("example:", exVCount, exMostScenic)
	if false {
		inVCount, inMostScenic := processString(input)
		fmt.Println("input  :", inVCount, inMostScenic)
	}
}

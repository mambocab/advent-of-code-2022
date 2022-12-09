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

func hgToVg(hg heightGrid) visibilityGrid {
	width, height := len(hg[0]), len(hg)
	vg := make(visibilityGrid, height)
	for rowIdx := range hg {
		vg[rowIdx] = make([]bool, width-1)
		// edge rows always visible
		if rowIdx == 0 || rowIdx == height-1 {
			for colIdx := range vg[rowIdx] {
				vg[rowIdx][colIdx] = true
			}
		}
		// edge columns always visible
		vg[rowIdx][0] = true
		vg[rowIdx][width-2] = true
	}

	for rowIdx, row := range hg {
		// println("row", rowIdx)
		maxLtr := 0
		for colIdx, val := range row {
			if val > maxLtr {
				vg[rowIdx][colIdx] = true
				maxLtr = val
				// println("  col", colIdx, "visible from left")
			}
		}
		maxRtl := 0
		for colIdx := len(row) - 1; colIdx >= 0; colIdx-- {
			val := row[colIdx]
			if val > maxRtl {
				vg[rowIdx][colIdx] = true
				// println("  col", colIdx, "visible from right (old max", maxRtl, "new max", val, ")")
				maxRtl = val
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
		}
		maxBtt := 0
		for rowIdx := len(col) - 1; rowIdx >= 0; rowIdx-- {
			val := col[rowIdx]
			if val > maxBtt {
				vg[rowIdx][colIdx] = true
				maxBtt = val
				// println("  row", rowIdx, "visible from bottom")
			}
		}
	}

	return vg
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

func lineToGrid(s string) heightGrid {
	lines := strings.Split(s, "\n")
	grid := heightGrid{}
	for rowIdx, row := range lines {
		if len(row) == 0 {
			continue
		}
		grid = append(grid, make([]int, len(lines)))
		for colIdx, c := range row {
			grid[rowIdx][colIdx] = int(c - '0')
		}
	}
	// fmt.Println(grid)
	return grid
}

func processString(s string) int {
	grid := lineToGrid(s)
	vizGrid := hgToVg(grid)
	// fmt.Printf("%v\n", vizGrid)
	return vizGrid.visibleCount()
}

func main() {
	fmt.Println("example:", processString(example))
	fmt.Println("input  :", processString(input))
}

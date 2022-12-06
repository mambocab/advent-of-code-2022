package main

import (
	_ "embed"
	"fmt"
)

//go:embed input.txt
var input string

func main() {
	i := 3
	for ; i < len(input); i++ {
		one, two, thr, fou := input[i-3], input[i-2], input[i-1], input[i]
		if one != two && one != thr && one != fou &&
			two != thr && two != fou &&
			thr != fou {
			break
		}
	}
	fmt.Println("part 1:", i+1)
}

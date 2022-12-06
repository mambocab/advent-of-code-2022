package main

import (
	_ "embed"
)

//go:embed input.txt
var input string

func main() {
	println(part1())
	println(part2())
}

func part1() int {
	i := 3
	for ; i < len(input); i++ {
		one, two, thr, fou := input[i-3], input[i-2], input[i-1], input[i]
		if one != two && one != thr && one != fou &&
			two != thr && two != fou &&
			thr != fou {
			break
		}
	}
	return i + 1
}

func part2() int {
	i := 13
	for ; i < len(input); i++ {
		if input[i-13] != input[i-12] && input[i-13] != input[i-11] && input[i-13] != input[i-10] && input[i-13] != input[i-9] && input[i-13] != input[i-8] && input[i-13] != input[i-7] && input[i-13] != input[i-6] && input[i-13] != input[i-5] && input[i-13] != input[i-4] && input[i-13] != input[i-3] && input[i-13] != input[i-2] && input[i-13] != input[i-1] && input[i-13] != input[i] &&
			input[i-12] != input[i-11] && input[i-12] != input[i-10] && input[i-12] != input[i-9] && input[i-12] != input[i-8] && input[i-12] != input[i-7] && input[i-12] != input[i-6] && input[i-12] != input[i-5] && input[i-12] != input[i-4] && input[i-12] != input[i-3] && input[i-12] != input[i-2] && input[i-12] != input[i-1] && input[i-12] != input[i] &&
			input[i-11] != input[i-10] && input[i-11] != input[i-9] && input[i-11] != input[i-8] && input[i-11] != input[i-7] && input[i-11] != input[i-6] && input[i-11] != input[i-5] && input[i-11] != input[i-4] && input[i-11] != input[i-3] && input[i-11] != input[i-2] && input[i-11] != input[i-1] && input[i-11] != input[i] &&
			input[i-10] != input[i-9] && input[i-10] != input[i-8] && input[i-10] != input[i-7] && input[i-10] != input[i-6] && input[i-10] != input[i-5] && input[i-10] != input[i-4] && input[i-10] != input[i-3] && input[i-10] != input[i-2] && input[i-10] != input[i-1] && input[i-10] != input[i] &&
			input[i-9] != input[i-8] && input[i-9] != input[i-7] && input[i-9] != input[i-6] && input[i-9] != input[i-5] && input[i-9] != input[i-4] && input[i-9] != input[i-3] && input[i-9] != input[i-2] && input[i-9] != input[i-1] && input[i-9] != input[i] &&
			input[i-8] != input[i-7] && input[i-8] != input[i-6] && input[i-8] != input[i-5] && input[i-8] != input[i-4] && input[i-8] != input[i-3] && input[i-8] != input[i-2] && input[i-8] != input[i-1] && input[i-8] != input[i] &&
			input[i-7] != input[i-6] && input[i-7] != input[i-5] && input[i-7] != input[i-4] && input[i-7] != input[i-3] && input[i-7] != input[i-2] && input[i-7] != input[i-1] && input[i-7] != input[i] &&
			input[i-6] != input[i-5] && input[i-6] != input[i-4] && input[i-6] != input[i-3] && input[i-6] != input[i-2] && input[i-6] != input[i-1] && input[i-6] != input[i] &&
			input[i-5] != input[i-4] && input[i-5] != input[i-3] && input[i-5] != input[i-2] && input[i-5] != input[i-1] && input[i-5] != input[i] &&
			input[i-4] != input[i-3] && input[i-4] != input[i-2] && input[i-4] != input[i-1] && input[i-4] != input[i] &&
			input[i-3] != input[i-2] && input[i-3] != input[i-1] && input[i-3] != input[i] &&
			input[i-2] != input[i-1] && input[i-2] != input[i] &&
			input[i-1] != input[i] {
			break
		}
	}
	return i + 1
}

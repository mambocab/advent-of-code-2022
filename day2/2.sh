#!/usr/bin/env sh

trap "exit" INT

{
    sed '
        # optional points for wins and draws
        /.*Z/a\
        6
        /.*Y/a\
        3
        # guaranteed points for choosing rock
        s/C Z/1/ ; s/A Y/1/ ; s/B X/1/
        # guaranteed points for choosing paper
        s/A Z/2/ ; s/B Y/2/ ; s/C X/2/
        # guaranteed points for choosing scissors
        s/B Z/3/ ; s/C Y/3/ ; s/A X/3/
    ' ./in/input.txt |
        tr -s '\n ' + |
        sed 's/+$//'
    echo
} | bc
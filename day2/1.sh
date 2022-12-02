#!/usr/bin/env sh

trap "exit" INT

while read -r round ; do 
    # 6 points for wins
    echo "$round" | grep 'A Y\|B Z\|C X' >/dev/null && echo 6
    # 3 points for draws
    echo "$round" | grep 'A X\|B Y\|C Z' >/dev/null && echo 3

    echo "$round" | grep 'X$' >/dev/null && echo 1
    echo "$round" | grep 'Y$' >/dev/null && echo 2
    echo "$round" | grep 'Z$' >/dev/null && echo 3
done < ./in/input.txt |
    tr '\n' + |
    sed 's/+$//' |
    sed 's/$/\n/' |
    bc
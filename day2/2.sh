#!/usr/bin/env sh

trap "exit" INT

while read -r round ; do 
    # 6 points for wins
    echo "$round" | grep 'Z$' >/dev/null && echo 6
    # 3 points for draws
    echo "$round" | grep 'Y$' >/dev/null && echo 3
    # choose rock when we need to win against scissors, draw against rock, lose against paper
    echo "$round" | grep 'C Z\|A Y\|B X' >/dev/null && echo 1
    # choose paper when we need to win against rock, draw against paper, lose against scissors
    echo "$round" | grep 'A Z\|B Y\|C X' >/dev/null && echo 2
    # choose scissors when we need to win against paper, draw against scissors, lose against rock
    echo "$round" | grep 'B Z\|C Y\|A X' >/dev/null && echo 3
done < ./in/input.txt |
    tr '\n' + |
    sed 's/+$//' |
    sed 's/$/\n/' |
    bc
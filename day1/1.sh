#!/usr/bin/env sh
tr '\n' '+' < ./in/input.txt |        # make each batch an addition command for bc
    sed 's:+*\(.*[^\+]\)+*$:\1\n:g' | # trim leading and trailing + just in case
    sed 's:++:\n\n:g' |               # one bc command per line
    bc |                              # this is where the magic happens
    sort -n |                         # numeric sort
    tail -n1                          # the biggest one!
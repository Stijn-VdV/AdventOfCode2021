## ---------------------------
## Advent of Code 2021
## Day 1 - part 1
##
## Challenge description: Count the number of times a depth measurement increases
##                        from the previous measurement.
##
## Author: Stijn Van de Vondel
##
## Date Created: 2021-12-01
##
## Copyright (c) Stijn Van de Vondel, 2021
## ---------------------------

# Import data
input <- readLines("day1/day1input.txt", warn = FALSE) |> as.integer()

# Count times a value increases from the previous
# diff(input) > 0 results in a vector of length = 1999
sum(diff(input) > 0)




## ---------------------------
## Advent of Code 2021
## Day 1 - part 2
##
## Challenge description: Count the number of times a depth measurement increases
##                        from the previous measurement.
##
## Copyright (c) Stijn Van de Vondel, 2021
## ---------------------------

# Import data
input <- readLines("day1/day1input.txt", warn = FALSE) |> as.integer()

# Calculate rolling sum
input_rsum <- input + dplyr::lead(input) + dplyr::lead(input, 2)

# Calculate number of times a value increased
sum(diff(input_rsum) > 0, na.rm = TRUE)

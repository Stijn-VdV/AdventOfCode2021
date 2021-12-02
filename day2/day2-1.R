## ---------------------------
## Advent of Code 2021
## Day 2 - part 1
##
## Challenge description: Calculate horizontal and vertical position from
##                        vector of directions and integers.
##
##
## Copyright (c) Stijn Van de Vondel, 2021
## ---------------------------

# - Import data ----
input <- readLines("day2/day2input.txt", warn = FALSE)

# - Check whether integers ever have more than 1 digit ----
# store words and numbers as character vector
input_vect <- unlist(strsplit(input, " "))

# extract only numbers, convert to integer and check whether any exceed 9L
input_numbers <- stringr::str_extract(input_vect, "\\d") |> na.omit() |> as.integer()

# conclusion: all numbers within 1-9 range
table(input_numbers > 9)

rm(list = setdiff(ls(), "input"))

# - Calculate final x, y and x*y positions ----
# function to list each element of input in x/y direction and integer
navigate_fun <- function(x) {
  # extract direction
  nav_dir <- strsplit(x, " ")[[1]][1]

  # extract steps
  nav_int <- strsplit(x, " ")[[1]][2] |> as.integer()

  # return in list
  list(nav_dir, nav_int)
}

# set starting X and Y positions
x <- 0
y <- 0
i <- 1

for (i in seq_along(input)) {
  # store interation in separate object for readability
  input_i <- input[i]

  # store navigation direction and integer separately
  nav_dir <- navigate_fun(input_i)[[1]]
  nav_int <- navigate_fun(input_i)[[2]]

  # change x and y depending on navigation direction
  if (nav_dir == "forward") {
    x <- x + nav_int
  }
  else if (nav_dir == "up") {
    # decreasing depth (-) when going up
    y <- y - nav_int
  }
  else if (nav_dir == "down"){
    # increasing depth (+) when going down
    y <- y + nav_int
  }

  # move to next iteration
  i <- i + 1
}

# final position
x*y

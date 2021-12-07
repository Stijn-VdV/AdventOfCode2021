## ---------------------------
## Advent of Code 2021
## Day 2 - part 2
##
## Challenge description: See below
##
##
## Copyright (c) Stijn Van de Vondel, 2021
## ---------------------------
# - Instructions ----
"In addition to horizontal position and depth, you'll also need to track a third
value, aim, which also starts at 0. The commands also mean something entirely
different than you first thought:

down X increases your aim by X units.
up X decreases your aim by X units.
forward X does two things:
It increases your horizontal position by X units.
It increases your depth by your aim multiplied by X.
Again note that since you're on a submarine, down and up do the opposite of what
you might expect: 'down' means aiming in the positive direction.

Now, the above example does something different:

  - forward 5 adds 5 to your horizontal position, a total of 5.
    Because your aim is 0, your depth does not change.
  - down 5 adds 5 to your aim, resulting in a value of 5.
  - forward 8 adds 8 to your horizontal position, a total of 13. Because your
    aim is 5, your depth increases by 8*5=40.
  - up 3 decreases your aim by 3, resulting in a value of 2.
  - down 8 adds 8 to your aim, resulting in a value of 10.
  - forward 2 adds 2 to your horizontal position, a total of 15.
    Because your aim is 10, your depth increases by 2*10=20 to a total of 60.

After following these new instructions, you would have a horizontal position of
15 and a depth of 60. (Multiplying these produces 900.)
"

# - Import data ----
input <- readLines("day2/day2input.txt", warn = FALSE)

# function to list each element of input in x/y direction and integer
navigate_fun <- function(x) {
  # extract direction
  nav_dir <- strsplit(x, " ")[[1]][1]

  # extract steps
  nav_int <- strsplit(x, " ")[[1]][2] |> as.integer()

  # return in list
  list(nav_dir, nav_int)
}

# - Calculate final x, y and x*y positions ----
# set starting X and Y positions
x <- 0
y <- 0
aim <- 0
i <- 1

for (i in seq_along(input)) {
  # store iteration in separate object for readability
  input_i <- input[i]

  # store navigation direction and integer separately
  nav_dir <- navigate_fun(input_i)[[1]]
  nav_int <- navigate_fun(input_i)[[2]]

  # if navigation direction == down or up --> change aim accordingly
  if (nav_dir %in% c("down", "up")) {
    aim <- aim + ifelse(nav_dir == "down", nav_int, -nav_int)
  }

  # if navigation direction == forward --> change x and y positions
  if (nav_dir == "forward") {
    x <- x + nav_int
    y <- y + (aim * nav_int)
  }

  # move to next iteration
  i <- i + 1
}

# final position: 2105273490
x*y

# - Test ----
input <- c("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")

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
aim <- 0
i <- 1

for (i in seq_along(input)) {
  # store iteration in separate object for readability
  input_i <- input[i]

  # store navigation direction and integer separately
  nav_dir <- navigate_fun(input_i)[[1]]
  nav_int <- navigate_fun(input_i)[[2]]

  if (nav_dir %in% c("down", "up")) {
    aim <- aim + ifelse(nav_dir == "down", nav_int, -nav_int)
  }

  if (nav_dir == "forward") {
    x <- x + nav_int
    y <- y + (aim * nav_int)
  }

  i <- i + 1
}

x # 15
y # 60
x*y # 900
# == correct --> now apply to real input







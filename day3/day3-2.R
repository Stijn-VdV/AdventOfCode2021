## ---------------------------
## Advent of Code 2021
## Day 3 - part 1
##
## Copyright (c) Stijn Van de Vondel, 2021
## ---------------------------
# - Instructions ----
"--- Part Two ---
Next, you should verify the life support rating, which can be determined by
multiplying the oxygen generator rating by the CO2 scrubber rating.

Both the oxygen generator rating and the CO2 scrubber rating are values that can
be found in your diagnostic report - finding them is the tricky part. Both values
are located using a similar process that involves filtering out values until only
one remains. Before searching for either rating value, start with the full list of
binary numbers from your diagnostic report and consider just the first bit of those
numbers. Then:

Keep only numbers selected by the bit criteria for the type of rating value for
which you are searching. Discard numbers which do not match the bit criteria.
If you only have one number left, stop; this is the rating value for which you
are searching.
Otherwise, repeat the process, considering the next bit to the right.

The bit criteria depends on which type of rating value you want to find:
To find oxygen generator rating, determine the most common value (0 or 1) in the
current bit position, and keep only numbers with that bit in that position. If 0
and 1 are equally common, keep values with a 1 in the position being considered.
To find CO2 scrubber rating, determine the least common value (0 or 1) in the
current bit position, and keep only numbers with that bit in that position. If 0
and 1 are equally common, keep values with a 0 in the position being considered.
For example, to determine the oxygen generator rating value using the same example
diagnostic report from above:

Start with all 12 numbers and consider only the first bit of each number. There
are more 1 bits (7) than 0 bits (5), so keep only the 7 numbers with a 1 in the
first position: 11110, 10110, 10111, 10101, 11100, 10000, and 11001.
Then, consider the second bit of the 7 remaining numbers: there are more 0 bits
(4) than 1 bits (3), so keep only the 4 numbers with a 0 in the second position:
10110, 10111, 10101, and 10000.
In the third position, three of the four numbers have a 1, so keep those three:
10110, 10111, and 10101.
In the fourth position, two of the three numbers have a 1, so keep those two:
10110 and 10111.
In the fifth position, there are an equal number of 0 bits and 1 bits (one each).
So, to find the oxygen generator rating, keep the number with a 1 in that position:
10111.
As there is only one number left, stop; the oxygen generator rating is 10111, or
23 in decimal.
Then, to determine the CO2 scrubber rating value from the same example above:

Start again with all 12 numbers and consider only the first bit of each number.
There are fewer 0 bits (5) than 1 bits (7), so keep only the 5 numbers with a 0
in the first position: 00100, 01111, 00111, 00010, and 01010.
Then, consider the second bit of the 5 remaining numbers: there are fewer 1 bits
(2) than 0 bits (3), so keep only the 2 numbers with a 1 in the second position:
01111 and 01010.
In the third position, there are an equal number of 0 bits and 1 bits (one each).
So, to find the CO2 scrubber rating, keep the number with a 0 in that position:
01010.
As there is only one number left, stop; the CO2 scrubber rating is 01010, or 10
in decimal.
Finally, to find the life support rating, multiply the oxygen generator rating
(23) by the CO2 scrubber rating (10) to get 230.

Use the binary numbers in your diagnostic report to calculate the oxygen generator
rating and CO2 scrubber rating, then multiply them together. What is the life
support rating of the submarine? (Be sure to represent your answer in decimal,
not binary.)"
# - Import data ----
input <- readLines("day3/day3_input.txt", warn = FALSE)

# - Calculate life support rating of the submarine ----

calc_life_support <- function(input) {
  # separate each bit into individual 0/1, convert to integer and save as data frame
  str_res <- sapply(strsplit(input, ""), \(x) substr(x, 1, 1) |> as.integer()) |> as.data.frame()

  # assign str_res to str_O2 and str_CO2 to separate loops
  str_O2 <- str_res
  str_CO2 <- str_res

  # loop to find oxygen generator rating
  for (i in 1:nrow(str_O2)) {

    if (length(str_O2) <= 2) {
      # if dataframe has <= 2 columns -> extract bit with 1 in i'th position
      str_O2 <- str_O2[which(str_O2[i, ] == 1)][[1]]

      # convert resulting string to integer
      res_O2 <- strtoi(paste0(str_O2, collapse = ""), base = 2L)

      # break loop -> oxygen generator rating has been found
      break

    }  else {
      # calculate rowsum = number of times 1 occurs
      row_sum <- rowSums(str_O2)

      # if 1 occurs at least half the length of str_res -> 1 occurs most frequently
      most_frequent_int <- ifelse(row_sum[i] >= length(str_O2)/2, 1, 0)

      # keep only bits with most_frequent_int in position i
      str_O2 <- str_O2[which(str_O2[i, ] == most_frequent_int)]

      # move to next iteration
      i <- i + 1
    }
  }

  # loop to find CO2 scrubber rating
  for (i in 1:nrow(str_CO2)) {

    if (length(str_CO2) <= 2) {
      # if dataframe has <= 2 columns -> extract bit with 0 in i'th position
      str_CO2 <- str_CO2[which(str_CO2[i, ] == 0)][[1]]

      # convert resulting string to integer
      res_CO2 <- strtoi(paste0(str_CO2, collapse = ""), base = 2L)

      # break loop -> CO2 scrubber rating has been found
      break

    }  else {
      # calculate rowsum = number of times 1 occurs
      row_sum <- rowSums(str_CO2)

      # if 1 occurs at least half the length of str_res -> 1 occurs most frequently
      most_frequent_int <- ifelse(row_sum[i] <= length(str_CO2)/2, 1, 0)

      # keep only bits with most_frequent_int in position i
      str_CO2 <- str_CO2[which(str_CO2[i, ] == most_frequent_int)]

      # move to next iteration
      i <- i + 1
    }
  }

  # print results
  print(paste("O2 generator rating x CO2 scrubber rating:"))
  print(paste(res_O2, "x", res_CO2, "=", res_O2*res_CO2))
}

# use function
calc_life_support(input)

# - Test ----
# example input
input <- c("00100", "11110", "10110", "10111", "10101", "01111",
                "00111", "11100", "10000", "11001", "00010", "01010")

# Attempt 1: break down example calculations
# separate each bit into individual 0/1, convert to integer and save as data frame
str_res <- sapply(strsplit(input, ""), \(x) substr(x, 1, 1) |> as.integer()) |> as.data.frame()
str_res

# separate each bit into individual 0/1, convert to integer and save as data frame
str_res <- sapply(strsplit(input, ""), \(x) substr(x, 1, 1) |> as.integer()) |> as.data.frame()
str_res

# calculate sum of each row = number of times 1 occurs
row_sum <- rowSums(str_res)

# if 1 occurs at least half the length of str_res -> 1 occrus most frequently
ifelse(row_sum[1] >= length(str_res)/2, 1, 0)

# keep only bits with 1 in first position
str_res <- str_res[which(str_res[1, ] == 1)]
row_sum <- rowSums(str_res)

# this time, 0 occurs more often than 1 --> keep only the numbers with 0 in second position
ifelse(row_sum[2] >= length(str_res)/2, 1, 0)

str_res <- str_res[which(str_res[2, ] == 0)]
row_sum <- rowSums(str_res)

# this time, 1 occurs more often than 0 --> keep only the numbers with 1 in third position
ifelse(row_sum[3] >= length(str_res)/2, 1, 0)

str_res <- str_res[which(str_res[3, ] == 1)]
row_sum <- rowSums(str_res)

# this time, 1 occurs more often than 0 --> keep only the numbers with 1 in fourth position
ifelse(row_sum[4] >= length(str_res)/2, 1, 0)

str_res <- str_res[which(str_res[4, ] == 1)]
row_sum <- rowSums(str_res)

# this time, 1 occurs more often than 0 --> keep only the numbers with 1 in fourth position
# equal number of 1s and 0s -> keep number with 1 in fifth position
ifelse(row_sum[5] >= length(str_res)/2, 1, 0)

str_res <- str_res[which(str_res[5, ] == 1)][[1]]
strtoi(paste0(str_res, collapse = ""), base = 2L)


# example input
input <- c("00100", "11110", "10110", "10111", "10101", "01111",
           "00111", "11100", "10000", "11001", "00010", "01010")


# separate each bit into individual 0/1, convert to integer and save as data frame
str_res <- sapply(strsplit(input, ""), \(x) substr(x, 1, 1) |> as.integer()) |> as.data.frame()
str_res

# calculate sum of each row = number of times 1 occurs
row_sum <- rowSums(str_res)

# if 1 occurs at least half the length of str_res -> 0 occurs least frequently
ifelse(row_sum[1] <= length(str_res)/2, 1, 0)

# keep only bits with 0 in first position
str_res <- str_res[which(str_res[1, ] == 0)]
row_sum <- rowSums(str_res)

# this time, 1 occurs less often than 0 --> keep only the numbers with 1 in second position
ifelse(row_sum[2] <= length(str_res)/2, 1, 0)

str_res <- str_res[which(str_res[2, ] == 1)]
row_sum <- rowSums(str_res)

# only two remaining bits--> keep the number with 0 in third position
str_res <- str_res[which(str_res[3, ] == 0)][[1]]

strtoi(paste0(str_res, collapse = ""), base = 2L)


# Attempt 2: test separate for loops
for (i in 1:nrow(str_res)) {

  print(paste("Iteration", i, "started"))
  print(str_res)

  print(paste("str_res <= 2:", length(str_res) <= 2))

  if (length(str_res) <= 2) {

    str_res <- str_res[which(str_res[i, ] == 1)][[1]]

    print(strtoi(paste0(str_res, collapse = ""), base = 2L))

    print("breaking")

    break

  }  else {

    # calculate rowsum = number of times 1 occurs
    row_sum <- rowSums(str_res)

    # if 1 occurs at least half the length of str_res -> 1 occurs most frequently
    most_frequent_int <- ifelse(row_sum[i] >= length(str_res)/2, 1, 0)

    # keep only bits with most_frequent_int in position i
    str_res <- str_res[which(str_res[i, ] == most_frequent_int)]

    i <- i + 1
  }

}

for (i in 1:nrow(str_res)) {

  print(paste("Iteration", i, "started"))
  print(str_res)

  print(paste("str_res <= 2:", length(str_res) <= 2))

  if (length(str_res) <= 2) {

    str_res <- str_res[which(str_res[i, ] == 0)][[1]]

    print(strtoi(paste0(str_res, collapse = ""), base = 2L))

    print("breaking")

    break

  }  else {

    # calculate rowsum = number of times 1 occurs
    row_sum <- rowSums(str_res)

    # if 1 occurs at least half the length of str_res -> 1 occurs most frequently
    most_frequent_int <- ifelse(row_sum[i] <= length(str_res)/2, 1, 0)

    # keep only bits with most_frequent_int in position i
    str_res <- str_res[which(str_res[i, ] == most_frequent_int)]

    i <- i + 1
  }

}

# compile loops into a single function --> see start of script for final function
# example input
input <- c("00100", "11110", "10110", "10111", "10101", "01111",
           "00111", "11100", "10000", "11001", "00010", "01010")

calc_life_support <- function(input) {
  # separate each bit into individual 0/1, convert to integer and save as data frame
  str_res <- sapply(strsplit(input, ""), \(x) substr(x, 1, 1) |> as.integer()) |> as.data.frame()

  # assign str_res to str_O2 and str_CO2 to separate loops
  str_O2 <- str_res
  str_CO2 <- str_res

  for (i in 1:nrow(str_O2)) {

    # print(paste("Iteration", i, "started"))
    # print(str_res)
    #
    # print(paste("str_res <= 2:", length(str_res) <= 2))

    if (length(str_O2) <= 2) {

      str_O2 <- str_O2[which(str_O2[i, ] == 1)][[1]]

      res_O2 <- strtoi(paste0(str_O2, collapse = ""), base = 2L)

      # print("breaking")

      break

    }  else {

      # calculate rowsum = number of times 1 occurs
      row_sum <- rowSums(str_O2)

      # if 1 occurs at least half the length of str_res -> 1 occurs most frequently
      most_frequent_int <- ifelse(row_sum[i] >= length(str_O2)/2, 1, 0)

      # keep only bits with most_frequent_int in position i
      str_O2 <- str_O2[which(str_O2[i, ] == most_frequent_int)]

      i <- i + 1
    }

  }

  for (i in 1:nrow(str_CO2)) {

    # print(paste("Iteration", i, "started"))
    # print(str_res)
    #
    # print(paste("str_res <= 2:", length(str_res) <= 2))

    if (length(str_CO2) <= 2) {

      str_CO2 <- str_CO2[which(str_CO2[i, ] == 0)][[1]]

      res_CO2 <- strtoi(paste0(str_CO2, collapse = ""), base = 2L)

      # print("breaking")

      break

    }  else {

      # calculate rowsum = number of times 1 occurs
      row_sum <- rowSums(str_CO2)

      # if 1 occurs at least half the length of str_res -> 1 occurs most frequently
      most_frequent_int <- ifelse(row_sum[i] <= length(str_CO2)/2, 1, 0)

      # keep only bits with most_frequent_int in position i
      str_CO2 <- str_CO2[which(str_CO2[i, ] == most_frequent_int)]

      i <- i + 1
    }

  }

  print(paste("O2 generator rating x CO2 scrubber rating:"))
  print(paste(res_O2, "x", res_CO2, "=", res_O2*res_CO2))

}

calc_life_support(input)

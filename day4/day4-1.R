## ---------------------------
## Advent of Code 2021
## Day 4 - part 1
##
## Instructions: https://adventofcode.com/2021/day/4
##
## Copyright (c) Stijn Van de Vondel, 2021
## ---------------------------

# - Import input ----
numbers <- readLines("day4/day4_input.txt", n = 1)
numbers <- strsplit(numbers, ",")[[1]] |> as.integer()

boards <- matrix(
  scan("day4/day4_input.txt", skip = 1),
  ncol = 5, byrow = TRUE) |> as.data.frame()

# - Convert boards to list of dataframes ----
# Convert boards to matrices: each has equal number of rows as it has columns = 5
col_num <- ncol(boards)
m_num <- nrow(boards)/col_num # 5x5 dataframes -> 100 matrices of 5x5

# group per 5x5 matrix and split accordingly
boards$group <- rep(c(1:m_num), each = col_num)
boards_list <- split(boards, boards$group)

# remove grouping column and add names for convenience
boards_list <- lapply(boards_list, \(x) x[, colnames(boards) != "group"])

names(boards_list) <- paste0("m", 1:m_num)

# remove obsolete objects
rm(col_num, m_num, boards)

# - Find winning board ----
# as long as 'stop' flag is FALSE -> continue looping
stop <- FALSE

# results of iteration stored separately
boards_res <- boards_list

# for every integer drawn from numbers
for (i in seq_along(numbers)) {

  # loop over the different boards, checking whether a number belongs to it
  for (j in seq_along(boards_res)) {

    # if a number is found in the board -> replace with NA
    boards_res[[j]][boards_res[[j]] == numbers[i]] <- NA

    # in case a row or column contains 5 NAs -> end loop and report result
    if (any(rowSums(is.na(boards_res[[j]])) == 5) | any(colSums(is.na(boards_res[[j]])) == 5)) {

      # print the name of the winning board, the last drawn number and the board itself
      paste("The winning board:", names(boards_res[j])) |> print()
      paste("The number at which the cycle ended is", numbers[i]) |> print()
      print("The resulting board (below):")
      boards_res[[j]] |> print()

      # print the sum of all non NA values in winning board
      result <- sum(boards_res[[j]], na.rm = TRUE)
      paste("The sum of the remaining numbers is", result) |> print()

      # print the final equation's result: (final number drawn) x (sum of board)
      paste("Final result:", numbers[i] * result) |> print()

      # end iteration
      stop <- TRUE
      break
    }
    if (stop){break}
  }
  if (stop){break}
}


# - Testing ----
numbers <- readLines("day4/day4_example_input.txt", n = 1)
numbers <- strsplit(numbers, ",")[[1]] |> as.integer()

boards <- matrix(
  scan("day4/day4_example_input.txt", skip = 1),
  ncol = 5, byrow = TRUE) |> as.data.frame()

# Convert boards to matrices: each has equal number of rows as it has columns = 5
col_num <- ncol(boards)
m_num <- nrow(boards)/col_num # 5x5 = 3 matrices

# group per 5x5 matrix and split accordingly
boards$group <- rep(c(1:m_num), each = col_num)
boards_list <- split(boards, boards$group)

# remove grouping column and add names for convenience
boards_list <- lapply(boards_list, \(x) x[, colnames(boards) != "group"])

names(boards_list) <- paste0("m", 1:m_num)

# remove obsolete objects
rm(col_num, m_num, boards)


# TRY FUNCTION ON A SINGLE df
one_df <- boards_list$m1

for (i in seq_along(numbers)) {
  # if any number in dataframe == numbers[i] -> replace with NA
  one_df[one_df == numbers[i]] <- NA

  # in case a row or column contains 5 NAs -> end loop
  if (any(rowSums(is.na(one_df)) == 5)) {
    msg <- paste("The number at which the cycle ended is", numbers[i])
    print(msg)
    print(one_df)

    result <- sum(one_df, na.rm = TRUE)
    print(paste("The sum of the remaining numbers is", result))

    break
  }
}


# TRY FUNCTION ON LIST OF DFS
# as long as 'stop' flag is FALSE -> continue looping
stop <- FALSE

# results of iteration stored separately
boards_res <- boards_list

# for every integer drawn from numbers
for (i in seq_along(numbers)) {

  # loop over the different boards, checking whether a number belongs to it
  for (j in seq_along(boards_res)) {

    # if a number is found in the board -> replace with NA
    boards_res[[j]][boards_res[[j]] == numbers[i]] <- NA

    # in case a row or column contains 5 NAs -> end loop and report result
    if (any(rowSums(is.na(boards_res[[j]])) == 5) | any(colSums(is.na(boards_res[[j]])) == 5)) {

      # print the name of the winning board, the last drawn number and the board itself
      paste("The winning board:", names(boards_res[j])) |> print()
      paste("The number at which the cycle ended is", numbers[i]) |> print()
      print("The resulting board (below):")
      boards_res[[j]] |> print()

      # print the sum of all non NA values in winning board
      result <- sum(boards_res[[j]], na.rm = TRUE)
      paste("The sum of the remaining numbers is", result) |> print()

      # print the final equation's result: (final number drawn) x (sum of board)
      paste("Final result:", numbers[i] * result) |> print()

      # end iteration
      stop <- TRUE
      break
    }
    if (stop){break}
  }
  if (stop){break}
}

library(magrittr)

test_input <- c(
  "L.LL.LL.LL",
  "LLLLLLL.LL",
  "L.L.L..L..",
  "LLLL.LL.LL",
  "L.LL.LL.LL",
  "L.LLLLL.LL",
  "..L.L.....",
  "LLLLLLLLLL",
  "L.LLLLLL.L",
  "L.LLLLL.LL"
)

real_input <- readLines("./inputs/day11-input.txt")

#- LOGIC ----------------------------------------------------------------------#

TXT_EMPTY    <- "L"
TXT_OCCUPIED <- "#"
TXT_FLOOR    <- "."

NUM_EMPTY    <- 0L
NUM_OCCUPIED <- 1L
NUM_FLOOR    <- 2L

convert_input_to_mx <- function(input) {
  rows <- length(input)
  cols <- nchar(input[1])
  
  
  int_array <- integer(rows * cols)
  txt_array <- input %>% paste(collapse = "") %>% strsplit(split = "") %>% unlist()
  int_array[txt_array == TXT_EMPTY] <- NUM_EMPTY
  int_array[txt_array == TXT_OCCUPIED] <- NUM_OCCUPIED
  int_array[txt_array == TXT_FLOOR] <- NUM_FLOOR
  
  mx <- matrix(int_array, nrow = rows, ncol = cols, byrow = TRUE)
}

count_occupied_adj <- function(mx, row, col) {
  # do not count seat itself
  NUM_DONOT_COUNT <- -1
  mx[row, col] <- NUM_DONOT_COUNT

  # define ranges
  r_from <- max(row - 1, 1)
  r_to   <- min(row + 1, nrow(mx))
  c_from <- max(col - 1, 1)
  c_to   <- min(col + 1, ncol(mx))
  
  neighborhood <- mx[r_from:r_to, c_from:c_to]
  sum(neighborhood == NUM_OCCUPIED)
}


decide <- function(mx, row, col) {
  seat_status <- mx[row, col]
  adj_occupied <-count_occupied_adj(mx, row, col)
  if (seat_status == NUM_EMPTY & adj_occupied == 0) NUM_OCCUPIED
  else if (seat_status == NUM_OCCUPIED & adj_occupied >= 4) NUM_EMPTY
  else seat_status
}

make_round <- function(mx) {
  mx_new <- mx
  for (row in 1:nrow(mx)) for (col in 1:ncol(mx)) {
    mx_new[row, col] <- decide(mx, row, col)
  }
  mx_new
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day11_part1_solution <- function(input) {
  mx <- convert_input_to_mx(input)
  i <- 0
  iter <- function(mx) {
    i <<- i + 1
    mx_new <- make_round(mx)
    if (all(mx_new == mx)) mx
    else iter(mx_new)
  }
  
  mx_new <- iter(mx)
  print(paste("number of rounds", i))
  
  sum(mx_new == NUM_OCCUPIED)
}

test_output_part1 <- 37
test_result <- day11_part1_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

real_result_part1 <- day11_part1_solution(real_input)
print(real_result_part1)

#- SOLUTION PART 2 ------------------------------------------------------------#

day11_part2_solution <- function(input) {
}

test_output_part2 <- ?
test_result <- day11_part2_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part2))

real_result_part2 <- day11_part2_solution(real_input)
print(real_result_part2)

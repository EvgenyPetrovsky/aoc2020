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
NUM_DONOT_COUNT <- 3L

convert_input_to_mx <- function(input) {

  cnt_rows <- length(input)
  cnt_cols <- nchar(input[1])

  int_array <- integer(cnt_rows * cnt_cols)
  txt_array <- input %>% paste(collapse = "") %>% strsplit(split = "") %>% unlist()
  int_array[txt_array == TXT_EMPTY] <- NUM_EMPTY
  int_array[txt_array == TXT_OCCUPIED] <- NUM_OCCUPIED
  int_array[txt_array == TXT_FLOOR] <- NUM_FLOOR

  mx <- matrix(int_array, nrow = cnt_rows, ncol = cnt_cols, byrow = TRUE)
}

count_occupied_adj <- function(mx, row, col) {

  cnt_rows <- nrow(mx)
  cnt_cols <- ncol(mx)

  # do not count seat itself
  mx[row, col] <- NUM_DONOT_COUNT

  # define ranges
  r_from <- max(row - 1, 1)
  r_to   <- min(row + 1, cnt_rows)
  c_from <- max(col - 1, 1)
  c_to   <- min(col + 1, cnt_cols)

  neighborhood <- mx[r_from:r_to, c_from:c_to]
  sum(neighborhood == NUM_OCCUPIED)
}


decide <- function(seat_status, occupied, max_occupied) {
  if (seat_status == NUM_EMPTY & occupied == 0) NUM_OCCUPIED
  else if (seat_status == NUM_OCCUPIED & occupied >= max_occupied) NUM_EMPTY
  else seat_status
}

make_round <- function(mx, occipied_cnt_fn, max_occupied) {

  cnt_rows <- nrow(mx)
  cnt_cols <- ncol(mx)

  mx_new <- mx
  for (row in 1:cnt_rows) for (col in 1:cnt_cols) {
    occupied <- occipied_cnt_fn(mx, row, col)
    seat_status <- mx[row, col]
    mx_new[row, col] <- decide(seat_status, occupied, max_occupied)
  }
  mx_new
}

status_in_direction <- function(mx, row, col, dr, dc) {
  new_row <- row + dr
  new_col <- col + dc
  cnt_rows <- nrow(mx)
  cnt_cols <- ncol(mx)
  seat_status <-
    if (dr == 0 & dc == 0) NUM_DONOT_COUNT
    else if (new_row < 1 | new_row > cnt_rows) NUM_EMPTY
    else if (new_col < 1 | new_col > cnt_cols) NUM_EMPTY
    else if (mx[new_row, new_col] == NUM_FLOOR)
      status_in_direction(mx, new_row, new_col, dr, dc)
    else mx[new_row, new_col]
  seat_status
}

count_occupied_vis <- function(mx, row, col) {
  directions <- expand.grid(dr = -1:1, dc = -1:1)
  seat_statuses <-
    mapply(
      FUN = function(dr, dc) status_in_direction(mx, row, col, dr, dc),
      directions$dr,
      directions$dc,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )

  sum(seat_statuses == NUM_OCCUPIED)
}
#- SOLUTION PART 1 ------------------------------------------------------------#

day11_part1_solution <- function(input) {
  mx <- convert_input_to_mx(input)
  fn <- count_occupied_adj
  max_occupied <- 4
  i <- 0
  iter <- function(mx) {
    i <<- i + 1
    mx_new <- make_round(mx, fn, max_occupied)
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
  mx <- convert_input_to_mx(input)
  fn <- count_occupied_vis
  max_occupied <- 5
  i <- 0
  iter <- function(mx) {
    i <<- i + 1
    mx_new <- make_round(mx, fn, max_occupied)
    if (all(mx_new == mx)) mx
    else iter(mx_new)
  }

  mx_new <- iter(mx)
  print(paste("number of rounds", i))

  sum(mx_new == NUM_OCCUPIED)
}

test_output_part2 <- 26
test_result <- day11_part2_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part2))

real_result_part2 <- day11_part2_solution(real_input)
print(real_result_part2)

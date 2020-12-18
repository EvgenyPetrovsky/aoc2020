library(magrittr)

test_input <- c(0, 3, 6)

real_input <- readLines("./inputs/day15-input.txt") %>% as.integer()

#- LOGIC ----------------------------------------------------------------------#

#' get last number find same number among predecessors
#' if number never appeared before then 0
#' else distance between last and 2nd to last appearance
tell_me_next_number <- function(numbers) {
  last_position <- length(numbers)
  last_number <- numbers[last_position]
  predecessors <- which(numbers[-last_position] == last_number)
  pred_position <- if (length(predecessors) == 0) 0 else max(predecessors)
  if (pred_position == 0) 0 else last_position - pred_position
}

tell_me_next_number_using_history <- function(history) {
  number <- history$last_number
  num_last_position <- history$number_position_last[number + 1]
  num_pred_position <- history$number_position_pred[number + 1]
  if (num_pred_position == 0) 0
  else num_last_position - num_pred_position
}

add_number_to_history <- function(history, number, position) {

  former_last_position <- history$number_position_last[number + 1]

  history$number_position_pred[number + 1] <- former_last_position
  history$number_position_last[number + 1] <- position

  history$last_number <- as.integer(number)
  history$last_position <- as.integer(position)

  history
}


init_history <- function(n) {
  list(
    number_position_last = rep(0L, n + 1),
    number_position_pred = rep(0L, n + 1),
    last_number = 0L,
    last_position = 0L
  )
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day15_part1_solution <- function(input) {
  finish <- 2020L
  history <-
    seq_along(input) %>%
    Reduce(f = function(z, x) {
      add_number_to_history(history = z, number = input[x], position = x)
    }, init = init_history(finish))

  input_len <- length(input) %>% as.integer()
  start <- input_len + 1L
  i <- start
  while (i <= finish) {
    if (i %% 10^5 == 0) print(i)
    next_number <- tell_me_next_number_using_history(history)
    history <- add_number_to_history(
      history = history,
      number = next_number,
      position = i)
    i <- i + 1
  }
  next_number
}

test_output_part1 <- 436
test_result <- day15_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day15_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))


#- SOLUTION PART 2 ------------------------------------------------------------#

day15_part2_solution <- function(input) {
  finish <- 300000
  history <-
    seq_along(input) %>%
    Reduce(f = function(z, x) {
      add_number_to_history(history = z, number = input[x], position = x)
    }, init = init_history(finish))

  input_len <- length(input) %>% as.integer()
  start <- input_len + 1L
  i <- start
  print(paste("time:", Sys.time(), "iteration", i))
  while (i <= finish) {
    if (i %% 10^5 == 0) print(paste(
      "time:", Sys.time(),
      "iteration", i,
      "history size:", object.size(history) %/% 2^20, "MB."))
    next_number <- tell_me_next_number_using_history(history)
    history <- add_number_to_history(
      history = history,
      number = next_number,
      position = i)
    i <- i + 1
  }
  next_number
}

day15_part2_solution_optimized <- function(input) {
  finish <- 30000000 %>% as.integer()
  history <-
    seq_along(input) %>%
    Reduce(f = function(z, x) {
      add_number_to_history(history = z, number = input[x], position = x)
    }, init = init_history(finish))

  input_len <- length(input) %>% as.integer()
  start <- input_len + 1L
  position <- start
  print(paste("time:", Sys.time(), "iteration:", position))
  while (position <= finish) {
    if (position %% 10^6 == 0) print(paste(
      "time:", Sys.time(),
      "iteration", format(position, scientific = F, width = 8),
      ""))

    number <- history$last_number
    num_last_position <- history$number_position_last[number + 1]
    num_pred_position <- history$number_position_pred[number + 1]
    next_number <-
      if (num_pred_position == 0) 0
      else num_last_position - num_pred_position

    former_last_position <- history$number_position_last[next_number + 1]

    history$number_position_pred[next_number + 1] <- former_last_position
    history$number_position_last[next_number + 1] <- position

    history$last_number <- as.integer(next_number)
    history$last_position <- as.integer(position)

    position <- position + 1
  }
  next_number
}


test_output_part2 <- 175594
test_result <- day15_part2_solution_optimized(test_input)

print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day15_part2_solution_optimized(real_input)
print(format(real_result_part2, scientific = FALSE))

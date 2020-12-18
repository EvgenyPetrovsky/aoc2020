library(magrittr)

test_input <- c(
  0,3,6
)

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
  last_number_idx <- as.character(history$last_number)
  num_last_position <- history$last_position
  num_pred_position <- history$number_positions[[last_number_idx]]$pred_position
  if (num_pred_position == 0) 0
  else num_last_position - num_pred_position
}

add_number_to_history <- function(history, number, position) {
  index <- as.character(number)
  item <- history$number_positions[[index]]
  former_last_position <- if (is.null(item)) 0 else item$last_position

  history$number_positions[[index]]$pred_position <- former_last_position
  history$number_positions[[index]]$last_position <- position

  history$last_number <- number
  history$last_position <- position

  history
}


#- SOLUTION PART 1 ------------------------------------------------------------#

day15_part1_solution_initial <- function(input) {
  input_len <- length(input)
  start <- input_len + 1
  finish <- 2020
  numbers <- 
    (start:finish) %>% 
    Reduce(f = function(z, x) c(z, tell_me_next_number(z)), init = input)
  numbers[finish]
}

day15_part1_solution <- function(input) {
  history <- 
    seq_along(input) %>% 
    Reduce(f = function(z, x) {
      add_number_to_history(history = z, number = input[x], position = x)
    }, init = list())
  
  input_len <- length(input)
  start <- input_len + 1
  i <- start
  finish <- 2020
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
  finish <- 30000000
  history <- 
    seq_along(input) %>% 
    Reduce(f = function(z, x) {
      add_number_to_history(history = z, number = input[x], position = x)
    }, init = list())
  
  input_len <- length(input)
  start <- input_len + 1
  i <- start
  print(paste("time:", Sys.time(), "iteration", i))
  while (i <= finish) {
    if (i %% 10^5 == 0) print(paste("time:", Sys.time(), "iteration", i))
    next_number <- tell_me_next_number_using_history(history)
    history <- add_number_to_history(
      history = history, 
      number = next_number, 
      position = i)
    i <- i + 1
  }
  next_number
}

test_output_part2 <- 175594
test_result <- day15_part2_solution(test_input)

print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day15_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

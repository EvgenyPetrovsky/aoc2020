library(magrittr)

test_input <- c(
  "389125467"
)

real_input <- readLines("./inputs/day23-input.txt")

#- LOGIC ----------------------------------------------------------------------#

const_MOVE_LEN <- 3L
const_CURRENT_POS <- 1L

parse_input <- function(input) {
  input %>% strsplit(split = "") %>% unlist() %>% as.integer()
}

#' The crab picks up the three cups that are immediately clockwise of the 
#' current cup. They are removed from the circle; cup spacing is adjusted as 
#' necessary to maintain the circle.
pick_cups <- function(cups, current_pos) {
  rep(cups, 2) %>% 
    magrittr::extract((current_pos+1):(current_pos+const_MOVE_LEN))
}

#' The crab selects a destination cup: the cup with a label equal to the current 
#' cup's label minus one. If this would select one of the cups that was just 
#' picked up, the crab will keep subtracting one until it finds a cup that 
#' wasn't just picked up. If at any point in this process the value goes below 
#' the lowest value on any cup's label, it wraps around to the highest value on 
#' any cup's label instead.
find_destination_cup <- function(
  current_cup, 
  remaining_cups, 
  picked_cups = NULL, 
  min_cup = min(remaining_cups), 
  max_cup = max(remaining_cups)
) {
  if (!is.null(picked_cups)) {
    candidates <- (current_cup-4):(current_cup-1)
    cup <- candidates[!candidates %in% picked_cups] %>% max()
    if (cup <= 0) {
      candidates <- (max_cup-3):(max_cup)
      cup <- candidates[!candidates %in% picked_cups] %>% max()
    } else {
      cup
    }
  } else {
  below <- remaining_cups[remaining_cups < current_cup]
  if (length(below) > 0) max(below)
  else max(remaining_cups)
  }
}

#' The crab places the cups it just picked up so that they are immediately 
#' clockwise of the destination cup. They keep the same order as when they were 
#' picked up.
place_cups <- function(cups_to_place, chose_from_cups, current_cup, destination_cup) {
  destination_pos <- which(chose_from_cups == destination_cup)
  chose_from_len <- length(chose_from_cups)
  
  cups_before_destination <- chose_from_cups[1:destination_pos]
  cups_after_destination <- 
    if(destination_pos < chose_from_len) chose_from_cups[(destination_pos+1):chose_from_len]
    else integer()

  new_cups_order <- c(
    cups_before_destination,
    cups_to_place,
    cups_after_destination,
    current_cup
  )
  
  new_cups_order
}

play_round <- function(
  cups, 
  min_cup = min(cups), 
  max_cup = max(cups), 
  cups_len = length(cups)
) {
  picked_cups <- cups[(const_CURRENT_POS+1):(const_CURRENT_POS+const_MOVE_LEN)]
  current_cup <- cups[const_CURRENT_POS]
  remaining_cups <- cups[(const_CURRENT_POS+const_MOVE_LEN+1):cups_len]
  destination_cup <- find_destination_cup(current_cup, remaining_cups, picked_cups, min_cup, max_cup)
  place_cups(picked_cups, remaining_cups, current_cup, destination_cup)
}

#' extract sequence of cups starting after cup with numer = start_cup 
#' of length = number_cups
extract_answer_sequence <- function(cups, start_cup, number_cups) {
  start_cup_pos <- which(cups == start_cup)
  end_cup_pos <- start_cup_pos + number_cups 
  rep(cups, 2) %>% magrittr::extract((start_cup_pos+1):end_cup_pos)
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day23_part1_solution <- function(input, rounds) {
  cups <- input %>% parse_input()
  result <- 1:rounds %>% Reduce(f = function(z, x) play_round(z), init = cups)

  result %>% 
    extract_answer_sequence(start_cup = 1, number_cups = length(cups) - 1) %>% 
    paste(collapse = "")
}

print("-- PART 1 ------------------")
test_output_part1 <- "92658374"
test_result <- day23_part1_solution(test_input, 10)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

test_output_part1 <- "67384529"
test_result <- day23_part1_solution(test_input, 100)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day23_part1_solution(real_input, 100)
print(paste("real result:", format(real_result_part1, scientific = FALSE)))

#- SOLUTION PART 2 ------------------------------------------------------------#

day23_part2_solution <- function(input, rounds = 10^7, cups_len = 10^6) {
  cups_input <- input %>% parse_input()
  input_len <- length(cups_input)
  cups <- c(
    cups_input, 
    seq.int(
      from = max(cups_input) + 1, 
      by = 1, 
      length.out = cups_len - input_len
    )
  )
  min_cup <- min(cups)
  max_cup <- max(cups)
  
  i <- 1L; j <- 1L
  result <- cups
  while (i <= rounds) {
    if(j == 1000L) {
      print(paste("Round:", format(i, width = 7), "Time:", Sys.time()))
      j <- 0
    }
    result <- play_round(result, min_cup, max_cup, cups_len)
    i <- i + 1; j <- j + 1
  }

  result %>% 
    extract_answer_sequence(start_cup = 1, number_cups = 2) %>% 
    paste(collapse = "")
}

print("-- PART 2 ------------------")
test_output_part2 <- -1
test_result <- day23_part2_solution(test_input, rounds = 3*10^3, cups_len = 10^6)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day23_part2_solution(real_input, 1)
print(paste("real result:", format(real_result_part1, scientific = FALSE)))

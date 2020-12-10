library(magrittr)

test_input <- c(
  16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4
)

test_input_2 <- c(
  28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3
)

real_input <- readLines("./inputs/day10-input.txt") %>% as.integer()

#- LOGIC ----------------------------------------------------------------------#
get_diff <- function(input, outlet, device) {
  len <- length(input)
  org_seq <- (input)
  out_seq <- c(outlet, org_seq)
  dev_seq <- c(org_seq, device)
  dev_seq - out_seq
}

valid_arrangement <- function(diff) {
  unique_diff_nom_values <- diff %>% unique()
  allowed_diff_nom_values <- c(1,3)
  all(unique_diff_nom_values %in% allowed_diff_nom_values)
}

num_diff <- function(diff, diff_size) {
  (diff == diff_size) %>% sum()
}
#- SOLUTION PART 1 ------------------------------------------------------------#

day10_part1_solution <- function(input, diff_size) {
  outlet <- 0
  device <- max(input) + 3
  sort(input) %>% get_diff(outlet, device) %>% num_diff(diff_size)
}

test_output_part1 <- 7
test_result <- day10_part1_solution(test_input, diff_size = 1)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

test_output_part1 <- 5
test_result <- day10_part1_solution(test_input, diff_size = 3)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

test_output_part1 <- 22
test_result <- day10_part1_solution(test_input_2, diff_size = 1)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

test_output_part1 <- 10
test_result <- day10_part1_solution(test_input_2, diff_size = 3)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))


real_result_part1_1 <- day10_part1_solution(real_input, 1)
real_result_part1_2 <- day10_part1_solution(real_input, 3)
print(real_result_part1_1 * real_result_part1_2)

#- SOLUTION PART 2 ------------------------------------------------------------#

day10_part2_solution <- function(input) {
  max_allowed_diff <- 3
  outlet <- 0
  device <- max(input) + max_allowed_diff
  
  iter <- function(int_input, pos) {
    int_len <- length(int_input)
    if (pos > length(int_input)) {
      0
    }
    else {
      nom_diff <- get_diff(int_input, outlet, device)
      max_nom_diff <- max(nom_diff)
      is_valid <- 
        if (pos > 1) 0
        else int_input %>% get_diff(outlet, device) %>% valid_arrangement() + 0
      if (max_nom_diff > max_allowed_diff)
        0
      else {
        if (pos + 1 < int_len & all(int_input[pos:(pos+1)] == 1))
          
          is_valid + iter(int_input[c(-pos,-pos-1)], 1) + iter(int_input, pos + 1)
        else 
          is_valid + iter(int_input, pos + 1)
      }
    }
  }
  iter(sort(input), pos = 1)
}

test_output_part2 <- 8
test_result <- day10_part2_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part2))


real_result_part2 <- day10_part2_solution(real_input)
print(real_result_part2)

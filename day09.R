library(magrittr)

test_input <- c(
  35,
  20,
  15,
  25,
  47,
  40,
  62,
  55,
  65,
  95,
  102,
  117,
  150,
  182,
  127,
  219,
  299,
  277,
  309,
  576
)

real_input <- readLines("./inputs/day09-input.txt") %>% as.double()

#- LOGIC ----------------------------------------------------------------------#

is_valid_number <- function(number, previous_numbers) {
  valid_numbers <- combn(previous_numbers, 2) %>% apply(MARGIN = 2, FUN = sum)
  number %in% valid_numbers
}

find_invalid_number_position <- function(numbers, preamble_size) {
  init_pos <- preamble_size + 1
  len <- length(numbers)
  iter <- function(pos) {
    if (pos > len) return(NA)
    preamble_beg <- pos-preamble_size
    preamble_end <- pos - 1
    preamble <- numbers[preamble_beg:preamble_end]
    number <- numbers[pos]
    if (!is_valid_number(number, preamble)) pos
    else iter(pos + 1)
  }
  iter(init_pos)
}

# R doesn't do tail-recursion optimization :-(
find_sequence <- function(numbers, result_sum) {
  len = length(numbers)
  init_pos = 1
  min_seq_len = 2
  iter <- function(pos, seq_len) {
    pos_end <- pos + seq_len - 1
    if (pos > len) {
      NA_real_
    } else {
      num_seq <- numbers[pos:pos_end]
      sum_num_seq <- sum(num_seq)
      if (sum_num_seq == result_sum) 
        num_seq
      else if (sum_num_seq > result_sum) 
        iter(pos = pos + 1, seq_len = min_seq_len)
      else 
        iter(pos = pos, seq_len = seq_len + 1)
    }
  }
  iter(pos = 1, seq_len = min_seq_len)
}

find_sequence_in_a_loop <- function(numbers, result_sum) {
  len = length(numbers)
  min_seq_len = 2
  pos = 1
  end_pos <- pos + min_seq_len - 1
  while (pos < len) {
    while(end_pos <= len & sum(numbers[pos:end_pos]) <= result_sum) {
      if (sum(numbers[pos:end_pos]) == result_sum) return(numbers[pos:end_pos])
      end_pos <- end_pos + 1
    }
    pos <- pos + 1
    end_pos <- pos + min_seq_len - 1
  }
  NA
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day09_part1_solution <- function(input, preamble_size) {
  ip <- find_invalid_number_position(input, preamble_size)
  input[ip]
}

test_output_part1 <- 127
test_result <- day09_part1_solution(test_input, preamble_size = 5)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

real_result_part1 <- day09_part1_solution(real_input, 25)
print(real_result_part1)

#- SOLUTION PART 2 ------------------------------------------------------------#

day09_part2_solution <- function(input, preamble_size) {
  iv <- day09_part1_solution(input, preamble_size)
  #hack_seq <- find_sequence(numbers = input, result_sum = iv)
  hack_seq <- find_sequence_in_a_loop(numbers = input, result_sum = iv)
  min(hack_seq) + max(hack_seq)
}

test_output_part2 <- 62
test_result <- day09_part2_solution(test_input, 5)
print(paste("test result:", test_result, "valid:", test_result == test_output_part2))


real_result_part2 <- day09_part2_solution(real_input, 25)
print(real_result_part2)

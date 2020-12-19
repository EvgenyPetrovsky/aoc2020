library(magrittr)

test_input <- c(
)

real_input <- readLines("./inputs/dayXX-input.txt")

#- LOGIC ----------------------------------------------------------------------#


#- SOLUTION PART 1 ------------------------------------------------------------#

dayXX_part1_solution <- function(input) {
  NULL
}

test_output_part1 <- -1
test_result <- dayXX_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- dayXX_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

dayXX_part2_solution <- function(input) {
  NULL
}

test_output_part2 <- -1
test_result <- dayXX_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- dayXX_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

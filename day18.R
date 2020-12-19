library(magrittr)

test_input <- c(
  "1 + 2 * 3 + 4 * 5 + 6"
)

real_input <- readLines("./inputs/day18-input.txt")

#- LOGIC ----------------------------------------------------------------------#

`%op_sum%` <- function(a, b) a + b
`%op_mul%` <- function(a, b) a * b

parse_input <- function(input_text) {
  input_text %>%
    gsub(pattern = "+", replacement = "%op_sum%", fixed = TRUE) %>%
    gsub(pattern = "*", replacement = "%op_mul%", fixed = TRUE)
}

evaluate <- function(expression) {
  parsed <- parse(text = expression)
  evaluated <- eval(parsed)
  evaluated
}

parse_input_2 <- function(input_text) {
  input_text %>%
    gsub(pattern = "+", replacement = "^", fixed = TRUE)
}

evaluate_2 <- function(expression) {
  `^` <- sum
  parsed <- parse(text = expression)
  evaluated <- eval(parsed)
  evaluated
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day18_part1_solution <- function(input) {
  new_input <- parse_input(input)
  eval_results <-
    new_input %>%
    Map(f = evaluate)

  Reduce(eval_results, f = sum)
}

test_output_part1 <- 71
test_result <- day18_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day18_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day18_part2_solution <- function(input) {
  new_input <- parse_input_2(input)
  eval_results <-
    new_input %>%
    Map(f = evaluate_2)

  Reduce(eval_results, f = sum)
}

test_output_part2 <- 231
test_result <- day18_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day18_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

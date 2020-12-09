test_input <- c(1721,979,366,299,675,1456)
real_input <- read.table("./inputs/day01-input.txt", header = F)[[1]]

#------------------------------------------------------------------------------#

test_output_part1 <- 514579

day01_part1_solution <- function(input) {
  sum_criterion <- 2020
  cmb <-combn(input, 2)
  col_filter <- apply(X = cmb, MARGIN = 2, FUN = function(x) sum(x) == sum_criterion)
  
  values <- cmb[,col_filter]
  result <- prod(values)
  result
}

test_result <- day01_part1_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output))

real_result_part1 <- day01_part1_solution(real_input)
print(real_result_part1)

#------------------------------------------------------------------------------#

test_output_part2 <- 241861950

day01_part2_solution <- function(input) {
  sum_criterion <- 2020
  cmb <-combn(input, 3)
  col_filter <- apply(X = cmb, MARGIN = 2, FUN = function(x) sum(x) == sum_criterion)
  
  values <- cmb[,col_filter]
  result <- prod(values)
  result
}

test_result <- day01_part2_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part2))

real_result_part2 <- day01_part2_solution(real_input)
print(real_result_part2)

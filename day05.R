test_input <- c("BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL")

real_input <- readLines("./inputs/day05-input.txt")

#- LOGIC ----------------------------------------------------------------------#

row_code <- function(seat_code) {substr(seat_code, 1, 7)}
col_code <- function(seat_code) {substr(seat_code, 8, 10)}

row_number <- function(row_code) {
  row_code %>%
    gsub(pattern = "F", replacement = 0) %>%
    gsub(pattern = "B", replacement = 1) %>%
    strtoi(base = 2)
}

col_number <- function(col_code) {
  col_code %>%
    gsub(pattern = "L", replacement = 0) %>%
    gsub(pattern = "R", replacement = 1) %>%
    strtoi(base = 2)
}

seat_id <- function(seat_code)  {
  seat_code %>%
    gsub(pattern = "F", replacement = 0) %>%
    gsub(pattern = "B", replacement = 1) %>%
    gsub(pattern = "L", replacement = 0) %>%
    gsub(pattern = "R", replacement = 1) %>%
    strtoi(base = 2)
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day05_part1_solution <- function(input) {
  seat_id(input) %>% max()
}

test_output_part1 <- max(c(567, 119, 820))
test_result <- day05_part1_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

real_result_part1 <- day05_part1_solution(real_input)
print(real_result_part1)

#- SOLUTION PART 2 ------------------------------------------------------------#

day05_part2_solution <- function(input) {
  seat_ids <- input %>% seat_id()
  free_seats <- rep(T, 128*8)
  free_seats[seat_ids] <- F
  which(free_seats)
}

real_result_part2 <- day05_part2_solution(real_input)
print(real_result_part2)

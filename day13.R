library(magrittr)

test_input <- c(
  "939",
  "7,13,x,x,59,x,31,19"
)

real_input <- readLines("./inputs/day13-input.txt")

#- LOGIC ----------------------------------------------------------------------#

input_time <- function(input) {
  input[1] %>% as.integer()
}

input_busses <- function(input) {
  input[2] %>%
    strsplit(split = ",") %>%
    unlist() %>%
    Filter(f = function(bus_id) bus_id != "x") %>%
    as.integer()
}

input_busses_part2 <- function(input) {
  input[2] %>%
    strsplit(split = ",") %>%
    unlist() %>%
    gsub(pattern = "x", replacement = "1", fixed = TRUE) %>%
    as.integer()
}

waiting_time <- function(start_time, bus_id) {
  time_ago <- start_time %% bus_id
  if (time_ago == 0) timea_ago
  else bus_id - time_ago
}

gcd <- function(a, b) {
  if (b == 0) a
  else gcd(b, a %% b)
}

lcm <- function(a, b) as.double(abs(a*b) / gcd(a, b))

iterations_to_zero <- function(start, inc, div) {
  i <- 1
  while (start != 0) {
    start <- (start + inc) %% div
    i <- i + 1
  }
  i
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day13_part1_solution <- function(input) {
  begin_time <- input_time(input)
  busses <- input_busses(input)

  waiting_times <- mapply(FUN = waiting_time, begin_time, busses)

  bus_idx <- which(waiting_times == min(waiting_times))[1]
  bus_id <- busses[bus_idx]
  waiting_time <- waiting_times[bus_idx]

  bus_id * waiting_time

}

test_output_part1 <- 295
test_result <- day13_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day13_part1_solution(real_input)
print(real_result_part1)

#- SOLUTION PART 2 ------------------------------------------------------------#

#' idea for new solution: start with inc = 1 and find a number when 1st element is 1
#' continue with inc = lcm(1st) and find number when 2nd el is 2
#' continue with inc = lcm(1st, 2nd) and find a number when 3rd el is 3, ... until all numbers are in

day13_part2_solution <- function(input, start_time = 0) {
  busses <- input_busses_part2(input)
  num_busses <- length(busses)
  max_bus_id <- max(busses) %>% as.integer()
  max_bus_idx <- which(busses == max_bus_id)[1]
  diff_times <- seq.int(
    from = -(max_bus_idx-1),
    by = 1,
    length.out = num_busses)

  filter_busses <- (busses != 1)
  busses_filtered <- busses[filter_busses]
  diff_times_filtered <- diff_times[filter_busses]
  time <- as.double(start_time - (start_time %% max_bus_id))
  diff_times_filtered <- as.integer((time + diff_times_filtered) %% busses_filtered)
  
  lcm_input <- mapply(
    FUN = function(x, y) 
      iterations_to_zero(x, max_bus_id, y), 
    diff_times_filtered, busses_filtered,
    SIMPLIFY = T, USE.NAMES = F
    )
  
  iterations_to_all_zero <- Reduce(x = lcm_input, f = lcm)
  
  winning_start_time <- time + (iterations_to_all_zero * max_bus_id) - max_bus_idx + 1
  winning_start_time
}

test_output_part2 <- 1068781
test_result <- day13_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

# more tests
test_result <- day13_part2_solution(c("X", "17,x,13,19"))
print(paste(
  "test result:", test_result,
  "valid:", test_result == 3417))

test_result <- day13_part2_solution(c("X", "67,7,59,61"))
print(paste(
  "test result:", test_result,
  "valid:", test_result == 754018))

test_result <- day13_part2_solution(c("X", "67,x,7,59,61"), 770000)
print(paste(
  "test result:", test_result,
  "valid:", test_result == 779210))

test_result <- day13_part2_solution(c("X", "67,7,x,59,61"), 10)
print(paste(
  "test result:", test_result,
  "valid:", test_result == 1261476))

test_result <- day13_part2_solution(c("X", "1789,37,47,1889"), 0)
print(paste(
  "test result:", test_result,
  "valid:", test_result == 1202161486))

real_result_part2 <- day13_part2_solution(real_input, 100000000000000)
print(format(real_result_part2, scientific = F))

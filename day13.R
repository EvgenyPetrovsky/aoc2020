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

day13_part2_solution <- function(input, start_time = 0) {
  busses <- input_busses_part2(input)
  num_busses <- length(busses)
  max_bus_id <- max(busses) %>% as.integer()
  max_bus_idx <- which(busses == max_bus_id)[1]
  diff_times <- seq.int(
    from = -(max_bus_idx-1),
    by = 1,
    length.out = num_busses)

  filter_busses <- (busses != 1 & busses != max_bus_id)
  busses_filtered <- busses[filter_busses]
  diff_times_filtered <- diff_times[filter_busses]

  time_check <- FALSE
  time <- as.double(start_time - (start_time %% max_bus_id))
  i <- 0
  i_print <- 10
  while (!time_check) {
    i <- i + 1
    if (i == i_print) {
      i_print <- i_print * 10
      print(paste("iteration", i))
      print(paste("time", time))
    }
    time <- (time + max_bus_id)
    diff_times_filtered <- (diff_times_filtered + max_bus_id) %% busses_filtered
    time_check <- all(diff_times_filtered == 0)
  }
  winning_start_time <- time - max_bus_idx + 1
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

test_result <- day13_part2_solution(c("X", "1789,37,47,1889"), 20000)
print(paste(
  "test result:", test_result,
  "valid:", test_result == 1202161486))

real_result_part2 <- day13_part2_solution(real_input, 100000000000000)
print(real_result_part2)

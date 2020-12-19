library(magrittr)

test_input <- c(
".#.",
"..#",
"###"
)

real_input <- readLines("./inputs/day17-input.txt")

#- LOGIC ----------------------------------------------------------------------#

parse_input <- function(input) {
  char_ACTIVE_STATE <- "#"
  nx <- input[1] %>% nchar()
  ny <- length(input)
  states <- array(data = 0L, dim = c(nx, ny))
  for (y in seq_along(input)) {
    xs <-
      (strsplit(input[y], split = "") %>% unlist() == char_ACTIVE_STATE) %>%
      which()
    for (x in xs) {
      states[x, y] <- 1L
    }
  }
  states
}

fill_init_states <- function(input_states, nr_cycles) {
  dims <- dim(input_states)
  init_states <- array(data = 0L, dim = c(dims[1:2], 1) + nr_cycles * 2)
  for (x in 1:dims[1]) for (y in 1:dims[2]) {
    init_states[x + nr_cycles, y + nr_cycles, 1 + nr_cycles] <-
      input_states[x, y]
  }
  init_states
}

run_activation <- function(states) {
  new_states <- states
  dims <- dim(states)
  xs <- 1:dims[1]; ys <- 1:dims[2]; zs <- 1:dims[3]
  for (x in xs) for (y in ys) for (z in zs) {
    node_state <- states[x, y, z]
    active_neighbors <-
      sum(states[
        xs[xs >= x - 1 & xs <= x + 1],
        ys[ys >= y - 1 & ys <= y + 1],
        zs[zs >= z - 1 & zs <= z + 1]
      ]) - node_state
    new_states[x, y, z] <-
      # If a cube is active and exactly 2 or 3 of its neighbors are also active,
      # the cube remains active
      if (node_state == 1L & active_neighbors %in% 2:3) 1L
      # Otherwise, the cube becomes inactive
      else if (node_state == 1L) 0L
      # If a cube is inactive but exactly 3 of its neighbors are active,
      # the cube becomes active
      else if (active_neighbors == 3) 1L
      # Otherwise, the cube remains inactive
      else 0L
  }
  new_states
}


fill_init_states_4d <- function(input_states, nr_cycles) {
  dims <- dim(input_states)
  init_states <- array(data = 0L, dim = c(dims[1:2], 1, 1) + nr_cycles * 2)
  for (x in 1:dims[1]) for (y in 1:dims[2]) {
    init_states[x + nr_cycles, y + nr_cycles, 1 + nr_cycles, 1 + nr_cycles] <-
      input_states[x, y]
  }
  init_states
}

run_activation_4d <- function(states) {
  new_states <- states
  dims <- dim(states)
  xs <- 1:dims[1]; ys <- 1:dims[2]; zs <- 1:dims[3]; ws <- 1:dims[4]
  for (x in xs) for (y in ys) for (z in zs) for (w in ws) {
    node_state <- states[x, y, z, w]
    active_neighbors <-
      sum(states[
        xs[xs >= x - 1 & xs <= x + 1],
        ys[ys >= y - 1 & ys <= y + 1],
        zs[zs >= z - 1 & zs <= z + 1],
        ws[ws >= w - 1 & ws <= w + 1]
      ]) - node_state
    new_states[x, y, z, w] <-
      # If a cube is active and exactly 2 or 3 of its neighbors are also active,
      # the cube remains active
      if (node_state == 1L & active_neighbors %in% 2:3) 1L
    # Otherwise, the cube becomes inactive
    else if (node_state == 1L) 0L
    # If a cube is inactive but exactly 3 of its neighbors are active,
    # the cube becomes active
    else if (active_neighbors == 3) 1L
    # Otherwise, the cube remains inactive
    else 0L
  }
  new_states
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day17_part1_solution <- function(input) {
  input_states <- parse_input(input)
  nr_cycles <- 6
  init_states <- fill_init_states(input_states, nr_cycles)
  final_states <-
    (1:nr_cycles) %>%
    Reduce(
      f = function(z, x) run_activation(z),
      init = init_states)
  sum(final_states)
}

test_output_part1 <- 112
test_result <- day17_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day17_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day17_part2_solution <- function(input) {
  input_states <- parse_input(input)
  nr_cycles <- 6
  init_states <- fill_init_states_4d(input_states, nr_cycles)
  final_states <-
    (1:nr_cycles) %>%
    Reduce(
      f = function(z, x) run_activation_4d(z),
      init = init_states)
  sum(final_states)
}

test_output_part2 <- 848
test_result <- day17_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day17_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

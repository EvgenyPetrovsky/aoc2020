library(magrittr)

test_input <- c("F10", "N3", "F7", "R90", "F11")

real_input <- readLines("./inputs/day12-input.txt")

#- LOGIC ----------------------------------------------------------------------#

DIR_EAST  <- 0
DIR_NORTH <- 1
DIR_WEST  <- 2
DIR_SOUTH <- 3

number_steps <- function(move_instruction) {
  if (substr(move_instruction, 1, 1) %in% c("L", "R")) 0
  else substr(move_instruction, 2, 100) %>% as.integer()
}

face_direction <- function(direction, move_instruction) {
  degrees <-
    if (substr(move_instruction, 1, 1) %in% c("L", "R"))
      substr(move_instruction, 2, 100) %>% as.integer()
    else 0
  change_in_dir <-
    list(
      F = 0, R = -1 * degrees %/% 90, L = +1 * degrees %/% 90,
      E = 0, N = 0, W = 0, S = 0
    )
  new_direction <- direction + change_in_dir[[substr(move_instruction, 1, 1)]]
  new_direction %% 4
}

move_direction <- function(face_direction, move_instruction) {
  list(
    F = face_direction, R = face_direction, L = face_direction,
    E = DIR_EAST, N = DIR_NORTH, W = DIR_WEST, S = DIR_SOUTH
  ) %>% magrittr::extract2(substr(move_instruction, 1, 1))
}

#' Calculate Manhattane distance between two points
manhattan_distance <- function(p1, p2) {
  (p1 - p2) %>% abs() %>% sum()
}

#' make a move from position p, facing direction dir,
#' according to move instruction move_c
#' returns new position
make_move <- function(position, move_direction, number_steps) {
  delta_step <-
    list(c(1, 0), c(0, 1), c(-1, 0), c(0, -1)) %>%
    magrittr::extract2(move_direction + 1)
  delta_position <- (delta_step * number_steps)

  position + delta_position
}

define_waypoint <- function(curr_waypoint, move_instruction) {
  ltr <- substr(move_instruction, 1, 1)
  nmr <- substr(move_instruction, 2, 100) %>%  as.integer()

  if (ltr %in% c("E", "N", "W", "S")) {
    delta_waypoint <-
      list(E = c(1, 0), N = c(0, 1), W = c(-1, 0), S = c(0, -1)) %>%
      magrittr::extract2(ltr) %>%
      magrittr::multiply_by(nmr)
    curr_waypoint + delta_waypoint
  } else if (ltr %in% c("L", "R")) {
    rotate_wp <- function(current_wp, times) {
      if (times == 0) current_wp
      else {
        x <- current_wp[1]; y <- current_wp[2]
        new_wp <- c(-y, x)
        rotate_wp(new_wp, times - 1)
      }
    }
    # 1 turn right = 3 turns left
    rotate_times <- ((nmr %/% 90) * (if (ltr == "R") 3 else 1)) %% 4
    rotate_wp(curr_waypoint, rotate_times)
  }
  else curr_waypoint
}

move_using_waypoint <- function(curr_position, waypoint, move_instruction) {
  ltr <- substr(move_instruction, 1, 1)
  nmr <- substr(move_instruction, 2, 100) %>%  as.integer()

  if (ltr == "F")
    waypoint * nmr + curr_position
  else
    curr_position
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day12_part1_solution <- function(input) {
  init_position <- c(0, 0)
  total_instructions <- length(input)
  iter <- function(num_instr, position, face_dir) {
    if (num_instr > total_instructions) position
    else {
      move_ins <- input[num_instr]
      new_face_dir <- face_direction(face_dir, move_ins)
      move_dir <- move_direction(new_face_dir, move_ins)
      num_steps <- number_steps(move_ins)
      # make a move
      new_position <- make_move(position, move_dir, num_steps)
      # iterate
      iter(num_instr + 1, new_position, new_face_dir)
    }
  }
  final_position <- iter(1, init_position, DIR_EAST)

  distance <- manhattan_distance(init_position, final_position)
  distance
}

test_output_part1 <- 25
test_result <- day12_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day12_part1_solution(real_input)
print(real_result_part1)

#- SOLUTION PART 2 ------------------------------------------------------------#

day12_part2_solution <- function(input) {
  init_waypoint <- c(10, 1)
  init_position <- c(0, 0)

  iter <- function(position, waypoint, instructions) {
    # if no more instructions - return position
    if (length(instructions) == 0) position
    # else apply instruction
    else {
      instruction <- instructions[1]
      new_waypoint <- define_waypoint(waypoint, instruction)
      new_position <- move_using_waypoint(position, waypoint, instruction)
      iter(new_position, new_waypoint, instructions[-1])
    }
  }
  final_position <- iter(init_position, init_waypoint, input)
  manhattan_distance(init_position, final_position)
}

test_output_part2 <- 286
test_result <- day12_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day12_part2_solution(real_input)
print(real_result_part2)

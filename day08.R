library(magrittr)

test_input <- c(
"nop +0",
"acc +1",
"jmp +4",
"acc +3",
"jmp -3",
"acc -99",
"acc +1",
"jmp -4",
"acc +6"
)

real_input <- readLines("./inputs/day08-input.txt")

#- LOGIC ----------------------------------------------------------------------#

#' convert text into list(op, num)
parse_instruction <- function(instruction_text) {
  elements <-
    instruction_text %>%
    strsplit(split = " ") %>%
    unlist()
  list(op = elements[1], num = as.integer(elements[2]))
}

parse_instructions <- function(instructions_text) {
  instructions_text %>%
    Map(f = parse_instruction)
}

#' interpret instruction to update accumulator
interpret_instruction_acc <- function(instruction, acc) {
  with(instruction, {
    accarg <- list(acc = num, nop = 0, jmp = 0)
    accarg[[op]]
  }) + acc
}

#' interpret instruction to update position
interpret_instruction_pos <- function(instruction, pos) {
  with(instruction, {
    offset <- list(acc = 1, nop = 1, jmp = num)
    offset[[op]]
  }) + pos
}

#' interpret program instructions one by one updating position and accumulator
interpret_instructions <- function(instructions) {
  instructions_count <- length(instructions)
  iter <- function(pos, acc, pos_history) {
    if (pos %in% pos_history) {
      acc
    } else if (pos > instructions_count) {
      acc
    } else {
      instruction <- instructions[[pos]]
      new_acc <- instruction %>% interpret_instruction_acc(acc)
      new_pos <- instruction %>% interpret_instruction_pos(pos)
      new_pos_history <- c(pos_history, pos)
      iter(new_pos, new_acc, new_pos_history)
    }
  }
  iter(pos = 1, acc = 0, pos_history = integer())
}

#' check whether program can finish successfully by reaching the end
successfully_terminates <- function(instructions) {
  instructions_count <- length(instructions)
  iter <- function(pos, pos_history) {
    if (pos %in% pos_history) {
      FALSE
    } else if (pos > instructions_count) {
      TRUE
    } else {
      instruction <- instructions[[pos]]
      new_pos <- instruction %>% interpret_instruction_pos(pos)
      new_pos_history <- c(pos_history, pos)
      iter(new_pos, new_pos_history)
    }
  }
  iter(pos = 1, pos_history = integer())
}

#' replace nop with jmp and jmp with nop for specified position
fix_program <- function(instructions, pos) {
  change <- list(nop = "jmp", jmp = "nop", acc = "acc")
  op <- instructions[[pos]]$op
  new_instructions <- instructions
  new_instructions[[pos]]$op <- change[[op]]
  new_instructions
}
#- SOLUTION PART 1 ------------------------------------------------------------#

day08_part1_solution <- function(input) {
  instructions <- input %>% parse_instructions()
  instructions %>% interpret_instructions()
}

test_output_part1 <- 5
test_result <- day08_part1_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

real_result_part1 <- day08_part1_solution(real_input)
print(real_result_part1)

#- SOLUTION PART 2 ------------------------------------------------------------#

day08_part2_solution <- function(input) {
  instructions <- input %>% parse_instructions()
  instructions_count <- length(instructions)
  iter <- function(pos) {
    if (pos > instructions_count) {
      NA
    } else {
      new_program <- instructions %>% fix_program(pos)
      if (new_program %>% successfully_terminates())
        interpret_instructions(new_program)
      else iter(pos+1)
    }
  }
  iter(pos = 1L)
}

test_output_part2 <- 8
test_result <- day08_part2_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part2))


real_result_part2 <- day08_part2_solution(real_input)
print(real_result_part2)

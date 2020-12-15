library(magrittr)

test_input <- c(
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
  "mem[8] = 11",
  "mem[7] = 101",
  "mem[8] = 0"
)

test_input_2 <- c(
  "mask = 000000000000000000000000000000X1001X",
  "mem[42] = 100",
  "mask = 00000000000000000000000000000000X0XX",
  "mem[26] = 1"
)

real_input <- readLines("./inputs/day14-input.txt")

#- LOGIC ----------------------------------------------------------------------#

const_MASK = 1
const_ASSIGNMENT = 2

identify_instruction_type <- function(text_line) {
  word <- text_line %>% 
    gregexpr(pattern = "^[[:alpha:]]+") %>%
    regmatches(x = text_line)
  if (word == "mask") as.integer(const_MASK)
  else if (word == "mem") as.integer(const_ASSIGNMENT)
  else NA_integer_
}

extract_assignment_address <- function(text_line) {
  lhs <-
    text_line %>% 
    strsplit(split = " = ") %>% 
    unlist() %>% 
    magrittr::extract(1)
  # actually mem[xxx] is good enough to be stored in a list as an element name
  lhs
}

extract_assignment_number  <- function(text_line) {
  rhs <-
    text_line %>% 
    strsplit(split = " = ") %>% 
    unlist() %>% 
    magrittr::extract(2)
  as.double(rhs)
}

extract_mask <- function(text_line) {
  text_line %>% 
    strsplit(split = " = ") %>% 
    unlist() %>% 
    magrittr::extract(2)
}

convert_mask_to_array <- function(mask) 
  mask %>% strsplit(split = "") %>% unlist

apply_mask_to_number <- function(mask, number) {
  mask_array <- convert_mask_to_array(mask)
  
  bit_array <- convert_num_to_bit_array(number)  
  
  mask_on_top <- 
    ifelse(mask_array == "X", as.character(bit_array), mask_array)
  
  new_number <-
    convert_bit_to_num(mask_on_top)

  new_number
}

extract_assignment_address_number <- function(text_line) {
  lhs <-
    text_line %>% 
    strsplit(split = " = ") %>% 
    unlist() %>% 
    magrittr::extract(1)
  # actually mem[xxx] is good enough to be stored in a list as an element name
  lhs %>%
    gregexpr(pattern = "\\d+") %>%
    regmatches(x = lhs) %>%
    as.double()
}

convert_num_to_bit_array <- function(number) {
  div_len <- 20
  div_num <- 2^div_len
  
  number_minor <- number %% div_num
  number_major <- number %/% div_num
  
  array_minor <- number_minor %>% 
    intToBits() %>% 
    as.integer() %>% 
    c(., rep(0, 20)) %>%
    magrittr::extract(1:20) %>%
    rev()
  array_major <- number_major %>% 
    intToBits() %>% 
    as.integer() %>% 
    c(., rep(0, 16)) %>%
    magrittr::extract(1:16) %>%
    rev()
  
  c(array_major, array_minor)
}

convert_bit_to_num <- function(bit_array) {
    bit_array %>%
    as.double() %>%
    Reduce(f = function(z, x) 2*z + x, right = FALSE)
}

apply_mask_to_addess_number <- function(mask, address_number) {
  mask_array <- convert_mask_to_array(mask)
  
  address_number_array <- convert_num_to_bit_array(address_number)  
  
  mask_on_top <- 
    ifelse(mask_array == "0", as.character(address_number_array), mask_array)
  
  paste(mask_on_top, collapse = "")
}

#' takes mask, and replaces every X with 
#'  - mask having 0 instead of X
#'  plus
#'  - mask having 1 instead of X
expand_floating_bits <- function(mask) {
  len <- nchar(mask)
  iter <- function(mask, pos) {
    if (pos > len) mask
    else if (substr(mask[1], pos, pos) == "X") {
      m0 <- mask; substr(m0, pos, pos) <- "0"
      m1 <- mask; substr(m1, pos, pos) <- "1"
      c(
        iter(mask = m0, pos = pos + 1),
        iter(mask = m1, pos = pos + 1))
    } else iter(mask, pos = pos + 1)
  }
  iter(mask, 1)
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day14_part1_solution <- function(input) {
  init <- list(mask = character(), mem = list())
  iter <- function(input, state) {
    if (length(input) == 0) state
    else {
      line <- input[1]
      new_state <- state
      instruction_type <- identify_instruction_type(line)
      if (instruction_type == const_MASK) 
        new_state$mask <- extract_mask(line)
      else if (instruction_type == const_ASSIGNMENT) {
        msk <- state$mask
        adr <- extract_assignment_address(line)
        num <- extract_assignment_number(line)
        new <- apply_mask_to_number(msk, num)
        new_state$mem[[adr]] <- new
      }
      iter(input[-1], new_state)
    }
  }
  state <- iter(input, init)
  
  state$mem %>% Reduce(f = sum)
}

test_output_part1 <- 165
test_result <- day14_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day14_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day14_part2_solution <- function(input) {
  init <- list(mask = character(), mem = list())
  iter <- function(input, state) {
    if (length(input) == 0) state
    else {
      line <- input[1]
      print(paste(line))
      new_state <- state
      instruction_type <- identify_instruction_type(line)
      if (instruction_type == const_MASK) 
        new_state$mask <- extract_mask(line)
      else if (instruction_type == const_ASSIGNMENT) {
        msk <- state$mask
        adr <- line %>%
          extract_assignment_address_number() %>% 
          apply_mask_to_addess_number(mask = msk) %>%
          expand_floating_bits()
        num <- extract_assignment_number(line)
        new <- adr %>% 
          Reduce(
            f = function(z, x) z %>% inset2(x, num), 
            init = state$mem)
        new_state$mem <- new
      }
      iter(input[-1], new_state)
    }
  }
  state <- iter(input, init)

  state$mem %>% Reduce(f = sum)
}

test_output_part2 <- 208
test_result <- day14_part2_solution(test_input_2)

print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day14_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

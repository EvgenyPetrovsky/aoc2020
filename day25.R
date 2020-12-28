library(magrittr)

test_input <- c(
  "5764801",
  "17807724"
)

real_input <- readLines("./inputs/day25-input.txt")

#- LOGIC ----------------------------------------------------------------------#

const_SUBJECT_NUMBER <- 7L
const_DIVISOR <- 20201227L

parse_input <- function(input) {
  x <- as.integer(input)
  list(card = x[1], door = x[2])
}

#' returns public key
transform_subject_number <- function(
  subject_number = const_SUBJECT_NUMBER, 
  loop_size
) {
  i <- 1
  val <- 1
  while (i <= loop_size) {
    val <- (val * subject_number) %% const_DIVISOR
    i <- i + 1
    }
  val
}

#' determines loop size based on public key and subject number
determine_loop_size <- function(
  public_key, 
  subject_number = const_SUBJECT_NUMBER
) {
  i <- 0
  val <- 1
  while (val != public_key) {
    val <- (val * subject_number) %% const_DIVISOR
    i <- i + 1
    }
  i
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day25_part1_solution <- function(input) {
  public_keys <- input %>% parse_input()
  card_loop_size <- determine_loop_size(public_key = public_keys$card)
  door_loop_size <- determine_loop_size(public_key = public_keys$door)
  
  encription_key <- transform_subject_number(
    subject_number = public_keys$door, 
    loop_size = card_loop_size)
  
  encription_key
}

test_output_part1 <- 14897079
test_result <- day25_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day25_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day25_part2_solution <- function(input) {
  NULL
}

test_output_part2 <- -1
test_result <- day25_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day25_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

test_input <- c(
  "1-3 a: abcde", 
  "1-3 b: cdefg", 
  "2-9 c: ccccccccc"
)
real_input <- readLines("./inputs/day02-input.txt")


get_policy <- function(policy_password) {
  strsplit(policy_password, ": ") %>%
    Map(f = function(x) x[1]) %>%
    unlist()
}

get_password <- function(policy_password) {
  strsplit(policy_password, ": ") %>%
    Map(f = function(x) x[2]) %>%
    unlist()
}

get_policy_char <- function(policy) {
  strsplit(policy, " ") %>%
    Map(f = function(x) x[2]) %>%
    unlist()
}

get_policy_range <- function(policy) {
  strsplit(policy, " ") %>%
    Map(f = function(x) x[1]) %>%
    unlist()
}

get_policy_min <- function(policy_range) {
  strsplit(policy_range, "-") %>%
    Map(f = function(x) x[1]) %>%
    unlist() %>% as.integer()
}

get_policy_max <- function(policy_range) {
  strsplit(policy_range, "-") %>%
    Map(f = function(x) x[2]) %>%
    unlist() %>% as.integer()
}

parse_policy_password <- function(policy_password) {
  policy <- get_policy(policy_password)
  password <- get_password(policy_password)
  policy_char <- get_policy_char(policy)
  policy_range <- get_policy_range(policy)
  policy_min <- get_policy_min(policy_range)
  policy_max <- get_policy_max(policy_range)
  list(
    policy_char = policy_char,
    policy_min = policy_min,
    policy_max = policy_max,
    password = password)
}

check_password <- function(password, policy_char, policy_min, policy_max) {
  occurences <- 
    password %>% 
    strsplit("") %>% unlist() %>% 
    table() %>% magrittr::extract(policy_char) %>% unname()
  if (is.na(occurences)) FALSE
  else if (occurences >= policy_min & occurences <= policy_max) TRUE
  else FALSE
}

day02_part1_solution <- function(input) {
  input %>%
    Map(f = parse_policy_password) %>%
    Map(f = function(x) check_password(x$password, x$policy_char, x$policy_min, x$policy_max)) %>%
    Filter(f = function(x) x == TRUE) %>%
    length()
}

#------------------------------------------------------------------------------#

test_result <- day02_part1_solution(test_input)
test_output_part1 <- 2
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

real_result_part1 <- day02_part1_solution(real_input)
print(real_result_part1)


#------------------------------------------------------------------------------#

check_password_part2 <- function(password, policy_char, policy_min, policy_max) {
  occurences <- 
    password %>% 
    strsplit("") %>% 
    Map(f = function(x) x[c(policy_min, policy_max)]) %>%
    table() %>% magrittr::extract(policy_char) %>% unname()
  if (is.na(occurences)) FALSE
  else if (occurences == 1) TRUE
  else FALSE
}

day02_part2_solution <- function(input) {
  input %>%
    Map(f = parse_policy_password) %>%
    Map(f = function(x) check_password_part2(x$password, x$policy_char, x$policy_min, x$policy_max)) %>%
    Filter(f = function(x) x == TRUE) %>%
    length()
}

test_result <- day02_part2_solution(test_input)
test_output_part2 <- 1
print(paste("test result:", test_result, "valid:", test_result == test_output_part2))

real_result_part2 <- day02_part2_solution(real_input)
print(real_result_part2)

library(magrittr)

test_input <- c(
"class: 1-3 or 5-7",
"row: 6-11 or 33-44",
"seat: 13-40 or 45-50",
"",
"your ticket:",
"7,1,14",
"",
"nearby tickets:",
"7,3,47",
"40,4,50",
"55,2,20",
"38,6,12"
)

real_input <- readLines("./inputs/day16-input.txt")

#- LOGIC ----------------------------------------------------------------------#

constraint_text_to_ranges <- function(constraint_text) {
  constraint_text %>%
    strsplit(split = ":") %>%
    unlist() %>%
    magrittr::extract(2) %>%
    trimws() %>%
    strsplit(split = "or") %>%
    unlist() %>%
    trimws() %>%
    strsplit(split = "-") %>%
    Map(f = function(x) x %>% unlist() %>% as.integer()) %>%
    Reduce(f = function(z, x) {
      z[x[1]:x[2]] <- TRUE
      z
      }, init = rep(FALSE, 1000))
}

constraint_text_to_label <- function(constraint_text) {
  constraint_text %>%
    strsplit(split = ":") %>%
    unlist() %>%
    magrittr::extract(1)
}

parse_input <- function(input) {
  # find separations of sections
  section_borders <- which(input == "")
  constraint_input <- input[1:(section_borders[1]-1)]
  your_ticket_input <- input[(section_borders[1] + 2):(section_borders[2]-1)]
  neighbor_ticket_input <- input[(section_borders[2] + 2):(length(input))]

  new_input <- list()

  new_input$constraints <-
    constraint_input %>%
    Reduce(f = function(z, x) {
      z[[constraint_text_to_label(x)]] <- constraint_text_to_ranges(x)
      z
    }, init = list())
  new_input[["you"]] <- ticket_text_to_numbers(your_ticket_input)

  new_input[["neighbors"]] <-
    neighbor_ticket_input %>%
    strsplit(split = ",") %>%
    Map(f = as.integer)

  new_input
}

check_number_conforms_constraints <- function(number, constraints) {
  constraints %>%
    Map(f = function(x) x[number]) %>%
    # number should meet any of the requirements / constraints / ranges
    Reduce(f = any)
}

check_ticket_conforms_constraints <- function(ticket, constraints) {
  ticket %>%
    Map(f = function(x) check_number_conforms_constraints(x, constraints)) %>%
    # all numbers in ticket should meet constraints
    Reduce(f = all)
}

find_ticket_invalid_number <- function(ticket, constraints) {
  valid_numbers <- 
    ticket %>%
    Map(f = function(x) check_number_conforms_constraints(x, constraints)) %>%
    unlist()
  
  ticket[valid_numbers == FALSE]
}
#- SOLUTION PART 1 ------------------------------------------------------------#

day16_part1_solution <- function(input) {
  ticket <- parse_input(input)
  
  ticket$neighbors %>%
    Map(f = function(x) find_ticket_invalid_number(x, ticket$constraints)) %>%
    Reduce(f = sum)
}

test_output_part1 <- 71
test_result <- day16_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day16_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day16_part2_solution <- function(input) {
  NULL
}

test_output_part2 <- -1
test_result <- day16_part2_solution(test_input)

print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day16_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

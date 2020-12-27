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

test_input_2 <- c(
  "class: 0-1 or 4-19",
  "row: 0-5 or 8-19",
  "seat: 0-13 or 16-19",
  "",
  "your ticket:",
  "11,12,13",
  "",
  "nearby tickets:",
  "3,9,18",
  "15,1,5",
  "5,14,9"
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
      z[(x[1]+1):(x[2]+1)] <- TRUE
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
  new_input$you <- ticket_text_to_numbers(your_ticket_input)

  new_input$neighbors <-
    neighbor_ticket_input %>%
    strsplit(split = ",") %>%
    Map(f = as.integer)

  new_input
}

check_number_conforms_constraints <- function(number, constraints) {
  constraints %>%
    Map(f = function(x) x[number+1]) %>%
    # number should meet at least one constraint
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

number_position_for_constraint <- function(constraint, tickets) {
  positions <-
      tickets %>%
      Map(f = function(x) constraint[(x+1)]) %>%
      Reduce(f = function(z, x) {z & x}) %>%
      which()

  #positions[1]
  positions
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
  ticket <- parse_input(input)

  all_tickets <- c(list(ticket$you), ticket$neighbors)
  all_valid_tickets <-
    all_tickets %>%
    Filter(f = function(x) {
      check_ticket_conforms_constraints(x, ticket$constraints)
    })

  field_types_map <-
    ticket$constraints %>%
    Map(f = function(x) number_position_for_constraint(x, all_valid_tickets))

  sorted_fields <- field_types_map %>%
    Map(f = length) %>%
    unlist %>%
    sort() %>%
    names()

  # we need to exclude ambiguous results by starting with field that has only 1
  # column and exclude that columns from other fieldss
  field_positions <- sorted_fields %>%
    Reduce(f = function(z, x) {
      pos <- z[[x]]
      Map(z, f = function(x) {
        if (all(x == pos)) x
        else x[x != pos]
      })
    }, init = field_types_map)

  departure_field_positions <-
    field_positions[grepl(pattern = "departure", names(field_types_map))] %>%
    unlist()

  ticket$you %>% magrittr::extract(departure_field_positions) %>% prod()

}

test_result <- day16_part2_solution(test_input_2)

real_result_part2 <- day16_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

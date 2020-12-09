test_input <- c(
"abc",
"",
"a",
"b",
"c",
"",
"ab",
"ac",
"",
"a",
"a",
"a",
"a",
"",
"b"
)

real_input <- readLines("./inputs/day06-input.txt")

#- LOGIC ----------------------------------------------------------------------#
# returns list of groups with answers of every passenger as an element of a list
split_into_groups <- function(input) {
  end_of_line <- ":eol:"
  input %>%
    paste(collapse = end_of_line) %>%
    strsplit(split = paste0(end_of_line, end_of_line)) %>%
    unlist() %>%
    strsplit(split = end_of_line)
}

count_anyone_unique_answers <- function(group_answers) {
  group_answers %>%
    Map(f = function(x) paste(x, collapse = "")) %>%
    unlist() %>%
    strsplit(split = "") %>%
    Map(f = unique) %>%
    Map(f = length) %>%
    unlist()
}



count_everyone_answers <- function(group_answers) {
  group_answers %>%
    Map(f = function(x) {
      x %>% 
        strsplit(x, split = "") %>%
        Reduce(f = intersect, init = letters)
    }) %>%
    Map(f = unique) %>%
    Map(f = length) %>%
    unlist()
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day06_part1_solution <- function(input) {
  input %>% split_into_groups %>% count_anyone_unique_answers %>% sum
}

test_output_part1 <- 11
test_result <- day06_part1_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

real_result_part1 <- day06_part1_solution(real_input)
print(real_result_part1)

#- SOLUTION PART 2 ------------------------------------------------------------#

day06_part2_solution <- function(input) {
  input %>% split_into_groups %>% count_everyone_answers %>% sum
}

test_output_part2 <- 6
test_result <- day06_part2_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part2))


real_result_part2 <- day06_part2_solution(real_input)
print(real_result_part2)

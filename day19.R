library(magrittr)

test_input <- c(
  "0: 4 1 5",
  "1: 2 3 | 3 2",
  "2: 4 4 | 5 5",
  "3: 4 5 | 5 4",
  "4: \"a\"",
  "5: \"b\"",
  "",
  "ababbb",
  "bababa",
  "abbbab",
  "aaabbb",
  "aaaabbb"
)

test_input_2 <- c(
  "42: 9 14 | 10 1",
  "9: 14 27 | 1 26",
  "10: 23 14 | 28 1",
  "1: \"a\"",
  #"11: 42 31",
  "11: 42 31 | 42 11 31",
  "5: 1 14 | 15 1",
  "19: 14 1 | 14 14",
  "12: 24 14 | 19 1",
  "16: 15 1 | 14 14",
  "31: 14 17 | 1 13",
  "6: 14 14 | 1 14",
  "2: 1 24 | 14 4",
  "0: 8 11",
  "13: 14 3 | 1 12",
  "15: 1 | 14",
  "17: 14 2 | 1 7",
  "23: 25 1 | 22 14",
  "28: 16 1",
  "4: 1 1",
  "20: 14 14 | 1 15",
  "3: 5 14 | 16 1",
  "27: 1 6 | 14 18",
  "14: \"b\"",
  "21: 14 1 | 1 14",
  "25: 1 1 | 1 14",
  "22: 14 14",
  #"8: 42",
  "8: 42 | 42 8",
  "26: 14 22 | 1 20",
  "18: 15 15",
  "7: 14 5 | 1 21",
  "24: 14 1",
  "",
  "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
  "bbabbbbaabaabba",
  "babbbbaabbbbbabbbbbbaabaaabaaa",
  "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
  "bbbbbbbaaaabbbbaaabbabaaa",
  "bbbababbbbaaaaaaaabbababaaababaabab",
  "ababaaaaaabaaab",
  "ababaaaaabbbaba",
  "baabbaaaabbaaaababbaababb",
  "abbbbabbbbaaaababbbbbbaaaababb",
  "aaaaabbaabaaaaababaa",
  "aaaabbaaaabbaaa",
  "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
  "babaaabbbaaabaababbaabababaaab",
  "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
)

real_input <- readLines("./inputs/day19-input.txt")
real_input_2 <- readLines("./inputs/day19_part2-input.txt")

#- LOGIC ----------------------------------------------------------------------#

#' extract rules from input
collect_input_rules <- function(input) {
  border <- which(input == "")
  input[1:(border-1)]
}

# extract strings to be checked from the input
collect_input_strings <- function(input) {
  border <- which(input == "")
  input[(border + 1):length(input)]
}

#' returns a list where its elements
#' - either another list that refer to other rules (can be more than 1)
#' - or a vector with finite 1 letter
parse_input_rules <- function(input_rules) {
  input_rules %>%
    strsplit(split = ": ") %>%
    # extract rule identifiers and make a list
    Reduce(
      f = function(z, x) z %>% inset2(x[1], x[2]),
      init = list()
    ) %>%
    Map(f = parse_rule_line)
}

#' takes one rule line and extracts either letter or lisr of vectors that refer
#' to other rules
parse_rule_line <- function(rule_line) {
  # if only one letter
  if (grepl(pattern = "^\"[[:alpha:]]\"$", rule_line)) {
    gsub(pattern = "\"", replacement = "", rule_line)
  # else - references to other rules
  } else {
    rule_line %>%
      strsplit(split = " | ", fixed = TRUE) %>%
      unlist() %>%
      Map(f = function(x) x %>% trimws() %>% strsplit(split = " ") %>% unlist())
  }
}

#' use rules to generate regex patter, alternatives are |,
#' all rules are enclosed in parentethes
generate_regex_pattern <- function(rules, starting_rule_id = "0") {
  iter <- function(rule_id) {
    rule <- rules[[rule_id]]
    ptn <-
      if (typeof(rule) == "character") {
        rule
      } else {
        rule %>%
          Map(f = function(x) {
            res <-
              x %>%
              Map(f = function(x) {
                # if rule refers to itself - make a recursion using \g{<group>}
                if (x == rule_id) paste0("\\g<r", rule_id, ">?")
                else iter(x)
              }) %>%
              Reduce(f = paste0)
            # if rule contains recursion - define group name for referencing
            if (rule_id %in% x) paste0("(?<r", rule_id, ">", res, ")")
            else res
          }) %>%
          Reduce(f = function(z, x) paste0(z, "|", x))
      }
    paste0("(", ptn, ")")
  }
  paste0("^", iter(starting_rule_id), "$")
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day19_part1_solution <- function(input) {
  rules <- input %>% collect_input_rules() %>% parse_input_rules()
  messages <- input %>% collect_input_strings()
  pattern <- generate_regex_pattern(rules)
  grepl(pattern, messages) %>% sum()
}

test_output_part1 <- 2
test_result <- day19_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day19_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day19_part2_solution <- function(input) {
  rules <- input %>% collect_input_rules() %>% parse_input_rules()
  messages <- input %>% collect_input_strings()
  pattern <- generate_regex_pattern(rules)
  grepl(pattern, messages, perl = TRUE) %>% sum()
}

#test_output_part2 <- 3
test_output_part2 <- 12
test_result <- day19_part2_solution(test_input_2)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day19_part2_solution(real_input_2)
print(format(real_result_part2, scientific = FALSE))


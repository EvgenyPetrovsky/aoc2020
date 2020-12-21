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

real_input <- readLines("./inputs/day19-input.txt")

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
generate_regex_pattern <- function(rules) {
  starting_rule_id <- "0"
  iter <- function(rule_id) {
    rule <- rules[[rule_id]]
    ptn <-
      if (typeof(rule) == "character") {
        rule
      } else {
        rule %>%
          Map(f = function(x)
            x %>% Map(f = function(x) iter(x)) %>% Reduce(f = paste0)
          ) %>%
          Reduce(f = function(z, x) paste0(z, "|", x))
      }
    paste0("(", ptn, ")")
  }
  paste0("^", iter(starting_rule_id), "$")
}

##' returns how many characters of text were matched by rules
#check_text_using_rules <- function(rules, text, rule_id) {
#  rule <- rules[[rule_id]]
#  len_text <- nchar(text)
#  if (typeof(rule) == "character") {
#    # if rule is character them simply compare text to character and
#    # if equal then 1 otherwise 0
#    if (substring(text, 1, 1) == rule) 1 else 0
#  } else {
#    # if rule is a list then check all alternatives (list elements)
#    # each alternatine may refer to one or many rules
#    rule %>%
#      Map(f = function(x) {
#        n_char_matched <- 0
#        for (r in x) {
#          chk_res <- check_text_using_rules(
#            rules,
#            substr(text, n_char_matched + 1, 1000),
#            r)
#          # leave loop if result is invalid
#          if (chk_res == 0 ) {
#            n_char_matched <- 0
#            break
#          } else {
#            n_char_matched <- n_char_matched + chk_res
#          }
#        }
#        n_char_matched
#      }) %>%
#      Reduce(f = max, init = 0)
#  }
#}
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
  NULL
}

test_output_part2 <- -1
test_result <- day19_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day19_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))


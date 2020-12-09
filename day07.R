library(magrittr)

test_input <- c(
"light red bags contain 1 bright white bag, 2 muted yellow bags.",
"dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
"bright white bags contain 1 shiny gold bag.",
"muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
"shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
"dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
"vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
"faded blue bags contain no other bags.",
"dotted black bags contain no other bags."
)

test_input_2 <- c(
"shiny gold bags contain 2 dark red bags.",
"dark red bags contain 2 dark orange bags.",
"dark orange bags contain 2 dark yellow bags.",
"dark yellow bags contain 2 dark green bags.",
"dark green bags contain 2 dark blue bags.",
"dark blue bags contain 2 dark violet bags.",
"dark violet bags contain no other bags."
)

real_input <- readLines("./inputs/day07-input.txt")

#- LOGIC ----------------------------------------------------------------------#


#' Function takes a string of text and returns list
#'   list(color = list([(number_in, color_in)])
#' where color is a color of the bag and color_in + number_in is
#'   color and number of bags that can be iside starting bag

parse_rule <- function(text_rule) {
  color <- gregexpr(
      pattern = "^([[:alpha:]]+ )+(bags contain)",
      text = text_rule, ignore.case = T
    ) %>%
    regmatches(x = text_rule) %>%
    gsub(pattern = " bags contain", replacement = "")

  content <-
    gregexpr(
      pattern = "bags contain [[:alnum:], ]+[^\\.]",
      text = text_rule, ignore.case = T
    ) %>%
    regmatches(x = text_rule) %>%
    gsub(pattern = " no ", replacement = " 0 ") %>%
    gsub(pattern = "bags contain ", replacement = "") %>%
    strsplit(split = ", ") %>%
    unlist() %>%
    Map(f = function(x) {
      number_in <- trimws(x) %>%
        gregexpr(pattern = "\\d+") %>%
        regmatches(x = x) %>%
        as.numeric()
      color_in <- trimws(x) %>%
        gsub(pattern = "^\\d+", replacement = "") %>%
        gsub(pattern = " bag(s)?$", replacement = "") %>%
        trimws()
      list(number_in = number_in, color_in = color_in)
    })

  list() %>% magrittr::inset2(color, content)
}

parse_rules <- function(text_rules) {
  text_rules %>%
    Map(f = parse_rule) %>%
    Reduce(f = c)
}

#' function takes list of rules and builds index
#'
#' Returns list(color = [color_in])
build_color_rule_index <- function(rules) {
  #colors <- (rules)
  #colors %>%
  rules %>% 
    Map(f = function(x) {
      x %>% Map(f= function(xx) xx$color_in) %>% Reduce(f = c)
    })
}

#' function takes list of rules and invers them - showing which bag colors can
#' be inside which bag colors
#'
#' Returns list(color_in = [color])
invert_color_index <- function(index) {
  index %>% names() %>%
    Map(f = function(color) {
      index[[color]] %>%
        Map(f = function(color_in) list(color = color, color_in = color_in))
    }) %>%
    Reduce(f = c) %>%
    Reduce(f = function(z, x) {
      el <- z[[x$color_in]]
      if (is.null(el)) z %>% inset2(x$color_in, x$color)
      else z %>% inset2(x$color_in, c(el, x$color))
    }, init = list())
}

#' function traverses index and gathers all colors that are referred by initial
#' color
#'
#' returns [color]
traverse_color_index <- function(index, init_color) {
  # recursive function for accumulation of result
  traverse_acc <- function(color, acc) {
    new_acc <- c(acc, color)
    if (color %in% acc) {
      acc
    } else if (is.null(index[[color]])) {
      c(acc, color)
    } else {
      index[[color]] %>%
        Map(f = function(x) traverse_acc(x, new_acc)) %>%
        Reduce(f = c) %>%
        unique()
    }
  }
  index[[init_color]] %>%
    Map(f = function(color) traverse_acc(color, character())) %>%
    Reduce(f = c) %>%
    unique()
}

#' traverse all rules and calculate amount of bags for specified color
traverse_count_bags <- function(rules, color) {
  if (is.null(rules[[color]])) {
    1
  } else {
    1 + 
    rules[[color]] %>%
      Map(f = function(content_item) {
        number_in <- content_item$number_in
        color_in <- content_item$color_in
        if (number_in == 0) {
          0
        } else {
          print(paste(color, "+", number_in, color_in))
          number_in * traverse_count_bags(rules, color_in)
        }
      }) %>%
      Reduce(f = sum)
  }
}
#- SOLUTION PART 1 ------------------------------------------------------------#

day07_part1_solution <- function(input) {
  bag_color <- "shiny gold"
  input %>% 
    parse_rules() %>% 
    build_color_rule_index() %>% 
    invert_color_index() %>%
    traverse_color_index(init_color = bag_color) %>%
    length()
}

test_output_part1 <- 4
test_result <- day07_part1_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

real_result_part1 <- day07_part1_solution(real_input)
print(real_result_part1)

#- SOLUTION PART 2 ------------------------------------------------------------#

day07_part2_solution <- function(input) {
  bag_color <- "shiny gold"
  inside <- 
    input %>% 
    parse_rules() %>% 
    traverse_count_bags(color = bag_color)
  # result is a number of bags inside shiny gold + 1 shiny gold
  # we need to reduce it by 1
  inside - 1
}

test_output_part2 <- 32
test_result <- day07_part2_solution(test_input)
test_result_2 <- day07_part2_solution(test_input_2)
print(paste("test result:", test_result, "valid:", test_result == test_output_part2))


real_result_part2 <- day07_part2_solution(real_input)
print(real_result_part2)

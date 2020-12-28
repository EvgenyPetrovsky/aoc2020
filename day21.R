library(magrittr)

test_input <- c(
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
  "trh fvjkl sbzzf mxmxvkd (contains dairy)",
  "sqjhc fvjkl (contains soy)",
  "sqjhc mxmxvkd sbzzf (contains fish)"
)

real_input <- readLines("./inputs/day21-input.txt")

#- LOGIC ----------------------------------------------------------------------#

#' returns list of objects with
#'   ingredients: vector of ingredients
#'   allergens: vector of allergens
parse_input <- function(input) {
  input %>%
    Map(f = function(x) {
      ingredients <- x %>%
        gregexpr(pattern = "^[a-z ]+") %>%
        regmatches(x = x) %>%
        unlist() %>%
        trimws() %>%
        strsplit(split = " ") %>%
        unlist()
      allergens <-
        x %>%
        gregexpr(pattern = "\\(contains [a-z]+(, [a-z]+)*\\)$") %>%
          regmatches(x = x) %>%
          unlist() %>%
          substring(11, nchar(.)-1) %>%
          strsplit(split = ", ") %>%
          unlist()
      list(ingredients = ingredients, allergens = allergens)
    })
}

#' create a list of allergens that contain list of foods
foods_by_allergens <- function(foods) {
  foods %>%
  Reduce(f = function(z, food) {
    food$allergens %>% Reduce(f = function(z, allergen) {
      z[[allergen]] <- c(z[[allergen]], list(food$ingredients))
      z
    }, init = z)
  }, init = list())
}

#' find best allergen matches
find_allergen_matches <- function(foods_by_allergens) {
  allergens_sorted <-
    foods_by_allergens %>%
    Map(f = length) %>%
    unlist() %>%
    sort() %>%
    rev() %>%
    names()
  matches <-
    allergens_sorted %>%
    Map(f = function(x) foods_by_allergens[[x]] %>% Reduce(f = intersect))
  matches
}

#' remove ambiguous terms from list
#' by starting on list item referring to 1 element and removing that element
#' from other list items
#'
disambiguate_matches <- function(matches) {
  len <- Map(matches, f = function(x) x %>% unique() %>% length()) %>% unlist()

  # if we have single definition term
  if (any (len == 1)) {
    idx <- which(len == 1)[1]

    # element name and definition
    nam <- names(matches)[idx]
    def <- matches[[idx]]

    # exclude definition from a list of remaining terms
    new_matches <-
      # explicitly exclude the element with single match
      matches[-idx] %>%
      # exclude definition from remaining list of terms
      Map(f = function(x) x[x != def])

    # concatenate list of a single term
    # with function call for further disambiguation
    c(matches[idx], disambiguate_matches(new_matches))
  } else {
    matches
  }
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day21_part1_solution <- function(input) {
  foods <- input %>% parse_input()

  allergic_ingredients <-
    foods %>%
    foods_by_allergens() %>%
    find_allergen_matches() %>%
    disambiguate_matches()

  allergic_ingredients_plain <- allergic_ingredients %>% Reduce(f = c)

  answer <-
    foods %>%
    Reduce(f = function(z, food) {
      bool_allergic = (food$ingredients %in% allergic_ingredients_plain)
      z + sum(bool_allergic == FALSE)
    }, init = 0)
  
  answer
}

test_output_part1 <- 5
test_result <- day21_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day21_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day21_part2_solution <- function(input) {
  foods <- input %>% parse_input()
  
  allergic_ingredients <-
    foods %>%
    foods_by_allergens() %>%
    find_allergen_matches() %>%
    disambiguate_matches()
  
  answer <-
    allergic_ingredients[names(allergic_ingredients) %>% sort] %>%
    Reduce(f = function(z, x) paste(z, x, sep = ","))
  
  answer
}

test_output_part2 <- "mxmxvkd,sqjhc,fvjkl"
test_result <- day21_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day21_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

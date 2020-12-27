library(magrittr)

test_input <- c(
)

real_input <- readLines("./inputs/day20-input.txt")

#- LOGIC ----------------------------------------------------------------------#

#' produces list of tiles every element of each is an object with properties
#'   id: number
#'   content: [character]
#'   orientation:
#'     flipped: logical
#'     turned: integer (from 0 to 3, shows rotation by 90 degree counterclockwise)
parse_input <- function(input) {
  input <- input[input != ""]
  g <-
    input %>%
    Reduce(f = function(z,x) {
      z + grepl("^Tile \\d+:$", x)
    }, accumulate = T, init = 0) %>%
    magrittr::extract(-1)
  input_split <-
    input %>%
    split(g) %>%
    Map(f = function(x) {
      first_line <- x[1]
      tile_content <- x[-1]
      id <- first_line %>%
        gregexpr(pattern = "\\d+") %>%
        regmatches(x = first_line) %>%
        magrittr::extract2(1) %>%
        as.integer()
      content <- {
        rows <- length(tile_content)
        cols <- nchar(tile_content[1])
        tile_content %>%
          strsplit(split = "") %>%
          unlist() %>%
          matrix(rows, cols, byrow = T)
      }
      list(id = id, content = content, flipped = FALSE, turned = 0)
    })
}

## TILE MODIFIERS

#' flip rows: 1st row becomes last and last row becomes 1st
flip_tile <- function(tile) {
  tile %>%
    magrittr::inset2("flipped", xor(tile$flipped, TRUE)) %>%
    magrittr::inset2("content", apply(tile$content, 2, rev))
}

turn_tile <- function(tile) {
  tile %>%
    magrittr::inset2("turned", (tile$turned + 1) %% 3) %>%
    magrittr::inset2("content", t(apply(tile$content, 2, rev)))
}

## TILE PROPERTIES

#' returns tile borders, based on tile content and orientation
#' object with properties
#'  top: [character]
#'  bottom: [character]
#'  left: [character]
#'  right: [character]
tile_borders <- function(tile) {
  content <- tile$content
  rows <- nrow(content)
  cols <- ncol(content)
  list(
    top = content[1, 1:cols],
    bottom = content[rows, 1:cols],
    left = content[1:rows, 1],
    right = content[cols, 1:rows]
  )
}
## FIELD

#' function adds tile to the given position (c(x,y)) of the field and returns new field including added tile
#' if tile is occupied then function raises an error
add_tile <- function(field, position, tile) {
  NULL
}

#' function that returns constraints in form of tile borders for a given position in the field
#' if there are no adjucent tiles on a side then side is not included into result.
#' for example if position has adjacent tiles only on left and bottom sides, function will return list(left = ..., bottom = ...)
field_position_constraints <- function {
  NULL
}

## TILE SEARCH

#' we need to find a tile that will take position c(1,1) on the field
find_first_tile <- function(???) {
  NULL
}

#' function takes all free tiles and constraints and returns
#' LIST of tiles with MODIFIED orientation that conform constraints
find_tile_given_constraints <- function(tiles, constraints) {
  NULL
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day20_part1_solution <- function(input) {
  NULL
}

test_output_part1 <- -1
test_result <- day20_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day20_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day20_part2_solution <- function(input) {
  NULL
}

test_output_part2 <- -1
test_result <- day20_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day20_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

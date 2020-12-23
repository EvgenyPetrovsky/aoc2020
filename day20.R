library(magrittr)

test_input <- c(
)

real_input <- readLines("./inputs/dayXX-input.txt")

#- LOGIC ----------------------------------------------------------------------#

#' produces list of tiles every element of each is an object with properties
#'   id: number
#'   content: [character]
#'   orientation: 
#'     flipped: logical
#'     turned: integer (from 0 to 3, shows rotation by 90 degree counterclockwise)
parse_input <- function(input) {
  
}

## TILE MODIFIERS

flip_tile <- function(tile) {
  NULL
}

turn_tile <- function(tile, times) {
  NULL
}

## TILE PROPERTIES

#' returns tile borders, based on tile content and orientation
#' object with properties
#'  top: [character]
#'  bottom: [character]
#'  left: [character]
#'  right: [character]
tile_borders <- function(tile) {
  
}

## FIELD 

#' function adds tile to the given position (c(x,y)) of the field and returns new field including added tile
#' if tile is occupied then function raises an error
add_tile <- function(field, position, tile) {
  NULL
}

#' functuion that returns constraints in form of tile borders for a given position in the field
#' if there are no adjucent tiles on a side then side is not included into result.
#' for example if position has adjucent tiles only on left and bottom sides, function will return list(left = ..., bottom = ...)
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

dayXX_part1_solution <- function(input) {
  NULL
}

test_output_part1 <- -1
test_result <- dayXX_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- dayXX_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

dayXX_part2_solution <- function(input) {
  NULL
}

test_output_part2 <- -1
test_result <- dayXX_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- dayXX_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

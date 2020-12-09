test_input <- c(
"..##.......",
"#...#...#..",
".#....#..#.",
"..#.#...#.#",
".#...##..#.",
"..#.##.....",
".#.#.#....#",
".#........#",
"#.##...#...",
"#...##....#",
".#..#...#.#"
)
real_input <- readLines("./inputs/day03-input.txt")



tree_square <- "#"
open_square <- "."

# find what is in square based on coordinate
square_value <- function(pattern, coord) {
  y_period <- length(pattern)
  x_period <- nchar(pattern[1])

  x_inner <- (coord$x - 1) %% x_period + 1
  y_inner <- (coord$y - 1) %% y_period + 1

  pattern %>% 
    magrittr::extract(y_inner) %>% 
    substr(start = x_inner, stop = x_inner)
}

move <- function(coord, slope) {
  list(
    x = coord$x + slope$x,
    y = coord$y + slope$y
  )
}

slope_trajectory <- function(height, start_coord, slope) {
  if (height < start_coord$y + slope$y) {
    start_coord
  } else {
    seq.int(from = start_coord$y, to = height, by = slope$y) %>%
      magrittr::extract(-1) %>%
      Reduce(
        f = function(z,x) move(z, slope),
        init = start_coord, accumulate = T) 
  }
}

#------------------------------------------------------------------------------#

day03_part1_solution <- function(pattern) {
  start_coord <- list(x = 1, y = 1)
  slope_angle <- list(x = 3, y = 1)
  height <- length(pattern)
  slope_trajectory(height, start_coord, slope_angle) %>% 
    Map(f = function(x) square_value(pattern = pattern, coord = x)) %>% 
    Filter(f = function(x) x == tree_square) %>%
    length()
}

test_output_part1 <- 7
test_result <- day03_part1_solution(test_input)
print(paste("test result:", test_result, "valid:", test_result == test_output_part1))

real_result_part1 <- day03_part1_solution(real_input)
print(real_result_part1)

#------------------------------------------------------------------------------#

day03_part2_solution <- function(pattern) {
  start_coord <- list(x = 1, y = 1)
  slopes <- list(
    a = list(x=1,y=1),
    b = list(x=3,y=1),
    c = list(x=5,y=1),
    d = list(x=7,y=1),
    e = list(x=1,y=2)
  )
  height <- length(pattern)
  slopes %>%
    Map(f = function(slope_angle) {
      slope_trajectory(height, start_coord, slope_angle) %>%
        Map(f = function(x) square_value(pattern = pattern, coord = x)) %>% 
        Filter(f = function(x) x == tree_square) %>%
        length()
    })
}

test_output_part2 <- list(a=2,b=7,c=3,d=4,e=2)
test_result <- day03_part2_solution(test_input)

real_result_part2 <- day03_part2_solution(real_input) %>% Reduce(f = prod)
print(real_result_part2)



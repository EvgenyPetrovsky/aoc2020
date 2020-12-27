library(magrittr)

test_input <- c(
  "sesenwnenenewseeswwswswwnenewsewsw",
  "neeenesenwnwwswnenewnwwsewnenwseswesw",
  "seswneswswsenwwnwse",
  "nwnwneseeswswnenewneswwnewseswneseene",
  "swweswneswnenwsewnwneneseenw",
  "eesenwseswswnenwswnwnwsewwnwsene",
  "sewnenenenesenwsewnenwwwse",
  "wenwwweseeeweswwwnwwe",
  "wsweesenenewnwwnwsenewsenwwsesesenwne",
  "neeswseenwwswnwswswnw",
  "nenwswwsewswnenenewsenwsenwnesesenew",
  "enewnwewneswsewnwswenweswnenwsenwsw",
  "sweneswneswneneenwnewenewwneswswnese",
  "swwesenesewenwneswnwwneseswwne",
  "enesenwswwswneneswsenwnewswseenwsese",
  "wnwnesenesenenwwnenwsewesewsesesew",
  "nenewswnwewswnenesenwnesewesw",
  "eneswnwswnwsenenwnwnwwseeswneewsenese",
  "neswnwewnwnwseenwseesewsenwsweewe",
  "wseweeenwnesenwwwswnew"
)

real_input <- readLines("./inputs/day24-input.txt")

#- LOGIC ----------------------------------------------------------------------#

const_BLACK <- FALSE
const_WHITE <- TRUE

const_MOVES <- list(
  e  = c(+2, +0),
  ne = c(+1, +1),
  nw = c(-1, +1),
  w  = c(-2, +0),
  sw = c(-1, -1),
  se = c(+1, -1))

#' parse line of directions
#'
#' returns vector of instructions how to move across hex-grid
parse_line <- function(line) {
  if (line == "") character()
  else {
    ch1 <- substring(line, 1, 1)
    ch2 <- substring(line, 1, 2)
    instruction <- if (ch2 %in% names(const_MOVES)) ch2 else ch1
    remaining <- substring(line, nchar(instruction) + 1)
    c(instruction, parse_line(remaining))
  }
}

#' coordinate text
coord_txt <- function(tile_coordinate)
  paste0("(", paste(tile_coordinate, collapse = ","), ")")

#' parse input
#' returns list of vectors with instructions of e, ne, nw, w, sw, se
parse_input <- function(input) input %>% Map(f = parse_line)

floor_tile_color <- function(floor, tile_coordinate) {
  coord_txt <- coord_txt(tile_coordinate)
  if(is.null(floor[[coord_txt]])) const_WHITE
  else floor[[coord_txt]]$color
}

#' flip tile on the floor
#'
#' returns new configuration of the floor
flip_floor_tile <- function(floor, tile_coordinate) {
  coord_txt <- coord_txt(tile_coordinate)
  color <- floor_tile_color(floor, tile_coordinate)

  # if BLACK then WHITE; if WHITE then BLACK
  new_color <- xor(color, TRUE)
  tile <- list(
    coord = tile_coordinate,
    color = new_color)
  floor %>% magrittr::inset2(coord_txt, tile)
}

#' identify final coordinate of a tile based on given move instructions
definitive_tile_coordinate <- function(instructions) {
  instructions %>%
    Map(f = function(x) const_MOVES[[x]]) %>%
    Reduce(f = `+`, init = c(0, 0))
}

#' identify adjacent tiles coordinates
adjacent_tile_coordinates <- function(tile_coordinate) {
  const_MOVES %>% Map(f = function(x) tile_coordinate + x)
}

#' update color of a floor tile for a new day
daily_tile_update <- function(floor, tile_coordinate) {
  color <- floor_tile_color(floor, tile_coordinate)
  adjacent_tile_colors <-
    adjacent_tile_coordinates(tile_coordinate) %>%
    Map(f = function(x) floor_tile_color(floor, x)) %>%
    unlist()
  nr_adj_black <- sum(adjacent_tile_colors == const_BLACK)
  nr_adj_white <- sum(adjacent_tile_colors == const_WHITE)
  new_color <-
    if (color == const_BLACK & (nr_adj_black == 0 | nr_adj_black > 2))
      const_WHITE
    else if (color == const_WHITE & nr_adj_black == 2)
      const_BLACK
    else
      color
  tile <- list(coord = tile_coordinate, color = new_color)
  tile
}

daily_floor_update <- function(floor) {
  adjacent_tile_coordinates <-
    floor %>%
    Reduce(f = function(z, x) c(z, adjacent_tile_coordinates(x$coord)), list())
  used_tile_coordibates <- floor %>% Map(f = function(x) x$coord)

  all_tile_coordinates <-
    c(adjacent_tile_coordinates, used_tile_coordibates) %>%
    Reduce(f = function(z, x) {
      z %>% magrittr::inset2(coord_txt(x), x)
    }, init = list())

  new_floor <-
    all_tile_coordinates %>%
    # calculate tiles for all involved tiles
    Map(f = function(tile_coordinate) daily_tile_update(floor, tile_coordinate)) %>%
    # make a floor out of tiles
    Reduce(f = function(floor, tile) {
      floor %>% magrittr::inset2(coord_txt(tile$coord), tile)
    }, init = list()) %>%
    # keep only black tiles (for performance reasons)
    # because white tiles are default
    Filter(f = function(tile) tile$color == const_BLACK)
  new_floor
}

plot_floor <- function(floor) {
  x <- floor %>% Reduce(f = function(z, x) c(z, x$coord[1]), init = integer())
  y <- floor %>% Reduce(f = function(z, x) c(z, x$coord[2]), init = integer())
  plot(x, y, pch = 16)
}

#- SOLUTION PART 1 ------------------------------------------------------------#

day24_part1_solution <- function(input) {
  tiles <-
    input %>%
    parse_input() %>%
    Map(f = definitive_tile_coordinate)
  empty_floor <- list()
  tiled_floor <-
    tiles %>% Reduce(f = flip_floor_tile, init = empty_floor)

  plot_floor(tiled_floor)

    tiled_floor %>% Filter(f = function(x) x$color == const_BLACK) %>% length()
}

test_output_part1 <- 10
test_result <- day24_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day24_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day24_part2_solution <- function(input, days) {
  tiles <-
    input %>%
    parse_input() %>%
    Map(f = definitive_tile_coordinate)
  empty_floor <- list()
  tiled_floor <-
    tiles %>% Reduce(f = flip_floor_tile, init = empty_floor)
  
  floor_after_days <-
    head((1:days), days) %>%
    Reduce(f = function(floor, day) {
      new_floor <- daily_floor_update(floor)
      if (day %% 10 == 0) {
        print(paste(Sys.time(), "Day:", day, "Floor_size:", length(floor)))
      }
      new_floor
    }, init = tiled_floor)

  plot_floor(floor_after_days)
  
  floor_after_days %>%
    Filter(f = function(x) x$color == const_BLACK) %>%
    length()
}

test_output_part2 <- 15
test_result <- day24_part2_solution(test_input, 1)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

test_output_part2 <- 37
test_result <- day24_part2_solution(test_input, 10)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

test_output_part2 <- 132
test_result <- day24_part2_solution(test_input, 20)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

test_output_part2 <- 566
test_result <- day24_part2_solution(test_input, 50)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day24_part2_solution(real_input, 100)
print(format(real_result_part2, scientific = FALSE))

library(magrittr)

test_input <- c(
  "Tile 2311:","..##.#..#.","##..#.....","#...##..#.","####.#...#","##.##.###.","##...#.###",".#.#.#..##","..#....#..","###...#.#.","..###..###",
  "",
  "Tile 1951:","#.##...##.","#.####...#",".....#..##","#...######",".##.#....#",".###.#####","###.##.##.",".###....#.","..#.#..#.#","#...##.#..",
  "",
  "Tile 1171:","####...##.","#..##.#..#","##.#..#.#.",".###.####.","..###.####",".##....##.",".#...####.","#.##.####.","####..#...",".....##...",
  "",
  "Tile 1427:","###.##.#..",".#..#.##..",".#.##.#..#","#.#.#.##.#","....#...##","...##..##.","...#.#####",".#.####.#.","..#..###.#","..##.#..#.",
  "",
  "Tile 1489:","##.#.#....","..##...#..",".##..##...","..#...#...","#####...#.","#..#.#.#.#","...#.#.#..","##.#...##.","..##.##.##","###.##.#..",
  "",
  "Tile 2473:","#....####.","#..#.##...","#.##..#...","######.#.#",".#...#.#.#",".#########",".###.#..#.","########.#","##...##.#.","..###.#.#.",
  "",
  "Tile 2971:","..#.#....#","#...###...","#.#.###...","##.##..#..",".#####..##",".#..####.#","#..#.#..#.","..####.###","..#.#.###.","...#.#.#.#",
  "",
  "Tile 2729:","...#.#.#.#","####.#....","..#.#.....","....#..#.#",".##..##.#.",".#.####...","####.#.#..","##.####...","##..#.##..","#.##...##.",
  "",
  "Tile 3079:","#.#.#####.",".#..######","..#.......","######....","####.#..#.",".#...#.##.","#.#####.##","..#.###...","..#.......","..#.###..."
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
      list(id = id, content = content, flipped = FALSE, turned = 0, type = "tile")
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

#' convert position coordinates into text for field element names
position_to_txt <- function(position) {
  paste0("(", position[1], ",", position[2], ")")
}

#' function adds tile to the given position (c(x,y)) of the field and returns new field including added tile
#' if tile is occupied then function raises an error
add_on_field <- function(field, position, tile_or_outer) {
  field %>% 
    magrittr::inset2(
      position_to_txt(position), 
      tile_or_outer %>% magrittr::inset2("position", position)
    )
}

#' returns list of neighbor positions for a given position
field_position_neighbor_positions <- function(field, position) {
  list(right = c(1,0), top = c(0,1), left = c(-1,0), bottom = c(0,-1)) %>%
    Map(f = function(x) x + position)
}

#' check whether given position on the field is empty
field_position_is_empty <- function(field, position) {
  pos_txt <- position_to_txt(position)
  is.null(field[[pos_txt]])
}

#' check whether given position on the field filled with tile
field_position_is_tile <- function(field, position) {
  pos_txt <- position_to_txt(position)
  !is.null(pos_txt) && field[[pos_txt]][["type"]] == "tile"
}

#' function that returns constraints in form of tile borders for a given position in the field
#' if there are no adjucent tiles on a side then side is not included into result.
#' for example if position has adjacent tiles only on left and bottom sides, function will return list(left = ..., bottom = ...)
field_position_constraints <- function(field, position) {
  # named vector of sides around the position
  mirror <- c(right = "left", left = "right", top = "bottom", bottom = "top")
  
  # find surrounding tiles on the field
  neighbors <- 
    field_position_neighbor_positions(field, position) %>%
    Filter(f = function(x) field_position_is_tile(field, x))
  
  # take only adjucent borders of surrounding tiles (bottom border for upper tile, etc.)
  names(neighbors) %>%
    Map(f = function(relative_position_name) {
      tile_borders(neighbors[[relative_position_name]]) %>%
      magrittr::extract2(mirror[relative_position_name])
    })
}

#' check whether tile conforms given all constraints
tile_conforms_contraints <- function(tile, constraints) {
  borders <- tile_borders(tile)
  names(constraints) %>% 
    Map(f = function(side) constraints[[side]] == borders[[side]]) %>%
    Reduce(f = all, init = TRUE)  
}

## TILE SEARCH

#' we need to find a tile that will take position c(1,1) on the field
find_first_tile <- function(???) {
  NULL
}

#' function takes all free tiles and constraints and returns
#' properly oriented tile with MODIFIED orientation that conforms constraints
find_tile_given_constraints <- function(constraints, tiles) {

  # check tile with under different angles, flip it, check it again
  orient_funs <- c(identity, rep(turn_tile, 3), flip_tile, rep(turn_tile, 3))
  
  tiles_megaset <- 
    tiles %>%
    Reduce(f = function(z, tile) {
      tile_orientations <- Map(orient_funs, function(orient) orient(tile)) 
      c(z, tile_orientations)
    }, init = list())

  found <- tiles_megaset %>% 
    # this approach is bad because we do not see how tile must be oriented
    Filter(f = function(tile) tile_conforms_contraints(tile, constraints))
    
  if (len(found) == 0) NULL else found[[1]]
}

#' given field with placed elements and list of tiles - filter out tiles which
#' are already placed on the field (based on their id)
find_lose_tiles <- function(field, tiles) {
  used_tile_ids <- field %>% 
    Filter(f = function(x) field_position_is_tile(field, x)) %>%
    Map(f = function(x) x$id)
  
  tiles %>% 
    Filter(f = function(x) (x %in% used_tile_ids) == FALSE)
}

#' find tile for a given position
find_tile_given_position <- function(field, position, lose_tiles) {
  current_tile <- field %>% magrittr::extract2(position_to_txt(position))
  if !is.null(current_tile) current_tile 
  else {
    found <- 
      field %>% 
        field_position_constraints(position) %>% 
        find_tile_given_constraints(lose_tiles)
    
    if (is.null(found)) list(type = "outer space", position = position) 
    else found
  }
}

#' function that recursively calls itself to populate field with tiles
#' if field is empty 
#'   function will take random tile and place it into 
#'   position (0, 0), then function will call itself
#' if field is not empty
#'   function will find empty neighbors of tiled fields and search for
#'   mathcing tile among available tiles applying constraints from tiles
#'   which are already placed on the field (also considering their orientation)
fill_field_with_tiles <- function(field = NULL, tiles) {
  if (length(tiles) == 0) field
  else if (is.null(field) | length(field) == 0) {
    empty_field <- list()
    init_position <- c(0,0)
    first_tile <- tiles[[1]]
    field <- empty_field %>% add_on_field(init_position, first_tile)
    remaining_tiles <- find_lose_tiles(field, tiles)
    fill_field_with_tiles(field, remaining_tiles)
  } else {
    empty_neighor_positions <- field %>% 
      Map(f = function(tile) {
        field_position_neighbor_positions(field, tile$position)
      }) %>% 
      Reduce(f = c, init = list()) %>%
      Filter(f = function(position) field_position_is_empty(field, position))
    if (length(empty_neighor_positions) == 0) stop(paste(
        "unexpected termination:", 
        "no empty positions on field while tiles are available"))
    for_positoin <- empty_neighor_positions[[1]]
    new_field <- field %>% 
      find_tile_given_position(for_positoin) %>%
      add_on_field(field = field, tile_or_outer = ., position = for_positoin)
    remaining_tiles <- find_lose_tiles(new_field, tiles) <- 
    fill_field_with_tiles(new_field, remaining_tiles)
  }
}
#'
#- SOLUTION PART 1 ------------------------------------------------------------#

day20_part1_solution <- function(input) {
  tiles <- input %>% parse_input()
  
  field <- fill_field_with_tiles
  
  field %>% 
    Map(f = function(tile) paste("position:", tile$position, "id:", tile$id)) %>%
    Map(f = print)
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

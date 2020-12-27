library(magrittr)

test_input <- c(
  "Player 1:",
  "9",
  "2",
  "6",
  "3",
  "1",
  "",
  "Player 2:",
  "5",
  "8",
  "4",
  "7",
  "10"
)

real_input <- readLines("./inputs/day22-input.txt")

#- LOGIC ----------------------------------------------------------------------#

#' function that creates state of the game (players and their deck content)
new_state <- function(p1, p2) list(p1 = p1, p2 = p2)

#' takes text input and returns list of player 1 (p1) and 2 (p2) decks
parse_input <- function(input) {
  div <- which(input == "")
  len <- length(input)
  new_state(
    p1 = input[2:(div - 1)] %>% as.integer(),
    p2 = input[(div + 2):len] %>% as.integer()
  )
}

#' function takes state of the game (players and their decks) and applies game 
#' rules to play one round. 
#' 
#' The result is a new state of the world.
play_round <- function(state) {
  p1_card <- state$p1[1]
  p2_card <- state$p2[1]
  p1_deck <- state$p1[-1]
  p2_deck <- state$p2[-1]
  if (p1_card > p2_card) 
    new_state(p1 = c(p1_deck, p1_card, p2_card), p2 = p2_deck)
  else 
    new_state(p1 = p1_deck, p2 = c(p2_deck, p2_card, p1_card))
}

#' check if game is over
#' 
#' Game is over when one of the players has no cards
game_is_over <- function(state) 
  length(state$p1) == 0 | length(state$p2) == 0


#' play game
#' 
#' Plays rounds again and again until game is over
play_full_game <- function(state) {
  while (!game_is_over(state)) {
    state <- play_round(state)
  }
  state
}

#' determine winner
#' 
#' when game is over winner is one who has cards in their deck
determine_winner <- function(state) {
  if (length(state$p1) == 0) "p2"
  else "p1"
}

#' score of winner
#' 
#' calculate score of winner, bottom card in the deck has multiplier = 1,
#' card above bottom has multiplier = 2, and so on.
#' game score is sum of pairwise multiplication of card rank and multiplier.
score <- function(deck) sum(rev(deck) * seq_along(deck))

#- SOLUTION PART 1 ------------------------------------------------------------#

day22_part1_solution <- function(input) {
  state <- input %>% parse_input() %>% play_full_game()
  winner <- state %>% determine_winner()
  score <- state[[winner]] %>% score()
  
  score
}

test_output_part1 <- 306
test_result <- day22_part1_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part1))

real_result_part1 <- day22_part1_solution(real_input)
print(format(real_result_part1, scientific = FALSE))

#- SOLUTION PART 2 ------------------------------------------------------------#

day22_part2_solution <- function(input) {
  NULL
}

test_output_part2 <- -1
test_result <- day22_part2_solution(test_input)
print(paste(
  "test result:", test_result,
  "valid:", test_result == test_output_part2))

real_result_part2 <- day22_part2_solution(real_input)
print(format(real_result_part2, scientific = FALSE))

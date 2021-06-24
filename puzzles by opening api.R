# puzzles_by_opening_api.R

library(tidyverse)
library(chess)

puzzles <- data.table::fread("C:\\Users\\chris\\OneDrive\\Desktop\\Chess\\databases\\Lichess puzzles\\lichess_db_puzzle(1).csv")
colnames(puzzles) <- c("PuzzleId","FEN","Moves","Rating","RatingDeviation","Popularity","NbPlays","Themes","GameUrl")

# Find game IDs in puzzles
puzzles <- puzzles %>%
  mutate(game_id = str_remove(GameUrl, "https://lichess.org/")) %>%
  mutate(game_id = str_remove(game_id, "/black"),
         game_id = substr(game_id, start = 1, stop = str_locate(game_id, "#") - 1)) %>%
  mutate(pgn = NA,
         eco = NA,
         opening = NA)

#### API time ####
library(rvest)

  # Retrieves the pgn of the game the puzzle originated from.
get_pgn <- function(game_id) {
  
  pgn <- read_html(paste0("https://lichess.org/game/export/", game_id))  %>%
    html_text() %>%
    str_replace_all("[\n]", "") %>%
    str_replace_all('\\"', "")
  
  return(pgn)
  
}

counter <- 0
start_time <- Sys.time()
start_chunk_time <- Sys.time()

for(i in 1:nrow(puzzles)){
  
  puzzles$pgn[i] <- get_pgn(puzzles$game_id[i])
  
  if(i %% 10000 == 0){
    counter <- counter + 1
    print(i)
    
    if(counter >= 1){
      
      print(paste0("This chunk took ", 
                   round(Sys.time() - start_chunk_time, digits = 2), 
                   " hours."))
      
      start_chunk_time <- Sys.time()
      
    }
    
    data.table::fwrite(puzzles, 
                       file = paste0("C:/chess/puzzles by opening/results/puzzles_openings ",
                                     str_replace_all(Sys.time(), ":", "_"),
                                     ".csv"))
    
  }
  
}

Sys.time() - start_time

#### In case of break in runs, go here ####
puzzles_files <- list.files("C:/chess/puzzles by opening/results/")
puzzles_latest <- data.table::fread(paste0("C:/chess/puzzles by opening/results/", puzzles_files[length(puzzles_files)]))

n_complete_pgn <- puzzles_latest %>% filter(!is.na(pgn), pgn != "") %>% nrow()

counter <- floor(n_complete_pgn / 10000)

start_time <- Sys.time()
start_chunk_time <- Sys.time()

for(i in n_complete_pgn:nrow(puzzles_latest)){
  
  if(nchar(puzzles_latest$game_id[i]) < 8){
    puzzles_latest$pgn[i] <- NA
  } else {
    
    puzzles_latest$pgn[i] <- get_pgn(puzzles_latest$game_id[i])
    
  }
  
  
  
  if(i %% 10000 == 0 | i == nrow(puzzles_latest)){
    counter <- counter + 1
    print(i)
    
    if(counter > 1){
      
      print(paste0("This chunk took ", round(Sys.time() - start_chunk_time, digits = 2), " hours."))
      start_chunk_time <- Sys.time()
      
    }
    
    data.table::fwrite(puzzles_latest, 
                       file = paste0("C:/chess/puzzles by opening/results/puzzles_openings ",
                                     str_replace_all(Sys.time(), ":", "_"),
                                     ".csv"))
    
  }
  
}

Sys.time() - start_time

#### Parse PGN for ECO and Opening ####

get_eco <- function(pgn){
  
  eco <- pgn %>% 
    str_extract(pattern = regex("\\[ECO [A-Z]\\d\\d\\]"))
  
  return(eco)
  
}

get_opening <- function(pgn){
  
  opening <- pgn %>%
    str_extract(pattern = regex("\\[Opening .*?\\]"))
  
  return(opening)
  
}

puzzles_files <- list.files("C:/chess/puzzles by opening/results/")
puzzles_latest <- data.table::fread(paste0("C:/chess/puzzles by opening/results/", puzzles_files[length(puzzles_files)])) %>%
  filter(pgn != "")

puzzles_latest %>% colnames()

# Create a new table that has openings and ECO codes for each puzzle.
puzzles_latest <- puzzles_latest %>%
  mutate(eco = get_eco(pgn),
         opening = get_opening(pgn))

# Save the new table of puzzles
# data.table::fwrite(puzzles_latest, 
#                    file = paste0("C:/chess/puzzles by opening/results/puzzles_openings ",
#                                  str_replace_all(Sys.time(), ":", "_"),
#                                  ".csv"))

#### Turn puzzles from Lichess into Lucas Chess (R) friendly puzzles ####
# Description of Lichess puzzles from lichess.org: 
# "FEN is the position before the opponent makes their move.
# The position to present to the player is after applying the first move to that FEN.
# The second move is the beginning of the solution."

# So, for each puzzle we need to advance the game one move, then save the new FEN.

# A function to create a solution to the puzzles, in SAN. Function is used in create_puzzle_pgn.
create_solution <- function(next_moves, new_game){
  
  if(length(next_moves) == 1){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       notation = "uci")
    
  } else if(length(next_moves) == 2){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       notation = "uci")
    
  } else if (length(next_moves) == 3){
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       notation = "uci")
    
  } else if (length(next_moves) == 4){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       notation = "uci")
    
  } else if (length(next_moves) == 5){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       notation = "uci")
    
  } else if (length(next_moves) == 6){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       notation = "uci")
    
  } else if (length(next_moves) == 7){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       notation = "uci")
    
  } else if (length(next_moves) == 8){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       notation = "uci")
    
  } else if (length(next_moves) == 9){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       notation = "uci")
    
  } else if (length(next_moves) == 10){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       notation = "uci")
    
  } else if (length(next_moves) == 11){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       notation = "uci")
    
  } else if (length(next_moves) == 12){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       notation = "uci")
    
  } else if (length(next_moves) == 13){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       notation = "uci")
    
  } else if (length(next_moves) == 14){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       notation = "uci")
    
  } else if (length(next_moves) == 15){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       notation = "uci")
    
  } else if (length(next_moves) == 16){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       notation = "uci")
    
  } else if (length(next_moves) == 17){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       notation = "uci")
    
  } else if (length(next_moves) == 18){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       notation = "uci")
    
  } else if (length(next_moves) == 19){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       notation = "uci")
    
  } else if (length(next_moves) == 20){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       notation = "uci")
    
  } else if (length(next_moves) == 21){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       notation = "uci")
    
  } else if (length(next_moves) == 22){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       next_moves[22],
                       notation = "uci")
    
  } else if (length(next_moves) == 23){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       next_moves[22],
                       next_moves[23],
                       notation = "uci")
    
  } else if (length(next_moves) == 24){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       next_moves[22],
                       next_moves[23],
                       next_moves[24],
                       notation = "uci")
    
  } else if (length(next_moves) == 25){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       next_moves[22],
                       next_moves[23],
                       next_moves[24],
                       next_moves[25],
                       notation = "uci")
    
  } else if (length(next_moves) == 26){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       next_moves[22],
                       next_moves[23],
                       next_moves[24],
                       next_moves[25],
                       next_moves[26],
                       notation = "uci")
    
  } else if (length(next_moves) == 27){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       next_moves[22],
                       next_moves[23],
                       next_moves[24],
                       next_moves[25],
                       next_moves[26],
                       next_moves[27],
                       notation = "uci")
    
  } else if (length(next_moves) == 28){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       next_moves[22],
                       next_moves[23],
                       next_moves[24],
                       next_moves[25],
                       next_moves[26],
                       next_moves[27],
                       next_moves[28],
                       notation = "uci")
    
  } else if (length(next_moves) == 29){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       next_moves[22],
                       next_moves[23],
                       next_moves[24],
                       next_moves[25],
                       next_moves[26],
                       next_moves[27],
                       next_moves[28],
                       next_moves[29],
                       notation = "uci")
    
  } else if (length(next_moves) == 30){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       next_moves[22],
                       next_moves[23],
                       next_moves[24],
                       next_moves[25],
                       next_moves[26],
                       next_moves[27],
                       next_moves[28],
                       next_moves[29],
                       next_moves[30],
                       notation = "uci")
    
  } else if (length(next_moves) == 31){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       next_moves[22],
                       next_moves[23],
                       next_moves[24],
                       next_moves[25],
                       next_moves[26],
                       next_moves[27],
                       next_moves[28],
                       next_moves[29],
                       next_moves[30],
                       next_moves[31],
                       notation = "uci")
    
  } else if (length(next_moves) == 32){
    
    end_tactic <- move(game = new_game, 
                       next_moves[1],
                       next_moves[2],
                       next_moves[3],
                       next_moves[4],
                       next_moves[5],
                       next_moves[6],
                       next_moves[7],
                       next_moves[8],
                       next_moves[9],
                       next_moves[10],
                       next_moves[11],
                       next_moves[12],
                       next_moves[13],
                       next_moves[14],
                       next_moves[15],
                       next_moves[16],
                       next_moves[17],
                       next_moves[18],
                       next_moves[19],
                       next_moves[20],
                       next_moves[21],
                       next_moves[22],
                       next_moves[23],
                       next_moves[24],
                       next_moves[25],
                       next_moves[26],
                       next_moves[27],
                       next_moves[28],
                       next_moves[29],
                       next_moves[30],
                       next_moves[31],
                       next_moves[32],
                       notation = "uci")
  }
  
  solution_san <- pgn(back(end_tactic, steps = length(next_moves) - 1))
  
  return(solution_san)
  
}

# A function to create puzzle pgn
create_puzzle_pgn <- function(
  fen, puz_moves, game_url, eco, opening, rating, themes
  ){
  
  # create game
  game <- chess::game(fen = fen)
  
  # Determine moves
  first_move <- str_trim(substr(puz_moves, 1, 5))
  next_moves <- str_remove(puz_moves, first_move) %>% str_trim() %>%
    str_split(pattern = " ") 
  
  next_moves <- next_moves[[1]]

  # game after advancing one move
  new_game <- move(game = game, first_move, notation = "uci")
  new_fen <- fen(new_game)
  
  # Variables for other headers
  to_move <- ifelse(turn(new_game), "White", "Black")
  puz_start <- chess::move_number(game = new_game)
  
  # Create solution to puzzle
  solution_san <- create_solution(next_moves = next_moves,
                                  new_game = new_game)
  
  # Produces a pgn chunk
  pgn_chunk <- paste(
    # paste0('[Event "Some puzzles"]'),
    paste0('[Site "', game_url, '"] '),
    paste0('[Date "2021.06.06"]'),
    # paste0('[Round "-"]'),
    paste0('[ToMove "', to_move, ' to move."]'),
    # paste0('[Black " "]'),
    paste0('[ECO "', eco, ' "]'),
    paste0('[Opening "', opening, '"]'),
    paste0('[Rating "', rating, '"]'),
    paste0('[BeginMove "', puz_start, '"]'),
    paste0('[Themes "', themes, '"]'),
    paste0('[Result "*"]'),
    paste0('[FEN "', new_fen, '"] '),
    paste0(solution_san, " *"), "", "",
    sep = "\n"
  )
  
  return(pgn_chunk)
  
}

# Test on a subset
puz_samples <- puzzles_latest %>% sample_n(100)

for(i in 1:nrow(puz_samples)){
  
  if(i == 1){
    
    fen <- puz_samples$FEN[i]
    puz_moves <- puz_samples$Moves[i]
    game_url <- puz_samples$GameUrl[i]
    eco <- puz_samples$eco[i]
    opening <- puz_samples$opening[i]
    rating <- puz_samples$Rating[i]
    themes <- puz_samples$Themes[i]
    
    our_pgn <- create_puzzle_pgn(fen = fen,
                                puz_moves = puz_moves,
                                game_url = game_url,
                                eco = eco,
                                opening = opening,
                                rating = rating,
                                themes = themes)
    
  } else {
    
    fen <- puz_samples$FEN[i]
    puz_moves <- puz_samples$Moves[i]
    game_url <- puz_samples$GameUrl[i]
    eco <- puz_samples$eco[i]
    opening <- puz_samples$opening[i]
    rating <- puz_samples$Rating[i]
    themes <- puz_samples$Themes[i]
    
    our_pgn <- paste0(our_pgn, 
                      create_puzzle_pgn(fen = fen,
                                        puz_moves = puz_moves,
                                        game_url = game_url,
                                        eco = eco,
                                        opening = opening,
                                        rating = rating,
                                        themes = themes)) 
    
  }
  
}

write_path <- "C:/chess/pgn/test.pgn"

writeLines(our_pgn, con = write_path) # It works in Lucas Chess (R)!

rm(puz_samples)

# Write a pgn for each eco
eco_unique <- unique(puzzles_latest$eco)

for(i in 1:length(eco_unique)){
  
  eco_puzzles <- puzzles_latest %>%
    filter(eco == eco_unique[i])
  
  print(paste0("Working on ECO ", eco_unique[i]))
  
  for(i in 1:nrow(eco_puzzles)){
    
    if(i == 1){
      
      fen <- eco_puzzles$FEN[i]
      puz_moves <- eco_puzzles$Moves[i]
      game_url <- eco_puzzles$GameUrl[i]
      eco <- eco_puzzles$eco[i]
      opening <- eco_puzzles$opening[i]
      rating <- eco_puzzles$Rating[i]
      themes <- eco_puzzles$Themes[i]
      
      eco_pgn <- create_puzzle_pgn(fen = fen,
                                   puz_moves = puz_moves,
                                   game_url = game_url,
                                   eco = eco,
                                   opening = opening,
                                   rating = rating,
                                   themes = themes)
      
    } else {
      
      fen <- eco_puzzles$FEN[i]
      puz_moves <- eco_puzzles$Moves[i]
      game_url <- eco_puzzles$GameUrl[i]
      eco <- eco_puzzles$eco[i]
      opening <- eco_puzzles$opening[i]
      rating <- eco_puzzles$Rating[i]
      themes <- eco_puzzles$Themes[i]
      
      eco_pgn <- paste0(eco_pgn, 
                        create_puzzle_pgn(fen = fen,
                                          puz_moves = puz_moves,
                                          game_url = game_url,
                                          eco = eco,
                                          opening = opening,
                                          rating = rating,
                                          themes = themes)) 
      
    }
    
  }
  
  write_path <- paste0("C:/chess/puzzles by opening/eco puzzles/puzzles_", eco, ".pgn")
  
  writeLines(eco_pgn, con = write_path)
  
}
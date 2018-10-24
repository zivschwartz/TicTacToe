#Display 
display <- function(state){
  for(i in 1:9){ 
    if(is.na(state[i]) == T){
      state[i] <- i 
    } #replaces NA with correct position number
  }
  cat("", state[1], "|", state[2], "|", state[3], "\n")
  cat("---+---+---", "\n")
  cat("", state[4], "|", state[5], "|", state[6], "\n")
  cat("---+---+---", "\n")
  cat("", state[7], "|", state[8], "|", state[9], "\n")
}

#Update
update <- function(state, who, pos){
  while((pos %in% (1:9)) == F | is.na(pos) == T | identical(as.character(state[pos]), "x") | identical(as.character(state[pos]), "o")){ #checks if pos is an integer from 1-9, if there are an NA's, or if the player plays an impossible character (not x or o)
    print("Impossible move. Enter 1-9: ")
    pos <- as.integer(readline("Choose where to play instead (Enter 1-9): "))
  }
  state[pos] <- who
  return(state)
}

#Check Winner
check_winner <- function(state){
  winner <<- F
  if(((sum(grepl("x", state)) + sum(grepl("o", state))) == 9)){ #checks if all 9 spots on the board are filled up with x's and o's 
    for(i in 1:8){
      if(identical(state[unlist(triples[i])], rep("x", 3)) == F & identical(state[unlist(triples[i])], rep("o", 3)) == F){ #checks if the triple subset of state is identical to three straight x's & o's
        if(i != 8){
          next
        }else{
          print("The game ends in a draw.")
          winner <<- T
        break
        }
      }
    }
  }
  else{
    for(i in 1:8){
      if(identical(state[unlist(triples[i])], rep("o", 3)) == T){ #checks if the triple subset of state is identical to three straight o's
        print("O wins!")
        winner <<- T
      break
      }
      else if(i != 8){
        next
      }else{
        for(i in 1:8){
          if(identical(state[unlist(triples[i])], rep("x", 3)) == T){ #checks if the triple subset of state is identical to three straight o's
            print("X wins!")
            winner <<- T
          break
          }else if(i != 8){ 
            next
          }else{
            print("There is no winner at this moment.")
            break
          }
        }
      }
    }
  }
}

#Computer Turn 
computer_turn <- function(state){
  triples <<- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7))
  for(i in 1:9){
    if(is.na(state[i]) == T){
      state[i] <- i
    }
  }
  if((sum(grepl("x", state)) > sum(grepl("o", state))) == T){ #determines comp character as o since if there are more x's then o's on the board it's o's turn
    comp <<- "o"
    human <<- "x"
  }
  else if((sum(grepl("x", state)) == sum(grepl("o", state))) == T){ #determines comp character as x since if there are equal numbers for x and o on the board it's x's turn
    comp <<- "x"
    human <<- "o"
  }
  for(i in 1:8){
    if((sum(grepl(comp, state[unlist(triples[i])])) == 2) & (human %in% state[unlist(triples[i])][grep(comp, state[unlist(triples[i])], invert = T)]) == F){ #strategy code to block/win any player who has two of the three positions of the triple. This checks if there are 2 positions filled and that one is still open. 
      return(as.integer(state[unlist(triples[i])][grep(comp, state[unlist(triples[i])], invert = T)]))
      break
    }
    else if(i != 8){
      next
    }else{
      for(i in 1:8){
        if((sum(grepl(human, state[unlist(triples[i])])) == 2) & (comp %in% state[unlist(triples[i])][grep("x", state[unlist(triples[i])], invert = T)]) == F){ #strategy code to block/win any player who has two of the three positions of the triple. This checks if there are 2 positions filled and that one is still open.
          return(as.integer(state[unlist(triples[i])][grep(human, state[unlist(triples[i])], invert = T)]))
          break
        }
        else if(i != 8){
          next
        }else if(("x" %in% state) == F){ #strategy code for playing the first move in the center of the board, at position 5
          return(5)
          }
        else{
          return(as.integer((sample(state[grep("x", state, invert = T)][grep("o", state[grep("x", state, invert = T)], invert = T)], 1)))) #strategy code for comp to randomly generate a position to play based off what positions are not taken
        }
      }
    } 
  }
}

#Play 
play <- function(){
  triples <<- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7))
  winner <<- F
  state <<- c("1","2","3","4","5","6","7","8","9")
  players <- as.integer(readline("How many players?: "))
  while(((players %in% 1:2) == F | is.na(players) == T)){ #makes sure the user input is valid
    players <- as.integer(readline("Not a valid choice. Enter 1 or 2 players: "))
  }
  display(state)
  if(players == 1){
    human <- as.character(readline("Choose x or o. X goes first: "))
    while((identical(human, "x") == F) & (identical(human, "o") == F)){ #makes sure the user input is valid
      human <- as.character(readline("Not a valid choice. Enter 1-9: "))
    }
    if(identical(human, "x") == T){
      while(winner == F){
        pos <- as.integer(readline("Choose where to go, Enter 1-9: "))
        while(is.na(pos) == T){ 
         pos <- as.integer(readline("Not a valid choice. Enter 1-9: "))
        }
        state <- update(state, human, pos)
        display(state)
        check_winner(state)
        if(winner == T){
          break
        }else{
          print("Computer's turn.")
          comp <- "o"
          pos <- as.integer(computer_turn(state))
          state <- update(state, comp, pos)
          display(state)
          check_winner(state)
          if(winner == T){
            break
          }
        }
      }
      while(winner == T){
        break
      }
    }
    else if(identical(human, "o") == T){
      while(winner == F){
        print("Computer's turn.")
        comp <- "x"
        pos <- as.integer(computer_turn(state))
        state <- update(state, comp, pos)
        display(state)
        check_winner(state)
        if(winner == T){
          break
        }else{
          pos <- as.integer(readline("Choose where to go, Enter 1-9: "))
          while(is.na(pos) == T){
            pos <- as.integer(readline("Not a valid choice. Enter 1 or 2 players: "))
          }
          state <- update(state, human, pos)
          display(state)
          check_winner(state)
          if(winner == T){
            break
          }
        }
      }
      while(winner == T){
        break
      }
    }
  }
  else if(players == 2){
    while(winner == F){
      pos <- as.integer(readline("X's move: "))
      while(is.na(pos) == T){
        pos <- as.integer(readline("Not a valid choice. Enter 1-9: "))
      }
      state <- update(state, "x", pos)
      display(state)
      check_winner(state)
      if(winner == T){
        break
      }
      pos <- as.integer(readline("O's move: "))
      while(is.na(pos) == T){
        pos <- as.integer(readline("Not a valid choice. Enter 1-9: "))
      }
      state <- update(state, "o", pos)
      display(state)
      check_winner(state)
      if(winner == T){
        break
      }
    }
    while(winner == T){
      break
    }
  }
}

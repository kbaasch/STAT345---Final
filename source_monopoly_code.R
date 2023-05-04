## libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

##the board

monopolyBoard <- read.csv("MonopolyBoard.csv", header = T)
monopolyBoard


## lets_move
# Using dice rolls to determine where a player will land on the board
#input: "location", where the player is on the board
#output: the players new location on the board after the have rolled

#note that this is only accounting for standard dice rolls, no special rules yet...

lets_move = function(location) {
  dice_roll_1 <- sample(1:6, 1, replace = TRUE) #A random roll of a six-sided die
  dice_roll_2 <- sample(1:6, 1, replace = TRUE) #A random roll of a six-sided die
  
  roll_total <- sum(dice_roll_1, dice_roll_2) #The sum of the two rolls
  
  location = location + roll_total #Updating the player's location based on their rolls from the current turn
  
  if(location <= 40) { #if statement checking the players location
    location = location
  } else { #if the sum of the player's previous location and their rolls is greater than 40, this moves the player past GO and the start moving from space 1
    location = location - 40
  }
  
  print(location)
  
}

lets_move(20)



## who_goes_first2
who_goes_first2 <- function(num_players){
  initial_rolls <- c()
  player_order <- length(num_players)
  for (i in 1:num_players) {
    player_i_roll <- sum(sample(1:6, 2, replace=TRUE))
    initial_rolls <- c(initial_rolls, player_i_roll) # vector of every player's first roll
  }
  
  if(sum(initial_rolls == max(initial_rolls)) >= 2){ # if more 1 person has the highest roll
    
    while (sum(initial_rolls == max(initial_rolls)) >= 2) { # keep rolling as long as players are tied
      tied_players <- which(initial_rolls==max(initial_rolls)) # identify which players tied
      
      tie_breaker_rolls <- c()
      for (i in tied_players) { # tied players roll again
        player_i_roll2 <- sum(sample(1:6, 2, replace=TRUE))
        tie_breaker_rolls <- c(tie_breaker_rolls, player_i_roll2)
        initial_rolls <- tie_breaker_rolls # update initial rolls
      }
    }
    highest_roll <- max(tie_breaker_rolls)
    highest_player <- tied_players[which(tie_breaker_rolls == highest_roll)]
  }
  
  else {
    highest_player <- which.max(initial_rolls) # who rolled the highest number
    
    # creating player order
    player_order[1] <- as.integer(highest_player)
    
    for (j in 2:num_players) {
      
      player_order[j] <- as.integer(player_order[j-1] + 1)
      if (player_order[j] > num_players) {
        player_order[j] <- 1
      }
    }
    #print(player_order)
  }
  playerlist <- (list(highest_player, player_order))
  return(playerlist)
}

who_goes_first2(4)


## lets_play

#A function to play Monopoly!
#Utilizes lets_move to move the players around the board
#input: "turns"; the amount of turns each player will take (i.e. the number of rounds in the game), "players"; a numeric value 1-4 representing the number of players in the game
#output: a dataframe showing each players' location after each of their turns; a density plot showing each players' location data throughout the game; a density plot showing the overall location data throughout the game

lets_play = function(turns, players) {
  
  location_data <- as.data.frame(matrix(nrow = turns, ncol = players)) # dataframe will keep data for locations of players for each turn
  
  # puts players in order
  colnames(location_data) <- who_goes_first2(players)[[2]]
  
  location = 1 #initializes the player at space 1 (i.e. GO!)
  location_data[1,] <- location
  
  for (i in 2:turns) { # iterates through number of turns
    for (j in 1:players) { # rotates through each player
      location = lets_move(location_data[i-1,j])[[1]] # takes location from lets_move2 function and assigns it to location
      location_data[i,j] <- as.integer(location) # puts location value into the dataframe of locations for each player
    }
  }
  print(location_data) 
  
  
  ########################## plots ###############################
  
  #vector of colors
  colors <- c("red", "blue", "green","purple")
  
  # creates density plot of frequency of landing on each space for each player
  plot(density(location_data[,1]), xlim = c(0,40), col = "black", xlab="Spaces", main= "Density of Landing Locations for All Players")
  
  for (k in 2:players) {
    lines(density(location_data[,k]), col = colors[k-1]) # adds a density line for each player after the first
  }
  
  # creates density plot for total frequency landing on each space (all players spaces included)
  all_spaces <- as.vector(as.matrix(location_data))
  plot(density(all_spaces), xlim = c(0,40), col = "black", xlab="Spaces", main="Density of Landing Locations Overall")
  
  #creates frequency plot for frequency of landing on certain spaces for each player
  for(k in 1:players){
    plot(location_data[,k], xlim= c(0,40), col=colors[k], type="h", xlab="Spaces", ylab="Frequency", main="Frequency of Landing on Each Space for Each Player")
  }
  
}

lets_play(100, 4)




## double_down <- lets_move2
#Double Down -- addition of jail and double rolls

#Incorporating double rolls and sending players to jail when the players are rolling and moving
#input: "location"; a numeric value 1 to 40; where the player in on the board
#output: the players new location on the board

lets_move2 = function(location) {
  dice_roll_1 <- sample(1:6, 1, replace = TRUE) #a standard, six-sided dice roll
  dice_roll_2 <- sample(1:6, 1, replace = TRUE) #a standard, six-sided dice roll
  
  roll_total <- sum(dice_roll_1, dice_roll_2) #the sum of the two dice rolls
  roll_double <- dice_roll_1 == dice_roll_2 #checking if the player rolled a double
  
  location = location + roll_total # updates location to be previous location + roll total
  double3 = "false" # double3 is assigned "true" if the player double rolls 3x (as seen in if statement below), otherwise it is assigned "false" 
  
  # first double roll -> roll again
  if (roll_double == TRUE) { #checks if the player rolled a double with their first two rolls. if "TRUE", the player rolls again
    dice_roll_3 <- sample(1:6, 1, replace = TRUE) #the player's third dice roll
    dice_roll_4 <- sample(1:6, 1, replace = TRUE) #the player's fourth dice roll
    
    roll_total2 <- sum(dice_roll_3, dice_roll_4) #the sum of the player's third and fourth rolls
    roll_double2 <- dice_roll_3 == dice_roll_4 #checks to see if the player rolled a double again
    
    location = location + roll_total2 #the player's new location
    double3 = "false"
    
    # if player is at a location past 40, subtract 40 so they go back to the GO space at location "41"
    # this is included in each if statement below so that it applies if player has to roll again
    if (location <= 40) { #checks to make sure that the player's location isn't over 40 (i.e not on the board)
      location = location
    } else { #If greater than 40, it subtracts 40 from location to allow player to continue to move on the board
      location = location - 40
    }
    
    # second double roll -> roll again
    if (roll_double2 == TRUE) { #if the player has rolled two doubles, they roll again
      dice_roll_5 <- sample(1:6, 1, replace = TRUE) #the player's fifth roll
      dice_roll_6 <- sample(1:6, 1, replace = TRUE) #the player's sixth roll
      
      roll_total3 <- sum(dice_roll_5, dice_roll_6) #the sum of the fifth and sixth roll
      roll_double3 <- dice_roll_5 == dice_roll_6 #checking to see if the player rolled a third double
      
      location = location + roll_total3 #the players new location
      double3 = "false"
      
      if (location <= 40) { #checks the player's location again
        location = location
      } else {
        location = location - 40
      }
      
      # third double roll -> go to jail
      if (roll_double3 == TRUE) { #checks if the player rolled three doubles in a row, if TRUE, the players goes to jail (space 11)
        location = monopolyBoard$spaces[monopolyBoard$name == "Jail"]
        double3 = "true"
      }
    }
  }  
  
  if (location <= 40) { #checks the players location on the board
    location = location
  } else {
    location = location - 40
  }
  
  listdata <- (list(location, double3)) # creates list for outputs "location" and "double3"
  # if you need to access location from this function, use lets_move2(location_data[i-1,j])[[1]]
  # if you need to access double3, use lets_move2(double3)[[2]]
  return(listdata)
  
}


## lets_play2


#A function to move players around the board while incorporating double rolls and going to jail
#input: "turns"; the amount of turns each player takes (i.e. how many rounds the game is), "players"; a numeric value 1-4 representing the number of players in the game
#output: a dataframe showing each players' location after each of their turns; a density plot showing each players' location data throughout the game; a density plot showing the overall location data throughout the game

lets_play2 = function(turns, players) {
  
  
  location_data <- as.data.frame(matrix(nrow = turns, ncol = players)) # dataframe will keep data for locations of players for each turn
  double3data <- as.data.frame(matrix(nrow = turns, ncol = players)) # dataframe will say "true" if player double rolled 3x (which lands them in jail) and "false" if they didn't double roll 3x
  
  # puts players in order
  colnames(location_data) <- who_goes_first2(players)[[2]]
  
  location = 1 # each player starts at location 1
  location_data[1,] <- location
  double3 = 0 # initializing "double3" which will be later assigned a "true" or "false" based on if player double rolled 3x
  
  for (i in 2:turns) { # iterates through number of turns
    
    for (j in 1:players) { # rotates through each player
      
      location = lets_move2(location_data[i-1,j])[[1]] # takes location from lets_move2 function and assigns it to location
      location_data[i,j] <- as.integer(location) # puts location value into the dataframe of locations for each player
      double3data[i,j] <- lets_move2(double3)[[2]] # takes "true" or "false" value from double3 and adds it to double3data dataframe
      
      # deals with landing in jail from "go to jail" space and staying there for next 3 turns
      if (31 %in% location_data[i-1,j]) { # if you landed on 31 in the last turn (i-1 turn), you go to jail (space 11) for the ith turn
        location <- 11
        location_data[i,j] <- as.integer(location) # new location is 11
      } else if (11 %in% location_data[i-1,j] & 31 %in% location_data[i-2,j]) { # if you were on space 11 last turn AND you were on space 31 2 turns ago, you stay in jail (space 11) for turn i
        location <- 11
        location_data[i,j] <- as.integer(location)
      } else if (11 %in% location_data[i-2,j] & 31 %in% location_data[i-3,j]) {
        location <- 11
        location_data[i,j] <- as.integer(location)
      }
      
      # landing in jail from 3x double roll and staying there for next 3 turns
      if (11 %in% location_data[i,j] & "true" %in% double3data[i-1,j]) { # if player's location is 11 (jail) and double3data is"true" (player double rolled 3x), player stays in jail. all 3 if/else if statements keep player in jail for the next 3 turns
        location <- 11
        location_data[i,j] <- as.integer(location)
      } else if (11 %in% location_data[i-1,j] & "true" %in% double3data[i-2,j]) { # 
        location <- 11
        location_data[i,j] <- as.integer(location)
      } else if (11 %in% location_data[i-2,j] & "true" %in% double3data[i-3,j]) {
        location <- 11
        location_data[i,j] <- as.integer(location)
      }
      
    }
  }
  print(location_data)
  print(double3)
  
  
  ########################## plots ###############################
  
  #vector of colors
  colors <- c("red", "blue", "green","purple")
  
  # creates density plot of frequency of landing on each space for each player
  plot(density(location_data[,1]), xlim = c(0,40), col = "black", xlab="Spaces", main= "Density of Landing Locations for All Players")
  
  for (k in 2:players) {
    lines(density(location_data[,k]), col = colors[k-1]) # adds a density line for each player after the first
  }
  
  # creates density plot for total frequency landing on each space (all players spaces included)
  all_spaces <- as.vector(as.matrix(location_data))
  plot(density(all_spaces), xlim = c(0,40), col = "black", xlab="Spaces", main="Density of Landing Locations Overall")
  
  #creates frequency plot for frequency of landing on certain spaces for each player
  for(k in 1:players){
    plot(location_data[,k], xlim= c(0,40), col=colors[k], type="h", xlab="Spaces", ylab="Frequency", main="Frequency of Landing on Each Space for Each Player")
  }
  
}

lets_play2(100, 4)
##the community chest and chance cards

Card_Numbers <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) # the card numbers

Chance_Cards <- c("Advance to Boardwalk", "Advance to Go (Collect $200)", "Advance to Illinois Avenue. If you pass Go, collect $200", "Advance to St. Charles Place. If you pass Go, collect $200", "Advance to the nearest Railroad. If unowned, you may buy it from the Bank. If owned, pay owner twice the rental to which they are otherwise entitled", "Advance to the nearest Railroad. If unowned, you may buy it from the Bank. If owned, pay owner twice the rental to which they are otherwise entitled", "Advance token to nearest Utility. If unowned, you may buy it from the Bank. If owned, throw dice and pay owner a total ten times amount thrown", "Bank pays you dividend of $50", "Get Out of Jail Free", "Go Back 3 Spaces", "Go to Jail. Go directly to Jail, do not pass Go, do not collect $200", "Make general repairs on all your property. For each house pay $25. For each hotel pay $100", "Speeding fine $15", "Take a trip to Reading Railroad. If you pass Go, collect $200", "You have been elected Chairman of the Board. Pay each player $50", "Your building loan matures. Collect $150") # the card information

Chance <- data.frame(Chance_Cards, Card_Numbers) #a dataframe associating each chance card with a number



Community_Chest_Cards <- c("Advance to Go (Collect $200)", "Bank error in your favor. Collect $200", "Doctor’s fee. Pay $50", "From sale of stock you get $50", "Get Out of Jail Free", "Go to Jail. Go directly to jail, do not pass Go, do not collect $200", "Holiday fund matures. Receive $100", "Income tax refund. Collect $20", "It is your birthday. Collect $10 from every player", "Life insurance matures. Collect $100", "Pay hospital fees of $100", "Pay school fees of $50", "Receive $25 consultancy fee", "You are assessed for street repair. $40 per house. $115 per hotel", "You have won second prize in a beauty contest. Collect $10", "You inherit $100") # the card information

Community_Chest <- data.frame(Community_Chest_Cards, Card_Numbers) #a dataframe associating each community chest card with a number


##drawing community chest and chance cards
#A function that randomly selects a numeric value 1:16 representing a Chance card and updates the player's location based on the card information
#input: "location"; a numeric value 1:40 representing the player's location on the board
#output: the player's new location on the board

Drawing_Chance = function(location) {
  location = location #the player's current location
  draw_card <- sample(1:16, 1, replace = TRUE) #randomly selecting a card from 1:16
  player_card <- Chance$Chance_Cards[draw_card] #finding the card in the Chance deck
  print(player_card)
  
  #The following if statements update the player's location based on the card that was drawn (if applicable)  
  if(draw_card == 1) {
    location = 1
  }
  
  if(draw_card == 2) {
    location = 1
  }
  
  if(draw_card == 3) {
    location = 25
  }
  
  if(draw_card == 4){
    location = 12
  }
  
  if(draw_card == 5){ #takes into account which space the player is nearest to to determine updated location
    if(location >= 37 && location <= 5){
      location = 6
    } else {
      if (location >= 6 && location <= 15){
        location = 16
      } else {
        if(location >= 16 && location <= 25){
          location = 26
        } else {
          location = 36
        }
      }
    }
  }
  
  if(draw_card == 6){ #takes into account which space the player is nearest to to determine updated location
    if(location >= 37 && location <= 5){
      location = 6
    } else {
      if (location >= 6 && location <= 15){
        location = 16
      } else {
        if(location >= 16 && location <= 25){
          location = 26
        } else {
          location = 36
        }
      }
    }
  }
  
  if(draw_card == 7){ #takes into account which space the player is nearest to to determine updated location
    if(location >= 29 && location <= 12){
      location = 13
    } else {
      location = 29
    }
  }
  
  if(draw_card == 11) {
    location = 11
  }
  
  if(draw_card == 14) {
    location = 6
  }
  
  if(draw_card == 10) {
    location = location - 3
  }
  
  location = location
  return(location)
}


#A function that randomly selects a numeric value 1:16 representing a Community Chest card and updates the player's location based on the card information
#input: "location"; a numeric value 1:40 representing the player's location on the board
#output: the player's new location on the board

Drawing_Community_Chest = function(location) {
  location = location #the player's current location
  draw_card_chest <- sample(1:16, 1, replace = TRUE) #randomly selecting a card from 1:16
  player_card_chest <- Community_Chest$Community_Chest_Cards[draw_card_chest] #finding the card in the Community Chest deck
  print(player_card_chest)
  
  #The following if statements update the player's location based on the card that was drawn (if applicable) 
  if (draw_card_chest == 1) {
    location = 1
  } else {
    if (draw_card_chest == 6) {
      location = 11
    } else {
      location = location
    }
    
  }
  
  location = location 
  return(location)
}


#A function that expands on lets_move2 to add the actions of Community Chest and Chance spaces and their respective cards
#input: "location"; a numeric value 1 to 40; where the player in on the board
#output: the players new location on the board

lets_move3 = function(location) {
  dice_roll_1 <- sample(1:6, 1, replace = TRUE) 
  dice_roll_2 <- sample(1:6, 1, replace = TRUE) 
  
  roll_total <- sum(dice_roll_1, dice_roll_2)
  roll_double <- dice_roll_1 == dice_roll_2
  
  location = location + roll_total
  double3 = "false"
  
  
  if (location <= 40) {
    location = location
  } else {
    location = location - 40
  }
  
  
  if(location == 8 || location == 23 || location == 27) { #check's if the player's location is a Chance space
    location = Drawing_Chance(location) #if TRUE, the player draws a Chance card using the Drawing_Chance function which updates their location
  }
  
  if(location == 3 || location == 18 || location == 34) { #check's if the player's location is a Community Chest space
    location = Drawing_Community_Chest(location) #if TRUE, the player draws a Community Chest card using the Drawing_Community_Chest which updates their location
  }
  
  # first double roll -> roll again
  if (roll_double == TRUE) {
    dice_roll_3 <- sample(1:6, 1, replace = TRUE)
    dice_roll_4 <- sample(1:6, 1, replace = TRUE)
    
    roll_total2 <- sum(dice_roll_3, dice_roll_4)
    roll_double2 <- dice_roll_3 == dice_roll_4
    
    location = location + roll_total2
    double3 = "false"
    
    if (location <= 40) {
      location = location
    } else {
      location = location - 40
    }
    
    
    if(location == 8 || location == 23 || location == 27) { #check's if the player's location is a Chance space
      location = Drawing_Chance(location) #if TRUE, the player draws a Chance card using the Drawing_Chance function which updates their location
    }
    
    if(location == 3 || location == 18 || location == 34) { #check's if the player's location is a Community Chest space
      location = Drawing_Community_Chest(location) #if TRUE, the player draws a Community Chest card using the Drawing_Community_Chest which updates their location
    }
    
    # second double roll -> roll again
    if (roll_double2 == TRUE) {
      dice_roll_5 <- sample(1:6, 1, replace = TRUE)
      dice_roll_6 <- sample(1:6, 1, replace = TRUE)
      
      roll_total3 <- sum(dice_roll_5, dice_roll_6)
      roll_double3 <- dice_roll_5 == dice_roll_6
      
      location = location + roll_total3
      double3 = "false"
      
      if (location <= 40) {
        location = location
      } else {
        location = location - 40
      }
      
      if(location == 8 || location == 23 || location == 27) { #check's if the player's location is a Chance space
        location = Drawing_Chance(location) #if TRUE, the player draws a Chance card using the Drawing_Chance function which updates their location
      }
      
      if(location == 3 || location == 18 || location == 34) { #check's if the player's location is a Community Chest space
        location = Drawing_Community_Chest(location) #if TRUE, the player draws a Community Chest card using the Drawing_Community_Chest which updates their location
        
      }
      
      # third double roll -> go to jail
      if (roll_double3 == TRUE) {
        location = monopolyBoard$spaces[monopolyBoard$name == "Jail"]
        double3 = "true"
        
      }
    }
  }  
  
  location = location 
  
  listdata <- (list(location, double3))
  return(listdata)
  
}

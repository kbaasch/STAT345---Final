## libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

##the board

monopolyBoard <- read.csv("MonopolyBoard.csv", header = T)
monopolyBoard[is.na(monopolyBoard)] <- 0
monopolyBoard


############################################ lets_move ##############################################

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


############################################### lets_play ##########################################

#A function to play Monopoly!
#Utilizes lets_move to move the players around the board
#input: "turns"; the amount of turns each player will take (i.e. the number of rounds in the game), "players"; a numeric value 1-4 representing the number of players in the game
#output: a dataframe showing each players' location after each of their turns; a density plot showing each players' location data throughout the game; a density plot showing the overall location data throughout the game

lets_play = function(turns, players) {
  
  location_data <- as.data.frame(matrix(nrow = turns, ncol = players)) # data frame that will keep data for locations of players for each turn
  
  # puts players in order
  colnames(location_data) <- who_goes_first2(players)[[2]]
  
  location = 1 #initializes the player at space 1 (i.e. GO!)
  location_data[1,] <- location
  
  for (i in 2:turns) { # iterates through number of turns
    for (j in 1:players) { # rotates through each player
      location = lets_move(location_data[i-1,j])[[1]] # takes location from lets_move2 function and assigns it to location
      location_data[i,j] <- as.integer(location) # puts location value into the data frame of locations for each player
    }
  }
  
  #print(location_data) 
  
  ########################## property plot ###############################
  properties_landed <- as.vector(as.matrix(location_data))
  
  properties_landed <- properties_landed[properties_landed == 2 | properties_landed == 4 | properties_landed == 6 | properties_landed == 7 | properties_landed == 9 | properties_landed == 10 | properties_landed == 12 | properties_landed == 13 | properties_landed == 14 | properties_landed == 15 | properties_landed == 16 | properties_landed == 17 | properties_landed == 19 | properties_landed == 20 | properties_landed == 22 | properties_landed == 24 | properties_landed == 25 | properties_landed == 26 | properties_landed == 27 | properties_landed == 28 | properties_landed == 29 | properties_landed == 30 | properties_landed == 32 | properties_landed == 33 | properties_landed == 35 | properties_landed == 36 | properties_landed == 38 | properties_landed == 40]
  
  property_names <- c("Mediterranean", "Baltic", "Reading Railroad", "Oriental", "Vermot", "Conecticut", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Tennessee", "New York", "Kentucky", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Pacific", "N. Carolina", "Pennsylvania", "Short Line Railroad", "Park Place", "Boardwalk")
  
  properties_landed <- as.data.frame(properties_landed) 
  properties_landed <- as.data.frame(properties_landed)
  properties_landed <- properties_landed %>% group_by(properties_landed) %>% summarise(n())
  properties_landed <- as.data.frame(properties_landed)
  properties_landed <- cbind(property_names, properties_landed)
  names(properties_landed) <- c("Property", "Location", "Occurances")
  #print(properties_landed)
  
  colors_properties <- c("purple", "purple", "black", "lightblue", "lightblue", "lightblue", "violet", "lightgray", "violet", "violet", "black", "orange", "orange", "orange", "red", "red", "red", "black", "yellow", "yellow", "lightgray", "yellow", "darkgreen", "darkgreen", "darkgreen", "black", "blue", "blue")
  
  
  #creates a frequency plot for the number of times each property is landed on 
  property_plot <- ggplot(properties_landed, aes(x = Property, y = Occurances)) +
    geom_bar(stat = "identity", fill = colors_properties, color = "black") +
    scale_x_discrete(limits = properties_landed$Property) +
    ggtitle("Frequency of Landing on each Property") +
    xlab("Property") +
    ylab("Frequency") +
    theme(axis.text.x.bottom = element_text(angle=90, vjust = 0.001)) +
    scale_color_manual(name = "Properties",
                       breaks = c("Mediterranean", "Baltic", "Reading Railroad", "Oriental", "Vermot", "Conecticut", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Tennessee", "New York", "Kentucky", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Pacific", "N. Carolina", "Pennsylvania", "Short Line Railroad", "Park Place", "Boardwalk"),
                       values = colors_properties) 
  
  print(property_plot)
  
  
  ########################## location frequency plot ###############################
  locations_landed <- as.vector(as.matrix(location_data))
  
  location_names <- c("GO", "Mediterranean", "Community Chest 1", "Baltic ", "Income Tax", "Reading Railroad", "Oriental","Chance 1", "Vermot", "Conecticut", "Jail", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Community Chest 2", "Tennessee", "New York","Free Parking", "Kentucky", "Chance 2", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Go To Jail", "Pacific", "N. Carolina", "Community Chest 3", "Pennsylvania", "Short Line Railroad", "Chance 3", "Park Place", "Luxury Tax", "Boardwalk")
  
  locations_landed <- as.data.frame(locations_landed) 
  locations_landed <- as.data.frame(locations_landed)
  locations_landed <- locations_landed %>% group_by(locations_landed) %>% summarise(n())
  locations_landed <- as.data.frame(locations_landed)
  locations_landed <- cbind(location_names, locations_landed)
  names(locations_landed) <- c("Name", "Location", "Occurances")
  #print(locations_landed)
  
  colors_locations <- c("white","purple","navy", "purple","white", "black", "lightblue","navy", "lightblue", "lightblue", "white", "violet", "lightgray", "violet", "violet", "black", "orange","navy", "orange", "orange","white", "red","navy", "red", "red", "black", "yellow", "yellow", "lightgray", "yellow","white", "darkgreen", "darkgreen","navy", "darkgreen", "black","navy", "blue","white", "blue")
  
  
  #creates a frequency plot for the number of times each property is landed on 
  location_plot <- ggplot(locations_landed, aes(x = Name, y = Occurances)) +
    geom_bar(stat = "identity", fill = colors_locations, color = "black") +
    scale_x_discrete(limits = locations_landed$Name) +
    ggtitle("Frequency of Landing on each Location") +
    xlab("Location") +
    ylab("Frequency") +
    theme(axis.text.x.bottom = element_text(angle=90, vjust = 0.001)) +
    scale_color_manual(name = "Properties",
                       breaks = c("GO", "Mediterranean", "Community Chest 1", "Baltic ", "Income Tax", "Reading Railroad", "Oriental","Chance 1", "Vermot", "Conecticut", "Jail", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Community Chest 2", "Tennessee", "New York","Free Parking", "Kentucky", "Chance 2", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Go To Jail", "Pacific", "N. Carolina", "Community Chest 3", "Pennsylvania", "Short Line Railroad", "Chance 3", "Park Place", "Luxury Tax", "Boardwalk"),
                       values = colors_locations) 
  
  print(location_plot)  
  
  
  ########################## location plots ###############################
  
  #vector of colors
  colors <- c("red", "blue", "green","purple")
  
  # creates density plot of frequency of landing on each space for each player
  plot(density(location_data[,1]), xlim = c(0,40), col = "black", xlab="Spaces", main= "Density of Landing Locations for All Players")
  
  for (k in 2:players) {
    lines(density(location_data[,k]), col = colors[k-1]) # adds a density line for each player after the first
  }
  
  # creates density plot for total frequency landing on each space (all players spaces included)
  all_spaces <- as.vector(as.matrix(location_data))
  # plot(density(all_spaces), xlim = c(0,40), col = "black", xlab="Spaces", main="Density of Landing Locations Overall")
  
  #creates frequency plot for frequency of landing on certain spaces for each player
  #for(k in 1:players){
  # plot(location_data[,k], xlim= c(0,40), col=colors[k], type="h", xlab="Spaces", ylab="Frequency", main="Frequency of Landing on Each Space for Each Player")
  # }
  
  plot(all_spaces, xlim = c(0,40), col = "blue", type="h", xlab = "Spaces", ylab = "Frequency", main = "Frequency of Landing Locations Overall")
  
  # second set of frequency plots for individual player locations
  # colors2 <- c("coral1", "steelblue1", "palegreen2","plum2")
  # freq_data <- data.frame(space = 1:40, freq = tabulate(location_data[, 1], nbins = 40))
  # density_plot <- ggplot(freq_data, aes(x = space, y = freq)) +
  #  geom_bar(stat = "identity", fill = colors2[1], color = "black") +
  #  ggtitle(paste0("Player Frequency of Landing on Each Space")) +
  #  xlab("Spaces") +
  #  ylab("Frequency") +
  #  theme_minimal()
  
  # print(density_plot)
}





## double_down <- lets_move2
#Double Down -- addition of jail and double rolls

############################################# lets_move2 ###########################################

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


############################################### lets_play2 ###########################################


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
  #print(location_data)
  #print(double3)
  
  
  ########################## property plot ###############################
  
  properties_landed <- as.vector(as.matrix(location_data))
  
  properties_landed <- properties_landed[properties_landed == 2 | properties_landed == 4 | properties_landed == 6 | properties_landed == 7 | properties_landed == 9 | properties_landed == 10 | properties_landed == 12 | properties_landed == 13 | properties_landed == 14 | properties_landed == 15 | properties_landed == 16 | properties_landed == 17 | properties_landed == 19 | properties_landed == 20 | properties_landed == 22 | properties_landed == 24 | properties_landed == 25 | properties_landed == 26 | properties_landed == 27 | properties_landed == 28 | properties_landed == 29 | properties_landed == 30 | properties_landed == 32 | properties_landed == 33 | properties_landed == 35 | properties_landed == 36 | properties_landed == 38 | properties_landed == 40]
  
  property_names <- c("Mediterranean", "Baltic", "Reading Railroad", "Oriental", "Vermot", "Conecticut", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Tennessee", "New York", "Kentucky", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Pacific", "N. Carolina", "Pennsylvania", "Short Line Railroad", "Park Place", "Boardwalk")
  
  properties_landed <- as.data.frame(properties_landed) 
  properties_landed <- as.data.frame(properties_landed)
  properties_landed <- properties_landed %>% group_by(properties_landed) %>% summarise(n())
  properties_landed <- as.data.frame(properties_landed)
  properties_landed <- cbind(property_names, properties_landed)
  names(properties_landed) <- c("Property", "Location", "Occurances")
  #print(properties_landed)
  
  colors_properties <- c("purple", "purple", "black", "lightblue", "lightblue", "lightblue", "violet", "lightgray", "violet", "violet", "black", "orange", "orange", "orange", "red", "red", "red", "black", "yellow", "yellow", "lightgray", "yellow", "darkgreen", "darkgreen", "darkgreen", "black", "blue", "blue")
  
  
  #creates a frequency plot for the number of times each property is landed on 
  property_plot <- ggplot(properties_landed, aes(x = Property, y = Occurances)) +
    geom_bar(stat = "identity", fill = colors_properties, color = "black") +
    scale_x_discrete(limits = properties_landed$Property) +
    ggtitle("Frequency of Landing on each Property") +
    xlab("Property") +
    ylab("Frequency") +
    theme(axis.text.x.bottom = element_text(angle=90, vjust = 0.001)) +
    scale_color_manual(name = "Properties",
                       breaks = c("Mediterranean", "Baltic", "Reading Railroad", "Oriental", "Vermot", "Conecticut", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Tennessee", "New York", "Kentucky", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Pacific", "N. Carolina", "Pennsylvania", "Short Line Railroad", "Park Place", "Boardwalk"),
                       values = colors_properties)
  
  print(property_plot)
  
  
  ########################## location frequency plot ###############################
  locations_landed <- as.vector(as.matrix(location_data))
  
  location_names <- c("GO", "Mediterranean", "Community Chest 1", "Baltic ", "Income Tax", "Reading Railroad", "Oriental","Chance 1", "Vermot", "Conecticut", "Jail", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Community Chest 2", "Tennessee", "New York","Free Parking", "Kentucky", "Chance 2", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Go To Jail", "Pacific", "N. Carolina", "Community Chest 3", "Pennsylvania", "Short Line Railroad", "Chance 3", "Park Place", "Luxury Tax", "Boardwalk")
  
  locations_landed <- as.data.frame(locations_landed) 
  locations_landed <- as.data.frame(locations_landed)
  locations_landed <- locations_landed %>% group_by(locations_landed) %>% summarise(n())
  locations_landed <- as.data.frame(locations_landed)
  locations_landed <- cbind(location_names, locations_landed)
  names(locations_landed) <- c("Name", "Location", "Occurances")
  #print(locations_landed)
  
  colors_locations <- c("white","purple","navy", "purple","white", "black", "lightblue","navy", "lightblue", "lightblue", "white", "violet", "lightgray", "violet", "violet", "black", "orange","navy", "orange", "orange","white", "red","navy", "red", "red", "black", "yellow", "yellow", "lightgray", "yellow","white", "darkgreen", "darkgreen","navy", "darkgreen", "black","navy", "blue","white", "blue")
  
  
  #creates a frequency plot for the number of times each property is landed on 
  location_plot <- ggplot(locations_landed, aes(x = Name, y = Occurances)) +
    geom_bar(stat = "identity", fill = colors_locations, color = "black") +
    scale_x_discrete(limits = locations_landed$Name) +
    ggtitle("Frequency of Landing on each Location") +
    xlab("Location") +
    ylab("Frequency") +
    theme(axis.text.x.bottom = element_text(angle=90, vjust = 0.001)) +
    scale_color_manual(name = "Properties",
                       breaks = c("GO", "Mediterranean", "Community Chest 1", "Baltic ", "Income Tax", "Reading Railroad", "Oriental","Chance 1", "Vermot", "Conecticut", "Jail", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Community Chest 2", "Tennessee", "New York","Free Parking", "Kentucky", "Chance 2", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Go To Jail", "Pacific", "N. Carolina", "Community Chest 3", "Pennsylvania", "Short Line Railroad", "Chance 3", "Park Place", "Luxury Tax", "Boardwalk"),
                       values = colors_locations) 
  
  print(location_plot)  
  
  
  ########################## location plots ###############################
  
  #vector of colors
  colors <- c("red", "blue", "green","purple")
  
  # creates density plot of frequency of landing on each space for each player
  plot(density(location_data[,1]), xlim = c(0,40), col = "black", xlab="Spaces", main= "Density of Landing Locations for All Players")
  
  for (k in 2:players) {
    lines(density(location_data[,k]), col = colors[k-1]) # adds a density line for each player after the first
  }
  
  # creates density plot for total frequency landing on each space (all players spaces included)
  all_spaces <- as.vector(as.matrix(location_data))
  #plot(density(all_spaces), xlim = c(0,40), col = "black", xlab="Spaces", main="Density of Landing Locations Overall")
  
  #creates frequency plot for frequency of landing on certain spaces for each player
  # for(k in 1:players){
  # plot(location_data[,k], xlim= c(0,40), col=colors[k], type="h", xlab="Spaces", ylab="Frequency", main="Frequency of Landing on Each Space for Each Player")
  # }
  
  plot(all_spaces, xlim = c(0,40), col = "blue", type="h", xlab = "Spaces", ylab = "Frequency", main = "Frequency of Landing Locations Overall")
  
  # second set of frequency plots for individual player locations
  #for (k in 1:players) {
  #  colors <- c("coral1", "steelblue1", "palegreen2","plum2")
  #  freq_data <- data.frame(space = 1:40, freq = tabulate(location_data[, k], nbins = 40))
  #  density_plot <- ggplot(freq_data, aes(x = space, y = freq)) +
  #   geom_bar(stat = "identity", fill = colors[k], color = "black") +
  #   ggtitle(paste0("Player ", k, ": Frequency of Landing on Each Space")) +
  #   xlab("Spaces") +
  #   ylab("Frequency") +
  #   theme_minimal()
  
  #  print(density_plot)
  #}
  
}

#################################### community chest and chance cards #############################################

Card_Numbers <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) # the card numbers

Chance_Cards <- c("Advance to Boardwalk", "Advance to Go (Collect $200)", "Advance to Illinois Avenue. If you pass Go, collect $200", "Advance to St. Charles Place. If you pass Go, collect $200", "Advance to the nearest Railroad. If unowned, you may buy it from the Bank. If owned, pay owner twice the rental to which they are otherwise entitled", "Advance to the nearest Railroad. If unowned, you may buy it from the Bank. If owned, pay owner twice the rental to which they are otherwise entitled", "Advance token to nearest Utility. If unowned, you may buy it from the Bank. If owned, throw dice and pay owner a total ten times amount thrown", "Bank pays you dividend of $50", "Get Out of Jail Free", "Go Back 3 Spaces", "Go to Jail. Go directly to Jail, do not pass Go, do not collect $200", "Make general repairs on all your property. For each house pay $25. For each hotel pay $100", "Speeding fine $15", "Take a trip to Reading Railroad. If you pass Go, collect $200", "You have been elected Chairman of the Board. Pay each player $50", "Your building loan matures. Collect $150") # the card information

Chance <- data.frame(Chance_Cards, Card_Numbers) #a dataframe associating each chance card with a number



Community_Chest_Cards <- c("Advance to Go (Collect $200)", "Bank error in your favor. Collect $200", "Doctor’s fee. Pay $50", "From sale of stock you get $50", "Get Out of Jail Free", "Go to Jail. Go directly to jail, do not pass Go, do not collect $200", "Holiday fund matures. Receive $100", "Income tax refund. Collect $20", "It is your birthday. Collect $10 from every player", "Life insurance matures. Collect $100", "Pay hospital fees of $100", "Pay school fees of $50", "Receive $25 consultancy fee", "You are assessed for street repair. $40 per house. $115 per hotel", "You have won second prize in a beauty contest. Collect $10", "You inherit $100") # the card information

Community_Chest <- data.frame(Community_Chest_Cards, Card_Numbers) #a dataframe associating each community chest card with a number


##drawing community chest and chance cards
#A function that randomly selects a numeric value 1:16 representing a Chance card and updates the player's location based on the card information
#input: "location"; a numeric value 1:40 representing the player's location on the board
#output: the player's new location on the board

########################################### Drawing chance/cc functions ###########################################

Drawing_Chance = function(location) {
  location = location #the player's current location
  amount <- 0 #amount is later used to updated the player's bank in lets_play4
  amount2 <- 0 #amount2 is later used to update free parking in lets_play5
  draw_card <- sample(1:16, 1, replace = TRUE) #randomly selecting a card from 1:16 cards
  player_card <- Chance$Chance_Cards[draw_card] #finding the card in the Chance deck
  
  #The following if statements update the player's location based on the card that was drawn (if applicable)  
  if(draw_card == 1) { # Advance to Boardwalk
    location = 1
  }
  
  if(draw_card == 2) { # Advance to Go (Collect $200)
    location = 1
    amount = 200
  }
  
  if(draw_card == 3) { #Advance to Illinois Avenue. If you pass Go, collect $200
    if(location > 25){
      amount = 200
    }else{
      amount = amount
    }
    location = 25
  }
  
  if(draw_card == 4){ # Advance to St. Charles Place. If you pass Go, collect $200
    if(location > 13 ){
      amount = 200
    }else{
      amount = amount
    }
    location = 12
  }
  
  if(draw_card == 5){ # Advance to the nearest Railroad.
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
  
  if(draw_card == 6){ # Advance to the nearest Railroad.
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
  
  if(draw_card == 7){ #	Advance token to nearest Utility.
    if(location >= 29 && location <= 12){
      location = 13
    } else {
      location = 29
    }
  }
  
  if(draw_card == 8){ #Bank pays you dividend of $50
    amount = 50
  }
  
  if(draw_card == 10) { #Go Back 3 Spaces
    location = location - 3
  }
  
  if(draw_card == 11) { #Go to Jail. Go directly to Jail
    location = 11
  }
  
  if(draw_card == 13){ #Speeding fine $15
    amount = -15
    amount2 = 15
  }
  
  if(draw_card == 14) { #Take a trip to Reading Railroad.
    if(location > 6){
      amount = 200
    }else{
      amount = amount
    }
    location = 6
  }
  
  if(draw_card == 16){ #Your building loan matures.
    amount = 150
  }
  
  
  location = location
  amount = amount
  amount2 = amount2
  list_location_amount <- list(location, amount, amount2)
  return(list_location_amount)
}



#A function that randomly selects a numeric value 1:16 representing a Community Chest card and updates the player's location based on the card information
#input: "location"; a numeric value 1:40 representing the player's location on the board
#output: the player's new location on the board

Drawing_Community_Chest = function(location) {
  location = location
  amount <- 0
  amount2 <- 0
  draw_card_chest <- sample(1:16, 1, replace = TRUE)
  player_card_chest <- Community_Chest$Community_Chest_Cards[draw_card_chest]
  
  
  if (draw_card_chest == 1) { # Advance to Go (Collect $200)
    location = 1
    amount = 200
  }
  
  #Bank error in your favor. Collect $200
  if(draw_card_chest == 2){
    amount = 200
  }
  
  #Doctor’s fee. Pay $50
  if(draw_card_chest == 3){
    amount = -50
    amount2 = 50
  }
  
  #From sale of stock you get $50
  if(draw_card_chest == 4){
    amount = 50
  }
  
  if(draw_card_chest == 6) { # Go to Jail
    location = 11
  }
  
  #Holiday fund matures. Receive $100
  if(draw_card_chest == 7){
    amount = 100
  }
  
  #Income tax refund. Collect $20
  if(draw_card_chest == 8){
    amount = 20
  }
  
  #Life insurance matures. Collect $100
  if(draw_card_chest == 10){
    amount = 100
  }
  
  #Pay hospital fees of $100
  if(draw_card_chest == 11){
    amount = -100
    amount2 = 100
  }
  
  #Pay school fees of $50
  if(draw_card_chest == 12){
    amount = -50
    amount2 = 50
  }
  
  #Receive $25 consultancy fee
  if(draw_card_chest == 13){
    amount = 25
  }
  
  #You have won second prize in a beauty contest. Collect $10
  if(draw_card_chest == 15){
    amount = 10
  }
  
  #You inherit $100
  if(draw_card_chest == 16){
    amount = 100
  }
  
  location = location 
  amount = amount
  amount2 = amount2
  list_location_amount <- list(location,amount,amount2)
  return(list_location_amount)
}

#################################################### lets_move3 #################################################

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

################################################### lets_play3 ##################################################

lets_play3 = function(turns, players) {
  
  location_data <- as.data.frame(matrix(nrow = turns, ncol = players))
  double3data <- as.data.frame(matrix(nrow = turns, ncol = players))
  
  colnames(location_data) <- who_goes_first2(players)[[2]]
  
  location = 1
  location_data[1,] <- location
  double3 = 0
  
  for (i in 2:turns) {
    
    for (j in 1:players) {
      
      location = lets_move3(location_data[i-1,j])[[1]]
      location_data[i,j] <- as.integer(location)
      double3data[i,j] <- lets_move3(double3)[[2]]
      
      # deals with landing in jail from "go to jail" space and staying there for next 3 turns
      if (31 %in% location_data[i-1,j]) { # if you land on 31 (go to jail), next location is in jail (space 11)
        location <- 11
        location_data[i,j] <- as.integer(location)
      } else if (11 %in% location_data[i-1,j] & 31 %in% location_data[i-2,j]) {
        location <- 11
        location_data[i,j] <- as.integer(location)
      } else if (11 %in% location_data[i-2,j] & 31 %in% location_data[i-3,j]) {
        location <- 11
        location_data[i,j] <- as.integer(location)
      }
      
      # landing in jail from 3x double roll and staying there for next 3 turns
      if (11 %in% location_data[i,j] & "true" %in% double3data[i-1,j]) { # "true" means player double rolled 3x -> jail
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
  #print(location_data)
  #print(double3data)
  
  
  
  ########################## property plot ###############################
  properties_landed <- as.vector(as.matrix(location_data))
  
  properties_landed <- properties_landed[properties_landed == 2 | properties_landed == 4 | properties_landed == 6 | properties_landed == 7 | properties_landed == 9 | properties_landed == 10 | properties_landed == 12 | properties_landed == 13 | properties_landed == 14 | properties_landed == 15 | properties_landed == 16 | properties_landed == 17 | properties_landed == 19 | properties_landed == 20 | properties_landed == 22 | properties_landed == 24 | properties_landed == 25 | properties_landed == 26 | properties_landed == 27 | properties_landed == 28 | properties_landed == 29 | properties_landed == 30 | properties_landed == 32 | properties_landed == 33 | properties_landed == 35 | properties_landed == 36 | properties_landed == 38 | properties_landed == 40]
  
  property_names <- c("Mediterranean", "Baltic", "Reading Railroad", "Oriental", "Vermot", "Conecticut", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Tennessee", "New York", "Kentucky", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Pacific", "N. Carolina", "Pennsylvania", "Short Line Railroad", "Park Place", "Boardwalk")
  
  properties_landed <- as.data.frame(properties_landed) 
  properties_landed <- as.data.frame(properties_landed)
  properties_landed <- properties_landed %>% group_by(properties_landed) %>% summarise(n())
  properties_landed <- as.data.frame(properties_landed)
  properties_landed <- cbind(property_names, properties_landed)
  names(properties_landed) <- c("Property", "Location", "Occurances")
  #print(properties_landed)
  
  colors_properties <- c("purple", "purple", "black", "lightblue", "lightblue", "lightblue", "violet", "lightgray", "violet", "violet", "black", "orange", "orange", "orange", "red", "red", "red", "black", "yellow", "yellow", "lightgray", "yellow", "darkgreen", "darkgreen", "darkgreen", "black", "blue", "blue")
  
  #creates a frequency plot for the number of times each property is landed on 
  property_plot <- ggplot(properties_landed, aes(x = Property, y = Occurances)) +
    geom_bar(stat = "identity", fill = colors_properties, color = "black") +
    scale_x_discrete(limits = properties_landed$Property) +
    ggtitle("Frequency of Landing on each Property") +
    xlab("Property") +
    ylab("Frequency") +
    theme(axis.text.x.bottom = element_text(angle=90, vjust = 0.001)) +
    scale_color_manual(name = "Properties",
                       breaks = c("Mediterranean", "Baltic", "Reading Railroad", "Oriental", "Vermot", "Conecticut", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Tennessee", "New York", "Kentucky", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Pacific", "N. Carolina", "Pennsylvania", "Short Line Railroad", "Park Place", "Boardwalk"),
                       values = colors_properties)
  
  print(property_plot)
  
  
  
  ########################## location frequency plot ###############################
  locations_landed <- as.vector(as.matrix(location_data))
  
  location_names <- c("GO", "Mediterranean", "Community Chest 1", "Baltic ", "Income Tax", "Reading Railroad", "Oriental","Chance 1", "Vermot", "Conecticut", "Jail", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Community Chest 2", "Tennessee", "New York","Free Parking", "Kentucky", "Chance 2", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Go To Jail", "Pacific", "N. Carolina", "Community Chest 3", "Pennsylvania", "Short Line Railroad", "Chance 3", "Park Place", "Luxury Tax", "Boardwalk")
  
  locations_landed <- as.data.frame(locations_landed) 
  locations_landed <- as.data.frame(locations_landed)
  locations_landed <- locations_landed %>% group_by(locations_landed) %>% summarise(n())
  locations_landed <- as.data.frame(locations_landed)
  locations_landed <- cbind(location_names, locations_landed)
  names(locations_landed) <- c("Name", "Location", "Occurances")
  #print(locations_landed)
  
  colors_locations <- c("white","purple","navy", "purple","white", "black", "lightblue","navy", "lightblue", "lightblue", "white", "violet", "lightgray", "violet", "violet", "black", "orange","navy", "orange", "orange","white", "red","navy", "red", "red", "black", "yellow", "yellow", "lightgray", "yellow","white", "darkgreen", "darkgreen","navy", "darkgreen", "black","navy", "blue","white", "blue")
  
  
  #creates a frequency plot for the number of times each property is landed on 
  location_plot <- ggplot(locations_landed, aes(x = Name, y = Occurances)) +
    geom_bar(stat = "identity", fill = colors_locations, color = "black") +
    scale_x_discrete(limits = locations_landed$Name) +
    ggtitle("Frequency of Landing on each Location") +
    xlab("Location") +
    ylab("Frequency") +
    theme(axis.text.x.bottom = element_text(angle=90, vjust = 0.001)) +
    scale_color_manual(name = "Properties",
                       breaks = c("GO", "Mediterranean", "Community Chest 1", "Baltic ", "Income Tax", "Reading Railroad", "Oriental","Chance 1", "Vermot", "Conecticut", "Jail", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Community Chest 2", "Tennessee", "New York","Free Parking", "Kentucky", "Chance 2", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Go To Jail", "Pacific", "N. Carolina", "Community Chest 3", "Pennsylvania", "Short Line Railroad", "Chance 3", "Park Place", "Luxury Tax", "Boardwalk"),
                       values = colors_locations) 
  
  print(location_plot)  
  
  
  
  ########################## location plots ###############################    
  
  # creates density plot of frequency of landing on each space for each player
  colors <- c("red", "blue", "green","purple")
  
  plot(density(location_data[,1]), xlim = c(0,40), col = "black", xlab="Spaces", main= "Density of Landing Locations for All Players")
  
  for (k in 2:players) {
    lines(density(location_data[,k]), col = colors[k-1]) # adds a density line for each player after the first
  }
  
  # creates density plot for total frequency landing on each space (all players spaces included)
  all_spaces <- as.vector(as.matrix(location_data))
  #plot(density(all_spaces), xlim = c(0,40), col = "blue", xlab="Spaces", main="Density of Landing Locations Overall")
  
  # for(k in 1:players){
  #plot(location_data[,k], xlim= c(0,40), col=colors[k], type="h", xlab="Spaces", ylab="Frequency", main="Frequency of Landing on Each Space for Each Player")
  #  }
  
  plot(all_spaces, xlim = c(0,40), col = "blue", type="h", xlab = "Spaces", ylab = "Frequency", main = "Frequency of Landing Locations Overall")
  
  # second set of frequency plots for individual player locations
  #for (k in 1:players) {
  #  colors <- c("coral1", "steelblue1", "palegreen2","plum2")
  #  freq_data <- data.frame(space = 1:40, freq = tabulate(location_data[, k], nbins = 40))
  #  density_plot <- ggplot(freq_data, aes(x = space, y = freq)) +
  #   geom_bar(stat = "identity", fill = colors[k], color = "black") +
  #   ggtitle(paste0("Player ", k, ": Frequency of Landing on Each Space")) +
  #   xlab("Spaces") +
  #   ylab("Frequency") +
  #   theme_minimal()
  
  #  print(density_plot)
  #}
  
}

############################################### lets_move4 ######################################################

lets_move4 = function(location) {
  dice_roll_1 <- sample(1:6, 1, replace = TRUE) 
  dice_roll_2 <- sample(1:6, 1, replace = TRUE) 
  
  roll_total <- sum(dice_roll_1, dice_roll_2)
  roll_double <- dice_roll_1 == dice_roll_2
  
  location = location + roll_total
  double3 = "false"
  
  
  if (location <= 40) {
    location = location
    amount = 0
  } else {
    location = location - 40
    amount = 200
  }
  
  
  if(location == 8 || location == 23 || location == 27) { #check's if the player's location is a Chance space
    location = Drawing_Chance(location)[[1]] #if TRUE, the player draws a Chance card using the Drawing_Chance function which updates their location
  }
  
  if(location == 3 || location == 18 || location == 34) { #check's if the player's location is a Community Chest space
    location = Drawing_Community_Chest(location)[[1]] #if TRUE, the player draws a Community Chest card using the Drawing_Community_Chest which updates their location
  }
  
  # first double roll -> roll again
  if (roll_double == TRUE) {
    dice_roll_3 <- sample(1:6, 1, replace = TRUE)
    dice_roll_4 <- sample(1:6, 1, replace = TRUE)
    
    roll_total <- sum(dice_roll_3, dice_roll_4)
    roll_double2 <- dice_roll_3 == dice_roll_4
    
    location = location + roll_total
    double3 = "false"
    
    if (location <= 40) {
      location = location
      amount = 0
    } else {
      location = location - 40
      amount = 200
    }
    
    
    if(location == 8 || location == 23 || location == 27) { #check's if the player's location is a Chance space
      location = Drawing_Chance(location)[[1]] #if TRUE, the player draws a Chance card using the Drawing_Chance function which updates their location
    }
    
    if(location == 3 || location == 18 || location == 34) { #check's if the player's location is a Community Chest space
      location = Drawing_Community_Chest(location)[[1]] #if TRUE, the player draws a Community Chest card using the Drawing_Community_Chest which updates their location
    }
    
    # second double roll -> roll again
    if (roll_double2 == TRUE) {
      dice_roll_5 <- sample(1:6, 1, replace = TRUE)
      dice_roll_6 <- sample(1:6, 1, replace = TRUE)
      
      roll_total <- sum(dice_roll_5, dice_roll_6)
      roll_double3 <- dice_roll_5 == dice_roll_6
      
      location = location + roll_total
      double3 = "false"
      
      if (location <= 40) {
        location = location
        amount = 0
      } else {
        location = location - 40
        amount = 200
      }
      
      if(location == 8 || location == 23 || location == 27) { #check's if the player's location is a Chance space
        location = Drawing_Chance(location)[[1]] #if TRUE, the player draws a Chance card using the Drawing_Chance function which updates their location
      }
      
      if(location == 3 || location == 18 || location == 34) { #check's if the player's location is a Community Chest space
        location = Drawing_Community_Chest(location)[[1]] #if TRUE, the player draws a Community Chest card using the Drawing_Community_Chest which updates their location
        
      }
      
      # third double roll -> go to jail
      if (roll_double3 == TRUE) {
        location = monopolyBoard$spaces[monopolyBoard$name == "Jail"]
        double3 = "true"
        
      }
      
    }
  }  
  
  location = location 
  
  listdata <- (list(location, double3, amount))
  return(listdata)
  
}

################################################## lets_play4 ##################################################


################################################## lets_play5 ##################################################

lets_play5 = function(turns, players) {
  
  #creates the player banks
  Players <- c("Player 1", "Player 2", "Player 3", "Player 4")
  Player_Bank <- c(1250, 1250, 1250, 1250)
  The_Players <- data.frame(Players, Player_Bank)
  
  #determines which players own which properties
  Property <- c("Mediterranean", "Baltic", "Reading Railroad", "Oriental", "Vermot", "Connecticut", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Tennessee", "New York", "Kentucky", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Pacific", "N. Carolina", "Pennsylvania", "Short Line Railroad", "Park Place", "Boardwalk")
  Owned_By_Player <- sample(1:4, 28, replace = TRUE)
  Space <- c(2, 4, 6, 7, 9, 10, 12, 13, 14, 15, 16, 17, 19, 20, 22, 24, 25, 26, 27, 28, 29, 30, 32, 33, 35, 36, 38, 40)
  Property_Statuses <- data.frame(Property, Space, Owned_By_Player)
  
  #creates the free parking bank
  free_parking <- c("Free Parking")
  bank <- c(0)
  Free_Parking <- data.frame(free_parking, bank)
  
  location_data <- as.data.frame(matrix(nrow = turns, ncol = players))
  double3data <- as.data.frame(matrix(nrow = turns, ncol = players))
  
  colnames(location_data) <- who_goes_first2(players)[[2]]
  
  location = 1
  location_data[1,] <- location
  double3 = 0
  
  for (i in 2:turns) {
    
    for (j in 1:players) {
      
      location = lets_move4(location_data[i-1,j])[[1]]
      location_data[i,j] <- as.integer(location)
      double3data[i,j] <- lets_move4(location)[[2]]
      
      # deals with landing in jail from "go to jail" space and staying there for next 3 turns
      if (31 %in% location_data[i-1,j]) { # if you land on 31 (go to jail), next location is in jail (space 11)
        location <- 11
        location_data[i,j] <- as.integer(location)
      } else if (11 %in% location_data[i-1,j] & 31 %in% location_data[i-2,j]) {
        location <- 11
        location_data[i,j] <- as.integer(location)
      } else if (11 %in% location_data[i-2,j] & 31 %in% location_data[i-3,j]) {
        location <- 11
        location_data[i,j] <- as.integer(location)
      }
      
      # landing in jail from 3x double roll and staying there for next 3 turns
      if (11 %in% location_data[i,j] & "true" %in% double3data[i-1,j]) { # "true" means player double rolled 3x -> jail
        location <- 11
        location_data[i,j] <- as.integer(location)
      } else if (11 %in% location_data[i-1,j] & "true" %in% double3data[i-2,j]) { # 
        location <- 11
        location_data[i,j] <- as.integer(location)
      } else if (11 %in% location_data[i-2,j] & "true" %in% double3data[i-3,j]) {
        location <- 11
        location_data[i,j] <- as.integer(location)
      }
      
      
      # updating banks if players land on each others' properties
      if(location %in% c(2, 4, 7, 9, 10, 12, 13, 14, 15, 17, 19, 20, 22, 24, 25, 27, 28, 29, 30, 32, 33, 35, 38, 40)) {
        
        location = location
        spot <- which(Property_Statuses$Space == location)
        cost <- which(monopolyBoard$spaces == location)
        owned_by_player <- Property_Statuses$Owned_By_Player[spot]
        
        The_Players$Player_Bank[owned_by_player] <- The_Players$Player_Bank[owned_by_player] + monopolyBoard$rent_3[cost]
        The_Players$Player_Bank[j] <- The_Players$Player_Bank[j] - monopolyBoard$rent_3[cost]
      }
      
      #updating banks if players land on others' railroads
      if(location %in% c(6, 16, 26, 36)){
        
        location = location
        spot <- which(Property_Statuses$Space == location)
        cost <- which(monopolyBoard$spaces == location)
        owned_by_player <- Property_Statuses$Owned_By_Player[spot]
        
        The_Players$Player_Bank[owned_by_player] <- The_Players$Player_Bank[owned_by_player] + 200
        The_Players$Player_Bank[j] <- The_Players$Player_Bank[j] - 200
      }
      
      # Updating player bank and free parking if they gained or lost money from a Chance card
      if (Drawing_Chance(location_data[i,j])[[2]] != 0) {
        The_Players$Player_Bank[j] <- The_Players$Player_Bank[j] + Drawing_Chance(location_data[i,j])[[2]]  
        Free_Parking$bank <- Free_Parking$bank + Drawing_Chance(location_data[i,j])[[3]]
      }
      
      # Updating player bank if they gained or lost money from a Community Chest card 
      if (Drawing_Community_Chest(location_data[i,j])[[2]] != 0) {
        The_Players$Player_Bank[j] <- The_Players$Player_Bank[j] + Drawing_Community_Chest(location_data[i,j])[[2]]  
        Free_Parking$bank <- Free_Parking$bank + Drawing_Community_Chest(location_data[i,j])[[3]]
      }
      
      # Player gets $200 if they pass go
      if (lets_move3(location_data[i,j])[[3]] != 0) {
        The_Players$Player_Bank[j] <- The_Players$Player_Bank[j] + lets_move3(location_data[i,j])[[3]]  
      }
      
      # Paying when player lands on a tax space
      if (5 %in% location_data[i,j] || 39 %in% location_data[i,j]) {
        The_Players$Player_Bank[j] <- The_Players$Player_Bank[j] - monopolyBoard$cost[location_data[i,j]] # subtracting cost of tax from player's bank
        Free_Parking$bank <- Free_Parking$bank + monopolyBoard$cost[location_data[i,j]]
      }
      
      #Free Parking payments
      if (21 %in% location_data[i,j]) {
        The_Players$Player_Bank[j] <- The_Players$Player_Bank[j] + Free_Parking$bank
        Free_Parking$bank <- 0
      }
      
    }
  }
  
  #print(location_data)
  #print(The_Players)
  #print(Free_Parking)
  
  
  
  
  ########################## property plot ###############################
  
  properties_landed <- as.vector(as.matrix(location_data))
  
  properties_landed <- properties_landed[properties_landed == 2 | properties_landed == 4 | properties_landed == 6 | properties_landed == 7 | properties_landed == 9 | properties_landed == 10 | properties_landed == 12 | properties_landed == 13 | properties_landed == 14 | properties_landed == 15 | properties_landed == 16 | properties_landed == 17 | properties_landed == 19 | properties_landed == 20 | properties_landed == 22 | properties_landed == 24 | properties_landed == 25 | properties_landed == 26 | properties_landed == 27 | properties_landed == 28 | properties_landed == 29 | properties_landed == 30 | properties_landed == 32 | properties_landed == 33 | properties_landed == 35 | properties_landed == 36 | properties_landed == 38 | properties_landed == 40]
  
  property_names <- c("Mediterranean", "Baltic", "Reading Railroad", "Oriental", "Vermot", "Conecticut", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Tennessee", "New York", "Kentucky", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Pacific", "N. Carolina", "Pennsylvania", "Short Line Railroad", "Park Place", "Boardwalk")
  
  properties_landed <- as.data.frame(properties_landed) 
  properties_landed <- as.data.frame(properties_landed)
  properties_landed <- properties_landed %>% group_by(properties_landed) %>% summarise(n())
  properties_landed <- as.data.frame(properties_landed)
  properties_landed <- cbind(property_names, properties_landed)
  names(properties_landed) <- c("Property", "Location", "Occurances")
  #print(properties_landed)
  
  colors_properties <- c("purple", "purple", "black", "lightblue", "lightblue", "lightblue", "violet", "lightgray", "violet", "violet", "black", "orange", "orange", "orange", "red", "red", "red", "black", "yellow", "yellow", "lightgray", "yellow", "darkgreen", "darkgreen", "darkgreen", "black", "blue", "blue")
  
  #creates a frequency plot for the number of times each property is landed on 
  property_plot <- ggplot(properties_landed, aes(x = Property, y = Occurances)) +
    geom_bar(stat = "identity", fill = colors_properties, color = "black") +
    scale_x_discrete(limits = properties_landed$Property) +
    ggtitle("Frequency of Landing on each Property") +
    xlab("Property") +
    ylab("Frequency") +
    theme(axis.text.x.bottom = element_text(angle=90))
  scale_color_manual(name = "Properties",
                     breaks = c("Mediterranean", "Baltic", "Reading Railroad", "Oriental", "Vermot", "Conecticut", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Tennessee", "New York", "Kentucky", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Pacific", "N. Carolina", "Pennsylvania", "Short Line Railroad", "Park Place", "Boardwalk"),
                     values = colors_properties)
  
  print(property_plot) 
  
  
  ########################## location frequency plot ###############################
  locations_landed <- as.vector(as.matrix(location_data))
  
  location_names <- c("GO", "Mediterranean", "Community Chest 1", "Baltic ", "Income Tax", "Reading Railroad", "Oriental","Chance 1", "Vermot", "Conecticut", "Jail", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Community Chest 2", "Tennessee", "New York","Free Parking", "Kentucky", "Chance 2", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Go To Jail", "Pacific", "N. Carolina", "Community Chest 3", "Pennsylvania", "Short Line Railroad", "Chance 3", "Park Place", "Luxury Tax", "Boardwalk")
  
  locations_landed <- as.data.frame(locations_landed) 
  locations_landed <- as.data.frame(locations_landed)
  locations_landed <- locations_landed %>% group_by(locations_landed) %>% summarise(n())
  locations_landed <- as.data.frame(locations_landed)
  locations_landed <- cbind(location_names, locations_landed)
  names(locations_landed) <- c("Name", "Location", "Occurances")
  #print(locations_landed)
  
  colors_locations <- c("white","purple","navy", "purple","white", "black", "lightblue","navy", "lightblue", "lightblue", "white", "violet", "lightgray", "violet", "violet", "black", "orange","navy", "orange", "orange","white", "red","navy", "red", "red", "black", "yellow", "yellow", "lightgray", "yellow","white", "darkgreen", "darkgreen","navy", "darkgreen", "black","navy", "blue","white", "blue")
  
  
  #creates a frequency plot for the number of times each property is landed on 
  location_plot <- ggplot(locations_landed, aes(x = Name, y = Occurances)) +
    geom_bar(stat = "identity", fill = colors_locations, color = "black") +
    scale_x_discrete(limits = locations_landed$Name) +
    ggtitle("Frequency of Landing on each Location") +
    xlab("Location") +
    ylab("Frequency") +
    theme(axis.text.x.bottom = element_text(angle=90, vjust = 0.001)) +
    scale_color_manual(name = "Properties",
                       breaks = c("GO", "Mediterranean", "Community Chest 1", "Baltic ", "Income Tax", "Reading Railroad", "Oriental","Chance 1", "Vermot", "Conecticut", "Jail", "St. Charles", "Electric Company", "States", "Virginia", "Pennsylvania Railroad", "St. James", "Community Chest 2", "Tennessee", "New York","Free Parking", "Kentucky", "Chance 2", "Indiana", "Illinios", "B&O Railroad", "Atlantic", "Ventnor", "Water Works", "Marvin Gardens", "Go To Jail", "Pacific", "N. Carolina", "Community Chest 3", "Pennsylvania", "Short Line Railroad", "Chance 3", "Park Place", "Luxury Tax", "Boardwalk"),
                       values = colors_locations) 
  
  print(location_plot)  
  
  
  
  ########################## location plots ###############################
  
  # creates density plot of frequency of landing on each space for each player
  colors <- c("red", "blue", "green","purple")
  
  plot(density(location_data[,1]), xlim = c(0,40), col = "black", xlab="Spaces", main= "Density of Landing Locations for All Players")
  
  for (k in 2:players) {
    lines(density(location_data[,k]), col = colors[k-1]) # adds a density line for each player after the first
  }
  
  
  # creates density plot for total frequency landing on each space (all players spaces included)
  all_spaces <- as.vector(as.matrix(location_data))
  #plot(density(all_spaces), xlim = c(0,40), col = "black", xlab="Spaces", main="Density of Landing Locations Overall")
  
  #for(k in 1:players){
  plot(location_data[,1], xlim= c(0,40), col=colors[1], type="h", xlab="Spaces", ylab="Frequency", main="Player Frequency of Landing on Each Space")
  #  }
  
  # second set of frequency plots for individual player locations
  #for (k in 1:players) {
  #  colors <- c("coral1", "steelblue1", "palegreen2","plum2")
  #  freq_data <- data.frame(space = 1:40, freq = tabulate(location_data[, k], nbins = 40))
  #  density_plot <- ggplot(freq_data, aes(x = space, y = freq)) +
  #   geom_bar(stat = "identity", fill = colors[k], color = "black") +
  #   ggtitle(paste0("Player ", k, ": Frequency of Landing on Each Space")) +
  #   xlab("Spaces") +
  #   ylab("Frequency") +
  #   theme_minimal()
  
  #  print(density_plot)
  #}
}

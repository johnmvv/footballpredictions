packageList <- c("readr", "dplyr")

# Automatically installs packages if they are not installed
checkAndInstall <- function(packageName){
  if (!require(packageName, character.only = TRUE)){
    install.packages(packageName, dependencies = TRUE)
  }
  library(packageName, character.only = TRUE)
}
sapply(packageList, checkAndInstall)

# Reads the dataset
getDataset <- function(datasetLocation){
  dataset <- read.csv(datasetLocation, as.is = TRUE, header = TRUE)
  return(dataset)
}

# Gets the user input
getUserInput <- function(){
  teamA <- readline("Enter the first team: ")
  teamB <- readline("Enter the second team: ")
  datasetLocation <- readline("Enter the dataset location: ")
  return(c(teamA, teamB, datasetLocation))
}

# Function to calculate the exponential decay for the most recent matches
exponentialDecayHomeFor <- function(team, dataset, mean, decayFactor){
  teamMatches <- dataset[dataset$HomeTeam == team,]
  teamMatches <- teamMatches[order(teamMatches$Date, decreasing = TRUE),]
  decayWeights <- decayFactor^(0:(nrow(teamMatches)-1))
  return((sum(teamMatches$FTHG * decayWeights) / sum(decayWeights) )/ mean)
}

exponentialDecayHomeFor2 <- function(team, dataset, mean, teamstats, decayFactor){
  teamMatches <- dataset[dataset$HomeTeam == team,]
  teamMatches <- teamMatches[order(teamMatches$Date, decreasing = TRUE),]
  decayWeights <- decayFactor^(0:(nrow(teamMatches)-1))
  total <- 0
  for (ii in 1:nrow(teamMatches)) {
    total <- total + teamMatches$FTHG[ii] * decayWeights[ii] / teamstats[teamMatches$AwayTeam[ii], "AwayAgainst"]
  }
  return(total / sum(decayWeights))
}

exponentialDecayAwayFor <- function(team, dataset,mean, decayFactor){
  teamMatches <- dataset[dataset$AwayTeam == team,]
  teamMatches <- teamMatches[order(teamMatches$Date, decreasing = TRUE),]
  decayWeights <- decayFactor^(0:(nrow(teamMatches)-1))
  return((sum(teamMatches$FTAG * decayWeights) / sum(decayWeights) )/ mean)
}

exponentialDecayAwayFor2 <- function(team, dataset, mean, teamstats, decayFactor){
  teamMatches <- dataset[dataset$AwayTeam == team,]
  teamMatches <- teamMatches[order(teamMatches$Date, decreasing = TRUE),]
  decayWeights <- decayFactor^(0:(nrow(teamMatches)-1))
  total <- 0
  for (ii in 1:nrow(teamMatches)) {
    total <- total + teamMatches$FTAG[ii] * decayWeights[ii] / teamstats[teamMatches$HomeTeam[ii], "HomeAgainst"]
  }
  return(total / sum(decayWeights))
}

exponentialDecayHomeAgainst <- function(team, dataset,mean, decayFactor){
  teamMatches <- dataset[dataset$HomeTeam == team,]
  teamMatches <- teamMatches[order(teamMatches$Date, decreasing = TRUE),]
  decayWeights <- decayFactor^(0:(nrow(teamMatches)-1))
  return((sum(teamMatches$FTAG * decayWeights) / sum(decayWeights) )/ mean)
}

exponentialDecayHomeAgainst2 <- function(team, dataset, mean, teamstats, decayFactor){
  teamMatches <- dataset[dataset$HomeTeam == team,]
  teamMatches <- teamMatches[order(teamMatches$Date, decreasing = TRUE),]
  decayWeights <- decayFactor^(0:(nrow(teamMatches)-1))
  total <- 0
  for (ii in 1:nrow(teamMatches)) {
    total <- total + teamMatches$FTAG[ii] * decayWeights[ii] / teamstats[teamMatches$AwayTeam[ii], "AwayFor"]
  }
  return(total / sum(decayWeights))
}

exponentialDecayAwayAgainst <- function(team, dataset, mean, decayFactor){
  teamMatches <- dataset[dataset$AwayTeam == team,]
  teamMatches <- teamMatches[order(teamMatches$Date, decreasing = TRUE),]
  decayWeights <- decayFactor^(0:(nrow(teamMatches)-1))
  return((sum(teamMatches$FTHG * decayWeights) / sum(decayWeights) )/ mean)
}

exponentialDecayAwayAgainst2 <- function(team, dataset, mean, teamstats, decayFactor){
  teamMatches <- dataset[dataset$AwayTeam == team,]
  teamMatches <- teamMatches[order(teamMatches$Date, decreasing = TRUE),]
  decayWeights <- decayFactor^(0:(nrow(teamMatches)-1))
  total <- 0
  for (ii in 1:nrow(teamMatches)) {
    total <- total + teamMatches$FTHG[ii] * decayWeights[ii] / teamstats[teamMatches$HomeTeam[ii], "HomeFor"]
  }
  return(total / sum(decayWeights))
}

# Function to calculate the probabilities
calculateMeans <- function(teamA, teamB, dataset, decayFactor){

  #find mean with exponential decay for home and away goals and conceded
  teams <- sort(unique(c(dataset$HomeTeam, dataset$AwayTeam)))
  teamstats <- data.frame()
  teamstats1 <- data.frame()
  
  for (team in teams) {
    teamstats[team, "HomeFor"] <- exponentialDecayHomeFor(team, dataset, 1, decayFactor)
    teamstats[team, "AwayFor"] <- exponentialDecayAwayFor(team, dataset, 1, decayFactor)
    teamstats[team, "HomeAgainst"] <- exponentialDecayHomeAgainst(team, dataset, 1, decayFactor)
    teamstats[team, "AwayAgainst"] <- exponentialDecayAwayAgainst(team, dataset, 1, decayFactor)
  }
  meanHomeGoals <- mean(teamstats$HomeFor)
  meanAwayGoals <- mean(teamstats$AwayFor)
  meanHomeConceded <- mean(teamstats$HomeAgainst)
  meanAwayConceded <- mean(teamstats$AwayAgainst)

  for (team in teams) {
    teamstats1[team, "HomeFor"] <- exponentialDecayHomeFor(team, dataset, meanHomeGoals, decayFactor)
    teamstats1[team, "AwayFor"] <- exponentialDecayAwayFor(team, dataset, meanAwayGoals, decayFactor)
    teamstats1[team, "HomeAgainst"] <- exponentialDecayHomeAgainst(team, dataset, meanHomeConceded, decayFactor)
    teamstats1[team, "AwayAgainst"] <- exponentialDecayAwayAgainst(team, dataset, meanAwayConceded, decayFactor)
  }

  for (team in teams) {
    teamstats[team, "HomeFor"] <- exponentialDecayHomeFor2(team, dataset, meanHomeGoals, teamstats1, decayFactor)
    teamstats[team, "AwayFor"] <- exponentialDecayAwayFor2(team, dataset, meanAwayGoals, teamstats1, decayFactor)
    teamstats[team, "HomeAgainst"] <- exponentialDecayHomeAgainst2(team, dataset, meanHomeConceded, teamstats1, decayFactor)
    teamstats[team, "AwayAgainst"] <- exponentialDecayAwayAgainst2(team, dataset, meanAwayConceded, teamstats1, decayFactor)
  }

  weight = 0.33
  AattackStrength <- ((1-weight)*exponentialDecayHomeFor2(teamA, dataset, meanHomeGoals, teamstats, decayFactor) + weight*exponentialDecayAwayFor2(teamA,dataset, meanAwayGoals, teamstats, decayFactor))
  AdefenceStrength <- ((1-weight)*exponentialDecayHomeAgainst2(teamA, dataset, meanHomeConceded, teamstats, decayFactor) + weight*exponentialDecayAwayAgainst2(teamA, dataset, meanAwayConceded, teamstats, decayFactor))
  BattackStrength <- ((weight)*exponentialDecayHomeFor2(teamB, dataset, meanHomeGoals, teamstats, decayFactor) + (1-weight)*exponentialDecayAwayFor2(teamB, dataset, meanAwayGoals, teamstats, decayFactor))
  BdefenceStrength <- ((weight)*exponentialDecayHomeAgainst2(teamB, dataset, meanHomeConceded, teamstats, decayFactor) + (1-weight)*exponentialDecayAwayAgainst2(teamB, dataset, meanAwayConceded, teamstats, decayFactor))
  Ascore <- AattackStrength * BdefenceStrength * meanHomeGoals
  Bscore <- BattackStrength * AdefenceStrength * meanAwayGoals

  return(c(Ascore, Bscore))
}

getProbabilityTable <- function(Ascore, Bscore) {
  distributionAteam = c(dpois(0:4, lambda = Ascore), 1 - sum(dpois(0:4, lambda = Ascore)))
  distributionBteam = c(dpois(0:4, lambda = Bscore), 1 - sum(dpois(0:4, lambda = Bscore)))
  multiplication_table = outer(distributionAteam, distributionBteam, "*")
  probabilities = c("Team A" = sum(multiplication_table[lower.tri(multiplication_table)]), "Team B" = sum(multiplication_table[upper.tri(multiplication_table)]), "Draw" = sum(diag(multiplication_table)))
  mostLikelyScore = which(multiplication_table == max(multiplication_table), arr.ind = TRUE)
  mostLikelyScoreProbability = max(multiplication_table)
  return(probabilities)
}

kalmanFilterAllTeams <- function(dataset, vkl) {
  # Get the list of teams
  teams <- unique(c(dataset$HomeTeam, dataset$AwayTeam))

  weight = 0.33

  achvah <- vkl
  achvak <- 1-achvah
  
  # Create a list to hold the team ratings
  team_ratings <- list()
  
  # Initialize team ratings
  for (team in teams) {
    team_ratings[[team]] <- list(attackhome = 1.6, attackaway = 1.4, defensehome = 1.4, defenseaway = 1.6)
  }
  
  # Process each match
  for (i in 1:nrow(dataset)) {
    home_team <- dataset$HomeTeam[i]
    away_team <- dataset$AwayTeam[i]
    
    # Get the team ratings
    home_team_attack_rating <- (team_ratings[[home_team]]$attackhome)*(1-weight) + (team_ratings[[home_team]]$attackaway)*weight
    home_team_defense_rating <- (team_ratings[[home_team]]$defensehome)*(1-weight) + team_ratings[[home_team]]$defenseaway*weight
    away_team_attack_rating <- (team_ratings[[away_team]]$attackaway)*(1-weight) + team_ratings[[away_team]]$attackhome*weight
    away_team_defense_rating <- (team_ratings[[away_team]]$defenseaway)*(1-weight) + team_ratings[[away_team]]$defensehome*weight
    
    # Update the Kalman filter with the match result
    homescr <- dataset$FTHG[i]  # Measuremen+t for home team attack
    awayscr <- dataset$FTAG[i]  # Measurement for home team defense

    expected_home_score <- home_team_attack_rating * away_team_defense_rating
    expected_away_score <- away_team_attack_rating * home_team_defense_rating

    home_team_attack_rating <- achvah * home_team_attack_rating + achvak * (homescr / expected_home_score)
    home_team_defense_rating <- achvah * home_team_defense_rating + achvak * (awayscr / expected_away_score)
    away_team_attack_rating <- achvah * away_team_attack_rating + achvak * (awayscr / expected_away_score)
    away_team_defense_rating <- achvah * away_team_defense_rating + achvak * (homescr / expected_home_score)
    
    # Store the updated ratings
    team_ratings[[home_team]]$attackhome <- home_team_attack_rating
    team_ratings[[home_team]]$defensehome <- home_team_defense_rating
    team_ratings[[away_team]]$attackaway <- away_team_attack_rating
    team_ratings[[away_team]]$defenseaway <- away_team_defense_rating
  }
  
  return(team_ratings)
}

getscoreformkalman <- function(home_team, away_team, team_ratings) {
  weight = 0.33
  home_team_attack_rating <- (team_ratings[[home_team]]$attackhome)*(1-weight) + (team_ratings[[home_team]]$attackaway)*weight
  home_team_defense_rating <- (team_ratings[[home_team]]$defensehome)*(1-weight) + team_ratings[[home_team]]$defenseaway*weight
  away_team_attack_rating <- (team_ratings[[away_team]]$attackaway)*(1-weight) + team_ratings[[away_team]]$attackhome*weight
  away_team_defense_rating <- (team_ratings[[away_team]]$defenseaway)*(1-weight) + team_ratings[[away_team]]$defensehome*weight
  Ascore <- home_team_attack_rating * away_team_defense_rating
  Bscore <- home_team_defense_rating * away_team_attack_rating
  return(c(Ascore, Bscore))
}

getProbabilityTable <- function(Ascore, Bscore) {
  distributionAteam = c(dpois(0:4, lambda = Ascore), 1 - sum(dpois(0:4, lambda = Ascore)))
  distributionBteam = c(dpois(0:4, lambda = Bscore), 1 - sum(dpois(0:4, lambda = Bscore)))
  multiplication_table = outer(distributionAteam, distributionBteam, "*")
  probabilities = c("Team A" = sum(multiplication_table[lower.tri(multiplication_table)]), "Team B" = sum(multiplication_table[upper.tri(multiplication_table)]), "Draw" = sum(diag(multiplication_table)))
  outcome = names(probabilities)[which.max(probabilities)]
  mostLikelyScore = which(multiplication_table == max(multiplication_table), arr.ind = TRUE)
  mostLikelyScore = c(mostLikelyScore[1]-1, mostLikelyScore[2]-1)
  mostLikelyScoreProbability = max(multiplication_table)
  return(c(outcome, mostLikelyScore))
}


getprobkalman <- function(teamA, teamB, dataset, vkl){
  kalmanvalues <- kalmanFilterAllTeams(dataset, vkl)
  kalmanvalues <- getscoreformkalman(teamA, teamB, kalmanvalues)
  return(getProbabilityTable(kalmanvalues[1], kalmanvalues[2]))
}

getprobed <- function(teamA, teamB, dataset, vkl){
  means <- calculateMeans(teamA, teamB, dataset, vkl)
  return(getProbabilityTable(means[1], means[2]))
}

comparePredictions <- function(dataset, n, func, vkl) {
  numb <- 10
  # Split the dataset into a training set and a test set
  training_set <- dataset[1:n, ]
  test_set <- dataset[(n+1):(n+numb), ]

  # Initialize a data frame to hold the predictions and actual outcomes
  results <- data.frame(Predicted_TeamA_Score = numeric(), Predicted_TeamB_Score = numeric(), Actual_TeamA_Score = numeric(), Actual_TeamB_Score = numeric())

  # Make predictions for the next 10 games
  correct_outcomes <- 0
  correct_scores <- 0
  for (i in 1:numb) {
    # Get the actual scores
    actual_scores <- test_set[i, c("FTHG", "FTAG")]
    actual_outcome <- ifelse(actual_scores$FTHG > actual_scores$FTAG, "Team A", ifelse(actual_scores$FTHG < actual_scores$FTAG, "Team B", "Draw"))
    # Make a prediction
    prediction <- func(test_set$HomeTeam[i], test_set$AwayTeam[i], training_set, vkl)
    if (!is.na(prediction[1]) && !is.null(prediction[1]) && prediction[1] == actual_outcome && prediction[1] == actual_outcome) {
      correct_outcomes <- correct_outcomes + 1
    }
    if (all(prediction[2:3] == actual_scores)) {
      correct_scores <- correct_scores + 1
    }
  }

  results <- correct_outcomes/numb
  # Return the results
  return(results)
}


# Example usage:
dataset <- getDataset("dataset.csv")
kalmanscore <- list()
edscore <- list()

testpoints = list(3750, 4000, 4250, 4500, 4750, 5000, 5100, 5250, 5500, 5750, 6000, 6100, 6250, 6500, 6750, 7000)

#vkl <- 0.9769793
vkl <- 0.9769793
itteration <- 0
#train the sets
meank <- 0.518
lastdir <- 1

while(TRUE){
  itteration <- itteration + 1
  kalmanscore <- list()
  for (n in testpoints) {
    x = comparePredictions(dataset, n, getprobkalman, vkl)
    kalmanscore = append(kalmanscore, x)
    print(x)
  }
  flattened <- unlist(kalmanscore)
  mean_value <- mean(flattened)
  changevalue <- mean_value - meank
  meank <- mean_value
  print(changevalue)
  print(mean_value)
  print(vkl)
  print("---")
  if(changevalue == 0){
    changevalue <- 0.02
  }
  if(changevalue > 0){
    vkl <- vkl + changevalue*lastdir/itteration
  } else {
    lastdir <- lastdir*-1
    vkl <- vkl + changevalue*lastdir/itteration
  }
  if (itteration == 10) {
    print(vkl)
    break
  }
}

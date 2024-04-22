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

kalmanFilterAllTeams <- function(dataset, vkl) {
  # Get the list of teams
  teams <- unique(c(dataset$HomeTeam, dataset$AwayTeam))

  weight = 0.2

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


getscoreformkalman <- function(home_team, away_team, team_ratings, weight=0.33) {
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
  kalmanvalues <- getscoreformkalman(teamA, teamB, kalmanvalues, weight=0.2)
  return(getProbabilityTable(kalmanvalues[1], kalmanvalues[2]))
}

comparePredictions <- function(dataset, n, func, vkl) {
  numb <- 15
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
print(comparePredictions(dataset, 1000, getprobkalman, 0.975))
print(comparePredictions(dataset, 1000, getprobkalman, 0.935))

testpoints = list(4000, 4100, 4250, 4500, 4750, 5000, 5100, 5250, 5500, 5750, 6000, 6100, 6250, 6500, 6750, 7000)

scoresa = list()
vklss = list()

kalmanscore1 <- list()
for (n in testpoints) {
  x = comparePredictions(dataset, n, getprobkalman, 0.975)
  print(n)
  kalmanscore1 = append(kalmanscore1, x)
}

kalmanscore2 <- list()
for (n in testpoints) {
  x = comparePredictions(dataset, n, getprobkalman, 0.935)
  print(n)
  kalmanscore2 = append(kalmanscore2, x)
}
print(kalmanscore1)
print(kalmanscore2)

mks1 = mean(unlist(kalmanscore1))
mks2 = mean(unlist(kalmanscore2))
print(mks1)
print(mks2)

#barplot mks1, mks2
barplot(c(mks1, mks2), names.arg = c("0.975", "0.935"), col = c("blue", "red"), main = "Mean Correct Predictions", ylab = "Mean Correct Predictions", xlab = "Variance Kalman Filter")

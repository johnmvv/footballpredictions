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
  print(multiplication_table)

  return((outcome))
}

getprobkalman <- function(teamA, teamB, dataset, vkl){
    kalmanvalues <- kalmanFilterAllTeams(dataset, vkl)
    kalmanvalues <- getscoreformkalman(teamA, teamB, kalmanvalues, weight=0.25)
    winner <- getProbabilityTable(kalmanvalues[1], kalmanvalues[2])
    if (winner == "Team A") {
        return(teamA)
    } else if (winner == "Team B") {
        return(teamB)
    } else {
        return("Draw")
    }
}

# Example usage:
dataset <- getDataset("dataset.csv")
games <- list(list("West Ham", "Liverpool"), list("Fulham", "Crystal Palace"), list("Man United", "Burnley"), list("Newcastle", "Sheffield United"), list("Wolves", "Luton"), list("Everton", "Brentford"), list("Aston Villa", "Chelsea"), list("Bournemouth", "Brighton"), list("Tottenham", "Arsenal"), list("Nott'm Forest", "Man City"))
vkl = 0.935

for (game in games) {
    print(game)
    print(getprobkalman(game[[1]], game[[2]], dataset, vkl))
}

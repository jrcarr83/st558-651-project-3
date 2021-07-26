#read in the data
#remove columns 
data <- read_csv('data/nba_data.csv', col_types = cols(
          slugSeason = col_factor(),
          typeSeason = col_factor(),
          slugTeam = col_factor(),
          slugOpponent = col_factor(),
          isB2B = col_factor(),
          isB2BFirst = col_factor(),
          isB2BSecond = col_factor(),
          locationGame = col_factor(),
          outcomeGame = col_factor(),
          namePlayer = col_factor())) %>%
          select(-X1, -yearSeason, -slugLeague, -idGame, -idTeam, -idPlayer,
                 -hasVideo, -slugMatchup, -nameTeam, -slugTeamWinner, 
                 -slugTeamLoser, -isWin, -urlPlayerPhoto, -urlPlayerActionPhoto,
                 -urlPlayerHeadshot, -urlPlayerThumbnail, -urlPlayerStats, 
                 -urlTeamSeasonLogo, -isB2BFirst)
data$dateGame <- as.Date(data$dateGame, format="%Y/%m/%d")

model_data <- read_csv('data/model_data.csv', col_types = cols(
  slugTeam = col_factor(),
  slugOpponent = col_factor(),
  isB2B = col_factor(),
  isB2BSecond = col_factor(),
  opp_isB2B = col_factor(),
  opp_isB2BSecond = col_factor(),
  locationGame = col_factor(),
  outcomeGame = col_factor()))
model_data$dateGame <- as.Date(model_data$dateGame, format="%m/%d/%Y") 
model_data <- model_data %>% arrange(dateGame)


var_list <- structure(
  list(
    names = c("B2B First", "B2B Second", "Days Rest", "Games Played", "Home/Away", 
              "Points", "Team", "FGm", "FGa", "FG3m", "FG3a", "FG2m", "FG2a", 
              "FTm", "FTa", "OReb", "DReb", "TReb", "Ast", "Stl", "Blk", 
              "Tov", "PF", "Wins/Losses"),
    var1 = c("isB2B opp_isB2B", "isB2BSecond opp_isB2BSecond", 
             "countDaysRestTeam opp_countDaysRestTeam",
             "numberGameTeamSeason opp_numberGameTeamSeason", 
             "locationGame", "slugTeam slugOpponent", "pts1 pts2",
             "fgm1 fgm2", "fga1 ga2", "fg3m1 fg3m2", "fg3a1 fg3a2", 
             "fg2m1 fg2m2", "fg2a1 fg2a2", "ftm1 ftm12", "ftm2 ftm22",
             "oreb1 oreb2", "dreb1 dreb2", "treb1 treb2", "ast1 ast2", 
             "stl1 stl2", "blk1 blk2", "tov1 tov2", "pf1 pft2", 
             "wins1 losses1 wins2 losses2"),
    class = "data.frame",
    row.names = c(NA, -50L)
))
  
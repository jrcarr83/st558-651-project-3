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
                 -urlTeamSeasonLogo, -isB2BFirst) %>%
         mutate(isB2BFirst = if_else(
           isB2B == TRUE && isB2BSecond == FALSE, TRUE, FALSE))
data$isB2BFirst <- as.factor(data$isB2BFirst)
data$dateGame <- as.Date(data$dateGame, format="%Y/%m/%d")

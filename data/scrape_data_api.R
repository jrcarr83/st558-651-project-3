#devtools::install_github("abresler/nbastatR")
library("nbastatR")

library(future)
plan(multiprocess) 
regular_season <- game_logs(seasons = 2018, season_types = "Regular Season")
#post_season <- game_logs(seasons = 2019:2021, season_types = "Playoffs")
#post_season$game_type <- 'Post Season'

#total <- rbind(regular_season, post_season)
write.csv(regular_season, file = "nba_data.csv")

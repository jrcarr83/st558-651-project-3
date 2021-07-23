#devtools::install_github("abresler/nbastatR")
library("nbastatR")

library(future)
plan(multiprocess) 
regular_season <- game_logs(seasons = 2018:2021, season_types = "Regular Season")
regular_season$game_type <- 'Regular Season'
post_season <- game_logs(seasons = 2018:2021, season_types = "Playoffs")
post_season$game_type <- 'Post Season'

total <- rbind(regular_season, post_season)
write.csv(total, file = "nba_data.csv")

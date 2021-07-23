get_team_data <- function(data, stat_type) {
  if (stat_type == 'ttl') {
    data_table <- data %>%
      group_by(slugSeason, slugTeam, typeSeason, locationGame, outcomeGame) %>%
      summarise(fgm = sum(fgm),
                fga = sum(fga),
                pctFG = sum(fgm) / sum(fga),
                fg3m = sum(fg3m),
                fg3a = sum(fg3a), 
                pctFG3 = sum(fg3m) / sum(fg3a),
                fg2m = sum(fg2m),
                fg2a = sum(fg2a), 
                pctFG2 = sum(fg2m) / sum(fg2a),
                oreb = sum(oreb),
                dreb = sum(dreb),
                treb = sum(treb),
                ast = sum(ast),
                stl = sum(stl),
                blk = sum(blk),
                tov = sum(tov),
                pf = sum(pf),
                minutes = sum(minutes),
                games = n_distinct(dateGame)) %>%
      mutate(across(where(is.numeric), round, 1))
  } else if (stat_type == 'avg') {
    data_table <- data %>%
      group_by(slugSeason, slugTeam, typeSeason, locationGame, outcomeGame) %>%
      summarise(fgm = sum(fgm) / n_distinct(dateGame),
                fga = sum(fga) / n_distinct(dateGame),
                pctFG = sum(fgm) / sum(fga),
                fg3m = sum(fg3m) / n_distinct(dateGame),
                fg3a = sum(fg3a) / n_distinct(dateGame), 
                pctFG3 = sum(fg3m) / sum(fg3a),
                fg2m = sum(fg2m) / n_distinct(dateGame),
                fg2a = sum(fg2a) / n_distinct(dateGame), 
                pctFG2 = sum(fg2m) / sum(fg2a),
                oreb = sum(oreb) / n_distinct(dateGame),
                dreb = sum(dreb) / n_distinct(dateGame),
                treb = sum(treb) / n_distinct(dateGame),
                ast = sum(ast) / n_distinct(dateGame),
                stl = sum(stl) / n_distinct(dateGame),
                blk = sum(blk) / n_distinct(dateGame),
                tov = sum(tov) / n_distinct(dateGame),
                pf = sum(pf) / n_distinct(dateGame)) %>%
       mutate(across(where(is.numeric), round, 1))
  }
  
  return (data_table)
  
}


get_player_data <- function(data, stat_type) {
  if (stat_type == 'ttl') {
    data_table <- data %>%
      group_by(slugSeason, namePlayer, slugTeam, typeSeason, locationGame, outcomeGame) %>%
      summarise(fgm = sum(fgm),
                fga = sum(fga),
                pctFG = sum(fgm) / sum(fga),
                fg3m = sum(fg3m),
                fg3a = sum(fg3a), 
                pctFG3 = sum(fg3m) / sum(fg3a),
                fg2m = sum(fg2m),
                fg2a = sum(fg2a), 
                pctFG2 = sum(fg2m) / sum(fg2a),
                oreb = sum(oreb),
                dreb = sum(dreb),
                treb = sum(treb),
                ast = sum(ast),
                stl = sum(stl),
                blk = sum(blk),
                tov = sum(tov),
                pf = sum(pf),
                minutes = sum(minutes),
                games = n_distinct(dateGame))  %>%
      mutate(across(where(is.numeric), round, 1))
  } else if (stat_type == 'avg') {
    data_table <- data %>%
      group_by(slugSeason, namePlayer, slugTeam, typeSeason, locationGame, outcomeGame) %>%
      summarise(fgm = sum(fgm) / n_distinct(dateGame),
                fga = sum(fga) / n_distinct(dateGame),
                pctFG = sum(fgm) / sum(fga),
                fg3m = sum(fg3m) / n_distinct(dateGame),
                fg3a = sum(fg3a) / n_distinct(dateGame), 
                pctFG3 = sum(fg3m) / sum(fg3a),
                fg2m = sum(fg2m) / n_distinct(dateGame),
                fg2a = sum(fg2a) / n_distinct(dateGame), 
                pctFG2 = sum(fg2m) / sum(fg2a),
                oreb = sum(oreb) / n_distinct(dateGame),
                dreb = sum(dreb) / n_distinct(dateGame),
                treb = sum(treb) / n_distinct(dateGame),
                ast = sum(ast) / n_distinct(dateGame),
                stl = sum(stl) / n_distinct(dateGame),
                blk = sum(blk) / n_distinct(dateGame),
                tov = sum(tov) / n_distinct(dateGame),
                pf = sum(pf) / n_distinct(dateGame),
                minutes = sum(minutes) / n_distinct(dateGame))  %>%
      mutate(across(where(is.numeric), round, 1))
  } else if (stat_type == 'per30') {
    data_table <- data %>%
      group_by(slugSeason, namePlayer, slugTeam, typeSeason, locationGame, outcomeGame) %>%
      summarise(fgm = sum(fgm) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)),
                fga = sum(fga) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)),
                pctFG = sum(fgm) / sum(fga),
                fg3m = sum(fg3m) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)),
                fg3a = sum(fg3a) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)), 
                pctFG3 = sum(fg3m) / sum(fg3a),
                fg2m = sum(fg2m) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)),
                fg2a = sum(fg2a) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)), 
                pctFG2 = sum(fg2m) / sum(fg2a),
                oreb = sum(oreb) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)),
                dreb = sum(dreb) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)),
                treb = sum(treb) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)),
                ast = sum(ast) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)),
                stl = sum(stl) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)),
                blk = sum(blk) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)),
                tov = sum(tov) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)),
                pf = sum(pf) / n_distinct(dateGame) * 30 / (sum(minutes) / n_distinct(dateGame)))  %>%
      mutate(across(where(is.numeric), round, 1))
  }
  
  return (data_table)
  
}


30/17.5
17.5
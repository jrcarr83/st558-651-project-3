summary(data)


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
            oreb = sum(orb),
            dreb = sum(drb),
            treb = sum(trb),
            ast = sum(ast),
            stl = sum(stl),
            blk = sum(blk),
            tov = sum(tov),
            pf = sum(pf),
            minutes = sum(minutes),
            games = n_distinct(dateGame))


temp <- get_team_data(data, 'avg')

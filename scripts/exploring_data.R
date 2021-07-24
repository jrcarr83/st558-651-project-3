###summarizing by team
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

###summarizing by team
team <- data %>%
  group_by(slugTeam, dateGame, numberGameTeamSeason, 
           isB2B, isB2BFirst, isB2BSecond, countDaysRestTeam,
           slugOpponent, locationGame, outcomeGame) %>%
  summarise(pts = sum(fg2m*2 + fg3m*3 + ftm),
            fgm = sum(fgm),
            fga = sum(fga),
            pctFG = sum(fgm) / sum(fga),
            fg3m = sum(fg3m),
            fg3a = sum(fg3a), 
            pctFG3 = sum(fg3m) / sum(fg3a),
            fg2m = sum(fg2m),
            fg2a = sum(fg2a), 
            pctFG2 = sum(fg2m) / sum(fg2a),
            ftm = sum(ftm),
            fta = sum(fta), 
            pctFG2 = sum(fg2m) / sum(fg2a),
            oreb = sum(oreb),
            dreb = sum(dreb),
            treb = sum(treb),
            ast = sum(ast),
            stl = sum(stl),
            blk = sum(blk),
            tov = sum(tov),
            pf = sum(pf))

opp <- data %>%
  group_by(slugTeam, dateGame, numberGameTeamSeason, 
           isB2B, isB2BFirst, isB2BSecond, countDaysRestTeam) %>%
  summarise(opp_pts = sum(fg2m*2 + fg3m*3 + ftm),
            opp_fgm = sum(fgm),
            opp_fga = sum(fga),
            opp_pctFG = sum(fgm) / sum(fga),
            opp_fg3m = sum(fg3m),
            opp_fg3a = sum(fg3a), 
            opp_pctFG3 = sum(fg3m) / sum(fg3a),
            opp_fg2m = sum(fg2m),
            opp_fg2a = sum(fg2a), 
            opp_pctFG2 = sum(fg2m) / sum(fg2a),
            opp_ftm = sum(ftm),
            opp_fta = sum(fta), 
            opp_pctFG2 = sum(fg2m) / sum(fg2a),
            opp_oreb = sum(oreb),
            opp_dreb = sum(dreb),
            opp_treb = sum(treb),
            opp_ast = sum(ast),
            opp_stl = sum(stl),
            opp_blk = sum(blk),
            opp_tov = sum(tov),
            opp_pf = sum(pf)) %>%
    rename(slugOpponent = slugTeam,
           opp_numberGameTeamSeason = numberGameTeamSeason,
           opp_isB2B = isB2B, 
           opp_isB2BFirst = isB2BFirst, 
           opp_isB2BSecond = isB2BSecond, 
           opp_countDaysRestTeam = countDaysRestTeam)

joined <- team %>% inner_join(opp, by=c('slugOpponent', 'dateGame'))

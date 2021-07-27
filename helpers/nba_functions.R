#returns the base dataset summed across teams
#stat type can be tot4al or average
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

#returns the base dataset summed across players
#stat type can be tot4al, average, or per30
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

#this takes the base data set and joins it onto itself
#that way we get the box data from both teams on one line
get_joined_data <- function(data) {
  ###summarizing by home team
  team <- data %>% 
    group_by(slugTeam, dateGame, numberGameTeamSeason, 
             isB2B, isB2BSecond, countDaysRestTeam,
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
             isB2B, isB2BSecond, countDaysRestTeam) %>%
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
           opp_isB2BSecond = isB2BSecond, 
           opp_countDaysRestTeam = countDaysRestTeam)
  
  joined <- team %>% inner_join(opp, by=c('slugOpponent', 'dateGame')) %>%
              mutate(net_pts = pts - opp_pts) %>% ungroup()
  return (joined)
}

##will return a density plot based on the parameters passed
get_density_plot <- function(data, var, home_away, b2b, result) {
  #subset the data based on win/losses based on what the user wants
  if (result == 'win_only') {
    data <- data %>% filter(outcomeGame == 'W')
  } else if (result == 'loss_only') {
    data <- data %>% filter(outcomeGame == 'L')
  }
  
  p <- ggplot(data=data, aes_string(x=var)) + theme_modern_rc()
  if (home_away) {
    p <- p + geom_density(aes(fill=locationGame)) 
  } else {
    p <- p + geom_density()
  }
  
  if (b2b) {
    p <- p + facet_wrap(~ isB2BSecond) + 
      theme(strip.text = element_text(colour = 'white'))
  }
  
  return (ggplotly(p))
  
}

##will return a scatter plot based on the parameters passed
get_scatter_plot <- function(data, xvar, yvar, 
                             home_away, b2b, result) {
  #subset the data based on win/losses based on what the user wants
  if (result == 'win_only') {
    data <- data %>% filter(outcomeGame == 'W')
  } else if (result == 'loss_only') {
    data <- data %>% filter(outcomeGame == 'L')
  }
  
  p <- ggplot(data=data, aes_string(x=xvar, y=yvar)) + theme_modern_rc()
  if (home_away) {
    p <- p + geom_point(aes(color=locationGame)) 
  } else {
    p <- p + geom_point()
  }
  
  if (b2b) {
    p <- p + facet_wrap(~ isB2BSecond) +
      theme(strip.text = element_text(colour = 'white'))
  }
  
  return (ggplotly(p))
  
}

##will return a box plot based on the parameters passed
get_box_plot <- function(data, var, home_away, b2b, result) {
  #subset the data based on win/losses based on what the user wants
  if (result == 'win_only') {
    data <- data %>% filter(outcomeGame == 'W')
  } else if (result == 'loss_only') {
    data <- data %>% filter(outcomeGame == 'L')
  }
  
  p <- ggplot(data=data, aes_string(y=var)) + theme_modern_rc()
  if (home_away) {
    p <- p + geom_boxplot(aes(fill=locationGame))
  } else {
    p <- p + geom_boxplot()
  }
  
  if (b2b) {
    p <- p + facet_wrap(~ isB2BSecond) +
      theme(strip.text = element_text(colour = 'white'))
  }
  
  return (ggplotly(p))
  
}

#returns summary stats for variable(s)
get_summary_stats <- function(data, varname, home_away, b2b, result) {
  #filter based on result
  if (result == 'win_only') {
    data <- data %>% filter(outcomeGame == 'W')
  } else if (result == 'loss_only') {
    data <- data %>% filter(outcomeGame == 'L')
  }
  
  if (home_away) {
    if (b2b) {
      data <- data %>% group_by(locationGame, isB2BSecond) %>%
        summarise('# of Obs' = n(),
                  'Min' = min(!!rlang::sym(varname)),
                  '25% Quantile' = quantile(!!rlang::sym(varname), 0.25),
                  'Median' = median(!!rlang::sym(varname)),
                  'Mean' = mean(!!rlang::sym(varname)),
                  '75% Quantile' = quantile(!!rlang::sym(varname), 0.75),
                  'Max' = max(!!rlang::sym(varname))
          )
    } else {
      data <- data %>% group_by(locationGame) %>%
        summarise('# of Obs' = n(),
                  'Min' = min(!!rlang::sym(varname)),
                  '25% Quantile' = quantile(!!rlang::sym(varname), 0.25),
                  'Median' = median(!!rlang::sym(varname)),
                  'Mean' = mean(!!rlang::sym(varname)),
                  '75% Quantile' = quantile(!!rlang::sym(varname), 0.75),
                  'Max' = max(!!rlang::sym(varname))
        )
    }
  } else {
    if (b2b) {
      data <- data %>% group_by(isB2BSecond) %>%
        summarise('# of Obs' = n(),
                  'Min' = min(!!rlang::sym(varname)),
                  '25% Quantile' = quantile(!!rlang::sym(varname), 0.25),
                  'Median' = median(!!rlang::sym(varname)),
                  'Mean' = mean(!!rlang::sym(varname)),
                  '75% Quantile' = quantile(!!rlang::sym(varname), 0.75),
                  'Max' = max(!!rlang::sym(varname))
        )
    } else {
      data <- data %>% 
        summarise('# of Obs' = n(),
                  'Min' = min(!!rlang::sym(varname)),
                  '25% Quantile' = quantile(!!rlang::sym(varname), 0.25),
                  'Median' = median(!!rlang::sym(varname)),
                  'Mean' = mean(!!rlang::sym(varname)),
                  '75% Quantile' = quantile(!!rlang::sym(varname), 0.75),
                  'Max' = max(!!rlang::sym(varname))
        )
    }
  }
  
  data$var <- varname
  data <- data %>% relocate(var)
  
  return (data)
}

#this will take the data in
#iterate through it from last date to first date
#it will take the aggregate of the data for all dates before it
#for each team
#i used this to create the modeling data but the program 
#does not call it to save time (it was saved to csv)
aggregate_by_data <- function(data) {
  #new data frame to store data
  new_data <- tibble()
  #get list of dates to iterate backwards from
  dates <- distinct(data, dateGame) %>% arrange(desc(dateGame))
  for (i in 1:nrow(dates)) {
    base <- data %>% select(slugTeam, dateGame, numberGameTeamSeason,
                            isB2B, isB2BSecond, countDaysRestTeam,
                            slugOpponent, opp_numberGameTeamSeason,
                            opp_isB2B, opp_isB2BSecond, 
                            opp_countDaysRestTeam,
                            locationGame, outcomeGame,
                            pts, opp_pts) %>%
              mutate(act_net_pts = (pts - opp_pts)) %>%
              filter(dateGame == dates$dateGame[i]) %>%
              rename(act_pts = pts,
                     act_opp_pts = opp_pts)
    
    tm1 <- data %>% filter(dateGame < dates$dateGame[i]) %>%
             group_by(slugTeam) %>%
             summarise(pts1 = (sum(pts) - sum(opp_pts)) / n_distinct(dateGame),
                       fgm1 = (sum(fgm) - sum(opp_fgm)) / n_distinct(dateGame),
                       fga1 = (sum(fga) - sum(opp_fga)) / n_distinct(dateGame),
                       pctFG1 = sum(fgm) / sum(fga),
                       opp_pctFG1 = sum(opp_fgm) / sum(opp_fga),
                       fg3m1 = (sum(fg3m) - sum(opp_fg3m)) / n_distinct(dateGame),
                       fg3a1 = (sum(fg3a) - sum(opp_fg3a)) / n_distinct(dateGame), 
                       pctFG31 = sum(fg3m) / sum(fg3a),
                       opp_pctFG31 = sum(opp_fg3m) / sum(opp_fg3a),
                       fg2m1 = (sum(fg2m) - sum(opp_fg2m)) / n_distinct(dateGame),
                       fg2a1 = (sum(fg2a) - sum(opp_fg2a)) / n_distinct(dateGame), 
                       pctFG21 = sum(fg2m) / sum(fg2a),
                       opp_pctFG21 = sum(opp_fg2m) / sum(opp_fg2a),
                       ftm1 = (sum(ftm) - sum(opp_ftm)) / n_distinct(dateGame),
                       fta1 = (sum(fta) - sum(opp_fta)) / n_distinct(dateGame), 
                       pctFT1 = sum(ftm) / sum(fta),
                       opp_pctFT1 = sum(opp_ftm) / sum(opp_fta),
                       oreb1 = (sum(oreb) - sum(opp_oreb)) / n_distinct(dateGame),
                       dreb1 = (sum(dreb) - sum(opp_dreb)) / n_distinct(dateGame),
                       treb1 = (sum(treb) - sum(opp_treb)) / n_distinct(dateGame),
                       ast1 = (sum(ast) - sum(opp_ast)) / n_distinct(dateGame),
                       stl1 = (sum(stl) - sum(opp_stl)) / n_distinct(dateGame),
                       blk1 = (sum(blk) - sum(opp_blk)) / n_distinct(dateGame),
                       tov1 = (sum(tov) - sum(opp_pf)) / n_distinct(dateGame),
                       pf1 = (sum(pf) - sum(opp_pf)) / n_distinct(dateGame),
                       wins1 = sum(outcomeGame == 'W'),
                       losses1 = sum(outcomeGame == 'L')
             )
    tm2 <- data %>% filter(dateGame < dates$dateGame[i]) %>%
      group_by(slugTeam) %>%
      summarise(pts2 = (sum(pts) - sum(opp_pts)) / n_distinct(dateGame),
                fgm2 = (sum(fgm) - sum(opp_fgm)) / n_distinct(dateGame),
                fga2 = (sum(fga) - sum(opp_fga)) / n_distinct(dateGame),
                pctFG2 = sum(fgm) / sum(fga),
                opp_pctFG2 = sum(opp_fgm) / sum(opp_fga),
                fg3m2 = (sum(fg3m) - sum(opp_fg3m)) / n_distinct(dateGame),
                fg3a2 = (sum(fg3a) - sum(opp_fg3a)) / n_distinct(dateGame), 
                pctFG32 = sum(fg3m) / sum(fg3a),
                opp_pctFG32 = sum(opp_fg3m) / sum(opp_fg3a),
                fg2m2 = (sum(fg2m) - sum(opp_fg2m)) / n_distinct(dateGame),
                fg2a2 = (sum(fg2a) - sum(opp_fg2a)) / n_distinct(dateGame), 
                pctFG22 = sum(fg2m) / sum(fg2a),
                opp_pctFG22 = sum(opp_fg2m) / sum(opp_fg2a),
                ftm2 = (sum(ftm) - sum(opp_ftm)) / n_distinct(dateGame),
                fta2 = (sum(fta) - sum(opp_fta)) / n_distinct(dateGame), 
                pctFT2 = sum(ftm) / sum(fta),
                opp_pctFT2 = sum(opp_ftm) / sum(opp_fta),
                oreb2 = (sum(oreb) - sum(opp_oreb)) / n_distinct(dateGame),
                dreb2 = (sum(dreb) - sum(opp_dreb)) / n_distinct(dateGame),
                treb2 = (sum(treb) - sum(opp_treb)) / n_distinct(dateGame),
                ast2 = (sum(ast) - sum(opp_ast)) / n_distinct(dateGame),
                stl2 = (sum(stl) - sum(opp_stl)) / n_distinct(dateGame),
                blk2 = (sum(blk) - sum(opp_blk)) / n_distinct(dateGame),
                tov2 = (sum(tov) - sum(opp_pf)) / n_distinct(dateGame),
                pf2 = (sum(pf) - sum(opp_pf)) / n_distinct(dateGame),
                wins2 = sum(outcomeGame == 'W'),
                losses2 = sum(outcomeGame == 'L')
      ) %>%
      rename(slugOpponent = slugTeam)
    
    temp_data <- base %>%
      inner_join(tm1, by='slugTeam') %>%
      inner_join(tm2, by='slugOpponent')
    new_data <- rbind(new_data, temp_data)
  }#function for aggregating opponents
  return(new_data)
}

get_training_data <- function(data, split) {
  dates <- data$dateGame %>% unique()
  train_num <- round(length(dates) * split)
  train_dates <- dates[1:train_num]
  test_dates <- dates[(train_num+1):length(dates)]
  train <- data %>% filter(dateGame %in% train_dates)
  test <- data %>% filter(dateGame %in% test_dates)
  
  return (list(train=train, test=test))
}

get_lasso_fit <- function(data, vars1, vars2, vars3, folds, repeats) {
  #get list of variables to put into the model
  vars1 <- tibble(vars1)
  vars2 <- tibble(vars2)
  vars3 <- tibble(vars3)
  colnames(vars1) <- 'vars'
  colnames(vars2) <- 'vars'
  colnames(vars3) <- 'vars'
  temp <- rbind(vars1, vars2, vars3)
  temp <- var_list %>% filter(names %in% temp$vars)
  if (nrow(temp) == 0) {
    return (NULL)
  }
  
  user_vars <- 'act_net_pts'
  for (i in 1:nrow(temp)) {
    user_vars <- append(user_vars, unlist(strsplit(temp$vars[i], " ")))
  }
  user_data <- train %>% select(all_of(user_vars))
  num_vars <- ncol(user_data) - 1
  
  fit.control <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
  lasso.fit <- train(act_net_pts ~ .,
                  data=user_data,
                  preProc = c("center", "scale"),
                  method = 'glmnet',
                  trControl=fit.control,
                  tuneGrid = expand.grid(alpha = 1,
                                         lambda = seq(0.001,0.5,by = 0.01)))
  lasso.tune <- ggplot(lasso.fit) + theme_modern_rc() +
    xlab('Lambda') + ylab('RMSE') + ggtitle('Tuning')  + 
    theme(plot.title = element_text(size = 12, face = "bold"))
  pred <- predict(lasso.fit, train)
  pred <- data.frame(fitted = pred, residuals = pred-train$act_net_pts)
  lasso.resid <- ggplot(data=pred, aes(x=fitted, y=residuals)) +
    geom_point() + theme_modern_rc() +
    xlab('Fitted') + ylab('Residuals') + ggtitle('Fitted vs Residuals') +
    geom_hline(yintercept=0) + 
    theme(plot.title = element_text(size = 12, face = "bold"))

  return (list(lasso.fit = lasso.fit, 
               lasso.tune = lasso.tune,
               lasso.resid = lasso.resid,
               var_ct = num_vars))
}

get_tree_fit <- function(data, vars1, vars2, vars3, folds, repeats) {
  #get list of variables to put into the model
  vars1 <- tibble(vars1)
  vars2 <- tibble(vars2)
  vars3 <- tibble(vars3)
  colnames(vars1) <- 'vars'
  colnames(vars2) <- 'vars'
  colnames(vars3) <- 'vars'
  temp <- rbind(vars1, vars2, vars3)
  temp <- var_list %>% filter(names %in% temp$vars)
  if (nrow(temp) == 0) {
    return (NULL)
  }
  
  user_vars <- 'act_net_pts'
  for (i in 1:nrow(temp)) {
    user_vars <- append(user_vars, unlist(strsplit(temp$vars[i], " ")))
  }
  user_data <- train %>% select(all_of(user_vars))
  num_vars <- ncol(user_data) - 1
  
  fit.control <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
  tree.fit <- train(act_net_pts ~ .,
                     data=user_data,
                     method = 'rpart2',
                     trControl=fit.control,
                     tuneGrid =expand.grid(maxdepth = 2:10))
  tree.tune <- ggplot(tree.fit) + theme_modern_rc() +
    xlab('Tree Depth') + ylab('RMSE') + ggtitle('Tuning')  + 
    theme(plot.title = element_text(size = 12, face = "bold"))
  
  return (list(tree.fit = tree.fit, 
               tree.tune = tree.tune,
               var_ct = num_vars))
}

#takes in model info and keeps it in a tibble
get_model_info <- function(fit, var_ct, model_type) {
  if (model_type == 'lasso') {
    model_info <- tibble(var_ct, fit$bestTune$lambda, getTrainPerf(fit)$TrainRMSE)
  } else if (model_type == 'tree') {
    model_info <- tibble(var_ct, fit$bestTune$maxDepth, getTrainPerf(fit)$TrainRMSE)
  }
  colnames(model_info) <- c('# of Vars', 'Max Depth', 'RMSE')
  return (model_info)
}

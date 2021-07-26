dates <- model_data$dateGame %>% unique()
train_num <- round(length(dates) * 0.8)
train_dates <- dates[1:train_num]
test_dates <- dates[(train_num+1):length(dates)]
train <- model_data %>% filter(dateGame %in% train_dates)
test <- model_data %>% filter(dateGame %in% test_dates)

temp <- train %>% select(outcomeGame, pts1, pts2, wins1, losses1,
                         wins2, losses2, locationGame, blk1, blk2,
                         ast1, ast2, tov1, tov2, treb1, treb2,
                         oreb1, oreb2, dreb1, dreb2, isB2B, isB2BSecond,
                         opp_isB2B, opp_isB2BSecond, slugTeam, 
                         slugOpponent) %>% drop_na()

temp <- train %>% select(-dateGame, -act_pts, -act_opp_pts,
                         -act_net_pts) %>% drop_na()
library(caret)
fit.control <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
glm.cv <- train(outcomeGame ~ ., 
                 data=temp,
                 preProc = c("center", "scale"),
                 method = 'glm',
                 family='binomial',
                 trControl=fit.control)
glm.cv



fgm1	fga1	pctFG1	opp_pctFG1	fg3m1	fg3a1	pctFG31	opp_pctFG31	fg2m1	fg2a1	pctFG21	opp_pctFG21	ftm1	fta1	pctFT1	opp_pctFT1	oreb1	dreb1	treb1	ast1	stl1	blk1	tov1	pf1	wins1	losses1	pts2	fgm2	fga2	pctFG2	opp_pctFG2	fg3m2	fg3a2	pctFG32	opp_pctFG32	fg2m2	fg2a2	pctFG22	opp_pctFG22	ftm2	fta2	pctFT2	opp_pctFT2	oreb2	dreb2	treb2	ast2	stl2	blk2	tov2	pf2	wins2	losses2



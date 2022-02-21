current_tourn_year <- 2021
Bracket_Sim_Penalized = function(year, N, lambda)
{
  matchups = c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  regions = c("X","W","Z","Y")
  
  # Use train1 data set for known tournament results for training
  if (year != current_tourn_year)
  {
    bracket = train1[train1$Season == year,]
  }
  if (year == current_tourn_year)
  {
    # Use below for unknown predictions
    # Remove teams that lost play in game in current tournament
    bracket = kaggle1[kaggle1$Season == year,]
    bracket = bracket[bracket$Team1_Name != "Appalachian St" & bracket$Team2_Name != "Appalachian St",]
    bracket = bracket[bracket$Team1_Name != "Wichita St" & bracket$Team2_Name != "Wichita St",]
    bracket = bracket[bracket$Team1_Name != "Michigan St" & bracket$Team2_Name != "Michigan St",]
    bracket = bracket[bracket$Team1_Name != "Mt St Mary's" & bracket$Team2_Name != "Mt St Mary's",]
  }  
  
  # Add seed and region 
  results = data.frame(matrix(nrow = 64, ncol = 10))
  colnames(results) = c("Team_Id","Team_Name","Region","All_64","Top_32","Sweet_16","Elite_8",
                        "Final_4","Final","Champion")
  results[,1] = as.character(results[,1]);results[,2] = as.character(results[,2])
  results[,3] = as.character(results[,3]);results[,4] = as.numeric(results[,4])
  results[,5] = as.numeric(results[,5]);results[,6] = as.numeric(results[,6])
  results[,7] = as.numeric(results[,7]);results[,8] = as.numeric(results[,8])
  results[,9] = as.numeric(results[,9]);results[,10] = as.numeric(results[,10])
  
  Round1 = bracket[bracket$Round == 1,]
  if (sum(duplicated(Round1[, c("Season","slot")])) >= 1)
  {
    Round1 = Round1[-which(duplicated(Round1[, "slot"])),]
  }
  
  Round1_Team1 = Round1[, team1_cols]
  Round1_Team2 = Round1[, team2_cols]
  
  results$Team_Id = unique(c(sort(unique(Round1_Team1$Team1)),sort(unique(Round1_Team2$Team2))))
  results = merge(results[,-2], Teams[,-c(3,4)], by.x = "Team_Id", by.y = "TeamID")
  results1 = merge(results, Round1[, c("Team1","Seed_Team1","Team1_Region_Name","Region_Team1")], by.x = "Team_Id",
                   by.y = "Team1")
  results2 = merge(results, Round1[, c("Team2","Seed_Team2","Team2_Region_Name","Region_Team1")], by.x = "Team_Id",
                   by.y = "Team2")
  colnames(results1)[11:13] = c("Seed","Region_Name","Region")
  colnames(results2)[11:13] = c("Seed","Region_Name","Region")
  results = rbind(results1, results2)
  results$Season = rep(bracket$Season[1], 64)
  results = results[, -2]
  results = results[, c(13,1,9,10,2:8,11,12)]
  results[is.na(results)] = 0
  colnames(results)[3] = "Team_Name"
  
  # Add order by w, x, y, z 
  results = results[order(match(results$Region,regions), match(results$Seed,matchups)),]
  #results = results[order(match(results$Reg))]
  # Start for loop for simulations
  for (i in seq(1,N))
  {
    if (i%%10 == 0)
    {
      print("", quote = F)
      print(paste(i, " simulations"), quote = F)
      print("", quote = F)
    }
    results$All_64 = ifelse(results$Team_Id %in% c(Round1$Team1, Round1$Team2),results$All_64 + 1,results$All_64)
    
    ##############################################################################################
    Round2 = data.frame()
    for (i in seq(1,32,2))
    {
      Team1 = Round1_Team1[i,]
      Team2 = Round1_Team2[i,]
      
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      Game1$Team1_Win_Prob_Game1 = predict(cv.out,newx = data.matrix(Game1[, c(colnames(training_continuous))]), s=lambda, type= "response")
      Game1$Random = runif(1,0,1)
      
      Team3 = Round1_Team1[i+1,]
      Team4 = Round1_Team2[i+1,]
      
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      Game2$Team1_Win_Prob_Game2 = predict(cv.out,newx = data.matrix(Game2[, c(colnames(training_continuous))]), s=lambda, type= "response")
      Game2$Random = runif(1,0,1)
      # Call logistic regression model for predicting team 1 victory for game 2
      # Store Probabilty p for team 1 winning and 1 - p for team 2 winning
      # Random Number Generate 
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      Round2 = rbind(Round2, c)
    }
    
    results$Top_32 = ifelse(results$Team_Id %in% c(Round2$Team1, Round2$Team2),results$Top_32 + 1,results$Top_32)
    
    ###############################################################################################
    
    Round2_Team1 = Round2[, team1_cols]
    Round2_Team2 = Round2[, team2_cols]
    
    Round3 = data.frame()
    for (i in seq(1,16,2))
    {
      Team1 = Round2_Team1[i,]
      Team2 = Round2_Team2[i,]
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      Game1$Team1_Win_Prob_Game1 = predict(cv.out,newx = data.matrix(Game1[, c(colnames(training_continuous))]), s=lambda, type= "response")
      Game1$Random = runif(1,0,1)
      
      Team3 = Round2_Team1[i+1,]
      Team4 = Round2_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      Game2$Team1_Win_Prob_Game2 = predict(cv.out,newx = data.matrix(Game2[, c(colnames(training_continuous))]), s=lambda, type= "response")
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
      }
      
      Round3 = rbind(Round3, c)
    }
    results$Sweet_16 = ifelse(results$Team_Id %in% c(Round3$Team1, Round3$Team2),results$Sweet_16 + 1,results$Sweet_16)
    ##############################################################################################
    
    Round3_Team1 = Round3[, team1_cols]
    Round3_Team2 = Round3[, team2_cols]
    Round4 = data.frame()
    for (i in seq(1,8,2))
    {
      Team1 = Round3_Team1[i,]
      Team2 = Round3_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(cv.out,newx = data.matrix(Game1[, c(colnames(training_continuous))]), s=lambda, type= "response")
      Game1$Random = runif(1,0,1)
      
      Team3 = Round3_Team1[i+1,]
      Team4 = Round3_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(cv.out,newx = data.matrix(Game2[, c(colnames(training_continuous))]), s=lambda, type= "response")
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round4 = rbind(Round4, c)
    }
    Round4 = Round4[order(match(Round4$Team1_Region, regions)),]
    results$Elite_8 = ifelse(results$Team_Id %in% c(Round4$Team1, Round4$Team2),results$Elite_8 + 1,results$Elite_8)
    #############################################################################################
    
    Round4_Team1 = Round4[, team1_cols]
    Round4_Team2 = Round4[, team2_cols]
    Round5 = data.frame()
    
    for (i in seq(1,4,2))
    {
      Team1 = Round4_Team1[i,]
      Team2 = Round4_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(cv.out,newx = data.matrix(Game1[, c(colnames(training_continuous))]), s=lambda, type= "response")
      Game1$Random = runif(1,0,1)
      
      Team3 = Round4_Team1[i+1,]
      Team4 = Round4_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(cv.out,newx = data.matrix(Game2[, c(colnames(training_continuous))]), s=lambda, type= "response")
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
      }
      
      Round5 = rbind(Round5, c)
    }
    
    
    results$Final_4 = ifelse(results$Team_Id %in% c(Round5$Team1, Round5$Team2),results$Final_4 + 1,results$Final_4)
    #############################################################################################
    Round5_Team1 = Round5[, team1_cols]
    Round5_Team2 = Round5[, team2_cols]
    Round6 = data.frame()
    for (i in seq(1,1))
    {
      Team1 = Round5_Team1[i,]
      Team2 = Round5_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(cv.out,newx = data.matrix(Game1[, c(colnames(training_continuous))]), s=lambda, type= "response")
      Game1$Random = runif(1,0,1)
      
      Team3 = Round5_Team1[i+1,]
      Team4 = Round5_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(cv.out,newx = data.matrix(Game2[, c(colnames(training_continuous))]), s=lambda, type= "response")
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round6 = rbind(Round6, c)
    }
    
    Team1_Win_Prob_Game1 = predict(cv.out,newx = data.matrix(Round6[, c(colnames(training_continuous))]), s=lambda, type= "response")
    random = runif(1,0,1)
    if (Team1_Win_Prob_Game1 <= random)
    {
      print(paste0(Round6$Team1_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team1),results$Champion + 1,results$Champion)
    } else {
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team2),results$Champion + 1,results$Champion)
      print(paste0(Round6$Team2_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
    }
    results$Final = ifelse(results$Team_Id %in% c(Round6$Team1, Round6$Team2),results$Final + 1,results$Final)
  }
  results = results[, c("Season","Region_Name","Seed","Team_Name","Top_32","Sweet_16","Elite_8","Final_4","Final","Champion")]
  return(results)
}

Bracket_Sim_GLMNET = function(year, N)
{
  training_continuous <- training_continuous %>% dplyr::select(-Team1_Victory)
  matchups = c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  regions = c("X","W","Z","Y")
  
  # Use train1 data set for known tournament results for training
  if (year != current_tourn_year)
  {
    bracket = train1[train1$Season == year,]
  }
  if (year == current_tourn_year)
  {
    # Use below for unknown predictions
    # Remove teams that lost play in game in current tournament
    bracket = kaggle1[kaggle1$Season == year,]
    
    bracket = bracket[bracket$Team1_Name != "Appalachian St" & bracket$Team2_Name != "Appalachian St",]
    bracket = bracket[bracket$Team1_Name != "Wichita St" & bracket$Team2_Name != "Wichita St",]
    bracket = bracket[bracket$Team1_Name != "Michigan St" & bracket$Team2_Name != "Michigan St",]
    bracket = bracket[bracket$Team1_Name != "Mt St Mary's" & bracket$Team2_Name != "Mt St Mary's",]
  }  
  
  # Add seed and region 
  results = data.frame(matrix(nrow = 64, ncol = 10))
  colnames(results) = c("Team_Id","Team_Name","Region","All_64","Top_32","Sweet_16","Elite_8",
                        "Final_4","Final","Champion")
  results[,1] = as.character(results[,1]);results[,2] = as.character(results[,2])
  results[,3] = as.character(results[,3]);results[,4] = as.numeric(results[,4])
  results[,5] = as.numeric(results[,5]);results[,6] = as.numeric(results[,6])
  results[,7] = as.numeric(results[,7]);results[,8] = as.numeric(results[,8])
  results[,9] = as.numeric(results[,9]);results[,10] = as.numeric(results[,10])
  
  
  Round1 = bracket[bracket$Round == 1,]
  if (sum(duplicated(Round1[, c("Season","slot")])) >= 1)
  {
    Round1 = Round1[-which(duplicated(Round1[, "slot"])),]
  }
  
  Round1_Team1 = Round1[, team1_cols]
  Round1_Team2 = Round1[, team2_cols]
  
  results$Team_Id = unique(c(sort(unique(Round1_Team1$Team1)),sort(unique(Round1_Team2$Team2))))
  results = merge(results[,-2], Teams[,-c(3,4)], by.x = "Team_Id", by.y = "TeamID")
  results1 = merge(results, Round1[, c("Team1","Seed_Team1","Team1_Region_Name","Region_Team1")], by.x = "Team_Id",
                   by.y = "Team1")
  results2 = merge(results, Round1[, c("Team2","Seed_Team2","Team2_Region_Name","Region_Team2")], by.x = "Team_Id",
                   by.y = "Team2")
  colnames(results1)[11:13] = c("Seed","Region_Name","Region")
  colnames(results2)[11:13] = c("Seed","Region_Name","Region")
  results = rbind(results1, results2)
  results$Season = rep(bracket$Season[1], 64)
  results = results[, -2]
  results = results[, c(13,1,9,10,2:8,11,12)]
  results[is.na(results)] = 0
  colnames(results)[3] = "Team_Name"
  
  # Add order by w, x, y, z 
  results = results[order(match(results$Region,regions), match(results$Seed,matchups)),]
  #results = results[order(match(results$Reg))]
  # Start for loop for simulations
  for (i in seq(1,N))
  {
    if (i%%10 == 0)
    {
      print("", quote = F)
      print(paste(i, " simulations"), quote = F)
      print("", quote = F)
    }
    results$All_64 = ifelse(results$Team_Id %in% c(Round1$Team1, Round1$Team2),results$All_64 + 1,results$All_64)
    
    ##############################################################################################
    Round2 = data.frame()
    for (i in seq(1,32,2))
    {
      Team1 = Round1_Team1[i,]
      Team2 = Round1_Team2[i,]
      
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      Game1$Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round1_Team1[i+1,]
      Team4 = Round1_Team2[i+1,]
      
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      Game2$Team1_Win_Prob_Game2 = predict(mod[[1]],newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      # Call logistic regression model for predicting team 1 victory for game 2
      # Store Probabilty p for team 1 winning and 1 - p for team 2 winning
      # Random Number Generate 
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      Round2 = rbind(Round2, c)
    }
    
    results$Top_32 = ifelse(results$Team_Id %in% c(Round2$Team1, Round2$Team2),results$Top_32 + 1,results$Top_32)
    
    ###############################################################################################
    
    Round2_Team1 = Round2[, team1_cols]
    Round2_Team2 = Round2[, team2_cols]
    
    Round3 = data.frame()
    for (i in seq(1,16,2))
    {
      Team1 = Round2_Team1[i,]
      Team2 = Round2_Team2[i,]
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      Game1$Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round2_Team1[i+1,]
      Team4 = Round2_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      Game2$Team1_Win_Prob_Game2 = predict(mod[[1]],newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        #c = cbind(Team1, Team3) 
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = 2
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      }
      
      Round3 = rbind(Round3, c)
    }
    
    results$Sweet_16 = ifelse(results$Team_Id %in% c(Round3$Team1, Round3$Team2),results$Sweet_16 + 1,results$Sweet_16)
    ##############################################################################################
    
    Round3_Team1 = Round3[, team1_cols]
    Round3_Team2 = Round3[, team2_cols]
    Round4 = data.frame()
    for (i in seq(1,8,2))
    {
      Team1 = Round3_Team1[i,]
      Team2 = Round3_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round3_Team1[i+1,]
      Team4 = Round3_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(mod[[1]], newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round4 = rbind(Round4, c)
    }
    Round4 = Round4[order(match(Round4$Team1_Region, regions)),]
    results$Elite_8 = ifelse(results$Team_Id %in% c(Round4$Team1, Round4$Team2),results$Elite_8 + 1,results$Elite_8)
    #############################################################################################
    
    Round4_Team1 = Round4[, team1_cols]
    Round4_Team2 = Round4[, team2_cols]
    Round5 = data.frame()
    
    for (i in seq(1,4,2))
    {
      Team1 = Round4_Team1[i,]
      Team2 = Round4_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round4_Team1[i+1,]
      Team4 = Round4_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(mod[[1]],newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
      }
      
      Round5 = rbind(Round5, c)
    }
    
    
    results$Final_4 = ifelse(results$Team_Id %in% c(Round5$Team1, Round5$Team2),results$Final_4 + 1,results$Final_4)
    #############################################################################################
    Round5_Team1 = Round5[, team1_cols]
    Round5_Team2 = Round5[, team2_cols]
    Round6 = data.frame()
    for (i in seq(1,1))
    {
      Team1 = Round5_Team1[i,]
      Team2 = Round5_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round5_Team1[i+1,]
      Team4 = Round5_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(mod[[1]],newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round6 = rbind(Round6, c)
    }
    
    Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
    random = runif(1,0,1)
    if (Team1_Win_Prob_Game1 <= random)
    {
      print(paste0(Round6$Team1_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team1),results$Champion + 1,results$Champion)
    } else {
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team2),results$Champion + 1,results$Champion)
      print(paste0(Round6$Team2_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
    }
    results$Final = ifelse(results$Team_Id %in% c(Round6$Team1, Round6$Team2),results$Final + 1,results$Final)
  }
  results = results[, c("Season","Region_Name","Seed","Team_Name","Top_32","Sweet_16","Elite_8","Final_4","Final","Champion")]
  return(results)
}

Bracket_Sim_RF = function(year, N)
{
  training_continuous <- training_continuous %>% dplyr::select(-Team1_Victory)
  matchups = c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  regions = c("X","W","Z","Y")
  
  # Use train1 data set for known tournament results for training
  if (year != current_tourn_year)
  {
    bracket = train1[train1$Season == year,]
  }
  if (year == current_tourn_year)
  {
    # Use below for unknown predictions
    # Remove teams that lost play in game in current tournament
    bracket = kaggle1[kaggle1$Season == year,]
    
    bracket = bracket[bracket$Team1_Name != "Appalachian St" & bracket$Team2_Name != "Appalachian St",]
    bracket = bracket[bracket$Team1_Name != "Wichita St" & bracket$Team2_Name != "Wichita St",]
    bracket = bracket[bracket$Team1_Name != "Michigan St" & bracket$Team2_Name != "Michigan St",]
    bracket = bracket[bracket$Team1_Name != "Mt St Mary's" & bracket$Team2_Name != "Mt St Mary's",]
  }  
  
  # Add seed and region 
  results = data.frame(matrix(nrow = 64, ncol = 10))
  colnames(results) = c("Team_Id","Team_Name","Region","All_64","Top_32","Sweet_16","Elite_8",
                        "Final_4","Final","Champion")
  results[,1] = as.character(results[,1]);results[,2] = as.character(results[,2])
  results[,3] = as.character(results[,3]);results[,4] = as.numeric(results[,4])
  results[,5] = as.numeric(results[,5]);results[,6] = as.numeric(results[,6])
  results[,7] = as.numeric(results[,7]);results[,8] = as.numeric(results[,8])
  results[,9] = as.numeric(results[,9]);results[,10] = as.numeric(results[,10])
  
  
  Round1 = bracket[bracket$Round == 1,]
  if (sum(duplicated(Round1[, c("Season","slot")])) >= 1)
  {
    Round1 = Round1[-which(duplicated(Round1[, "slot"])),]
  }
  
  Round1_Team1 = Round1[, team1_cols]
  Round1_Team2 = Round1[, team2_cols]
  
  results$Team_Id = unique(c(sort(unique(Round1_Team1$Team1)),sort(unique(Round1_Team2$Team2))))
  results = merge(results[,-2], Teams[,-c(3,4)], by.x = "Team_Id", by.y = "TeamID")
  results1 = merge(results, Round1[, c("Team1","Seed_Team1","Team1_Region_Name","Region_Team1")], by.x = "Team_Id",
                   by.y = "Team1")
  results2 = merge(results, Round1[, c("Team2","Seed_Team2","Team2_Region_Name","Region_Team2")], by.x = "Team_Id",
                   by.y = "Team2")
  colnames(results1)[11:13] = c("Seed","Region_Name","Region")
  colnames(results2)[11:13] = c("Seed","Region_Name","Region")
  results = rbind(results1, results2)
  results$Season = rep(bracket$Season[1], 64)
  results = results[, -2]
  results = results[, c(13,1,9,10,2:8,11,12)]
  results[is.na(results)] = 0
  colnames(results)[3] = "Team_Name"
  
  # Add order by w, x, y, z 
  results = results[order(match(results$Region,regions), match(results$Seed,matchups)),]
  #results = results[order(match(results$Reg))]
  # Start for loop for simulations
  for (i in seq(1,N))
  {
    if (i%%10 == 0)
    {
      print("", quote = F)
      print(paste(i, " simulations"), quote = F)
      print("", quote = F)
    }
    results$All_64 = ifelse(results$Team_Id %in% c(Round1$Team1, Round1$Team2),results$All_64 + 1,results$All_64)
    
    ##############################################################################################
    Round2 = data.frame()
    for (i in seq(1,32,2))
    {
      Team1 = Round1_Team1[i,]
      Team2 = Round1_Team2[i,]
      
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      Game1$Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round1_Team1[i+1,]
      Team4 = Round1_Team2[i+1,]
      
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      Game2$Team1_Win_Prob_Game2 = predict(mod[[1]],newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      # Call logistic regression model for predicting team 1 victory for game 2
      # Store Probabilty p for team 1 winning and 1 - p for team 2 winning
      # Random Number Generate 
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      Round2 = rbind(Round2, c)
    }
    
    results$Top_32 = ifelse(results$Team_Id %in% c(Round2$Team1, Round2$Team2),results$Top_32 + 1,results$Top_32)
    
    ###############################################################################################
    
    Round2_Team1 = Round2[, team1_cols]
    Round2_Team2 = Round2[, team2_cols]
    
    Round3 = data.frame()
    for (i in seq(1,16,2))
    {
      Team1 = Round2_Team1[i,]
      Team2 = Round2_Team2[i,]
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      Game1$Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round2_Team1[i+1,]
      Team4 = Round2_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      Game2$Team1_Win_Prob_Game2 = predict(mod[[1]],newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        #c = cbind(Team1, Team3) 
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = 2
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      }
      
      Round3 = rbind(Round3, c)
    }
    
    results$Sweet_16 = ifelse(results$Team_Id %in% c(Round3$Team1, Round3$Team2),results$Sweet_16 + 1,results$Sweet_16)
    ##############################################################################################
    
    Round3_Team1 = Round3[, team1_cols]
    Round3_Team2 = Round3[, team2_cols]
    Round4 = data.frame()
    for (i in seq(1,8,2))
    {
      Team1 = Round3_Team1[i,]
      Team2 = Round3_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round3_Team1[i+1,]
      Team4 = Round3_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(mod[[1]],newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round4 = rbind(Round4, c)
    }
    Round4 = Round4[order(match(Round4$Team1_Region, regions)),]
    results$Elite_8 = ifelse(results$Team_Id %in% c(Round4$Team1, Round4$Team2),results$Elite_8 + 1,results$Elite_8)
    #############################################################################################
    
    Round4_Team1 = Round4[, team1_cols]
    Round4_Team2 = Round4[, team2_cols]
    Round5 = data.frame()
    
    for (i in seq(1,4,2))
    {
      Team1 = Round4_Team1[i,]
      Team2 = Round4_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round4_Team1[i+1,]
      Team4 = Round4_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(mod[[1]],newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
      }
      
      Round5 = rbind(Round5, c)
    }
    
    
    results$Final_4 = ifelse(results$Team_Id %in% c(Round5$Team1, Round5$Team2),results$Final_4 + 1,results$Final_4)
    #############################################################################################
    Round5_Team1 = Round5[, team1_cols]
    Round5_Team2 = Round5[, team2_cols]
    Round6 = data.frame()
    for (i in seq(1,1))
    {
      Team1 = Round5_Team1[i,]
      Team2 = Round5_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round5_Team1[i+1,]
      Team4 = Round5_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(mod[[1]],newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round6 = rbind(Round6, c)
    }
    
    Team1_Win_Prob_Game1 = predict(mod[[1]],newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
    random = runif(1,0,1)
    if (Team1_Win_Prob_Game1 <= random)
    {
      print(paste0(Round6$Team1_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team1),results$Champion + 1,results$Champion)
    } else {
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team2),results$Champion + 1,results$Champion)
      print(paste0(Round6$Team2_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
    }
    results$Final = ifelse(results$Team_Id %in% c(Round6$Team1, Round6$Team2),results$Final + 1,results$Final)
  }
  results = results[, c("Season","Region_Name","Seed","Team_Name","Top_32","Sweet_16","Elite_8","Final_4","Final","Champion")]
  return(results)
}

Bracket_Sim_GBM = function(year, N)
{
  training_continuous <- training_continuous %>% dplyr::select(-Team1_Victory)
  matchups = c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  regions = c("X","W","Z","Y")
  
  # Use train1 data set for known tournament results for training
  if (year != current_tourn_year)
  {
    bracket = train1[train1$Season == year,]
  }
  if (year == current_tourn_year)
  {
    # Use below for unknown predictions
    # Remove teams that lost play in game in current tournament
    bracket = kaggle1[kaggle1$Season == year,]
    
    bracket = bracket[bracket$Team1_Name != "Appalachian St" & bracket$Team2_Name != "Appalachian St",]
    bracket = bracket[bracket$Team1_Name != "Wichita St" & bracket$Team2_Name != "Wichita St",]
    bracket = bracket[bracket$Team1_Name != "Michigan St" & bracket$Team2_Name != "Michigan St",]
    bracket = bracket[bracket$Team1_Name != "Mt St Mary's" & bracket$Team2_Name != "Mt St Mary's",]
  }  
  
  # Add seed and region 
  results = data.frame(matrix(nrow = 64, ncol = 10))
  colnames(results) = c("Team_Id","Team_Name","Region","All_64","Top_32","Sweet_16","Elite_8",
                        "Final_4","Final","Champion")
  results[,1] = as.character(results[,1]);results[,2] = as.character(results[,2])
  results[,3] = as.character(results[,3]);results[,4] = as.numeric(results[,4])
  results[,5] = as.numeric(results[,5]);results[,6] = as.numeric(results[,6])
  results[,7] = as.numeric(results[,7]);results[,8] = as.numeric(results[,8])
  results[,9] = as.numeric(results[,9]);results[,10] = as.numeric(results[,10])
  
  
  Round1 = bracket[bracket$Round == 1,]
  if (sum(duplicated(Round1[, c("Season","slot")])) >= 1)
  {
    Round1 = Round1[-which(duplicated(Round1[, "slot"])),]
  }
  
  Round1_Team1 = Round1[, team1_cols]
  Round1_Team2 = Round1[, team2_cols]
  
  results$Team_Id = unique(c(sort(unique(Round1_Team1$Team1)),sort(unique(Round1_Team2$Team2))))
  results = merge(results[,-2], Teams[,-c(3,4)], by.x = "Team_Id", by.y = "TeamID")
  results1 = merge(results, Round1[, c("Team1","Seed_Team1","Team1_Region_Name","Region_Team1")], by.x = "Team_Id",
                   by.y = "Team1")
  results2 = merge(results, Round1[, c("Team2","Seed_Team2","Team2_Region_Name","Region_Team2")], by.x = "Team_Id",
                   by.y = "Team2")
  colnames(results1)[11:13] = c("Seed","Region_Name","Region")
  colnames(results2)[11:13] = c("Seed","Region_Name","Region")
  results = rbind(results1, results2)
  results$Season = rep(bracket$Season[1], 64)
  results = results[, -2]
  results = results[, c(13,1,9,10,2:8,11,12)]
  results[is.na(results)] = 0
  colnames(results)[3] = "Team_Name"
  
  # Add order by w, x, y, z 
  results = results[order(match(results$Region,regions), match(results$Seed,matchups)),]
  #results = results[order(match(results$Reg))]
  # Start for loop for simulations
  for (i in seq(1,N))
  {
    if (i%%10 == 0)
    {
      print("", quote = F)
      print(paste(i, " simulations"), quote = F)
      print("", quote = F)
    }
    results$All_64 = ifelse(results$Team_Id %in% c(Round1$Team1, Round1$Team2),results$All_64 + 1,results$All_64)
    
    ##############################################################################################
    Round2 = data.frame()
    for (i in seq(1,32,2))
    {
      Team1 = Round1_Team1[i,]
      Team2 = Round1_Team2[i,]
      
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      Game1$Team1_Win_Prob_Game1 = predict(gbm,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round1_Team1[i+1,]
      Team4 = Round1_Team2[i+1,]
      
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      Game2$Team1_Win_Prob_Game2 = predict(gbm,newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      # Call logistic regression model for predicting team 1 victory for game 2
      # Store Probabilty p for team 1 winning and 1 - p for team 2 winning
      # Random Number Generate 
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      Round2 = rbind(Round2, c)
    }
    
    results$Top_32 = ifelse(results$Team_Id %in% c(Round2$Team1, Round2$Team2),results$Top_32 + 1,results$Top_32)
    
    ###############################################################################################
    
    Round2_Team1 = Round2[, team1_cols]
    Round2_Team2 = Round2[, team2_cols]
    
    Round3 = data.frame()
    for (i in seq(1,16,2))
    {
      Team1 = Round2_Team1[i,]
      Team2 = Round2_Team2[i,]
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      Game1$Team1_Win_Prob_Game1 = predict(gbm,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round2_Team1[i+1,]
      Team4 = Round2_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      Game2$Team1_Win_Prob_Game2 = predict(gbm,newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        #c = cbind(Team1, Team3) 
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = 2
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      }
      
      Round3 = rbind(Round3, c)
    }
    
    results$Sweet_16 = ifelse(results$Team_Id %in% c(Round3$Team1, Round3$Team2),results$Sweet_16 + 1,results$Sweet_16)
    ##############################################################################################
    
    Round3_Team1 = Round3[, team1_cols]
    Round3_Team2 = Round3[, team2_cols]
    Round4 = data.frame()
    for (i in seq(1,8,2))
    {
      Team1 = Round3_Team1[i,]
      Team2 = Round3_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(gbm,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round3_Team1[i+1,]
      Team4 = Round3_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(gbm,newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round4 = rbind(Round4, c)
    }
    Round4 = Round4[order(match(Round4$Team1_Region, regions)),]
    results$Elite_8 = ifelse(results$Team_Id %in% c(Round4$Team1, Round4$Team2),results$Elite_8 + 1,results$Elite_8)
    #############################################################################################
    
    Round4_Team1 = Round4[, team1_cols]
    Round4_Team2 = Round4[, team2_cols]
    Round5 = data.frame()
    
    for (i in seq(1,4,2))
    {
      Team1 = Round4_Team1[i,]
      Team2 = Round4_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(gbm,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round4_Team1[i+1,]
      Team4 = Round4_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(gbm,newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
      }
      
      Round5 = rbind(Round5, c)
    }
    
    
    results$Final_4 = ifelse(results$Team_Id %in% c(Round5$Team1, Round5$Team2),results$Final_4 + 1,results$Final_4)
    #############################################################################################
    Round5_Team1 = Round5[, team1_cols]
    Round5_Team2 = Round5[, team2_cols]
    Round6 = data.frame()
    for (i in seq(1,1))
    {
      Team1 = Round5_Team1[i,]
      Team2 = Round5_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(gbm,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round5_Team1[i+1,]
      Team4 = Round5_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(gbm,newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round6 = rbind(Round6, c)
    }
    
    Team1_Win_Prob_Game1 = predict(gbm,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
    random = runif(1,0,1)
    if (Team1_Win_Prob_Game1 <= random)
    {
      print(paste0(Round6$Team1_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team1),results$Champion + 1,results$Champion)
    } else {
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team2),results$Champion + 1,results$Champion)
      print(paste0(Round6$Team2_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
    }
    results$Final = ifelse(results$Team_Id %in% c(Round6$Team1, Round6$Team2),results$Final + 1,results$Final)
  }
  results = results[, c("Season","Region_Name","Seed","Team_Name","Top_32","Sweet_16","Elite_8","Final_4","Final","Champion")]
  return(results)
}

Bracket_Sim_XGBoost = function(year, N)
{
  training_continuous <- training_continuous %>% dplyr::select(-Team1_Victory)
  matchups = c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  regions = c("X","W","Z","Y")
  
  # Use train1 data set for known tournament results for training
  if (year != current_tourn_year)
  {
    bracket = train1[train1$Season == year,]
  }
  if (year == current_tourn_year)
  {
    # Use below for unknown predictions
    # Remove teams that lost play in game in current tournament
    bracket = kaggle1[kaggle1$Season == year,]
    
    bracket = bracket[bracket$Team1_Name != "Appalachian St" & bracket$Team2_Name != "Appalachian St",]
    bracket = bracket[bracket$Team1_Name != "Wichita St" & bracket$Team2_Name != "Wichita St",]
    bracket = bracket[bracket$Team1_Name != "Michigan St" & bracket$Team2_Name != "Michigan St",]
    bracket = bracket[bracket$Team1_Name != "Mt St Mary's" & bracket$Team2_Name != "Mt St Mary's",]
  }  
  
  # Add seed and region 
  results = data.frame(matrix(nrow = 64, ncol = 10))
  colnames(results) = c("Team_Id","Team_Name","Region","All_64","Top_32","Sweet_16","Elite_8",
                        "Final_4","Final","Champion")
  results[,1] = as.character(results[,1]);results[,2] = as.character(results[,2])
  results[,3] = as.character(results[,3]);results[,4] = as.numeric(results[,4])
  results[,5] = as.numeric(results[,5]);results[,6] = as.numeric(results[,6])
  results[,7] = as.numeric(results[,7]);results[,8] = as.numeric(results[,8])
  results[,9] = as.numeric(results[,9]);results[,10] = as.numeric(results[,10])
  
  
  Round1 = bracket[bracket$Round == 1,]
  if (sum(duplicated(Round1[, c("Season","slot")])) >= 1)
  {
    Round1 = Round1[-which(duplicated(Round1[, "slot"])),]
  }
  
  Round1_Team1 = Round1[, team1_cols]
  Round1_Team2 = Round1[, team2_cols]
  
  results$Team_Id = unique(c(sort(unique(Round1_Team1$Team1)),sort(unique(Round1_Team2$Team2))))
  results = merge(results[,-2], Teams[,-c(3,4)], by.x = "Team_Id", by.y = "TeamID")
  results1 = merge(results, Round1[, c("Team1","Seed_Team1","Team1_Region_Name","Region_Team1")], by.x = "Team_Id",
                   by.y = "Team1")
  results2 = merge(results, Round1[, c("Team2","Seed_Team2","Team2_Region_Name","Region_Team2")], by.x = "Team_Id",
                   by.y = "Team2")
  colnames(results1)[11:13] = c("Seed","Region_Name","Region")
  colnames(results2)[11:13] = c("Seed","Region_Name","Region")
  results = rbind(results1, results2)
  results$Season = rep(bracket$Season[1], 64)
  results = results[, -2]
  results = results[, c(13,1,9,10,2:8,11,12)]
  results[is.na(results)] = 0
  colnames(results)[3] = "Team_Name"
  
  # Add order by w, x, y, z 
  results = results[order(match(results$Region,regions), match(results$Seed,matchups)),]
  #results = results[order(match(results$Reg))]
  # Start for loop for simulations
  for (i in seq(1,N))
  {
    if (i%%10 == 0)
    {
      print("", quote = F)
      print(paste(i, " simulations"), quote = F)
      print("", quote = F)
    }
    results$All_64 = ifelse(results$Team_Id %in% c(Round1$Team1, Round1$Team2),results$All_64 + 1,results$All_64)
    
    ##############################################################################################
    Round2 = data.frame()
    for (i in seq(1,32,2))
    {
      Team1 = Round1_Team1[i,]
      Team2 = Round1_Team2[i,]
      
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      Game1$Team1_Win_Prob_Game1 = predict(xgboost,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round1_Team1[i+1,]
      Team4 = Round1_Team2[i+1,]
      
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      Game2$Team1_Win_Prob_Game2 = predict(xgboost,newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      # Call logistic regression model for predicting team 1 victory for game 2
      # Store Probabilty p for team 1 winning and 1 - p for team 2 winning
      # Random Number Generate 
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      Round2 = rbind(Round2, c)
    }
    
    results$Top_32 = ifelse(results$Team_Id %in% c(Round2$Team1, Round2$Team2),results$Top_32 + 1,results$Top_32)
    
    ###############################################################################################
    
    Round2_Team1 = Round2[, team1_cols]
    Round2_Team2 = Round2[, team2_cols]
    
    Round3 = data.frame()
    for (i in seq(1,16,2))
    {
      Team1 = Round2_Team1[i,]
      Team2 = Round2_Team2[i,]
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      Game1$Team1_Win_Prob_Game1 = predict(xgboost,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round2_Team1[i+1,]
      Team4 = Round2_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      Game2$Team1_Win_Prob_Game2 = predict(xgboost,newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        #c = cbind(Team1, Team3) 
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = 2
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      }
      
      Round3 = rbind(Round3, c)
    }
    
    results$Sweet_16 = ifelse(results$Team_Id %in% c(Round3$Team1, Round3$Team2),results$Sweet_16 + 1,results$Sweet_16)
    ##############################################################################################
    
    Round3_Team1 = Round3[, team1_cols]
    Round3_Team2 = Round3[, team2_cols]
    Round4 = data.frame()
    for (i in seq(1,8,2))
    {
      Team1 = Round3_Team1[i,]
      Team2 = Round3_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(xgboost,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round3_Team1[i+1,]
      Team4 = Round3_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(xgboost,newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round4 = rbind(Round4, c)
    }
    Round4 = Round4[order(match(Round4$Team1_Region, regions)),]
    results$Elite_8 = ifelse(results$Team_Id %in% c(Round4$Team1, Round4$Team2),results$Elite_8 + 1,results$Elite_8)
    #############################################################################################
    
    Round4_Team1 = Round4[, team1_cols]
    Round4_Team2 = Round4[, team2_cols]
    Round5 = data.frame()
    
    for (i in seq(1,4,2))
    {
      Team1 = Round4_Team1[i,]
      Team2 = Round4_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(xgboost,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round4_Team1[i+1,]
      Team4 = Round4_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(xgboost,newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
      }
      
      Round5 = rbind(Round5, c)
    }
    
    
    results$Final_4 = ifelse(results$Team_Id %in% c(Round5$Team1, Round5$Team2),results$Final_4 + 1,results$Final_4)
    #############################################################################################
    Round5_Team1 = Round5[, team1_cols]
    Round5_Team2 = Round5[, team2_cols]
    Round6 = data.frame()
    for (i in seq(1,1))
    {
      Team1 = Round5_Team1[i,]
      Team2 = Round5_Team2[i,]
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob_Game1 = predict(xgboost,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round5_Team1[i+1,]
      Team4 = Round5_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob_Game2 = predict(xgboost,newdata = Game2[, c(colnames(training_continuous))], type= "prob")[2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round6 = rbind(Round6, c)
    }
    
    Team1_Win_Prob_Game1 = predict(xgboost,newdata = Game1[, c(colnames(training_continuous))], type= "prob")[2]
    random = runif(1,0,1)
    if (Team1_Win_Prob_Game1 <= random)
    {
      print(paste0(Round6$Team1_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team1),results$Champion + 1,results$Champion)
    } else {
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team2),results$Champion + 1,results$Champion)
      print(paste0(Round6$Team2_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
    }
    results$Final = ifelse(results$Team_Id %in% c(Round6$Team1, Round6$Team2),results$Final + 1,results$Final)
  }
  results = results[, c("Season","Region_Name","Seed","Team_Name","Top_32","Sweet_16","Elite_8","Final_4","Final","Champion")]
  return(results)
}

Bracket_Sim_NN = function(year, N)
{
  training_continuous <- training_continuous %>% dplyr::select(-Team1_Victory)
  matchups = c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  regions = c("X","W","Z","Y")
  
  # Use train1 data set for known tournament results for training
  if (year != current_tourn_year)
  {
    bracket = train1[train1$Season == year,]
  }
  if (year == current_tourn_year)
  {
    # Use below for unknown predictions
    # Remove teams that lost play in game in current tournament
    bracket = kaggle1[kaggle1$Season == year,]
    
    bracket = bracket[bracket$Team1_Name != "Appalachian St" & bracket$Team2_Name != "Appalachian St",]
    bracket = bracket[bracket$Team1_Name != "Wichita St" & bracket$Team2_Name != "Wichita St",]
    bracket = bracket[bracket$Team1_Name != "Michigan St" & bracket$Team2_Name != "Michigan St",]
    bracket = bracket[bracket$Team1_Name != "Mt St Mary's" & bracket$Team2_Name != "Mt St Mary's",]
  }  
  
  # Add seed and region 
  results = data.frame(matrix(nrow = 64, ncol = 10))
  colnames(results) = c("Team_Id","Team_Name","Region","All_64","Top_32","Sweet_16","Elite_8",
                        "Final_4","Final","Champion")
  results[,1] = as.character(results[,1]);results[,2] = as.character(results[,2])
  results[,3] = as.character(results[,3]);results[,4] = as.numeric(results[,4])
  results[,5] = as.numeric(results[,5]);results[,6] = as.numeric(results[,6])
  results[,7] = as.numeric(results[,7]);results[,8] = as.numeric(results[,8])
  results[,9] = as.numeric(results[,9]);results[,10] = as.numeric(results[,10])
  
  
  Round1 = bracket[bracket$Round == 1,]
  if (sum(duplicated(Round1[, c("Season","slot")])) >= 1)
  {
    Round1 = Round1[-which(duplicated(Round1[, "slot"])),]
  }
  
  Round1_Team1 = Round1[, team1_cols]
  Round1_Team2 = Round1[, team2_cols]
  
  results$Team_Id = unique(c(sort(unique(Round1_Team1$Team1)),sort(unique(Round1_Team2$Team2))))
  results = merge(results[,-2], Teams[,-c(3,4)], by.x = "Team_Id", by.y = "TeamID")
  results1 = merge(results, Round1[, c("Team1","Seed_Team1","Team1_Region_Name","Region_Team1")], by.x = "Team_Id",
                   by.y = "Team1")
  results2 = merge(results, Round1[, c("Team2","Seed_Team2","Team2_Region_Name","Region_Team2")], by.x = "Team_Id",
                   by.y = "Team2")
  colnames(results1)[11:13] = c("Seed","Region_Name","Region")
  colnames(results2)[11:13] = c("Seed","Region_Name","Region")
  results = rbind(results1, results2)
  results$Season = rep(bracket$Season[1], 64)
  results = results[, -2]
  results = results[, c(13,1,9,10,2:8,11,12)]
  results[is.na(results)] = 0
  colnames(results)[3] = "Team_Name"
  
  # Add order by w, x, y, z 
  results = results[order(match(results$Region,regions), match(results$Seed,matchups)),]
  #results = results[order(match(results$Reg))]
  # Start for loop for simulations
  for (i in seq(1,N))
  {
    if (i%%10 == 0)
    {
      print("", quote = F)
      print(paste(i, " simulations"), quote = F)
      print("", quote = F)
    }
    results$All_64 = ifelse(results$Team_Id %in% c(Round1$Team1, Round1$Team2),results$All_64 + 1,results$All_64)
    
    ##############################################################################################
    Round2 = data.frame()
    for (i in seq(1,32,2))
    {
      Team1 = Round1_Team1[i,]
      Team2 = Round1_Team2[i,]
      
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1[, c(colnames(training_continuous))]))
      Game1$Team1_Win_Prob_Game1 <- nn_prob[,2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round1_Team1[i+1,]
      Team4 = Round1_Team2[i+1,]
      
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game2[, c(colnames(training_continuous))]))
      Game2$Team1_Win_Prob_Game2 <- nn_prob[,2]
      Game2$Random = runif(1,0,1)
      # Call logistic regression model for predicting team 1 victory for game 2
      # Store Probabilty p for team 1 winning and 1 - p for team 2 winning
      # Random Number Generate 
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      Round2 = rbind(Round2, c)
    }
    
    results$Top_32 = ifelse(results$Team_Id %in% c(Round2$Team1, Round2$Team2),results$Top_32 + 1,results$Top_32)
    
    ###############################################################################################
    
    Round2_Team1 = Round2[, team1_cols]
    Round2_Team2 = Round2[, team2_cols]
    
    Round3 = data.frame()
    for (i in seq(1,16,2))
    {
      Team1 = Round2_Team1[i,]
      Team2 = Round2_Team2[i,]
      Game1 = cbind(Team1, Team2)
      #print(c(Game1$Team1_Name, Game1$Team2_Name, Game1$Round2,i))
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1[, c(colnames(training_continuous))]))
      Game1$Team1_Win_Prob_Game1 <- nn_prob[,2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round2_Team1[i+1,]
      Team4 = Round2_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$Team1_Name, Game2$Team2_Name, Game2$Round2,i+1))
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game2[, c(colnames(training_continuous))]))
      Game2$Team1_Win_Prob_Game2 <- nn_prob[,2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        #c = cbind(Team1, Team3) 
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = 2
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        #c$Round = c$Round + 1
      }
      
      Round3 = rbind(Round3, c)
    }
    
    results$Sweet_16 = ifelse(results$Team_Id %in% c(Round3$Team1, Round3$Team2),results$Sweet_16 + 1,results$Sweet_16)
    ##############################################################################################
    
    Round3_Team1 = Round3[, team1_cols]
    Round3_Team2 = Round3[, team2_cols]
    Round4 = data.frame()
    for (i in seq(1,8,2))
    {
      Team1 = Round3_Team1[i,]
      Team2 = Round3_Team2[i,]
      Game1 = cbind(Team1, Team2)
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1[, c(colnames(training_continuous))]))
      Game1$Team1_Win_Prob_Game1 <- nn_prob[,2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round3_Team1[i+1,]
      Team4 = Round3_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game2[, c(colnames(training_continuous))]))
      Game2$Team1_Win_Prob_Game2 <- nn_prob[,2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round4 = rbind(Round4, c)
    }
    Round4 = Round4[order(match(Round4$Team1_Region, regions)),]
    results$Elite_8 = ifelse(results$Team_Id %in% c(Round4$Team1, Round4$Team2),results$Elite_8 + 1,results$Elite_8)
    #############################################################################################
    
    Round4_Team1 = Round4[, team1_cols]
    Round4_Team2 = Round4[, team2_cols]
    Round5 = data.frame()
    
    for (i in seq(1,4,2))
    {
      Team1 = Round4_Team1[i,]
      Team2 = Round4_Team2[i,]
      Game1 = cbind(Team1, Team2)
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1[, c(colnames(training_continuous))]))
      Game1$Team1_Win_Prob_Game1 <- nn_prob[,2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round4_Team1[i+1,]
      Team4 = Round4_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game2[, c(colnames(training_continuous))]))
      Game2$Team1_Win_Prob_Game2 <- nn_prob[,2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
      }
      
      Round5 = rbind(Round5, c)
    }
    
    
    results$Final_4 = ifelse(results$Team_Id %in% c(Round5$Team1, Round5$Team2),results$Final_4 + 1,results$Final_4)
    #############################################################################################
    Round5_Team1 = Round5[, team1_cols]
    Round5_Team2 = Round5[, team2_cols]
    Round6 = data.frame()
    for (i in seq(1,1))
    {
      Team1 = Round5_Team1[i,]
      Team2 = Round5_Team2[i,]
      Game1 = cbind(Team1, Team2)
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1[, c(colnames(training_continuous))]))
      Game1$Team1_Win_Prob_Game1 <- nn_prob[,2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round5_Team1[i+1,]
      Team4 = Round5_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game2[, c(colnames(training_continuous))]))
      Game2$Team1_Win_Prob_Game2 <- nn_prob[,2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 >= Game1$Random & Game2$Team1_Win_Prob_Game2 < Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team1_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team1_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob_Game1 < Game1$Random & Game2$Team1_Win_Prob_Game2 >= Game2$Random) {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team1_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team1_Name)) & kaggle1$Season == Game1$Season,]
        
      } else {
        c = kaggle1[(kaggle1$Team1_Name %in% c(Game1$Team2_Name, Game2$Team2_Name) & kaggle1$Team2_Name %in% c(Game1$Team2_Name, Game2$Team2_Name)) & kaggle1$Season == Game1$Season,]
        
      }
      
      Round6 = rbind(Round6, c)
    }
    
    nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1[, c(colnames(training_continuous))]))
    Team1_Win_Prob_Game1 <- nn_prob[,2]
    random = runif(1,0,1)
    if (Team1_Win_Prob_Game1 <= random)
    {
      print(paste0(Round6$Team1_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team1),results$Champion + 1,results$Champion)
    } else {
      results$Champion = ifelse(results$Team_Id %in% c(Round6$Team2),results$Champion + 1,results$Champion)
      print(paste0(Round6$Team2_Name," won the ", Round6$Season[1]," march madness tournament"), quote = F)
    }
    results$Final = ifelse(results$Team_Id %in% c(Round6$Team1, Round6$Team2),results$Final + 1,results$Final)
  }
  results = results[, c("Season","Region_Name","Seed","Team_Name","Top_32","Sweet_16","Elite_8","Final_4","Final","Champion")]
  return(results)
}

Normalize_Sim <- function(results, N)
{
  results$Top_32 = ((results$Top_32 / N) * 100)
  results$Sweet_16 = ((results$Sweet_16 / N) * 100)
  results$Elite_8 = ((results$Elite_8 / N) * 100)
  results$Final_4 = ((results$Final_4 / N) * 100)
  results$Final = ((results$Final / N) * 100)
  results$Champion = ((results$Champion / N) * 100)
  return(results)
}

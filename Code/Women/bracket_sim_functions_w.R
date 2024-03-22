current_tourn_year <- 2024

Bracket_Sim <- function(year, N, model_type)
{
  train_continuous <- train_continuous %>% dplyr::select(-Team1_Victory)
  
  if (model_type %in% c("RF","GBM","XGBTree","GLMNET"))
  {
    shelve_df$Team1_Win_Prob <- predict(mod[[1]], newdata = shelve_df %>% select(names(train_continuous)), type= "prob")[2]
  }
  
  matchups <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  regions <- c("W","X","Y","Z")
  slot_order <- c("R1W1","R1W8","R1W5","R1W4","R1W6","R1W3","R1W7","R1W2",
                  "R1X1","R1X8","R1X5","R1X4","R1X6","R1X3","R1X7","R1X2",
                  "R1Y1","R1Y8","R1Y5","R1Y4","R1Y6","R1Y3","R1Y7","R1Y2",
                  "R1Z1","R1Z8","R1Z5","R1Z4","R1Z6","R1Z3","R1Z7","R1Z2",
                  "R2W1","R2W4","R2W3","R2W2","R2X1","R2X4","R2X3","R2X2",
                  "R2Y1","R2Y4","R2Y3","R2Y2","R2Z1","R2Z4","R2Z3","R2Z2",
                  "R3W1","R3W2","R3X1","R3X2","R3Y1","R3Y2","R3Z1","R3Z2",
                  "R4W1","R4X1","R4Y1","R4Z1","R5WX","R5YZ","R6CH")
  
  # Use training_df data set for known tournament results for training
  if (year != current_tourn_year)
  {
    bracket <- training_df %>% filter(Season == year)
    if (model_type %in% c("RF","GBM","XGBTree","GLMNET"))
    {
      bracket$Team1_Win_Prob <- predict(mod[[1]], newdata = bracket %>% select(names(train_continuous)), type= "prob")[2]
      bracket$Team1_Win_Prob <- ifelse(bracket$Team1_Win_Prob >= 0.94, 0.9999)
    }
    
  }
  if (year == current_tourn_year)
  {
    # Use below for unknown predictions
    # Remove teams that lost play in game in current tournament
    bracket <- shelve_df %>% filter(Season == year)
    
    bracket <- bracket[bracket$TeamName_Team1 != "Sacred Heart" & bracket$TeamName_Team2 != "Sacred Heart",]
    bracket <- bracket[bracket$TeamName_Team1 != "Columbia" & bracket$TeamName_Team2 != "Columbia",]
    bracket <- bracket[bracket$TeamName_Team1 != "Auburn" & bracket$TeamName_Team2 != "Auburn",]
    bracket <- bracket[bracket$TeamName_Team1 != "TN Martin" & bracket$TeamName_Team2 != "TN Martin",]
  }  
  
  # Add seed and region 
  results <- data.frame(matrix(nrow = 64, ncol = 10))
  colnames(results) <- c("Team_Id","Team_Name","Region","All_64","Top_32","Sweet_16","Elite_8",
                         "Final_4","Final","Champion")
  results[,1] <- as.character(results[,1]);results[,2] <- as.character(results[,2])
  results[,3] <- as.character(results[,3]);results[,4] <- as.numeric(results[,4])
  results[,5] <- as.numeric(results[,5]);results[,6] <- as.numeric(results[,6])
  results[,7] <- as.numeric(results[,7]);results[,8] <- as.numeric(results[,8])
  results[,9] <- as.numeric(results[,9]);results[,10] <- as.numeric(results[,10])
  
  Round1 <- bracket %>% filter(Round == 1)
  if (sum(duplicated(Round1[, c("Season","Slot")])) >= 1)
  {
    Round1 <- Round1[-which(duplicated(Round1[, "Slot"])),]
  }
  
  Round1_Team1 <- Round1 %>% select(all_of(team1_cols), Team1_Win_Prob)
  Round1_Team2 <- Round1 %>% select(all_of(team2_cols), Team1_Win_Prob)
  
  results$Team_Id <- unique(c(sort(unique(Round1_Team1$Team1)), sort(unique(Round1_Team2$Team2))))
  results <- merge(results %>% select(-Team_Name), Teams %>% select(TeamID, TeamName), 
                   by.x = "Team_Id", by.y = "TeamID")
  results1 <- merge(results, Round1 %>% select(Team1, Seed_Team1, Region_Name_Team1, Region_Team1), 
                    by.x = "Team_Id", by.y = "Team1")
  results2 <- merge(results, Round1 %>% select(Team2, Seed_Team2, Region_Name_Team2, Region_Team2), 
                    by.x = "Team_Id", by.y = "Team2")
  colnames(results1)[11:13] = c("Seed","Region_Name","Region")
  colnames(results2)[11:13] = c("Seed","Region_Name","Region")
  results <- rbind(results1, results2)
  results$Season <- rep(bracket$Season[1], 64)
  results <- results[, -2]
  results <- results %>% select(Season, Team_Id, TeamName, Seed, All_64, Top_32, 
                                Sweet_16, Elite_8, Final_4, Final, Champion, Region_Name, Region)
  results[is.na(results)] <- 0
  colnames(results)[3] <- "Team_Name"
  
  # Add order by w, x, y, z 
  results <- results[order(match(results$Region, regions), match(results$Seed, matchups)),]
  
  kaggle_submission <- list()
  for (j in seq(1,N))
  {
    kaggle_submission[[j]] <- data.frame(matrix(nrow = 63, ncol = 5))
    colnames(kaggle_submission[[j]]) <- c("RowId","Tournament","Bracket","Slot","Team")
    kaggle_submission[[j]]$Bracket <- j
    kaggle_submission[[j]]$Tournament <- 'W'
    
    results$All_64 <- ifelse(results$Team_Id %in% c(Round1$Team1, Round1$Team2),results$All_64 + 1,results$All_64)
    
    ##############################################################################################
    
    Round2 <- data.frame()
    for (i in seq(1,32,2))
    {
      Team1 <- Round1_Team1[i,]
      Team2 <- Round1_Team2[i,]
      Game1 <- cbind(Team1, Team2)
      Game1$Random <- runif(1,0,1)
      kaggle_submission[[j]]$Slot[i] <- Game1$Slot
      if (Game1$Team1_Win_Prob >= Game1$Random){
        kaggle_submission[[j]]$Team[i] <- Game1$RegionSeed_Team1
      } else {
        kaggle_submission[[j]]$Team[i] <- Game1$RegionSeed_Team2
      }
      
      Team3 <- Round1_Team1[i+1,]
      Team4 <- Round1_Team2[i+1,]
      Game2 <- cbind(Team3, Team4)
      Game2$Random <- runif(1,0,1)
      kaggle_submission[[j]]$Slot[i+1] <- Game2$Slot
      if (Game2$Team1_Win_Prob >= Game2$Random){
        kaggle_submission[[j]]$Team[i+1] <- Game2$RegionSeed_Team1
      } else {
        kaggle_submission[[j]]$Team[i+1] <- Game2$RegionSeed_Team2
      }
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      } else {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      }
      Round2 <- rbind(Round2, event)
    }
    results$Top_32 <- ifelse(results$Team_Id %in% c(Round2$Team1, Round2$Team2), results$Top_32 + 1, results$Top_32)
    
    ###############################################################################################
    
    Round2_Team1 <- Round2 %>% select(all_of(team1_cols), Team1_Win_Prob)
    Round2_Team2 <- Round2 %>% select(all_of(team2_cols), Team1_Win_Prob)
    
    Round3 <- data.frame()
    for (i in seq(1,16,2))
    {
      Team1 <- Round2_Team1[i,]
      Team2 <- Round2_Team2[i,]
      Game1 <- cbind(Team1, Team2)
      Game1$Random <- runif(1,0,1)
      kaggle_submission[[j]]$Slot[i+32] <- Game1$Slot
      if (Game1$Team1_Win_Prob >= Game1$Random){
        kaggle_submission[[j]]$Team[i+32] <- Game1$RegionSeed_Team1
      } else {
        kaggle_submission[[j]]$Team[i+32] <- Game1$RegionSeed_Team2
      }
      
      Team3 <- Round2_Team1[i+1,]
      Team4 <- Round2_Team2[i+1,]
      Game2 <- cbind(Team3, Team4)
      Game2$Random <- runif(1,0,1)
      kaggle_submission[[j]]$Slot[i+32+1] <- Game2$Slot
      if (Game2$Team1_Win_Prob >= Game2$Random){
        kaggle_submission[[j]]$Team[i+32+1] <- Game2$RegionSeed_Team1
      } else {
        kaggle_submission[[j]]$Team[i+32+1] <- Game2$RegionSeed_Team2
      }
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 3) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 3) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 3) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      } else {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 3) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      }
      Round3 <- rbind(Round3, event)
    }
    results$Sweet_16 <- ifelse(results$Team_Id %in% c(Round3$Team1, Round3$Team2),results$Sweet_16 + 1,results$Sweet_16)
    
    ##############################################################################################
    
    Round3_Team1 <- Round3 %>% select(all_of(team1_cols), Team1_Win_Prob)
    Round3_Team2 <- Round3 %>% select(all_of(team2_cols), Team1_Win_Prob)
    
    Round4 <- data.frame()
    for (i in seq(1,8,2))
    {
      Team1 <- Round3_Team1[i,]
      Team2 <- Round3_Team2[i,]
      Game1 <- cbind(Team1, Team2)
      Game1$Random <- runif(1,0,1)
      kaggle_submission[[j]]$Slot[i+48] <- Game1$Slot
      if (Game1$Team1_Win_Prob >= Game1$Random){
        kaggle_submission[[j]]$Team[i+48] <- Game1$RegionSeed_Team1
      } else {
        kaggle_submission[[j]]$Team[i+48] <- Game1$RegionSeed_Team2
      }
      
      Team3 <- Round3_Team1[i+1,]
      Team4 <- Round3_Team2[i+1,]
      Game2 <- cbind(Team3, Team4)
      Game2$Random <- runif(1,0,1)
      kaggle_submission[[j]]$Slot[i+48+1] <- Game2$Slot
      if (Game2$Team1_Win_Prob >= Game2$Random){
        kaggle_submission[[j]]$Team[i+48+1] <- Game2$RegionSeed_Team1
      } else {
        kaggle_submission[[j]]$Team[i+48+1] <- Game2$RegionSeed_Team2
      }
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 4) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 4) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 4) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      } else {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 4) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team1) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team1 == Game2$Region_Name_Team2) & (Game1$Region_Name_Team2 == Game2$Region_Name_Team1),]
      }
      Round4 <- rbind(Round4, event)
    }
    results$Elite_8 <- ifelse(results$Team_Id %in% c(Round4$Team1, Round4$Team2),results$Elite_8 + 1,results$Elite_8)
    
    #############################################################################################
    
    Round4_Team1 <- Round4 %>% select(all_of(team1_cols), Team1_Win_Prob) %>% arrange(Slot)
    Round4_Team2 <- Round4 %>% select(all_of(team2_cols), Team1_Win_Prob) %>% arrange(Slot)
    Round5 <- data.frame()
    
    for (i in seq(1,4,2))
    {
      Team1 <- Round4_Team1[i,]
      Team2 <- Round4_Team2[i,]
      Game1 <- cbind(Team1, Team2)
      Game1$Random <- runif(1,0,1)
      kaggle_submission[[j]]$Slot[i+56] <- Game1$Slot
      if (Game1$Team1_Win_Prob >= Game1$Random){
        kaggle_submission[[j]]$Team[i+56] <- Game1$RegionSeed_Team1
      } else {
        kaggle_submission[[j]]$Team[i+56] <- Game1$RegionSeed_Team2
      }
      
      Team3 <- Round4_Team1[i+1,]
      Team4 <- Round4_Team2[i+1,]
      Game2 <- cbind(Team3, Team4)
      Game2$Random <- runif(1,0,1)
      kaggle_submission[[j]]$Slot[i+56+1] <- Game2$Slot
      if (Game2$Team1_Win_Prob >= Game2$Random){
        kaggle_submission[[j]]$Team[i+56+1] <- Game2$RegionSeed_Team1
      } else {
        kaggle_submission[[j]]$Team[i+56+1] <- Game2$RegionSeed_Team2
      }
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 5),]
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 5),]
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 5),]
      } else {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 5),]
      }
      Round5 <- rbind(Round5, event)
    }
    results$Final_4 <- ifelse(results$Team_Id %in% c(Round5$Team1, Round5$Team2),results$Final_4 + 1,results$Final_4)
    
    #############################################################################################
    
    Round5_Team1 <- Round5 %>% select(all_of(team1_cols), Team1_Win_Prob)
    Round5_Team2 <- Round5 %>% select(all_of(team2_cols), Team1_Win_Prob)
    Round6 <- data.frame()
    for (i in seq(1,1))
    {
      Team1 <- Round5_Team1[i,]
      Team2 <- Round5_Team2[i,]
      Game1 <- cbind(Team1, Team2)
      Game1$Random = runif(1,0,1)
      kaggle_submission[[j]]$Slot[i+60] <- Game1$Slot
      if (Game1$Team1_Win_Prob >= Game1$Random){
        kaggle_submission[[j]]$Team[i+60] <- Game1$RegionSeed_Team1
      } else {
        kaggle_submission[[j]]$Team[i+60] <- Game1$RegionSeed_Team2
      }
      
      Team3 <- Round5_Team1[i+1,]
      Team4 <- Round5_Team2[i+1,]
      Game2 <- cbind(Team3, Team4)
      Game2$Random <- runif(1,0,1)
      kaggle_submission[[j]]$Slot[i+60+1] <- Game2$Slot
      if (Game2$Team1_Win_Prob >= Game2$Random){
        kaggle_submission[[j]]$Team[i+60+1] <- Game2$RegionSeed_Team1
      } else {
        kaggle_submission[[j]]$Team[i+60+1] <- Game2$RegionSeed_Team2
      }
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 6),]
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 6),]
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 6),]
      } else {
        event <- shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & (shelve_df$Season == Game1$Season) & (shelve_df$Round == 6),]
      }
      Round6 <- rbind(Round6, event)
    }
    
    random <- runif(1,0,1)
    kaggle_submission[[j]]$Slot[63] <- Round6$Slot
    if (Round6$Team1_Win_Prob >= random)
    {
      print(paste0(Round6$TeamName_Team1," won the ", Round6$Season[1]," march madness tournament"), quote = F)
      results$Champion <- ifelse(results$Team_Id %in% c(Round6$Team1), results$Champion + 1, results$Champion)
      kaggle_submission[[j]]$Team[63] <- Round6$RegionSeed_Team1
    } else {
      print(paste0(Round6$TeamName_Team2," won the ", Round6$Season[1]," march madness tournament"), quote = F)
      results$Champion <- ifelse(results$Team_Id %in% c(Round6$Team2),results$Champion + 1,results$Champion)
      kaggle_submission[[j]]$Team[63] <- Round6$RegionSeed_Team2
    }
    results$Final <- ifelse(results$Team_Id %in% c(Round6$Team1, Round6$Team2), results$Final + 1, results$Final)
    kaggle_submission[[j]] <- kaggle_submission[[j]][order(match(kaggle_submission[[j]]$Slot, slot_order)),]
    kaggle_submission[[j]]$RowId <- 63:125
    
    if (j%%10 == 0)
    {
      print("", quote = F)
      print(paste(j, " simulations"), quote = F)
      print("", quote = F)
    }
    
  }
  results <- results %>% select(Season, Region_Name, Seed, Team_Name, Top_32, Sweet_16, Elite_8, Final_4, Final, Champion)
  return(list(results, kaggle_submission))
}








######################################################################################

Bracket_Sim_Penalized = function(year, N, lambda)
{
  matchups = c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  regions = c("X","W","Z","Y")
  slot_order <- c("R1W1","R1W8","R1W5","R1W4","R1W6","R1W3","R1W7","R1W2",
                  "R1X1","R1X8","R1X5","R1X4","R1X6","R1X3","R1X7","R1X2",
                  "R1Y1","R1Y8","R1Y5","R1Y4","R1Y6","R1Y3","R1Y7","R1Y2",
                  "R1Z1","R1Z8","R1Z5","R1Z4","R1Z6","R1Z3","R1Z7","R1Z2",
                  "R2W1","R2W4","R2W3","R2W2","R2X1","R2X4","R2X3","R2X2",
                  "R2Y1","R2Y4","R2Y3","R2Y2","R2Z1","R2Z4","R2Z3","R2Z2",
                  "R3W1","R3W2","R3X1","R3X2","R3Y1","R3Y2","R3Z1","R3Z2",
                  "R4W1","R4X1","R4Y1","R4Z1","R5WX","R5YZ","R6CH")
  
  # Use training_df data set for known tournament results for training
  if (year != current_tourn_year)
  {
    bracket = training_df[training_df$Season == year,]
  }
  if (year == current_tourn_year)
  {
    # Use below for unknown predictions
    # Remove teams that lost play in game in current tournament
    bracket = shelve_df[shelve_df$Season == year,]
    bracket = bracket[bracket$Team1_Name != "Mississippi St" & bracket$Team2_Name != "Mississippi St",]
    bracket = bracket[bracket$Team1_Name != "TAM C. Christi" & bracket$Team2_Name != "TAM C. Christi",]
    bracket = bracket[bracket$Team1_Name != "Nevada" & bracket$Team2_Name != "Nevada",]
    bracket = bracket[bracket$Team1_Name != "F Dickinson" & bracket$Team2_Name != "F Dickinson",]
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
  if (sum(duplicated(Round1[, c("Season","Slot")])) >= 1)
  {
    Round1 = Round1[-which(duplicated(Round1[, "Slot"])),]
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
      Team1 <- Round1_Team1[i,]
      Team2 <- Round1_Team2[i,]
      
      Game1 = cbind(Team1, Team2)
      Game1$Team1_Win_Prob <- predict(cv.out,newx = data.matrix(Game1 %>% select(names(train_continuous))), s=lambda, type= "response")
      Game1$Random = runif(1,0,1)
      
      Team3 = Round1_Team1[i+1,]
      Team4 = Round1_Team2[i+1,]
      
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$TeamName_Team1, Game2$TeamName_Team2, Game2$Round2,i+1))
      Game2$Team1_Win_Prob = predict(cv.out,newx = data.matrix(Game2 %>% select(names(train_continuous))), s=lambda, type= "response")
      Game2$Random = runif(1,0,1)
      # Call logistic regression model for predicting team 1 victory for game 2
      # Store Probabilty p for team 1 winning and 1 - p for team 2 winning
      # Random Number Generate 
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
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
      #print(c(Game1$TeamName_Team1, Game1$TeamName_Team2, Game1$Round2,i))
      Game1$Team1_Win_Prob = predict(cv.out,newx = data.matrix(Game1 %>% select(names(train_continuous))), s=lambda, type= "response")
      Game1$Random = runif(1,0,1)
      
      Team3 = Round2_Team1[i+1,]
      Team4 = Round2_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$TeamName_Team1, Game2$TeamName_Team2, Game2$Round2,i+1))
      Game2$Team1_Win_Prob = predict(cv.out,newx = data.matrix(Game2 %>% select(names(train_continuous))), s=lambda, type= "response")
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
      } else {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
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
      Game1$Team1_Win_Prob = predict(cv.out,newx = data.matrix(Game1 %>% select(names(train_continuous))), s=lambda, type= "response")
      Game1$Random = runif(1,0,1)
      
      Team3 = Round3_Team1[i+1,]
      Team4 = Round3_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob = predict(cv.out,newx = data.matrix(Game2 %>% select(names(train_continuous))), s=lambda, type= "response")
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
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
      Game1$Team1_Win_Prob = predict(cv.out,newx = data.matrix(Game1 %>% select(names(train_continuous))), s=lambda, type= "response")
      Game1$Random = runif(1,0,1)
      
      Team3 = Round4_Team1[i+1,]
      Team4 = Round4_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob = predict(cv.out,newx = data.matrix(Game2 %>% select(names(train_continuous))), s=lambda, type= "response")
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
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
      Game1$Team1_Win_Prob = predict(cv.out,newx = data.matrix(Game1 %>% select(names(train_continuous))), s=lambda, type= "response")
      Game1$Random = runif(1,0,1)
      
      Team3 = Round5_Team1[i+1,]
      Team4 = Round5_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      Game2$Team1_Win_Prob = predict(cv.out,newx = data.matrix(Game2 %>% select(names(train_continuous))), s=lambda, type= "response")
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
      }
      
      Round6 = rbind(Round6, c)
    }
    
    Team1_Win_Prob = predict(cv.out,newx = data.matrix(Round6[, c(colnames(train_continuous))]), s=lambda, type= "response")
    random = runif(1,0,1)
    if (Team1_Win_Prob <= random)
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
  train_continuous <- train_continuous %>% dplyr::select(-Team1_Victory)
  matchups = c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  regions = c("X","W","Z","Y")
  
  # Use training_df data set for known tournament results for training
  if (year != current_tourn_year)
  {
    bracket = training_df[training_df$Season == year,]
  }
  if (year == current_tourn_year)
  {
    # Use below for unknown predictions
    # Remove teams that lost play in game in current tournament
    bracket = shelve_df[shelve_df$Season == year,]
    
    bracket = bracket[bracket$Team1_Name != "Mississippi St" & bracket$Team2_Name != "Mississippi St",]
    bracket = bracket[bracket$Team1_Name != "TAM C. Christi" & bracket$Team2_Name != "TAM C. Christi",]
    bracket = bracket[bracket$Team1_Name != "Nevada" & bracket$Team2_Name != "Nevada",]
    bracket = bracket[bracket$Team1_Name != "F Dickinson" & bracket$Team2_Name != "F Dickinson",]
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
  if (sum(duplicated(Round1[, c("Season","Slot")])) >= 1)
  {
    Round1 = Round1[-which(duplicated(Round1[, "Slot"])),]
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
      #print(c(Game1$TeamName_Team1, Game1$TeamName_Team2, Game1$Round2,i))
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1 %>% select(names(train_continuous))))
      Game1$Team1_Win_Prob <- nn_prob[,2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round1_Team1[i+1,]
      Team4 = Round1_Team2[i+1,]
      
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$TeamName_Team1, Game2$TeamName_Team2, Game2$Round2,i+1))
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game2 %>% select(names(train_continuous))))
      Game2$Team1_Win_Prob <- nn_prob[,2]
      Game2$Random = runif(1,0,1)
      # Call logistic regression model for predicting team 1 victory for game 2
      # Store Probabilty p for team 1 winning and 1 - p for team 2 winning
      # Random Number Generate 
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
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
      #print(c(Game1$TeamName_Team1, Game1$TeamName_Team2, Game1$Round2,i))
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1 %>% select(names(train_continuous))))
      Game1$Team1_Win_Prob <- nn_prob[,2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round2_Team1[i+1,]
      Team4 = Round2_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      #print(c(Game2$TeamName_Team1, Game2$TeamName_Team2, Game2$Round2,i+1))
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game2 %>% select(names(train_continuous))))
      Game2$Team1_Win_Prob <- nn_prob[,2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        #c = cbind(Team1, Team3) 
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        #c$Round = 2
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        #c$Round = c$Round + 1
      } else {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
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
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1 %>% select(names(train_continuous))))
      Game1$Team1_Win_Prob <- nn_prob[,2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round3_Team1[i+1,]
      Team4 = Round3_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game2 %>% select(names(train_continuous))))
      Game2$Team1_Win_Prob <- nn_prob[,2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
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
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1 %>% select(names(train_continuous))))
      Game1$Team1_Win_Prob <- nn_prob[,2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round4_Team1[i+1,]
      Team4 = Round4_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game2 %>% select(names(train_continuous))))
      Game2$Team1_Win_Prob <- nn_prob[,2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
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
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1 %>% select(names(train_continuous))))
      Game1$Team1_Win_Prob <- nn_prob[,2]
      Game1$Random = runif(1,0,1)
      
      Team3 = Round5_Team1[i+1,]
      Team4 = Round5_Team2[i+1,]
      Game2 = cbind(Team3, Team4)
      nn_prob <- NN %>% keras::predict_proba(data.matrix(Game2 %>% select(names(train_continuous))))
      Game2$Team1_Win_Prob <- nn_prob[,2]
      Game2$Random = runif(1,0,1)
      
      if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob >= Game1$Random & Game2$Team1_Win_Prob < Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team1, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
      } else if (Game1$Team1_Win_Prob < Game1$Random & Game2$Team1_Win_Prob >= Game2$Random) {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team1)) & shelve_df$Season == Game1$Season,]
        
      } else {
        c = shelve_df[(shelve_df$TeamName_Team1 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2) & shelve_df$TeamName_Team2 %in% c(Game1$TeamName_Team2, Game2$TeamName_Team2)) & shelve_df$Season == Game1$Season,]
        
      }
      
      Round6 = rbind(Round6, c)
    }
    
    nn_prob <- NN %>% keras::predict_proba(data.matrix(Game1 %>% select(names(train_continuous))))
    Team1_Win_Prob <- nn_prob[,2]
    random = runif(1,0,1)
    if (Team1_Win_Prob <= random)
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

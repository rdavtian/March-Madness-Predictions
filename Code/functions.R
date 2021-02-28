matchups <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
normFunc <- function(x){
  (x-mean(x, na.rm = T))/sd(x, na.rm = T)
}

tidy_coef <- function(x, min, glmnet)
{
  if (min)
  {
    x <- coef(x, s = "lambda.min")
  } else if (glmnet) {
    x <- coef(x$finalModel, x$bestTune$lambda)
  } else {
    x <- coef(x, s = "lambda.1se")
  }
  data.frame(term=rownames(x),
             estimate=matrix(x)[,1],
             stringsAsFactors = FALSE)
}

logloss = function(actual, predicted, eps = 1e-15) 
{
  predicted = pmin(pmax(predicted, eps), 1-eps) 
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}  

get_columns = function(data_frame, list_of_vars, match_letter = "") 
{
  if (match_letter != "")
  {
    cols = ifelse(names(data_frame) %in% list_of_vars | startsWith(names(data_frame), match_letter)
                  , names(data_frame), "NULL")
  }
  else
  {
    cols = ifelse(names(data_frame) %in% list_of_vars, names(data_frame), "NULL")
  }
  cols = cols[cols != "NULL"]
  return(data_frame[cols])
}

roundFinder <- function(region1, region2, seed1, seed2){
  if(region1 == region2 && seed1 == seed2){return(0)} 
  else if(region1 != region2){   
    if((region1 == 'W' && region2 == 'X')||(region1=='X' && region2=='W')|| (region1=='Y'&&region2=='Z')|| (region1=='Z'&&region2=='Y')){ 
      return(5)     
    }
    else return(6)  
  }
  else{  
    z <- as.character(as.numeric(seed1) + as.numeric(seed2))
    x <- as.character(min(c(as.numeric(seed1),as.numeric(seed2))))
    y <- as.character(max(c(as.numeric(seed1),as.numeric(seed2))))
    w <- switch(z, '17'=1, '9'=2, '25'=2, '2'=4, '3'=4, '4'=4, '5'=3, '7'=4, '11'=4, '13'=3, 
                '15'=4, '19'=4, '21'=3, '23'=4, '27'=4, '29'=3, '30'=4, '31'=4)      
    if(is.numeric(w)){
      return(w)
    }
    else{ 
      y <- as.character(y) 
      return(switch(as.character(x), '1'=switch(y,'9'=2,'5'=3,'13'=3,'7'=4,'11'=4,'15'=4),
                    '2'=switch(y,'10'=2,'6'=3,'14'=3,'4'=4,'8'=4,'12'=4,'16'=4),
                    '3'=switch(y, '11'=2,'7'=3,'15'=3,'5'=4,'9'=4,'13'=4),
                    '4'=switch(y,'12'=2,'8'=3,'16'=3,'6'=4,'10'=4,'14'=4),
                    '5'=switch(y,'13'=2,'9'=3,'7'=4,'11'=4,'15'=4),
                    '6'=switch(y,'14'=2,'10'=3,'8'=4,'12'=4,'16'=4),'7'=switch(y,'15'=2,'11'=3,'9'=4,'13'=4),
                    '8'=switch(y,'16'=2,'12'=3,'10'=4,'14'=4),'9'=switch(y,'13'=3,'11'=4,'15'=4),
                    '10'=switch(y,'14'=3,'12'=4,'16'=4),'11'=switch(y,'15'=3,'13'=4),
                    '12'=switch(y,'16'=3,'14'=4),'13'=4))
    }
  }
}

creating_slots <- function() 
{
  # Run manual ifelse code for adding slots 
  # Round 0
  train1$slot <- ifelse(train1$Round == 0, "Play_In", NA)
  
  # slot seeds Round 1 W region
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "W" 
                        & train1$Region_Team2 == "W" & (train1$Seed_Team1 %in%c(1,16) 
                        & train1$Seed_Team2 %in%c(1,16)), "R1W1", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "W" 
                        & train1$Region_Team2 == "W" & train1$Seed_Team1 %in%c(2,15) 
                        & train1$Seed_Team2 %in%c(2,15), "R1W2", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "W" 
                        & train1$Region_Team2 == "W" & train1$Seed_Team1 %in%c(3,14) 
                        & train1$Seed_Team2 %in%c(3,14), "R1W3", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "W" 
                        & train1$Region_Team2 == "W" & train1$Seed_Team1 %in%c(4,13) 
                        & train1$Seed_Team2 %in%c(4,13), "R1W4", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "W" 
                        & train1$Region_Team2 == "W" & (train1$Seed_Team1 %in%c(5,12) 
                        & train1$Seed_Team2 %in%c(5,12)), "R1W5", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "W" 
                        & train1$Region_Team2 == "W" & train1$Seed_Team1 %in%c(6,11) 
                        & train1$Seed_Team2 %in%c(6,11), "R1W6", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "W" 
                        & train1$Region_Team2 == "W" & train1$Seed_Team1 %in%c(7,10) 
                        & train1$Seed_Team2 %in%c(7,10), "R1W7", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "W" 
                        & train1$Region_Team2 == "W" & train1$Seed_Team1 %in%c(8,9) 
                        & train1$Seed_Team2 %in%c(8,9), "R1W8", train1$slot)
  
  ###############################################################################################
  # slot seeds Round 1 X region
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "X" 
                        & train1$Region_Team2 == "X" & (train1$Seed_Team1 %in%c(1,16) 
                        & train1$Seed_Team2 %in%c(1,16)), "R1X1", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "X" 
                        & train1$Region_Team2 == "X" & train1$Seed_Team1 %in%c(2,15) 
                        & train1$Seed_Team2 %in%c(2,15), "R1X2", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "X" 
                        & train1$Region_Team2 == "X" & train1$Seed_Team1 %in%c(3,14) 
                        & train1$Seed_Team2 %in%c(3,14), "R1X3", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "X" 
                        & train1$Region_Team2 == "X" & train1$Seed_Team1 %in%c(4,13) 
                        & train1$Seed_Team2 %in%c(4,13), "R1X4", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "X" 
                        & train1$Region_Team2 == "X" & (train1$Seed_Team1 %in%c(5,12) 
                        & train1$Seed_Team2 %in%c(5,12)), "R1X5", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "X" 
                        & train1$Region_Team2 == "X" & train1$Seed_Team1 %in%c(6,11) 
                        & train1$Seed_Team2 %in%c(6,11), "R1X6", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "X" 
                        & train1$Region_Team2 == "X" & train1$Seed_Team1 %in%c(7,10) 
                        & train1$Seed_Team2 %in%c(7,10), "R1X7", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "X" 
                        & train1$Region_Team2 == "X" & train1$Seed_Team1 %in%c(8,9) 
                        & train1$Seed_Team2 %in%c(8,9), "R1X8", train1$slot)
  #########################################################################################
  # slot seeds Round 1 Y region
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Y" 
                        & train1$Region_Team2 == "Y" & (train1$Seed_Team1 %in%c(1,16) 
                        & train1$Seed_Team2 %in%c(1,16)), "R1Y1", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Y" 
                        & train1$Region_Team2 == "Y" & train1$Seed_Team1 %in%c(2,15) 
                        & train1$Seed_Team2 %in%c(2,15), "R1Y2", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Y" 
                        & train1$Region_Team2 == "Y" & train1$Seed_Team1 %in%c(3,14) 
                        & train1$Seed_Team2 %in%c(3,14), "R1Y3", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Y" 
                        & train1$Region_Team2 == "Y" & train1$Seed_Team1 %in%c(4,13) 
                        & train1$Seed_Team2 %in%c(4,13), "R1Y4", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Y" 
                        & train1$Region_Team2 == "Y" & (train1$Seed_Team1 %in%c(5,12) 
                        & train1$Seed_Team2 %in%c(5,12)), "R1Y5", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Y" 
                        & train1$Region_Team2 == "Y" & train1$Seed_Team1 %in%c(6,11) 
                        & train1$Seed_Team2 %in%c(6,11), "R1Y6", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Y" 
                        & train1$Region_Team2 == "Y" & train1$Seed_Team1 %in%c(7,10) 
                        & train1$Seed_Team2 %in%c(7,10), "R1Y7", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Y" 
                        & train1$Region_Team2 == "Y" & train1$Seed_Team1 %in%c(8,9) 
                        & train1$Seed_Team2 %in%c(8,9), "R1Y8", train1$slot)
  
  #########################################################################################
  # slot seeds Round 1 Z region
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Z" 
                        & train1$Region_Team2 == "Z" & (train1$Seed_Team1 %in%c(1,16) 
                        & train1$Seed_Team2 %in%c(1,16)), "R1Z1", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Z" 
                        & train1$Region_Team2 == "Z" & train1$Seed_Team1 %in%c(2,15) 
                        & train1$Seed_Team2 %in%c(2,15), "R1Z2", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Z" 
                        & train1$Region_Team2 == "Z" & train1$Seed_Team1 %in%c(3,14) 
                        & train1$Seed_Team2 %in%c(3,14), "R1Z3", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Z" 
                        & train1$Region_Team2 == "Z" & train1$Seed_Team1 %in%c(4,13) 
                        & train1$Seed_Team2 %in%c(4,13), "R1Z4", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Z" 
                        & train1$Region_Team2 == "Z" & (train1$Seed_Team1 %in%c(5,12) 
                        & train1$Seed_Team2 %in%c(5,12)), "R1Z5", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Z" 
                        & train1$Region_Team2 == "Z" & train1$Seed_Team1 %in%c(6,11) 
                        & train1$Seed_Team2 %in%c(6,11), "R1Z6", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Z" 
                        & train1$Region_Team2 == "Z" & train1$Seed_Team1 %in%c(7,10) 
                        & train1$Seed_Team2 %in%c(7,10), "R1Z7", train1$slot)
  train1$slot <- ifelse(train1$Round == 1 & train1$Region_Team1 == "Z" 
                        & train1$Region_Team2 == "Z" & train1$Seed_Team1 %in%c(8,9) 
                        & train1$Seed_Team2 %in%c(8,9), "R1Z8", train1$slot)
  
  ##########################################################################################
  
  # slot seeds Round 2 W region
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "W" 
                       & train1$Region_Team2 == "W" & (train1$Seed_Team1 %in%c(1,16,8,9) 
                                                       & train1$Seed_Team2 %in%c(1,16,8,9)), "R2W1", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "W" 
                       & train1$Region_Team2 == "W" & train1$Seed_Team1 %in%c(2,15,7,10) 
                       & train1$Seed_Team2 %in%c(2,15,7,10), "R2W2", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "W" 
                       & train1$Region_Team2 == "W" & train1$Seed_Team1 %in%c(3,14,6,11) 
                       & train1$Seed_Team2 %in%c(3,14,6,11), "R2W3", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "W" 
                       & train1$Region_Team2 == "W" & train1$Seed_Team1 %in%c(4,13,5,12) 
                       & train1$Seed_Team2 %in%c(4,13,5,12), "R2W4", train1$slot)
  
  ###############################################################################################
  # slot seeds Round 2 X region
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "X" 
                       & train1$Region_Team2 == "X" & (train1$Seed_Team1 %in%c(1,16,8,9) 
                                                       & train1$Seed_Team2 %in%c(1,16,8,9)), "R2X1", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "X" 
                       & train1$Region_Team2 == "X" & train1$Seed_Team1 %in%c(2,15,7,10) 
                       & train1$Seed_Team2 %in%c(2,15,7,10), "R2X2", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "X" 
                       & train1$Region_Team2 == "X" & train1$Seed_Team1 %in%c(3,14,6,11) 
                       & train1$Seed_Team2 %in%c(3,14,6,11), "R2X3", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "X" 
                       & train1$Region_Team2 == "X" & train1$Seed_Team1 %in%c(4,13,5,12) 
                       & train1$Seed_Team2 %in%c(4,13,5,12), "R2X4", train1$slot)
  #########################################################################################
  # slot seeds Round 2 Y region
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "Y" 
                       & train1$Region_Team2 == "Y" & (train1$Seed_Team1 %in%c(1,16,8,9) 
                                                       & train1$Seed_Team2 %in%c(1,16,8,9)), "R2Y1", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "Y" 
                       & train1$Region_Team2 == "Y" & train1$Seed_Team1 %in%c(2,15,7,10) 
                       & train1$Seed_Team2 %in%c(2,15,7,10), "R2Y2", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "Y" 
                       & train1$Region_Team2 == "Y" & train1$Seed_Team1 %in%c(3,14,6,11) 
                       & train1$Seed_Team2 %in%c(3,14,6,11), "R2Y3", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "Y" 
                       & train1$Region_Team2 == "Y" & train1$Seed_Team1 %in%c(4,13,5,12) 
                       & train1$Seed_Team2 %in%c(4,13,5,12), "R2Y4", train1$slot)
  
  #########################################################################################
  # slot seeds Round 2 Z region
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "Z" 
                       & train1$Region_Team2 == "Z" & (train1$Seed_Team1 %in%c(1,16,8,9) 
                                                       & train1$Seed_Team2 %in%c(1,16,8,9)), "R2Z1", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "Z" 
                       & train1$Region_Team2 == "Z" & train1$Seed_Team1 %in%c(2,15,7,10) 
                       & train1$Seed_Team2 %in%c(2,15,7,10), "R2Z2", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "Z" 
                       & train1$Region_Team2 == "Z" & train1$Seed_Team1 %in%c(3,14,6,11) 
                       & train1$Seed_Team2 %in%c(3,14,6,11), "R2Z3", train1$slot)
  train1$slot <- ifelse(train1$Round == 2 & train1$Region_Team1 == "Z" 
                       & train1$Region_Team2 == "Z" & train1$Seed_Team1 %in%c(4,13,5,12) 
                       & train1$Seed_Team2 %in%c(4,13,5,12), "R2Z4", train1$slot)
  
  ##########################################################################################
  # slot seeds Round 3 W Region
  
  train1$slot <- ifelse(train1$Round == 3 & train1$Region_Team1 == "W" 
                       & train1$Region_Team2 == "W" & train1$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                       & train1$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3W1", train1$slot)
  
  train1$slot <- ifelse(train1$Round == 3 & train1$Region_Team1 == "W"   
                       & train1$Region_Team2 == "W" & train1$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                       & train1$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3W2", train1$slot)
  
  ############################################################################################
  # slot seeds Round 3 X Region
  train1$slot <- ifelse(train1$Round == 3 & train1$Region_Team1 == "X" 
                       & train1$Region_Team2 == "X" & train1$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                       & train1$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3X1", train1$slot)
  
  train1$slot <- ifelse(train1$Round == 3 & train1$Region_Team1 == "X" 
                       & train1$Region_Team2 == "X" & train1$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                       & train1$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3X2", train1$slot)
  ############################################################################################
  # slot seeds Round 3 Y Region
  train1$slot <- ifelse(train1$Round == 3 & train1$Region_Team1 == "Y" 
                       & train1$Region_Team2 == "Y" & train1$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                       & train1$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3Y1", train1$slot)
  
  train1$slot <- ifelse(train1$Round == 3 & train1$Region_Team1 == "Y" 
                       & train1$Region_Team2 == "Y" & train1$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                       & train1$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3Y2", train1$slot)
  ############################################################################################
  # slot seeds Round 3 Z Region
  train1$slot <- ifelse(train1$Round == 3 & train1$Region_Team1 == "Z" 
                       & train1$Region_Team2 == "Z" & train1$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                       & train1$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3Z1", train1$slot)
  
  train1$slot <- ifelse(train1$Round == 3 & train1$Region_Team1 == "Z" 
                       & train1$Region_Team2 == "Z" & train1$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                       & train1$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3Z2", train1$slot)
  ############################################################################################
  #Round 4 
  train1$slot <- ifelse(train1$Round == 4 & train1$Region_Team1 == "W" 
                       & train1$Region_Team2 == "W", "R4W1", train1$slot)
  
  train1$slot <- ifelse(train1$Round == 4 & train1$Region_Team1 == "X" 
                       & train1$Region_Team2 == "X", "R4X1", train1$slot)
  
  train1$slot <- ifelse(train1$Round == 4 & train1$Region_Team1 == "Y" 
                       & train1$Region_Team2 == "Y", "R4Y1", train1$slot)
  
  train1$slot <- ifelse(train1$Round == 4 & train1$Region_Team1 == "Z" 
                       & train1$Region_Team2 == "Z", "R4Z1", train1$slot)
  ########################################################################################
  # Round 5
  train1$slot <- ifelse(train1$Round == 5 & (train1$Region_Team1 == "W" | 
                                              train1$Region_Team2 == "W"), "R5WX",train1$slot)
  
  train1$slot <- ifelse(train1$Round == 5 & (train1$Region_Team1 == "Y" | 
                                              train1$Region_Team2 == "Y"), "R5YZ",train1$slot)
  # Round 6
  train1$slot <- ifelse(train1$Round == 6, "R6CH",train1$slot)
}

corr_eqn <- function(x,y, digits = 2, binary) 
{ 
  if (binary)
  {
    corr_coef <- round(ltm::biserial.cor(x, y, use = 'complete.obs', level = 2), digits = digits)
    paste("italic(r) == ", corr_coef)
  }
  else {
    corr_coef <- round(cor(x, y, use = 'complete.obs'), digits = digits)
    paste("italic(r) == ", corr_coef)
  }
}

correlation_plot <- function(data, x_var, y_var, x_coord, y_coord, binary) 
{
  if (binary)
  {
    labels <- data.frame(x = x_coord, y = y_coord,
                        label = corr_eqn(data[,x_var], data[, y_var], binary = T))
  }
  else {
    labels <- data.frame(x = x_coord, y = y_coord,
                         label = corr_eqn(data[,x_var], data[, y_var], binary = F))
  }
  x_var <- rlang::sym(quo_name(enquo(x_var)))
  y_var <- rlang::sym(quo_name(enquo(y_var)))
  
  
  ggplot2::ggplot(data = data) + ggplot2::aes(!! x_var, !! y_var) + 
    ggplot2::geom_point() + 
    ggplot2::geom_text(data = labels, ggplot2::aes(x, y, label = label), parse = TRUE, size = 7, col = 'darkred') + 
    ggplot2::geom_smooth(method = "lm", col = 'red') + ggplot2::ggtitle(paste(y_var, " Vs ", x_var)) +
    ggplot2::xlab(paste(x_var)) + ggplot2::ylab(paste(y_var))
}

get_all_pairwise_matchups <- function(start_year, end_year)
{
  all_years_submission = data.frame()
  for (k in seq(start_year, end_year))
  {
    if (k == 2020)
    {
      current_year <- read.csv("C:/Users/rusla/OneDrive/MarchMadness/March-Madness-Predictions/Stage1_2020/SampleSubmissionStage2.csv")
      current_year <- current_year %>%
        mutate(Season = as.numeric(substr(ID, 1, 4)),
               Team1 = as.numeric(substr(ID, 11, 14)),
               Team2 = as.numeric(substr(ID, 6, 9))) 
      colnames(current_year)[1:2] <- c("id","pred")
    }
    season = train1[train1$Season == k,]
    team1 = sort(unique(season$Team1))
    team2 = sort(unique(season$Team2))
    teams = sort(unique(c(team1,team2)))
    pairwise = expand.grid(teams, teams)
    pairwise = pairwise[pairwise$Var1 < pairwise$Var2,]
    pairwise = pairwise[order(pairwise$Var1),]
    year_submission = data.frame()
    for (i in 1:nrow(pairwise))
    {
      year_submission[i,1] = paste0(season[1,"Season"],"_",pairwise[i,1],"_",pairwise[i,2])
      year_submission[i,2] = pairwise[i,1]
      year_submission[i,3] = pairwise[i,2]
      year_submission[i,4] = season[1,"Season"]
      year_submission[i,5] = 0.5
    }
    
    all_years_submission <- rbind(all_years_submission, year_submission)
  }
  colnames(all_years_submission) <- c("id","Team2","Team1","Season","pred")
  all_years_submission <- all_years_submission[, c("id","pred","Season","Team1","Team2")]
  all_years_submission <- rbind(all_years_submission, current_year)
  all_years_submission <- all_years_submission[order(all_years_submission$Season,all_years_submission$Team2,all_years_submission$Team1),]
  all_years_submission <- tidyr::drop_na(all_years_submission)
  return(all_years_submission)
}

creating_slots_kaggle <- function() 
{
  # Run manual ifelse code for adding slots 
  # Round 0
  kaggle1$slot <- ifelse(kaggle1$Round == 0, "Play_In", NA)
  
  # slot seeds Round 1 W region
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & (kaggle1$Seed_Team1 %in%c(1,16) 
                                                          & kaggle1$Seed_Team2 %in%c(1,16)), "R1W1", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & kaggle1$Seed_Team1 %in%c(2,15) 
                         & kaggle1$Seed_Team2 %in%c(2,15), "R1W2", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & kaggle1$Seed_Team1 %in%c(3,14) 
                         & kaggle1$Seed_Team2 %in%c(3,14), "R1W3", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & kaggle1$Seed_Team1 %in%c(4,13) 
                         & kaggle1$Seed_Team2 %in%c(4,13), "R1W4", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & (kaggle1$Seed_Team1 %in%c(5,12) 
                                                          & kaggle1$Seed_Team2 %in%c(5,12)), "R1W5", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & kaggle1$Seed_Team1 %in%c(6,11) 
                         & kaggle1$Seed_Team2 %in%c(6,11), "R1W6", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & kaggle1$Seed_Team1 %in%c(7,10) 
                         & kaggle1$Seed_Team2 %in%c(7,10), "R1W7", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & kaggle1$Seed_Team1 %in%c(8,9) 
                         & kaggle1$Seed_Team2 %in%c(8,9), "R1W8", kaggle1$slot)
  
  ###############################################################################################
  # slot seeds Round 1 X region
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & (kaggle1$Seed_Team1 %in%c(1,16) 
                                                          & kaggle1$Seed_Team2 %in%c(1,16)), "R1X1", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & kaggle1$Seed_Team1 %in%c(2,15) 
                         & kaggle1$Seed_Team2 %in%c(2,15), "R1X2", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & kaggle1$Seed_Team1 %in%c(3,14) 
                         & kaggle1$Seed_Team2 %in%c(3,14), "R1X3", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & kaggle1$Seed_Team1 %in%c(4,13) 
                         & kaggle1$Seed_Team2 %in%c(4,13), "R1X4", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & (kaggle1$Seed_Team1 %in%c(5,12) 
                                                          & kaggle1$Seed_Team2 %in%c(5,12)), "R1X5", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & kaggle1$Seed_Team1 %in%c(6,11) 
                         & kaggle1$Seed_Team2 %in%c(6,11), "R1X6", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & kaggle1$Seed_Team1 %in%c(7,10) 
                         & kaggle1$Seed_Team2 %in%c(7,10), "R1X7", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & kaggle1$Seed_Team1 %in%c(8,9) 
                         & kaggle1$Seed_Team2 %in%c(8,9), "R1X8", kaggle1$slot)
  #########################################################################################
  # slot seeds Round 1 Y region
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & (kaggle1$Seed_Team1 %in%c(1,16) 
                                                          & kaggle1$Seed_Team2 %in%c(1,16)), "R1Y1", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & kaggle1$Seed_Team1 %in%c(2,15) 
                         & kaggle1$Seed_Team2 %in%c(2,15), "R1Y2", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & kaggle1$Seed_Team1 %in%c(3,14) 
                         & kaggle1$Seed_Team2 %in%c(3,14), "R1Y3", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & kaggle1$Seed_Team1 %in%c(4,13) 
                         & kaggle1$Seed_Team2 %in%c(4,13), "R1Y4", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & (kaggle1$Seed_Team1 %in%c(5,12) 
                                                          & kaggle1$Seed_Team2 %in%c(5,12)), "R1Y5", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & kaggle1$Seed_Team1 %in%c(6,11) 
                         & kaggle1$Seed_Team2 %in%c(6,11), "R1Y6", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & kaggle1$Seed_Team1 %in%c(7,10) 
                         & kaggle1$Seed_Team2 %in%c(7,10), "R1Y7", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & kaggle1$Seed_Team1 %in%c(8,9) 
                         & kaggle1$Seed_Team2 %in%c(8,9), "R1Y8", kaggle1$slot)
  
  #########################################################################################
  # slot seeds Round 1 Z region
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & (kaggle1$Seed_Team1 %in%c(1,16) 
                                                          & kaggle1$Seed_Team2 %in%c(1,16)), "R1Z1", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & kaggle1$Seed_Team1 %in%c(2,15) 
                         & kaggle1$Seed_Team2 %in%c(2,15), "R1Z2", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & kaggle1$Seed_Team1 %in%c(3,14) 
                         & kaggle1$Seed_Team2 %in%c(3,14), "R1Z3", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & kaggle1$Seed_Team1 %in%c(4,13) 
                         & kaggle1$Seed_Team2 %in%c(4,13), "R1Z4", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & (kaggle1$Seed_Team1 %in%c(5,12) 
                                                          & kaggle1$Seed_Team2 %in%c(5,12)), "R1Z5", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & kaggle1$Seed_Team1 %in%c(6,11) 
                         & kaggle1$Seed_Team2 %in%c(6,11), "R1Z6", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & kaggle1$Seed_Team1 %in%c(7,10) 
                         & kaggle1$Seed_Team2 %in%c(7,10), "R1Z7", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 1 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & kaggle1$Seed_Team1 %in%c(8,9) 
                         & kaggle1$Seed_Team2 %in%c(8,9), "R1Z8", kaggle1$slot)
  
  ##########################################################################################
  
  # slot seeds Round 2 W region
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & (kaggle1$Seed_Team1 %in%c(1,16,8,9) 
                                                          & kaggle1$Seed_Team2 %in%c(1,16,8,9)), "R2W1", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & kaggle1$Seed_Team1 %in%c(2,15,7,10) 
                         & kaggle1$Seed_Team2 %in%c(2,15,7,10), "R2W2", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & kaggle1$Seed_Team1 %in%c(3,14,6,11) 
                         & kaggle1$Seed_Team2 %in%c(3,14,6,11), "R2W3", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & kaggle1$Seed_Team1 %in%c(4,13,5,12) 
                         & kaggle1$Seed_Team2 %in%c(4,13,5,12), "R2W4", kaggle1$slot)
  
  ###############################################################################################
  # slot seeds Round 2 X region
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & (kaggle1$Seed_Team1 %in%c(1,16,8,9) 
                                                          & kaggle1$Seed_Team2 %in%c(1,16,8,9)), "R2X1", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & kaggle1$Seed_Team1 %in%c(2,15,7,10) 
                         & kaggle1$Seed_Team2 %in%c(2,15,7,10), "R2X2", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & kaggle1$Seed_Team1 %in%c(3,14,6,11) 
                         & kaggle1$Seed_Team2 %in%c(3,14,6,11), "R2X3", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & kaggle1$Seed_Team1 %in%c(4,13,5,12) 
                         & kaggle1$Seed_Team2 %in%c(4,13,5,12), "R2X4", kaggle1$slot)
  #########################################################################################
  # slot seeds Round 2 Y region
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & (kaggle1$Seed_Team1 %in%c(1,16,8,9) 
                                                          & kaggle1$Seed_Team2 %in%c(1,16,8,9)), "R2Y1", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & kaggle1$Seed_Team1 %in%c(2,15,7,10) 
                         & kaggle1$Seed_Team2 %in%c(2,15,7,10), "R2Y2", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & kaggle1$Seed_Team1 %in%c(3,14,6,11) 
                         & kaggle1$Seed_Team2 %in%c(3,14,6,11), "R2Y3", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & kaggle1$Seed_Team1 %in%c(4,13,5,12) 
                         & kaggle1$Seed_Team2 %in%c(4,13,5,12), "R2Y4", kaggle1$slot)
  
  #########################################################################################
  # slot seeds Round 2 Z region
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & (kaggle1$Seed_Team1 %in%c(1,16,8,9) 
                                                          & kaggle1$Seed_Team2 %in%c(1,16,8,9)), "R2Z1", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & kaggle1$Seed_Team1 %in%c(2,15,7,10) 
                         & kaggle1$Seed_Team2 %in%c(2,15,7,10), "R2Z2", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & kaggle1$Seed_Team1 %in%c(3,14,6,11) 
                         & kaggle1$Seed_Team2 %in%c(3,14,6,11), "R2Z3", kaggle1$slot)
  kaggle1$slot <- ifelse(kaggle1$Round == 2 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & kaggle1$Seed_Team1 %in%c(4,13,5,12) 
                         & kaggle1$Seed_Team2 %in%c(4,13,5,12), "R2Z4", kaggle1$slot)
  
  ##########################################################################################
  # slot seeds Round 3 W Region
  
  kaggle1$slot <- ifelse(kaggle1$Round == 3 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W" & kaggle1$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                         & kaggle1$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3W1", kaggle1$slot)
  
  kaggle1$slot <- ifelse(kaggle1$Round == 3 & kaggle1$Region_Team1 == "W"   
                         & kaggle1$Region_Team2 == "W" & kaggle1$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                         & kaggle1$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3W2", kaggle1$slot)
  
  ############################################################################################
  # slot seeds Round 3 X Region
  kaggle1$slot <- ifelse(kaggle1$Round == 3 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & kaggle1$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                         & kaggle1$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3X1", kaggle1$slot)
  
  kaggle1$slot <- ifelse(kaggle1$Round == 3 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X" & kaggle1$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                         & kaggle1$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3X2", kaggle1$slot)
  ############################################################################################
  # slot seeds Round 3 Y Region
  kaggle1$slot <- ifelse(kaggle1$Round == 3 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & kaggle1$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                         & kaggle1$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3Y1", kaggle1$slot)
  
  kaggle1$slot <- ifelse(kaggle1$Round == 3 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y" & kaggle1$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                         & kaggle1$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3Y2", kaggle1$slot)
  ############################################################################################
  # slot seeds Round 3 Z Region
  kaggle1$slot <- ifelse(kaggle1$Round == 3 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & kaggle1$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                         & kaggle1$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3Z1", kaggle1$slot)
  
  kaggle1$slot <- ifelse(kaggle1$Round == 3 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z" & kaggle1$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                         & kaggle1$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3Z2", kaggle1$slot)
  ############################################################################################
  #Round 4 
  kaggle1$slot <- ifelse(kaggle1$Round == 4 & kaggle1$Region_Team1 == "W" 
                         & kaggle1$Region_Team2 == "W", "R4W1", kaggle1$slot)
  
  kaggle1$slot <- ifelse(kaggle1$Round == 4 & kaggle1$Region_Team1 == "X" 
                         & kaggle1$Region_Team2 == "X", "R4X1", kaggle1$slot)
  
  kaggle1$slot <- ifelse(kaggle1$Round == 4 & kaggle1$Region_Team1 == "Y" 
                         & kaggle1$Region_Team2 == "Y", "R4Y1", kaggle1$slot)
  
  kaggle1$slot <- ifelse(kaggle1$Round == 4 & kaggle1$Region_Team1 == "Z" 
                         & kaggle1$Region_Team2 == "Z", "R4Z1", kaggle1$slot)
  ########################################################################################
  # Round 5
  kaggle1$slot <- ifelse(kaggle1$Round == 5 & (kaggle1$Region_Team1 == "W" | 
                                                 kaggle1$Region_Team2 == "W"), "R5WX",kaggle1$slot)
  
  kaggle1$slot <- ifelse(kaggle1$Round == 5 & (kaggle1$Region_Team1 == "Y" | 
                                                 kaggle1$Region_Team2 == "Y"), "R5YZ",kaggle1$slot)
  # Round 6
  kaggle1$slot <- ifelse(kaggle1$Round == 6, "R6CH",kaggle1$slot)
}

run_penalized_logit <- function(vars, alpha, min, k_fold)
{
  print(paste("vars: ", paste(vars, collapse = ', ')))
  
  cv.out <- cv.glmnet(data.matrix(training_continuous), 
                      as.factor(data.matrix(training_response[,1])), alpha = alpha, 
                      family = "binomial", type.measure = "class", nfolds = k_fold)
  plot(cv.out)
  
  lambda_min <- cv.out$lambda.min
  lambda_1se <- cv.out$lambda.1se
  
  if (min)
  {
    lasso_prob <- predict(cv.out,newx = data.matrix(testing_continuous), 
                          s=lambda_min, type= "response")
  } else {
    lasso_prob <- predict(cv.out,newx = data.matrix(testing_continuous), 
                          s=lambda_1se, type= "response")
  }
  lasso_prob <- ifelse(lasso_prob > 0.976, 0.99999, lasso_prob)
  lasso_prob <- ifelse(lasso_prob < 0.024, 0.00001, lasso_prob)
  loss <- round(logloss(testing_response[,1],lasso_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(lasso_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(testing_response, lasso_prob,pred_outcome))
  colnames(answer) <- c("True","Season","Pred_Prob","Pred_Outcome")
  
  count = 0
  for (i in 1:length(lasso_prob))
  {
    if (answer$True[i] == answer$Pred_Outcome[i])
    {
      count = count + 1
    }
  }
  # Prediction accuracy
  acc <- round(count / dim(answer)[1],4)
  print(paste("accuracy:", acc))
  
  return(list(cv.out, answer))
}

run_glmnet <- function(vars, parametersGrid, k_fold)
{
  set.seed(42)
  myFolds <- createFolds(training_continuous$Team1_Victory, k = k_fold)
  print(paste("vars: ", paste(vars, collapse = ', ')))
  training_continuous$Team1_Victory <- ifelse(training_continuous$Team1_Victory == 1, "Win","Loss")
  control <- trainControl(classProbs=TRUE, summaryFunction=mnLogLoss, number = k_fold,
                          index = myFolds, savePredictions = TRUE)
  glmnet <- train(as.formula(paste0("as.factor(Team1_Victory) ~ ", paste0(vars, collapse = " + "))), 
                  data = training_continuous, method = "glmnet", tuneGrid=parametersGrid, 
                  metric="logLoss", trControl=control, family = 'binomial',
                  preProcess = c('nzv','center','scale'))
  
  print(plot(glmnet))
  imp2 <- varImp(glmnet)
  print(barchart(sort(rowMeans(imp2$importance), decreasing = T), 
                 main = "GLMNet Variable Importance", xlab = "Average Level of Importance",
                 ylab = "Variables"))
  
  preds <- predict(glmnet, newdata = testing_continuous, type = "prob")
  glmnet_prob <- preds[, 2]
  
  preds_raw <- predict(glmnet, newdata = testing_continuous, type = 'raw')
  glmnet_prob <- ifelse(glmnet_prob > 0.99, 0.99999, glmnet_prob)
  glmnet_prob <- ifelse(glmnet_prob < 0.01, 0.00001, glmnet_prob)
  loss <- round(logloss(testing_response[,1],glmnet_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(glmnet_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(testing_response, glmnet_prob,pred_outcome))
  colnames(answer) <- c("True","Season","Pred_Prob","Pred_Outcome")
  
  count = 0
  for (i in 1:length(glmnet_prob))
  {
    if (answer$True[i] == answer$Pred_Outcome[i])
    {
      count = count + 1
    }
  }
  # Prediction accuracy
  acc <- round(count / dim(answer)[1],4)
  print(paste("accuracy:", acc))
  
  return(list(glmnet, answer))
}

run_random_forest <- function(vars, ntrees, k_fold)
{
  set.seed(42)
  myFolds <- createFolds(training_continuous$Team1_Victory, k = k_fold)
  print(paste("vars: ", paste(vars, collapse = ', ')))
  training_continuous$Team1_Victory <- ifelse(training_continuous$Team1_Victory == 1, "Win","Loss")
  control <- trainControl(classProbs=TRUE, summaryFunction=mnLogLoss, number = k_fold,
                          index = myFolds, savePredictions = TRUE)
  rF <- train(as.formula(paste0("as.factor(Team1_Victory) ~ ", paste0(vars, collapse = " + "))),
              ntree = ntrees, method = 'rf', metric="logLoss", trControl=control,
              data = training_continuous, importance = T, tuneLength = 15)
  print(plot(rF, main = 'Average Accuracy Across 5-Fold CV', 
       xlab = 'Number of Variables'))
  imp = varImp(rF)
  print(barchart(sort(rowMeans(imp$importance), decreasing = T), 
           main = "Random Forest Variable Importance", 
           xlab = "Average Level of Importance", ylab = "Variables"))
  
  preds <- predict(rF, newdata = testing_continuous, type = "prob")
  rf_prob <- preds[, 2]
  preds_raw <- predict(rF, newdata = testing_continuous, type = 'raw')
  rf_prob <- ifelse(rf_prob > 0.99, 0.99999, rf_prob)
  rf_prob <- ifelse(rf_prob < 0.01, 0.00001, rf_prob)
  loss <- round(logloss(testing_response[,1],rf_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(rf_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(testing_response, rf_prob,pred_outcome))
  colnames(answer) <- c("True","Season","Pred_Prob","Pred_Outcome")
  
  count = 0
  for (i in 1:length(rf_prob))
  {
    if (answer$True[i] == answer$Pred_Outcome[i])
    {
      count = count + 1
    }
  }
  # Prediction accuracy
  acc <- round(count / dim(answer)[1],4)
  print(paste("accuracy:", acc))
  
  return(list(rF, answer))
}

run_gbm <- function(vars, gbmGrid, k_fold)
{
  set.seed(42)
  myFolds <- createFolds(training_continuous$Team1_Victory, k = k_fold)
  print(paste("vars: ", paste(vars, collapse = ', ')))
  training_continuous$Team1_Victory <- ifelse(training_continuous$Team1_Victory == 1, "Win","Loss")
  control <- trainControl(classProbs=TRUE, summaryFunction=mnLogLoss, number = k_fold,
                          index = myFolds, savePredictions = TRUE)
  gbm <- train(as.formula(paste0("as.factor(Team1_Victory) ~ ", paste0(vars, collapse = " + "))),
               data = training_continuous, method="gbm", verbose=FALSE, tuneGrid = gbmGrid,
               metric="logLoss", trControl=control) 
  print(summary(gbm))
  preds <- predict(gbm, newdata = testing_continuous, type = "prob")
  gbm_prob <- preds[, 2]
  
  preds_raw <- predict(gbm, newdata = testing_continuous, type = 'raw')
  gbm_prob <- ifelse(gbm_prob > 0.99, 0.99999, gbm_prob)
  gbm_prob <- ifelse(gbm_prob < 0.01, 0.00001, gbm_prob)
  loss <- round(logloss(testing_response[,1],gbm_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(gbm_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(testing_response, gbm_prob,pred_outcome))
  colnames(answer) <- c("True","Season","Pred_Prob","Pred_Outcome")
  
  count = 0
  for (i in 1:length(gbm_prob))
  {
    if (answer$True[i] == answer$Pred_Outcome[i])
    {
      count = count + 1
    }
  }
  # Prediction accuracy
  acc <- round(count / dim(answer)[1],4)
  print(paste("accuracy:", acc))
  
  return(list(gbm, answer))
}

run_xgboost <- function(vars, parametersGrid, k_fold)
{
  set.seed(42)
  myFolds <- createFolds(training_continuous$Team1_Victory, k = k_fold)
  print(paste("vars: ", paste(vars, collapse = ', ')))
  training_continuous$Team1_Victory <- ifelse(training_continuous$Team1_Victory == 1, "Win","Loss")
  control <- trainControl(classProbs=TRUE, summaryFunction=mnLogLoss, number = k_fold,
                          index = myFolds, savePredictions = TRUE)
  xgboost <- train(as.formula(paste0("as.factor(Team1_Victory) ~ ", paste0(vars, collapse = " + "))), 
                   data = training_continuous, method = "xgbTree", 
                   metric="logLoss", trControl=control, tuneLength = 3) #tuneGrid=parametersGrid)
  
  #print(plot(xgboost))
  imp2 <- varImp(xgboost)
  print(barchart(sort(rowMeans(imp2$importance), decreasing = T), 
           main = "XGBoost Variable Importance", xlab = "Average Level of Importance",
           ylab = "Variables"))
  
  preds <- predict(xgboost, newdata = testing_continuous, type = "prob")
  xgboost_prob <- preds[, 2]
  
  preds_raw <- predict(xgboost, newdata = testing_continuous, type = 'raw')
  xgboost_prob <- ifelse(xgboost_prob > 0.99, 0.99999, xgboost_prob)
  xgboost_prob <- ifelse(xgboost_prob < 0.01, 0.00001, xgboost_prob)
  loss <- round(logloss(testing_response[,1],xgboost_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(xgboost_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(testing_response, xgboost_prob,pred_outcome))
  colnames(answer) <- c("True","Season","Pred_Prob","Pred_Outcome")
  
  count = 0
  for (i in 1:length(xgboost_prob))
  {
    if (answer$True[i] == answer$Pred_Outcome[i])
    {
      count = count + 1
    }
  }
  # Prediction accuracy
  acc <- round(count / dim(answer)[1],4)
  print(paste("accuracy:", acc))
  
  return(list(xgboost, answer))
}

run_nn <- function(vars, num_epochs)
{
  print(paste("vars: ", paste(vars, collapse = ', ')))
  y_binary <- to_categorical(training_continuous$Team1_Victory)
  model <- keras::keras_model_sequential() 
  model %>% keras::layer_dense(units = 32, activation = "relu", input_shape = ncol(training_continuous)-1) %>%
    keras::layer_dense(units = 16, activation = "relu") %>%
    keras::layer_dense(units = 8, activation = "relu") %>%
    keras::layer_dense(units = 4, activation = "relu") %>%
    keras::layer_dense(units = ncol(y_binary), activation = "softmax")
  
  compile(model, loss = "binary_crossentropy", optimizer = 'adam', metrics = "binary_crossentropy")
  history <- fit(model,data.matrix(subset(training_continuous, select = c(-Team1_Victory))), 
                 y_binary, epochs = num_epochs, validation_split = 0.2)
  plot(history)
  
  nn_prob <- model %>% keras::predict_proba(data.matrix(subset(testing_continuous, select=-c(Team1_Victory))))
  nn_prob <- nn_prob[,2]
  loss <- round(logloss(testing_response[,1],nn_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(nn_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(testing_response, nn_prob,pred_outcome))
  colnames(answer) <- c("True","Season","Pred_Prob","Pred_Outcome")
  
  count = 0
  for (i in 1:length(nn_prob))
  {
    if (answer$True[i] == answer$Pred_Outcome[i])
    {
      count = count + 1
    }
  }
  # Prediction accuracy
  acc <- round(count / dim(answer)[1],4)
  print(paste("accuracy:", acc))
  return(list(model, history, answer))
}

kaggle_predictions <- function(model_output, model_type, start_year, end_year, vars, min = F, names)
{
  kaggle_test <- kaggle1 %>% filter(Season %in% start_year:end_year)
  if (model_type == 'logit')
  {
    if (min)
    {
      kaggle_preds <- as.data.frame(1 - predict(model_output[[1]], 
                                                newx = data.matrix(kaggle_test[,vars]), s=lambda_min, type= "response"))
    } else {
      kaggle_preds <- as.data.frame(1 - predict(model_output[[1]], 
                                                newx = data.matrix(kaggle_test[,vars]), s=lambda_1se, type= "response"))
    }
  } else if (model_type %in% c('rf'))  
  {
    kaggle_preds <- predict(model_output[[1]], newdata = kaggle_test[,vars], type = "prob")[,1]
  } else if (model_type %in% c('nn'))
  {
    # model_output %>% keras::predict_proba(data.matrix(kaggle_test[,vars]))
    kaggle_preds <- model_output %>% keras::predict_proba(data.matrix(kaggle_test[,vars]))
    kaggle_preds <- kaggle_preds[,2]
  } else {
    kaggle_preds <- predict(model_output, newdata = kaggle_test[,vars], type = "prob")[,1]
  }
  ID <- kaggle_test %>% dplyr::select(id)
  kaggle_preds <- cbind(ID, kaggle_preds)
  names(kaggle_preds) <- c("ID","Pred")
  
  if (names)
  {
    kaggle_preds <- kaggle_preds %>% 
      mutate(Team1 = as.numeric(substr(ID, 6, 9)),
             Team2 = as.numeric(substr(ID, 11, 14))) %>%
      left_join(Teams[, c('Team_Id','Team_Name')], by = c("Team1" = "Team_Id")) %>%
      left_join(Teams[, c('Team_Id','Team_Name')], by = c("Team2" = "Team_Id")) %>%
      rename(Team1_Name = 'Team_Name.x', Team2_Name = 'Team_Name.y')
  }
  return(kaggle_preds %>% arrange(ID))
}

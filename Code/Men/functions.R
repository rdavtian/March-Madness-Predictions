# Informative Websites to visit
#https://www.masseyratings.com/ranks?s=cb&sym=cmp&top=4
#https://www.masseyratings.com/ranks?s=cb&dt=20200311
#https://www.usatoday.com/sports/ncaab/polls/ap-poll/2019-2020/2020-03-09/
#https://www.nbastuffer.com/nba-moneyball/
#https://statathlon.com/four-factors-basketball-success/
#https://www.teamrankings.com/ncaa-basketball/ranking/predictive-by-other?date=2021-03-14
#https://masseyratings.com/cb/ncaa-d1/ratings

#MTeamLocations.csv (edit)
#MTourneyHosts.csv (edit)
#Teams_03_23_Rankings.xlsx (edit)
#CitiesEnriched.csv (edit)

# load libraries needed
library(ggplot2)
library(knitr)
library(kableExtra)
library(magrittr)
library(ggthemes)
library(pscl) 
library(ROCR) 
library(glmnet)
library(sqldf)
library(geosphere)
library(dplyr)
library(caret)
library(usdm)
library(rpart)
library(rpart.plot)
library(class)
require(neuralnet)
require(nnet)
library(keras)
library(openxlsx)
library(stringr)
library(tidyr)
library(scales)
library(rlang)
library(DT)

matchups <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)

slot_order <- c("Play_In","R1W1","R1W8","R1W5","R1W4","R1W6","R1W3","R1W7","R1W2",
                "R1X1","R1X8","R1X5","R1X4","R1X6","R1X3","R1X7","R1X2",
                "R1Y1","R1Y8","R1Y5","R1Y4","R1Y6","R1Y3","R1Y7","R1Y2",
                "R1Z1","R1Z8","R1Z5","R1Z4","R1Z6","R1Z3","R1Z7","R1Z2",
                "R2W1","R2W4","R2W3","R2W2","R2X1","R2X4","R2X3","R2X2",
                "R2Y1","R2Y4","R2Y3","R2Y2","R2Z1","R2Z4","R2Z3","R2Z2",
                "R3W1","R3W2","R3X1","R3X2","R3Y1","R3Y2","R3Z1","R3Z2",
                "R4W1","R4X1","R4Y1","R4Z1","R5WX","R5YZ","R6CH")

# Calculate adjusted wins, adjusted losses, adjusted win %, use reg season compact 
get_adjusted_wins <- function(df)
{
  winners <- df %>%
    group_by(Season, WTeamID) %>%
    summarise(Adj_Wins = sum(Wadj_win), 
              W = sum(WTeamID_Won),
              .groups = "drop") %>%
    rename('Team' = WTeamID)
  
  losers <- df %>% 
    group_by(Season, LTeamID) %>%
    summarise(Adj_Losses = sum(Ladj_win),
              L = sum(LTeamID_Lost),
              .groups = "drop") %>%
    rename('Team' = LTeamID)
  
  adj_standings <- winners %>% 
    full_join(losers, by = c("Season","Team")) %>%
    filter(Season >= 2003) %>%
    tidyr::replace_na(list(Adj_Wins = 0, Adj_Losses = 0, W = 0, L = 0)) %>%
    mutate(Adj_Win_Pct = Adj_Wins / (Adj_Wins + Adj_Losses),
           Win_Pct = W / (W + L))
  
  return(adj_standings)
  
}

get_reg_season_avg_team_stats <- function(df)
{
  df <- df %>%
    mutate(WNum_Poss = (WFGA - WOR) + WTO + (0.475*WFTA),
           LNum_Poss = (LFGA - LOR) + LTO + (0.475*LFTA),
           WPace = (WNum_Poss / 40) * 100,
           LPace = (LNum_Poss / 40) * 100,
           WOff_Rating = (WScore / WNum_Poss) * 100,
           LOff_Rating = (LScore / LNum_Poss) * 100,
           WDef_Rating = (LScore / LNum_Poss) * 100,
           LDef_Rating = (WScore / WNum_Poss) * 100,
           WNet_Eff = WOff_Rating - LOff_Rating,
           LNet_Eff = LOff_Rating - WOff_Rating,
           WMargin_Adj = 100*((WScore - LScore) / WNum_Poss),
           LMargin_Adj = 100*((LScore - WScore) / LNum_Poss),
           WShoot_Eff = 100*(WScore / (WFGA + 0.475*WFTA)),
           LShoot_Eff = 100*(LScore / (LFGA + 0.475*LFTA)),
           WScore_Opp = ((WFGA + 0.475*WFTA) / (WScore)),
           LScore_Opp = ((LFGA + 0.475*LFTA) / (LScore)),
           WEFG_Pct = 100*(((WFGM - WFGM3) + (1.5*WFGM3)) / (WFGA)),
           LEFG_Pct = 100*(((LFGM - LFGM3) + (1.5*LFGM3)) / (LFGA)),
           WFTRate = WFTA / WFGA,
           LFTRate = LFTA / LFGA,
           W3PRate = WFGA3 / WFGA,
           L3PRate = LFGA3 / LFGA,
           WPie = ((WScore + WFGM + WFTM - WFGA - WFTA + WDR + 0.5*WOR + WAst + WStl + WBlk - WPF - WTO) / 
                     ((WScore + WFGM + WFTM - WFGA - WFTA + WDR + 0.5*WOR + WAst + WStl + WBlk - WPF - WTO) + 
                        (LScore + LFGM + LFTM - LFGA - LFTA + LDR + 0.5*LOR + LAst + LStl + LBlk - LPF - LTO))),
           LPie = ((LScore + LFGM + LFTM - LFGA - LFTA + LDR + 0.5*LOR + LAst + LStl + LBlk - LPF - LTO) / 
                     ((WScore + WFGM + WFTM - WFGA - WFTA + WDR + 0.5*WOR + WAst + WStl + WBlk - WPF - WTO) + 
                        (LScore + LFGM + LFTM - LFGA - LFTA + LDR + 0.5*LOR + LAst + LStl + LBlk - LPF - LTO))),
           WTie = 100*(WPie / (WPie + LPie)),
           LTie = 100*(LPie / (LPie + WPie)),
           WOR_Pct = 100*(WOR / (WOR + LDR)),
           LOR_Pct = 100*(LOR / (LOR + WDR)),
           WDR_Pct = 100*(WDR / (WDR + LOR)),
           LDR_Pct = 100*(LDR / (LDR + WOR)),
           WFT_Pct = 100*(WFTM / WFTA),
           LFT_Pct = 100*(LFTM / LFTA),
           WTO_Pct = 100*(WTO / WNum_Poss),
           LTO_Pct = 100*(LTO / LNum_Poss),
           WTS_Pct = 100 * (WScore / (2 * (WFGA + 0.475*WFTA))),
           LTS_Pct = 100 * (LScore / (2 * (LFGA + 0.475*LFTA))),
           W3PT_Pct = 100 * (WFGM3 / WFGA3),
           L3PT_Pct = 100 * (LFGM3 / LFGA3),
           WAstRatio = 100 * (WAst / (WFGA + (0.475 * WFTA) + WAst + WTO)),
           LAstRatio = 100 * (LAst / (LFGA + (0.475 * LFTA) + LAst + LTO)),
           WReb_Pct = 100*((WDR + WOR) / (WDR + WOR + LDR + LOR)),
           LReb_Pct = 100*((LDR + LOR) / (LDR + LOR + WDR + WOR)),
           WFour_Factor = (0.4*WEFG_Pct)+(0.25*WTO_Pct)+(0.2*WOR_Pct)+(0.15*(WFTA / WFGA)),
           LFour_Factor = (0.4*LEFG_Pct)+(0.25*LTO_Pct)+(0.2*LOR_Pct)+(0.15*(LFTA / LFGA)),
           WBLK_Pct = 100*(WBlk / (LFGA - LFGA3)),
           LBLK_Pct = 100*(LBlk / (WFGA - WFGA3)),
           WStl_Pct = 100*(WStl / LNum_Poss),
           LStl_Pct = 100*(LStl / WNum_Poss),
           WMargin = case_when(NumOT > 0 ~ as.numeric(1),
                               WScore - LScore > 20 ~ as.numeric(20),
                               TRUE ~ as.numeric(WScore - LScore)),
           LMargin = case_when(NumOT > 0 ~ as.numeric(1),
                               LScore - WScore < -20 ~ as.numeric(-20),
                               TRUE ~ as.numeric(LScore - WScore)))
  
  winner_vars <- c("Season", "DayNum", "NumOT", names(df[startsWith(names(df), "W")]))
  loser_vars <- c("Season", "DayNum", "NumOT", names(df[startsWith(names(df), "L")]))
  
  winner_df <- df[winner_vars] %>% dplyr::select(-WLoc)
  loser_df <- df[loser_vars]
  
  colnames(winner_df) <- stringr::str_remove(colnames(winner_df), '^W')
  colnames(loser_df) <- stringr::str_remove(colnames(winner_df), '^L')
  
  combined_df <- rbind(winner_df, loser_df)
  
  team_stats <- combined_df %>%
    group_by(Season, TeamID) %>% 
    mutate(weights = DayNum / sum(DayNum))
  
  team_stats <- team_stats %>% #filter(TeamID == 1181) %>% 
    group_by(Season, TeamID) %>%
    summarise(Score = weighted.mean(Score, weights, na.rm=TRUE),
              FGM = weighted.mean(FGM, weights, na.rm=TRUE),
              FGA = weighted.mean(FGA, weights, na.rm=TRUE),
              FGM3 = weighted.mean(FGM3, weights, na.rm=TRUE),
              FGA3 = weighted.mean(FGA3, weights, na.rm=TRUE),
              `3PRate` = weighted.mean(`3PRate`, weights, na.rm=TRUE),
              FTM = weighted.mean(FTM, weights, na.rm=TRUE),
              FTA = weighted.mean(FTA, weights, na.rm=TRUE),
              OR = weighted.mean(OR, weights, na.rm=TRUE),
              DR = weighted.mean(DR, weights, na.rm=TRUE),
              Ast = weighted.mean(Ast, weights, na.rm=TRUE),
              Stl = weighted.mean(Stl, weights, na.rm=TRUE),
              Blk = weighted.mean(Blk, weights, na.rm=TRUE),
              PF = weighted.mean(PF, weights, na.rm=TRUE),
              Num_Poss = weighted.mean(Num_Poss, weights, na.rm=TRUE),
              Pace = weighted.mean(Pace, weights, na.rm=TRUE),
              Off_Rating = weighted.mean(Off_Rating, weights, na.rm=TRUE),
              Def_Rating = weighted.mean(Def_Rating, weights, na.rm=TRUE),
              Net_Eff = weighted.mean(Net_Eff, weights, na.rm=TRUE),
              Margin_Adj = weighted.mean(Margin_Adj, weights, na.rm=TRUE),
              Shoot_Eff = weighted.mean(Shoot_Eff, weights, na.rm=TRUE),
              Score_Opp = weighted.mean(Score_Opp, weights, na.rm=TRUE),
              EFG_Pct = weighted.mean(EFG_Pct, weights, na.rm=TRUE),
              Pie = weighted.mean(Pie, weights, na.rm=TRUE),
              Tie = weighted.mean(Tie, weights, na.rm=TRUE),
              OR_Pct = weighted.mean(OR_Pct, weights, na.rm=TRUE),
              DR_Pct = weighted.mean(DR_Pct, weights, na.rm=TRUE),
              FT_Pct = weighted.mean(FT_Pct, weights, na.rm=TRUE),
              FTRate = weighted.mean(FTRate, weights, na.rm=TRUE),
              TO_Pct = weighted.mean(TO_Pct, weights, na.rm=TRUE),
              TS_Pct = weighted.mean(TS_Pct, weights, na.rm=TRUE),
              `3PT_Pct` = weighted.mean(`3PT_Pct`, weights, na.rm=TRUE),
              AstRatio = weighted.mean(AstRatio, weights, na.rm=TRUE),
              Reb_Pct = weighted.mean(Reb_Pct, weights, na.rm=TRUE),
              Four_Factor = weighted.mean(Four_Factor, weights, na.rm=TRUE),
              BLK_Pct = weighted.mean(BLK_Pct, weights, na.rm=TRUE),
              Stl_Pct = weighted.mean(Stl_Pct, weights, na.rm=TRUE),
              Margin = weighted.mean(Margin, weights, na.rm=TRUE),
              .groups = "drop") #%>% 
    #summarise_all(mean, na.rm = TRUE) %>% 
    #dplyr::select(-DayNum, -NumOT, -weights)
  
  colnames(team_stats)[3:ncol(team_stats)] <- paste0("Avg_", colnames(team_stats)[3:ncol(team_stats)])
  
  return(team_stats)
}

normFunc <- function(x) 
{
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

round_finder <- function(region1, region2, seed1, seed2){
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

get_slots <- function(df) 
{
  # Run manual ifelse code for adding slots 
  # Round 0
  df$Slot <- ifelse(df$Round == 0, "Play_In", NA)
  
  # slot seeds Round 1 W region
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "W" 
                        & df$Region_Team2 == "W" & (df$Seed_Team1 %in%c(1,16) 
                        & df$Seed_Team2 %in%c(1,16)), "R1W1", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "W" 
                        & df$Region_Team2 == "W" & df$Seed_Team1 %in%c(2,15) 
                        & df$Seed_Team2 %in%c(2,15), "R1W2", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "W" 
                        & df$Region_Team2 == "W" & df$Seed_Team1 %in%c(3,14) 
                        & df$Seed_Team2 %in%c(3,14), "R1W3", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "W" 
                        & df$Region_Team2 == "W" & df$Seed_Team1 %in%c(4,13) 
                        & df$Seed_Team2 %in%c(4,13), "R1W4", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "W" 
                        & df$Region_Team2 == "W" & (df$Seed_Team1 %in%c(5,12) 
                        & df$Seed_Team2 %in%c(5,12)), "R1W5", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "W" 
                        & df$Region_Team2 == "W" & df$Seed_Team1 %in%c(6,11) 
                        & df$Seed_Team2 %in%c(6,11), "R1W6", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "W" 
                        & df$Region_Team2 == "W" & df$Seed_Team1 %in%c(7,10) 
                        & df$Seed_Team2 %in%c(7,10), "R1W7", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "W" 
                        & df$Region_Team2 == "W" & df$Seed_Team1 %in%c(8,9) 
                        & df$Seed_Team2 %in%c(8,9), "R1W8", df$Slot)
  
  ###############################################################################################
  # slot seeds Round 1 X region
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "X" 
                        & df$Region_Team2 == "X" & (df$Seed_Team1 %in%c(1,16) 
                        & df$Seed_Team2 %in%c(1,16)), "R1X1", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "X" 
                        & df$Region_Team2 == "X" & df$Seed_Team1 %in%c(2,15) 
                        & df$Seed_Team2 %in%c(2,15), "R1X2", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "X" 
                        & df$Region_Team2 == "X" & df$Seed_Team1 %in%c(3,14) 
                        & df$Seed_Team2 %in%c(3,14), "R1X3", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "X" 
                        & df$Region_Team2 == "X" & df$Seed_Team1 %in%c(4,13) 
                        & df$Seed_Team2 %in%c(4,13), "R1X4", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "X" 
                        & df$Region_Team2 == "X" & (df$Seed_Team1 %in%c(5,12) 
                        & df$Seed_Team2 %in%c(5,12)), "R1X5", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "X" 
                        & df$Region_Team2 == "X" & df$Seed_Team1 %in%c(6,11) 
                        & df$Seed_Team2 %in%c(6,11), "R1X6", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "X" 
                        & df$Region_Team2 == "X" & df$Seed_Team1 %in%c(7,10) 
                        & df$Seed_Team2 %in%c(7,10), "R1X7", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "X" 
                        & df$Region_Team2 == "X" & df$Seed_Team1 %in%c(8,9) 
                        & df$Seed_Team2 %in%c(8,9), "R1X8", df$Slot)
  #########################################################################################
  # slot seeds Round 1 Y region
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Y" 
                        & df$Region_Team2 == "Y" & (df$Seed_Team1 %in%c(1,16) 
                        & df$Seed_Team2 %in%c(1,16)), "R1Y1", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Y" 
                        & df$Region_Team2 == "Y" & df$Seed_Team1 %in%c(2,15) 
                        & df$Seed_Team2 %in%c(2,15), "R1Y2", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Y" 
                        & df$Region_Team2 == "Y" & df$Seed_Team1 %in%c(3,14) 
                        & df$Seed_Team2 %in%c(3,14), "R1Y3", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Y" 
                        & df$Region_Team2 == "Y" & df$Seed_Team1 %in%c(4,13) 
                        & df$Seed_Team2 %in%c(4,13), "R1Y4", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Y" 
                        & df$Region_Team2 == "Y" & (df$Seed_Team1 %in%c(5,12) 
                        & df$Seed_Team2 %in%c(5,12)), "R1Y5", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Y" 
                        & df$Region_Team2 == "Y" & df$Seed_Team1 %in%c(6,11) 
                        & df$Seed_Team2 %in%c(6,11), "R1Y6", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Y" 
                        & df$Region_Team2 == "Y" & df$Seed_Team1 %in%c(7,10) 
                        & df$Seed_Team2 %in%c(7,10), "R1Y7", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Y" 
                        & df$Region_Team2 == "Y" & df$Seed_Team1 %in%c(8,9) 
                        & df$Seed_Team2 %in%c(8,9), "R1Y8", df$Slot)
  
  #########################################################################################
  # slot seeds Round 1 Z region
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Z" 
                        & df$Region_Team2 == "Z" & (df$Seed_Team1 %in%c(1,16) 
                        & df$Seed_Team2 %in%c(1,16)), "R1Z1", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Z" 
                        & df$Region_Team2 == "Z" & df$Seed_Team1 %in%c(2,15) 
                        & df$Seed_Team2 %in%c(2,15), "R1Z2", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Z" 
                        & df$Region_Team2 == "Z" & df$Seed_Team1 %in%c(3,14) 
                        & df$Seed_Team2 %in%c(3,14), "R1Z3", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Z" 
                        & df$Region_Team2 == "Z" & df$Seed_Team1 %in%c(4,13) 
                        & df$Seed_Team2 %in%c(4,13), "R1Z4", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Z" 
                        & df$Region_Team2 == "Z" & (df$Seed_Team1 %in%c(5,12) 
                        & df$Seed_Team2 %in%c(5,12)), "R1Z5", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Z" 
                        & df$Region_Team2 == "Z" & df$Seed_Team1 %in%c(6,11) 
                        & df$Seed_Team2 %in%c(6,11), "R1Z6", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Z" 
                        & df$Region_Team2 == "Z" & df$Seed_Team1 %in%c(7,10) 
                        & df$Seed_Team2 %in%c(7,10), "R1Z7", df$Slot)
  df$Slot <- ifelse(df$Round == 1 & df$Region_Team1 == "Z" 
                        & df$Region_Team2 == "Z" & df$Seed_Team1 %in%c(8,9) 
                        & df$Seed_Team2 %in%c(8,9), "R1Z8", df$Slot)
  
  ##########################################################################################
  
  # slot seeds Round 2 W region
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "W" 
                       & df$Region_Team2 == "W" & (df$Seed_Team1 %in%c(1,16,8,9) 
                                                       & df$Seed_Team2 %in%c(1,16,8,9)), "R2W1", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "W" 
                       & df$Region_Team2 == "W" & df$Seed_Team1 %in%c(2,15,7,10) 
                       & df$Seed_Team2 %in%c(2,15,7,10), "R2W2", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "W" 
                       & df$Region_Team2 == "W" & df$Seed_Team1 %in%c(3,14,6,11) 
                       & df$Seed_Team2 %in%c(3,14,6,11), "R2W3", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "W" 
                       & df$Region_Team2 == "W" & df$Seed_Team1 %in%c(4,13,5,12) 
                       & df$Seed_Team2 %in%c(4,13,5,12), "R2W4", df$Slot)
  
  ###############################################################################################
  # slot seeds Round 2 X region
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "X" 
                       & df$Region_Team2 == "X" & (df$Seed_Team1 %in%c(1,16,8,9) 
                                                       & df$Seed_Team2 %in%c(1,16,8,9)), "R2X1", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "X" 
                       & df$Region_Team2 == "X" & df$Seed_Team1 %in%c(2,15,7,10) 
                       & df$Seed_Team2 %in%c(2,15,7,10), "R2X2", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "X" 
                       & df$Region_Team2 == "X" & df$Seed_Team1 %in%c(3,14,6,11) 
                       & df$Seed_Team2 %in%c(3,14,6,11), "R2X3", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "X" 
                       & df$Region_Team2 == "X" & df$Seed_Team1 %in%c(4,13,5,12) 
                       & df$Seed_Team2 %in%c(4,13,5,12), "R2X4", df$Slot)
  #########################################################################################
  # slot seeds Round 2 Y region
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "Y" 
                       & df$Region_Team2 == "Y" & (df$Seed_Team1 %in%c(1,16,8,9) 
                                                       & df$Seed_Team2 %in%c(1,16,8,9)), "R2Y1", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "Y" 
                       & df$Region_Team2 == "Y" & df$Seed_Team1 %in%c(2,15,7,10) 
                       & df$Seed_Team2 %in%c(2,15,7,10), "R2Y2", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "Y" 
                       & df$Region_Team2 == "Y" & df$Seed_Team1 %in%c(3,14,6,11) 
                       & df$Seed_Team2 %in%c(3,14,6,11), "R2Y3", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "Y" 
                       & df$Region_Team2 == "Y" & df$Seed_Team1 %in%c(4,13,5,12) 
                       & df$Seed_Team2 %in%c(4,13,5,12), "R2Y4", df$Slot)
  
  #########################################################################################
  # slot seeds Round 2 Z region
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "Z" 
                       & df$Region_Team2 == "Z" & (df$Seed_Team1 %in%c(1,16,8,9) 
                                                       & df$Seed_Team2 %in%c(1,16,8,9)), "R2Z1", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "Z" 
                       & df$Region_Team2 == "Z" & df$Seed_Team1 %in%c(2,15,7,10) 
                       & df$Seed_Team2 %in%c(2,15,7,10), "R2Z2", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "Z" 
                       & df$Region_Team2 == "Z" & df$Seed_Team1 %in%c(3,14,6,11) 
                       & df$Seed_Team2 %in%c(3,14,6,11), "R2Z3", df$Slot)
  df$Slot <- ifelse(df$Round == 2 & df$Region_Team1 == "Z" 
                       & df$Region_Team2 == "Z" & df$Seed_Team1 %in%c(4,13,5,12) 
                       & df$Seed_Team2 %in%c(4,13,5,12), "R2Z4", df$Slot)
  
  ##########################################################################################
  # slot seeds Round 3 W Region
  
  df$Slot <- ifelse(df$Round == 3 & df$Region_Team1 == "W" 
                       & df$Region_Team2 == "W" & df$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                       & df$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3W1", df$Slot)
  
  df$Slot <- ifelse(df$Round == 3 & df$Region_Team1 == "W"   
                       & df$Region_Team2 == "W" & df$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                       & df$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3W2", df$Slot)
  
  ############################################################################################
  # slot seeds Round 3 X Region
  df$Slot <- ifelse(df$Round == 3 & df$Region_Team1 == "X" 
                       & df$Region_Team2 == "X" & df$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                       & df$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3X1", df$Slot)
  
  df$Slot <- ifelse(df$Round == 3 & df$Region_Team1 == "X" 
                       & df$Region_Team2 == "X" & df$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                       & df$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3X2", df$Slot)
  ############################################################################################
  # slot seeds Round 3 Y Region
  df$Slot <- ifelse(df$Round == 3 & df$Region_Team1 == "Y" 
                       & df$Region_Team2 == "Y" & df$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                       & df$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3Y1", df$Slot)
  
  df$Slot <- ifelse(df$Round == 3 & df$Region_Team1 == "Y" 
                       & df$Region_Team2 == "Y" & df$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                       & df$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3Y2", df$Slot)
  ############################################################################################
  # slot seeds Round 3 Z Region
  df$Slot <- ifelse(df$Round == 3 & df$Region_Team1 == "Z" 
                       & df$Region_Team2 == "Z" & df$Seed_Team1 %in% c(1,16,8,9,5,12,4,13) 
                       & df$Seed_Team2 %in% c(1,16,8,9,5,12,4,13), "R3Z1", df$Slot)
  
  df$Slot <- ifelse(df$Round == 3 & df$Region_Team1 == "Z" 
                       & df$Region_Team2 == "Z" & df$Seed_Team1 %in% c(2,15,7,10,6,11,3,14) 
                       & df$Seed_Team2 %in% c(2,15,7,10,6,11,3,14), "R3Z2", df$Slot)
  ############################################################################################
  #Round 4 
  df$Slot <- ifelse(df$Round == 4 & df$Region_Team1 == "W" & df$Region_Team2 == "W", "R4W1", df$Slot)
  
  df$Slot <- ifelse(df$Round == 4 & df$Region_Team1 == "X" & df$Region_Team2 == "X", "R4X1", df$Slot)
  
  df$Slot <- ifelse(df$Round == 4 & df$Region_Team1 == "Y" & df$Region_Team2 == "Y", "R4Y1", df$Slot)
  
  df$Slot <- ifelse(df$Round == 4 & df$Region_Team1 == "Z" & df$Region_Team2 == "Z", "R4Z1", df$Slot)
  ########################################################################################
  # Round 5
  df$Slot <- ifelse(df$Round == 5 & (df$Region_Team1 == "W" | df$Region_Team2 == "W"), "R5WX",df$Slot)
  
  df$Slot <- ifelse(df$Round == 5 & (df$Region_Team1 == "Y" | df$Region_Team2 == "Y"), "R5YZ",df$Slot)
  # Round 6
  df$Slot <- ifelse(df$Round == 6, "R6CH", df$Slot)
  
  return(df)
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

get_conf_tourn_record <- function(df)
{
  team1 <- df %>% 
    group_by(Season, ConfAbbrev, Team1) %>%
    summarise(total_wins_team1 = sum(Team1_Victory),
              total_losses_team1 = sum(Team2_Victory), .groups = "drop")  %>%
    rename("Team" = Team1)
  team2 <- df %>% 
    group_by(Season, ConfAbbrev, Team2) %>%
    summarise(total_wins_team2 = sum(Team2_Victory),
              total_losses_team2 = sum(Team1_Victory), .groups = "drop")  %>%
    rename("Team" = Team2)
  
  team_games <- team1 %>% 
    full_join(team2, by = c("Season","Team","ConfAbbrev")) %>% 
    mutate(total_wins_team1 = tidyr::replace_na(total_wins_team1, 0),
           total_wins_team2 = tidyr::replace_na(total_wins_team2, 0),
           total_losses_team1 = tidyr::replace_na(total_losses_team1, 0),
           total_losses_team2 = tidyr::replace_na(total_losses_team2, 0),
           conf_tourn_num_wins = total_wins_team1 + total_wins_team2,
           conf_tourn_num_losses = total_losses_team1 + total_losses_team2,
           conf_tourn_win_pct = conf_tourn_num_wins / (conf_tourn_num_wins + conf_tourn_num_losses),
           conf_champ = case_when(conf_tourn_win_pct == 1 ~ 1, TRUE ~ 0)) %>% 
    dplyr::select(Season, ConfAbbrev, Team, conf_tourn_num_wins, conf_tourn_num_losses, conf_tourn_win_pct, conf_champ)
  
  return(team_games)
}

# Take into account for num tournament games for each team prior to current tournament
past_tourney_history <- function(df) 
{
  team1 <- df %>% 
    group_by(Team1, Season) %>%
    summarise(total_games_team1 = n(), .groups = "drop")  %>%
    rename("Team" = Team1)
  team2 <- df %>% 
    group_by(Team2, Season) %>%
    summarise(total_games_team2 = n(),.groups = "drop")  %>%
    rename("Team" = Team2)
  
  team_games <- team1 %>% 
    full_join(team2, by = c("Season","Team")) %>% 
    mutate(total_games_team1 = tidyr::replace_na(total_games_team1, 0),
           total_games_team2 = tidyr::replace_na(total_games_team2, 0),
           num_games = total_games_team1 + total_games_team2,
           advanced = case_when(num_games >= 2 ~ 1, TRUE ~ 0),
           sweet_16 = case_when(num_games >= 3 ~ 1, TRUE ~ 0),
           elite_8 = case_when(num_games >= 4 ~ 1, TRUE ~ 0),
           final_4 = case_when(num_games >= 5 ~ 1, TRUE ~ 0)) %>% 
    arrange(Team, Season) %>%
    group_by(Team) %>% 
    mutate(num_tourney_games_since_1985 = cumsum(num_games),
           num_tourney_advanced_since_1985 = cumsum(advanced),
           num_tourney_sweet_16_since_1985 = cumsum(sweet_16),
           num_tourney_elite_8_since_1985 = cumsum(elite_8),
           num_tourney_final_4_since_1985 = cumsum(final_4)) %>%
    ungroup() %>% arrange(Team, Season) %>% group_by(Team) %>%
    mutate(num_tourney_games_since_1985 = dplyr::lag(num_tourney_games_since_1985, n = 1, default = 0),
           num_tourney_advanced_since_1985 = dplyr::lag(num_tourney_advanced_since_1985, n = 1, default = 0),
           num_tourney_sweet_16_since_1985 = dplyr::lag(num_tourney_sweet_16_since_1985, n = 1, default = 0),
           num_tourney_elite_8_since_1985 = dplyr::lag(num_tourney_elite_8_since_1985, n = 1, default = 0),
           num_tourney_final_4_since_1985 = dplyr::lag(num_tourney_final_4_since_1985, n = 1, default = 0)) %>% 
    dplyr::select(Season, Team, num_tourney_games_since_1985, num_tourney_advanced_since_1985,
                  num_tourney_sweet_16_since_1985, num_tourney_elite_8_since_1985,
                  num_tourney_final_4_since_1985)
  
  team_games_recent <- team1 %>% 
    full_join(team2, by = c("Season","Team")) %>% 
    mutate(total_games_team1 = tidyr::replace_na(total_games_team1, 0),
           total_games_team2 = tidyr::replace_na(total_games_team2, 0),
           num_games = total_games_team1 + total_games_team2,
           advanced = case_when(num_games >= 2 ~ 1, TRUE ~ 0),
           sweet_16 = case_when(num_games >= 3 ~ 1, TRUE ~ 0),
           elite_8 = case_when(num_games >= 4 ~ 1, TRUE ~ 0),
           final_4 = case_when(num_games >= 5 ~ 1, TRUE ~ 0)) %>% 
    arrange(Team, Season) %>%
    group_by(Team) %>% 
    summarise(num_tourney_games_since_1985 = sum(num_games),
              num_tourney_advanced_since_1985 = sum(advanced),
              num_tourney_sweet_16_since_1985 = sum(sweet_16),
              num_tourney_elite_8_since_1985 = sum(elite_8),
              num_tourney_final_4_since_1985 = sum(final_4),
              .groups = "drop") %>%
    mutate(Season = 2024) %>% 
    dplyr::select(Season, Team, num_tourney_games_since_1985, num_tourney_advanced_since_1985,
                  num_tourney_sweet_16_since_1985, num_tourney_elite_8_since_1985,
                  num_tourney_final_4_since_1985)
  
  team_games_stacked <- bind_rows(team_games, team_games_recent)
  
  return(team_games_stacked)
}

strength_of_schedule <- function(df)
{
  winners <- df %>%
    arrange(Season, WTeamID, DayNum) %>%
    group_by(Season, WTeamID) %>% 
    mutate(Current_Wins = cumsum(WTeamID_Won),
           Current_Wins = dplyr::lag(Current_Wins, n = 1, default = 0)) %>%
    rename('Team' = WTeamID) %>% dplyr::select(Season, DayNum, Team, Current_Wins)
  
  losers <- df %>%
    arrange(Season, LTeamID, DayNum) %>%
    group_by(Season, LTeamID) %>% 
    mutate(Current_Losses = cumsum(LTeamID_Lost),
           Current_Losses = dplyr::lag(Current_Losses, n = 1, default = 0)) %>%
    rename('Team' = LTeamID) %>% dplyr::select(Season, DayNum, Team, Current_Losses)
  
  merged <- dplyr::bind_rows(winners, losers) %>% 
    arrange(Season, Team, DayNum) %>%
    group_by(Season, Team) %>% 
    fill(Current_Wins, .direction = "up") %>%
    fill(Current_Wins) %>%
    fill(Current_Losses, .direction = "up") %>%
    fill(Current_Losses) %>%
    tidyr::replace_na(list(Current_Wins = 0, Current_Losses = 0)) %>% 
    mutate(Current_Win_Pct = Current_Wins / (Current_Wins + Current_Losses)) %>% 
    tidyr::replace_na(list(Current_Win_Pct = 0.5))
  
  df <- df %>% 
    inner_join(merged, by = c("Season","DayNum","Team1" = "Team")) %>% 
    inner_join(merged, by = c("Season","DayNum","Team2" = "Team"), suffix = c("_Team1","_Team2"))
  
  Team1_SOS <- df %>%
    group_by(Season, Team1) %>%
    summarise(Team1_Num_Games = n(), 
              Team1_SOS = mean(Current_Win_Pct_Team2, na.rm = T), .groups = "drop") %>%
    rename('Team' = 'Team1')
  
  Team2_SOS <- df %>%
    group_by(Season, Team2) %>%
    summarise(Team2_Num_Games = n(), 
              Team2_SOS = mean(Current_Win_Pct_Team1, na.rm = T), .groups = "drop") %>%
    rename('Team' = 'Team2')
  
  final_df <- Team1_SOS %>%
    full_join(Team2_SOS, by = c("Season","Team")) %>% 
    tidyr::replace_na(list(Team1_Num_Games = 0, Team2_Num_Games = 0,
                           Team1_SOS = 0, Team2_SOS = 0)) %>%
    mutate(SOS = ((Team1_Num_Games / (Team1_Num_Games + Team2_Num_Games)) * Team1_SOS) + 
             ((Team2_Num_Games / (Team1_Num_Games + Team2_Num_Games)) * Team2_SOS),
           SOS = rescale(SOS)) %>%
    dplyr::select(Season, Team, SOS)
  
  return(final_df)
}

# Reg Season Compact
get_coaches_reg_season_record <- function(df)
{
  df <- df %>%
    left_join(Coaches, join_by(Season == Season, Team1 == TeamID, DayNum >= FirstDayNum, DayNum <= LastDayNum)) %>%
    left_join(Coaches, join_by(Season == Season, Team2 == TeamID, DayNum >= FirstDayNum, DayNum <= LastDayNum),
              suffix = c("_Team1","_Team2")) %>%
    select(-FirstDayNum_Team1, -FirstDayNum_Team2,-LastDayNum_Team1,
           -LastDayNum_Team2) %>% 
    mutate(W_CoachName = case_when(Team1_Victory == 1 ~ CoachName_Team1, 
                                   TRUE ~ CoachName_Team2),
           L_CoachName = case_when(Team1_Victory == 0 ~ CoachName_Team1, 
                                   TRUE ~ CoachName_Team2))
  
  winners <- df %>% #filter(W_CoachName == 'kevin_nickelberry') %>% 
    arrange(Season, DayNum) %>%
    group_by(W_CoachName) %>% 
    mutate(Career_Num_Wins_Since_1985 = cumsum(WTeamID_Won),
           Career_Num_Wins_Since_1985 = dplyr::lag(Career_Num_Wins_Since_1985, n = 1, default = 0)) %>%
    rename('Team' = WTeamID, 'CoachName' = 'W_CoachName') %>% 
    dplyr::select(Season, DayNum, Team, CoachName, Career_Num_Wins_Since_1985)
  
  losers <- df %>% #filter(L_CoachName == 'kevin_nickelberry') %>% 
    arrange(Season, DayNum) %>%
    group_by(L_CoachName) %>% 
    mutate(Career_Num_Losses_Since_1985 = cumsum(LTeamID_Lost),
           Career_Num_Losses_Since_1985 = dplyr::lag(Career_Num_Losses_Since_1985, n = 1, default = 0)) %>%
    rename('Team' = LTeamID, 'CoachName' = 'L_CoachName') %>% 
    dplyr::select(Season, DayNum, Team, CoachName, Career_Num_Losses_Since_1985)
  
  merged <- dplyr::bind_rows(winners, losers) %>% 
    arrange(CoachName, Season, DayNum) %>%
    group_by(CoachName) %>% 
    fill(Career_Num_Wins_Since_1985, .direction = "up") %>%
    fill(Career_Num_Wins_Since_1985) %>%
    fill(Career_Num_Losses_Since_1985, .direction = "up") %>%
    fill(Career_Num_Losses_Since_1985) %>%
    tidyr::replace_na(list(Career_Num_Wins_Since_1985 = 0, Career_Num_Losses_Since_1985 = 0)) %>% 
    mutate(Career_Win_Pct_Since_1985 = Career_Num_Wins_Since_1985 / (Career_Num_Wins_Since_1985 + Career_Num_Losses_Since_1985)) %>% 
    tidyr::replace_na(list(Career_Win_Pct_Since_1985 = 0.5)) %>% 
    group_by(Season, Team, CoachName) %>% 
    filter(DayNum == max(DayNum)) %>% select(-DayNum)
  
  return(merged)
}

get_coaches_tourney_record <- function(df)
{
  
  df <- df %>%
    left_join(Coaches, join_by(Season == Season, Team1 == TeamID, DayNum >= FirstDayNum, DayNum <= LastDayNum)) %>%
    left_join(Coaches, join_by(Season == Season, Team2 == TeamID, DayNum >= FirstDayNum, DayNum <= LastDayNum),
              suffix = c("_Team1","_Team2")) %>%
    select(-FirstDayNum_Team1, -FirstDayNum_Team2,-LastDayNum_Team1,
           -LastDayNum_Team2) %>% 
    mutate(W_CoachName = case_when(Team1_Victory == 1 ~ CoachName_Team1, 
                                   TRUE ~ CoachName_Team2),
           L_CoachName = case_when(Team1_Victory == 0 ~ CoachName_Team1, 
                                   TRUE ~ CoachName_Team2))
  
  winners <- df %>% #filter(WTeamID == 1181) %>% 
    arrange(Season, DayNum) %>%
    group_by(W_CoachName) %>% 
    mutate(Career_Tourney_Wins_Since_1985 = cumsum(WTeamID_Won),
           Career_Tourney_Wins_Since_1985 = dplyr::lag(Career_Tourney_Wins_Since_1985, n = 1, default = 0)) %>%
    rename('Team' = WTeamID, 'CoachName' = 'W_CoachName') %>% 
    dplyr::select(Season, DayNum, Team, CoachName, Career_Tourney_Wins_Since_1985)
  
  losers <- df %>% #filter(LTeamID == 1181) %>% 
    arrange(Season, DayNum) %>%
    group_by(L_CoachName) %>% 
    mutate(Career_Tourney_Losses_Since_1985 = cumsum(LTeamID_Lost),
           Career_Tourney_Losses_Since_1985 = dplyr::lag(Career_Tourney_Losses_Since_1985, n = 1, default = 0)) %>%
    rename('Team' = LTeamID, 'CoachName' = 'L_CoachName') %>% 
    dplyr::select(Season, DayNum, Team, CoachName, Career_Tourney_Losses_Since_1985)
  
  merged <- dplyr::bind_rows(winners, losers) %>% 
    arrange(CoachName, Season, DayNum) %>%
    group_by(CoachName) %>% 
    fill(Career_Tourney_Wins_Since_1985, .direction = "up") %>%
    fill(Career_Tourney_Wins_Since_1985) %>%
    fill(Career_Tourney_Losses_Since_1985, .direction = "up") %>%
    fill(Career_Tourney_Losses_Since_1985) %>%
    tidyr::replace_na(list(Career_Tourney_Wins_Since_1985 = 0, Career_Tourney_Losses_Since_1985 = 0)) %>% 
    mutate(Career_Tourney_Pct_Since_1985 = Career_Tourney_Wins_Since_1985 / (Career_Tourney_Wins_Since_1985 + Career_Tourney_Losses_Since_1985)) %>% 
    tidyr::replace_na(list(Career_Tourney_Pct_Since_1985 = 0.5)) %>% 
    group_by(Season, Team, CoachName) %>% 
    filter(DayNum == min(DayNum)) %>% select(-DayNum)
  
  winners <- df %>% #filter(WTeamID == 1181) %>%
    group_by(W_CoachName) %>% 
    summarise(Career_Tourney_Wins_Since_1985 = sum(WTeamID_Won), .groups = "drop") %>%
    rename('CoachName' = 'W_CoachName') %>% 
    mutate(Season = 2024)
  
  losers <- df %>% #filter(LTeamID == 1181) %>% 
    group_by(L_CoachName) %>% 
    summarise(Career_Tourney_Losses_Since_1985 = sum(LTeamID_Lost), .groups = "drop") %>%
    rename('CoachName' = 'L_CoachName') %>% 
    mutate(Season = 2024)
  
  merged2 <- winners %>% 
    inner_join(losers, by = c("Season","CoachName")) %>%
    mutate(Career_Tourney_Pct_Since_1985 = Career_Tourney_Wins_Since_1985 / (Career_Tourney_Wins_Since_1985 + Career_Tourney_Losses_Since_1985)) %>% 
    select(Season, CoachName, everything()) %>% 
    inner_join(Coaches %>% select(Season,TeamID, CoachName) %>% rename("Team" = "TeamID"), by = c("Season","CoachName"))
  
  merged3 <- bind_rows(merged, merged2)
  
  return(merged3)
  
}


get_team_diffs <- function(df)
{
  
  stats <- c("Avg_Score","Avg_FGM","Avg_FGA","Avg_FGM3","Avg_FGA3","Avg_FTM","Avg_FTA",
             "Avg_OR","Avg_DR","Avg_Ast","Avg_TO","Avg_Stl","Avg_Blk","Avg_PF",
             "Avg_Num_Poss","Avg_Off_Rating","Avg_Def_Rating","Avg_Net_Eff",
             "Avg_Margin_Adj","Avg_Shoot_Eff","Avg_Score_Opp","Avg_EFG_Pct","Avg_Pie",
             "Avg_Tie","Avg_OR_Pct","Avg_DR_Pct","Avg_FT_Pct","Avg_TO_Pct","Avg_TS_Pct",
             "Avg_3PT_Pct","Avg_AstRatio","Avg_Reb_Pct","Avg_Four_Factor","Avg_BLK_Pct",
             "Avg_Margin","Adj_Wins","W_","Adj_Losses","L_","Adj_Win_Pct","Win_Pct",
             "Avg_Pace","Avg_3PRate","Avg_FTRate","Avg_Stl_Pct",
             "num_tourney_games_since_1985","num_tourney_advanced_since_1985",
             "SOS","Double_Digit_Seed","Dist","Power","conf_tourn_num_wins",
             "conf_tourn_num_losses","conf_tourn_win_pct","conf_champ")
  
  df <- df %>% 
    mutate(Avg_Score_Diff = Avg_Score_Team1 - Avg_Score_Team2,
           Avg_FGM_Diff = Avg_FGM_Team1 - Avg_FGM_Team2,
           Avg_FGA_Diff = Avg_FGA_Team1 - Avg_FGA_Team2,
           Avg_FGM3_Diff = Avg_FGM3_Team1 - Avg_FGM3_Team2,
           Avg_FTM_Diff = Avg_FTM_Team1 - Avg_FTM_Team2,
           Avg_FTA_Diff = Avg_FTA_Team1 - Avg_FTA_Team2,
           Avg_FTRate_Diff = Avg_FTRate_Team1 - Avg_FTRate_Team2,
           Avg_OR_Diff = Avg_OR_Team1 - Avg_OR_Team2,
           Avg_DR_Diff = Avg_DR_Team1 - Avg_DR_Team2,
           Avg_Ast_Diff = Avg_Ast_Team1 - Avg_Ast_Team2,
           Avg_TO_Pct_Diff = Avg_TO_Pct_Team1 - Avg_TO_Pct_Team2,
           Avg_Stl_Diff = Avg_Stl_Team1 - Avg_Stl_Team2,
           Avg_Stl_Pct_Diff = Avg_Stl_Pct_Team1 - Avg_Stl_Pct_Team2,
           Avg_Blk_Diff = Avg_Blk_Team1 - Avg_Blk_Team2,
           Avg_PF_Diff = Avg_PF_Team1 - Avg_PF_Team2,
           Avg_Num_Poss_Diff = Avg_Num_Poss_Team1 - Avg_Num_Poss_Team2,
           Avg_Pace_Diff = Avg_Pace_Team1 - Avg_Pace_Team2,
           Avg_Off_Rating_Diff = Avg_Off_Rating_Team1 - Avg_Off_Rating_Team2,
           Avg_Def_Rating_Diff = Avg_Def_Rating_Team1 - Avg_Def_Rating_Team2,
           Avg_Net_Eff_Diff = Avg_Net_Eff_Team1 - Avg_Net_Eff_Team2,
           Avg_Margin_Adj_Diff = Avg_Margin_Adj_Team1 - Avg_Margin_Adj_Team2,
           Avg_Shoot_Eff_Diff = Avg_Shoot_Eff_Team1 - Avg_Shoot_Eff_Team2,
           Avg_Score_Opp_Diff = Avg_Score_Opp_Team1 - Avg_Score_Opp_Team2,
           Avg_EFG_Pct_Diff = Avg_EFG_Pct_Team1 - Avg_EFG_Pct_Team2,
           Avg_Pie_Diff = Avg_Pie_Team1 - Avg_Pie_Team2,
           Avg_Tie_Diff = Avg_Tie_Team1 - Avg_Tie_Team2,
           Avg_OR_Pct_Diff = Avg_OR_Pct_Team1 - Avg_OR_Pct_Team2,
           Avg_DR_Pct_Diff = Avg_DR_Pct_Team1 - Avg_DR_Pct_Team2,
           Avg_FT_Pct_Diff = Avg_FT_Pct_Team1 - Avg_FT_Pct_Team2,
           Avg_TO_Pct_Diff = Avg_TO_Pct_Team1 - Avg_TO_Pct_Team2,
           Avg_TS_Pct_Diff = Avg_TS_Pct_Team1 - Avg_TS_Pct_Team2,
           Avg_3PT_Pct_Diff = Avg_3PT_Pct_Team1 - Avg_3PT_Pct_Team2,
           Avg_3PRate_Diff = Avg_3PRate_Team1 - Avg_3PRate_Team2,
           Avg_AstRatio_Diff = Avg_AstRatio_Team1 - Avg_AstRatio_Team2,
           Avg_Reb_Pct_Diff = Avg_Reb_Pct_Team1 - Avg_Reb_Pct_Team2,
           Avg_Four_Factor_Diff = Avg_Four_Factor_Team1 - Avg_Four_Factor_Team2,
           Avg_BLK_Pct_Diff = Avg_BLK_Pct_Team1 - Avg_BLK_Pct_Team2,
           Avg_Margin_Diff = Avg_Margin_Team1 - Avg_Margin_Team2,
           Adj_Wins_Diff = Adj_Wins_Team1 - Adj_Wins_Team2,
           W_Diff = W_Team1 - W_Team2,
           Adj_Losses_Diff = Adj_Losses_Team1 - Adj_Losses_Team2,
           L_Diff = L_Team1 - L_Team2,
           Adj_Win_Pct_Diff = Adj_Win_Pct_Team1 - Adj_Win_Pct_Team2,
           Win_Pct_Diff = Win_Pct_Team1 - Win_Pct_Team2,
           num_tourney_games_since_1985_Diff = num_tourney_games_since_1985_Team1 - num_tourney_games_since_1985_Team2,
           num_tourney_advanced_since_1985_Diff = num_tourney_advanced_since_1985_Team1 - num_tourney_advanced_since_1985_Team2,
           SOS_Diff = SOS_Team1 - SOS_Team2,
           Double_Digit_Seed_Diff = Team1_Double_Digit_Seed - Team2_Double_Digit_Seed,
           Dist_Diff = Team1_Dist - Team2_Dist,
           Power_Diff = as.numeric(as.character(Team1_Power)) - as.numeric(as.character(Team2_Power)),
           conf_tourn_num_wins_Diff = conf_tourn_num_wins_Team1 - conf_tourn_num_wins_Team2,
           conf_tourn_num_losses_Diff = conf_tourn_num_losses_Team1 - conf_tourn_num_losses_Team2,
           conf_tourn_win_pct_Diff = conf_tourn_win_pct_Team1 - conf_tourn_win_pct_Team2,
           conf_champ_Diff = conf_champ_Team1 - conf_champ_Team2,
           Avg_Rank_Diff = avg_rank_Team1 - avg_rank_Team2,
           Coach_Career_Reg_Season_Wins_Diff = Career_Num_Wins_Since_1985_Team1 - Career_Num_Wins_Since_1985_Team2,
           Coach_Career_Tourney_Wins_Diff = Career_Tourney_Wins_Since_1985_Team1 - Career_Tourney_Wins_Since_1985_Team2,
           Head_to_Head_Win_Pct_Diff = Team1_Matchup_Win_Pct - Team2_Matchup_Win_Pct
    )
  
  return(df)
}

get_season_matchups <- function(df)
{
  df2 <- df %>% filter(Season >= 2003) %>% 
    mutate(matchup = paste0(Team1, "_", Team2)) %>%
    group_by(Season, matchup) %>%
    summarise(Num_Wins_Matchup_Team1 = sum(Team1_Victory),
              Num_Losses_Matchup_Team1 = sum(Team2_Victory),
              Num_Wins_Matchup_Team2 = sum(Team2_Victory),
              Num_Losses_Matchup_Team2 = sum(Team1_Victory),
              Team1_Matchup_Win_Pct = Num_Wins_Matchup_Team1 / (Num_Wins_Matchup_Team1 + Num_Wins_Matchup_Team2),
              Team2_Matchup_Win_Pct = Num_Wins_Matchup_Team2 / (Num_Wins_Matchup_Team1 + Num_Wins_Matchup_Team2),
              .groups = "drop") %>%
    tidyr::separate_wider_delim(matchup, delim = "_", names = c("Team1", "Team2")) %>% 
    mutate(Team1 = as.integer(Team1), Team2 = as.integer(Team2))
  
  return(df2)
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

get_all_pairwise_matchups <- function(df, start_year, end_year)
{
  all_years_submission = data.frame()
  for (k in seq(start_year, end_year))
  {
    #if (k == 2020)
    #{
      #next
    #}
    if (k == 2024)
    {
      current_year <- read.csv("C:/Users/rusla/OneDrive/MarchMadness/March-Madness-Predictions/Data/SampleSubmission2024.csv") %>% 
        filter(Type == 'M') %>% select(-Type)
      current_year <- current_year %>%
        mutate(Season = as.numeric(substr(ID, 1, 4)),
               Team1 = as.numeric(substr(ID, 11, 14)),
               Team2 = as.numeric(substr(ID, 6, 9))) 
      colnames(current_year)[1:2] <- c("id","pred")
    }
    season <- df %>% filter(Season == k)
    team1 <- sort(unique(season$Team1))
    team2 <- sort(unique(season$Team2))
    teams <- sort(unique(c(team1,team2)))
    pairwise <- expand.grid(teams, teams)
    pairwise <- pairwise[pairwise$Var1 < pairwise$Var2,]
    pairwise <- pairwise[order(pairwise$Var1),]
    year_submission <- data.frame()
    for (i in 1:nrow(pairwise))
    {
      year_submission[i,1] <- paste0(season[1,"Season"],"_",pairwise[i,1],"_",pairwise[i,2])
      year_submission[i,2] <- pairwise[i,1]
      year_submission[i,3] <- pairwise[i,2]
      year_submission[i,4] <- season[1,"Season"]
      year_submission[i,5] <- 0.5
    }
    
    all_years_submission <- rbind(all_years_submission, year_submission)
  }
  colnames(all_years_submission) <- c("id","Team2","Team1","Season","pred")
  all_years_submission <- all_years_submission %>% select(id, pred, Season, Team1, Team2)
  all_years_submission <- rbind(all_years_submission, current_year)
  all_years_submission <- all_years_submission[order(all_years_submission$Season,all_years_submission$Team2,all_years_submission$Team1),]
  all_years_submission <- tidyr::drop_na(all_years_submission)
  return(all_years_submission)
}

run_penalized_logit <- function(vars, alpha, min, k_fold)
{
  print(paste("vars: ", paste(vars, collapse = ', ')))
  
  cv.out <- cv.glmnet(data.matrix(train_continuous), 
                      as.factor(data.matrix(train_response[,1])), alpha = alpha, 
                      family = "binomial", type.measure = "class", nfolds = k_fold)
  plot(cv.out)
  
  lambda_min <- cv.out$lambda.min
  lambda_1se <- cv.out$lambda.1se
  
  if (min)
  {
    lasso_prob <- predict(cv.out,newx = data.matrix(test_continuous), 
                          s=lambda_min, type= "response")
  } else {
    lasso_prob <- predict(cv.out,newx = data.matrix(test_continuous), 
                          s=lambda_1se, type= "response")
  }
  lasso_prob <- ifelse(lasso_prob > 0.95, 0.999, lasso_prob)
  lasso_prob <- ifelse(lasso_prob < 0.05, 0.001, lasso_prob)
  loss <- round(logloss(test_response[,1],lasso_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(lasso_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(test_response, lasso_prob,pred_outcome))
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

run_glmnet <- function(vars, tunelength, k_fold)
{
  set.seed(42)
  myFolds <- createFolds(train_continuous$Team1_Victory, k = k_fold)
  print(paste("vars: ", paste(vars, collapse = ', ')))
  train_continuous$Team1_Victory <- ifelse(train_continuous$Team1_Victory == 1, "Win","Loss")
  control <- trainControl(classProbs=TRUE, summaryFunction=mnLogLoss, number = k_fold,
                          index = myFolds, savePredictions = TRUE)
  glmnet <- train(as.formula(paste0("as.factor(Team1_Victory) ~ ", paste0(vars, collapse = " + "))), 
                  data = train_continuous, method = "glmnet", tuneLength=tunelength, 
                  metric="logLoss", trControl=control, family = 'binomial',
                  preProcess = c('nzv','center','scale'))
  
  print(plot(glmnet))
  imp2 <- varImp(glmnet)
  print(barchart(sort(rowMeans(imp2$importance), decreasing = T), 
                 main = "GLMNet Variable Importance", xlab = "Average Level of Importance",
                 ylab = "Variables"))
  
  preds <- predict(glmnet, newdata = test_continuous, type = "prob")
  glmnet_prob <- preds[, 2]
  
  preds_raw <- predict(glmnet, newdata = test_continuous, type = 'raw')
  glmnet_prob <- ifelse(glmnet_prob > 0.95, 0.999, glmnet_prob)
  glmnet_prob <- ifelse(glmnet_prob < 0.05, 0.001, glmnet_prob)
  loss <- round(logloss(test_response[,1],glmnet_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(glmnet_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(test_response, glmnet_prob,pred_outcome))
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
  myFolds <- createFolds(train_continuous$Team1_Victory, k = k_fold)
  print(paste("vars: ", paste(vars, collapse = ', ')))
  train_continuous$Team1_Victory <- ifelse(train_continuous$Team1_Victory == 1, "Win","Loss")
  control <- trainControl(classProbs=TRUE, summaryFunction=mnLogLoss, number = k_fold,
                          index = myFolds, savePredictions = TRUE)
  rF <- train(as.formula(paste0("as.factor(Team1_Victory) ~ ", paste0(vars, collapse = " + "))),
              ntree = ntrees, method = 'rf', metric="logLoss", trControl=control,
              data = train_continuous, importance = T, tuneLength = 15)
  print(plot(rF, main = 'Average Accuracy Across 5-Fold CV', 
       xlab = 'Number of Variables'))
  imp = varImp(rF)
  print(barchart(sort(rowMeans(imp$importance), decreasing = T), 
           main = "Random Forest Variable Importance", 
           xlab = "Average Level of Importance", ylab = "Variables"))
  
  preds <- predict(rF, newdata = test_continuous, type = "prob")
  rf_prob <- preds[, 2]
  preds_raw <- predict(rF, newdata = test_continuous, type = 'raw')
  rf_prob <- ifelse(rf_prob > 0.95, 0.999, rf_prob)
  rf_prob <- ifelse(rf_prob < 0.05, 0.001, rf_prob)
  loss <- round(logloss(test_response[,1],rf_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(rf_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(test_response, rf_prob,pred_outcome))
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
  myFolds <- createFolds(train_continuous$Team1_Victory, k = k_fold)
  print(paste("vars: ", paste(vars, collapse = ', ')))
  train_continuous$Team1_Victory <- ifelse(train_continuous$Team1_Victory == 1, "Win","Loss")
  control <- trainControl(classProbs=TRUE, summaryFunction=mnLogLoss, number = k_fold,
                          index = myFolds, savePredictions = TRUE)
  gbm <- train(as.formula(paste0("as.factor(Team1_Victory) ~ ", paste0(vars, collapse = " + "))),
               data = train_continuous, method="gbm", verbose=FALSE, tuneGrid = gbmGrid,
               metric="logLoss", trControl=control) 
  print(summary(gbm))
  preds <- predict(gbm, newdata = test_continuous, type = "prob")
  gbm_prob <- preds[, 2]
  
  preds_raw <- predict(gbm, newdata = test_continuous, type = 'raw')
  gbm_prob <- ifelse(gbm_prob > 0.95, 0.999, gbm_prob)
  gbm_prob <- ifelse(gbm_prob < 0.05, 0.001, gbm_prob)
  loss <- round(logloss(test_response[,1],gbm_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(gbm_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(test_response, gbm_prob,pred_outcome))
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
  myFolds <- createFolds(train_continuous$Team1_Victory, k = k_fold)
  print(paste("vars: ", paste(vars, collapse = ', ')))
  train_continuous$Team1_Victory <- ifelse(train_continuous$Team1_Victory == 1, "Win","Loss")
  control <- trainControl(classProbs=TRUE, summaryFunction=mnLogLoss, number = k_fold,
                          index = myFolds, savePredictions = TRUE)
  xgboost <- train(as.formula(paste0("as.factor(Team1_Victory) ~ ", paste0(vars, collapse = " + "))), 
                   data = train_continuous, method = "xgbTree", 
                   metric="logLoss", trControl=control, tuneLength = 3) #tuneGrid=parametersGrid)
  
  imp2 <- varImp(xgboost)
  print(barchart(sort(rowMeans(imp2$importance), decreasing = T), 
           main = "XGBoost Variable Importance", xlab = "Average Level of Importance",
           ylab = "Variables"))
  
  preds <- predict(xgboost, newdata = test_continuous, type = "prob")
  xgboost_prob <- preds[, 2]
  
  preds_raw <- predict(xgboost, newdata = test_continuous, type = 'raw')
  xgboost_prob <- ifelse(xgboost_prob > 0.95, 0.999, xgboost_prob)
  xgboost_prob <- ifelse(xgboost_prob < 0.05, 0.001, xgboost_prob)
  loss <- round(logloss(test_response[,1],xgboost_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(xgboost_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(test_response, xgboost_prob,pred_outcome))
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

run_model <- function(vars, model_type, tuneLength, k_fold, plot = FALSE)
{
  set.seed(0)
  myFolds <- createFolds(train_continuous$Team1_Victory, k = k_fold)
  print(paste("vars: ", paste(vars, collapse = ', ')))
  train_continuous$Team1_Victory <- ifelse(train_continuous$Team1_Victory == 1, "Win","Loss")
  control <- trainControl(classProbs=TRUE, summaryFunction=mnLogLoss, number = k_fold,
                          index = myFolds, savePredictions = TRUE)
  model <- train(as.formula(paste0("as.factor(Team1_Victory) ~ ", paste0(vars, collapse = " + "))), 
                 data = train_continuous, method = model_type, 
                 metric="logLoss", trControl=control, tuneLength = tuneLength)
  if ((model_type != 'gbm') & (plot == TRUE))
  {
    imp2 <- varImp(model)
    print(barchart(sort(rowMeans(imp2$importance), decreasing = T), main = paste0(model_type, " Variable Importance"), xlab = "Average Level of Importance", ylab = "Variables"))
  }
  if (model_type == 'gbm')
  {
    var_imp <- summary(model)[2]
    labels <- row.names(var_imp)
    var_imp <- var_imp[1:10,]
    labels <- labels[1:10]
    df <- data.frame(labels, var_imp)
    print(ggplot(df, aes(x = reorder(labels, -var_imp), y = var_imp)) +
      geom_bar(stat = "identity", fill = "black") +
      ggtitle(paste0("GBM Var Imp Predicting Team 1 Victory")) + 
      coord_flip() + scale_y_continuous(name="Variable Important (0-100)") +
      scale_x_discrete(name="") + 
      theme(plot.title=element_text(hjust=0.5,vjust=0,size=13,face = 'bold'), 
            plot.subtitle=element_text(face="bold", hjust= 0.5, vjust= .09, colour="#3C3C3C", size = 10)) +
      theme(axis.text.x=element_text(vjust = .5, size=11,colour="#535353",face="bold")) +
      theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
      theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
      theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=0)) +
      theme(panel.grid.major.y = element_line(color = "#bad2d4", size = .5)) +
      theme(panel.grid.major.x = element_line(color = "#bdd2d4", size = .5)))
  }
  preds <- predict(model, newdata = test_continuous, type = "prob")
  preds_prob <- preds[, 2]
  preds_raw <- predict(model, newdata = test_continuous, type = 'raw')
  preds_prob <- ifelse(preds_prob > 0.95, 0.95, preds_prob)
  preds_prob <- ifelse(preds_prob < 0.05, 0.05, preds_prob)
  loss <- round(logloss(test_response[,1], preds_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(preds_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(test_response, preds_prob, pred_outcome))
  colnames(answer) <- c("True","Season","Pred_Prob","Pred_Outcome")
  
  count = 0
  for (i in 1:length(preds_prob))
  {
    if (answer$True[i] == answer$Pred_Outcome[i])
    {
      count = count + 1
    }
  }
  # Prediction accuracy
  acc <- round(count / dim(answer)[1],4)
  print(paste("accuracy:", acc))
  
  return(list(model, answer))
}

run_nn <- function(vars, num_epochs)
{
  print(paste("vars: ", paste(vars, collapse = ', ')))
  y_binary <- to_categorical(train_continuous$Team1_Victory)
  model <- keras::keras_model_sequential() 
  model %>% keras::layer_dense(units = 32, activation = "relu", input_shape = ncol(train_continuous)-1) %>%
    keras::layer_dense(units = 16, activation = "relu") %>%
    keras::layer_dense(units = 8, activation = "relu") %>%
    keras::layer_dense(units = 4, activation = "relu") %>%
    keras::layer_dense(units = ncol(y_binary), activation = "softmax")
  
  compile(model, loss = "binary_crossentropy", optimizer = 'adam', metrics = "binary_crossentropy")
  history <- fit(model,data.matrix(subset(train_continuous, select = c(-Team1_Victory))), 
                 y_binary, epochs = num_epochs, validation_split = 0.2)
  plot(history)
  
  nn_prob <- model %>% keras::predict_proba(data.matrix(subset(test_continuous, select=-c(Team1_Victory))))
  nn_prob <- nn_prob[,2]
  loss <- round(logloss(test_response[,1],nn_prob),4)
  print(paste("logloss:", loss))
  
  pred_outcome <- ifelse(nn_prob >= 0.5, 1, 0)
  answer <- as.data.frame(cbind(test_response, nn_prob,pred_outcome))
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
  kaggle_test <- shelve_df %>% filter(Season %in% start_year:end_year)
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
    kaggle_preds <- predict(model_output[[1]], newdata = kaggle_test[,vars], type = "prob")[,1]
  }
  ID <- kaggle_test %>% dplyr::select(id)
  kaggle_preds <- cbind(ID, kaggle_preds)
  names(kaggle_preds) <- c("ID","Pred")
  
  if (names)
  {
    kaggle_preds <- kaggle_preds %>% 
      mutate(Team1 = as.numeric(substr(ID, 6, 9)),
             Team2 = as.numeric(substr(ID, 11, 14))) %>%
      left_join(Teams[, c('TeamID','TeamName')], by = c("Team1" = "TeamID")) %>%
      left_join(Teams[, c('TeamID','TeamName')], by = c("Team2" = "TeamID")) %>%
      rename(Team1_Name = 'TeamName.x', Team2_Name = 'TeamName.y')
  }
  return(kaggle_preds %>% arrange(ID))
}

color_gradient <- function(dt, column_name)#, gradient_colors = c("#6666FF", "#DDDDDD", "#FF6666")) 
{
  #col_func <- colorRampPalette(c("#CC4C02", "#FE9929", "#FED98E", "#FFFFD4")) #colorRampPalette(gradient_colors)
  col_func <- colorRampPalette(c("orangered","#FE9929", "#FED98E", "#FFFFD4"))
  dt %>% 
    formatStyle(column_name, 
                backgroundColor = styleEqual(
                  sort(unique(dt$x$data[[column_name]]), decreasing = TRUE),
                  col_func(length(unique(dt$x$data[[column_name]])))), 
                opacity = 1, 
                fontWeight = 'bold') 
}

# Format options
options(kableExtra.auto_format = FALSE)
options(knitr.table.format = "html")

# set working directory to load in functions
setwd("C:/Users/rusla/OneDrive/MarchMadness/March-Madness-Predictions/Code/Men")

# load in functions files
source('functions.R')
source('bracket_sim_functions.R')

# set working directory to load in data files
setwd("C:/Users/rusla/OneDrive/MarchMadness/March-Madness-Predictions/Data")

# Load in Kaggle Data
Seasons <- read.csv("MSeasons.csv") %>% 
  pivot_longer(cols = starts_with("Region"), names_to = "Region", names_prefix = "Region", 
               values_to = "Region_Name", values_drop_na = TRUE)
Reg_Season_Detailed <- read.csv("MRegularSeasonDetailedResults.csv")
Reg_Season_Compact <- read.csv("MRegularSeasonCompactResults.csv") %>% 
  mutate(Wadj_win = case_when(WLoc ==  'H' ~ 0.6, WLoc == 'N' ~ 1, TRUE ~ 1.6),
         Ladj_win = case_when(WLoc ==  'H' ~ 0.6, WLoc == 'N' ~ 1, TRUE ~ 1.6),
         WTeamID_Won = 1, 
         LTeamID_Lost = 1,
         Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team1_Victory = case_when(WTeamID == Team1 ~ 1, TRUE ~ 0),
         Team1_Loss = case_when(Team1_Victory == 1 ~ 0, TRUE ~ 1),
         Team2_Victory = Team1_Loss, Team2_Loss = Team1_Victory)
Teams <- read.csv("MTeams.csv")
Tourney_Detailed <- read.csv("MNCAATourneyDetailedResults.csv")
Tourney_Compact <- read.csv("MNCAATourneyCompactResults.csv")  %>%
  mutate(Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team1_Victory = case_when(WTeamID == Team1 ~ 1, TRUE ~ 0),
         WTeamID_Won = 1, 
         LTeamID_Lost = 1)
Tourney_Seeds <- read.csv("MNCAATourneySeeds.csv") %>%
  rename('RegionSeed' = Seed) %>%
  mutate(Region = as.character(substr(RegionSeed, 1, 1)),
         Seed = as.character(substr(RegionSeed, 2, 3)))
Tourney_Slots <- read.csv("MNCAATourneySlots.csv")
Submission <- read.csv("SampleSubmission2024.csv")
Teams_Location <- read.csv("MTeamLocations.csv")
Tourney_Hosts <- read.csv("MTourneyHosts.csv") %>% select(-Team1_Name, -Team2_Name)
Coaches <- read.csv("MTeamCoaches.csv")
Game_Cities <- read.csv("MGameCities.csv") %>% 
  mutate(Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID))) %>%
  dplyr::select(-WTeamID, -LTeamID)
Cities_Enriched <- read.csv("CitiesEnriched.csv")
ConfTournament <- read.csv("MConferenceTourneyGames.csv") %>% 
  mutate(Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team1_Victory = case_when(Team1 == WTeamID ~ 1, TRUE ~ 0),
         Team2_Victory = case_when(Team2 == WTeamID ~ 1, TRUE ~ 0))
ConfNames <- read.csv("MTeamConferences.csv")
Tourney_Seed_Round_Slots <- read.csv('MNCAATourneySeedRoundSlots.csv') %>%
  rename('RegionSeed' = Seed, 'Round' = 'GameRound') %>%
  mutate(Region = as.character(substr(RegionSeed, 1, 1)),
         Seed = as.character(substr(RegionSeed, 2, 3)),
         GameSlot = as.character(GameSlot))

Massey1 <- read.csv("MMasseyOrdinals.csv") 
Massey2 <- read.csv("MMasseyOrdinals_S2024_D133_59sys.csv") 
Massey <- bind_rows(Massey1, Massey2) %>% 
  group_by(Season, TeamID) %>% 
  summarise(avg_rank = mean(OrdinalRank),
            median_rank = median(OrdinalRank), .groups = "drop")

# Calculate adjusted wins, adjusted losses, adjusted win %
Adjusted_Standings <- get_adjusted_wins(Reg_Season_Compact)

# Create more advanced metrics for regular season detailed set
Team_Avgs <- get_reg_season_avg_team_stats(Reg_Season_Detailed)

# Take into account for num tournament games for each team prior to current tournament
Tourney_History <- past_tourney_history(Tourney_Compact)

conf_tourn_results <- get_conf_tourn_record(ConfTournament)

SOS <- strength_of_schedule(Reg_Season_Compact)

coaches_reg_season <- get_coaches_reg_season_record(Reg_Season_Compact)

coaches_tourney <- get_coaches_tourney_record(Tourney_Compact)

matchup <- get_season_matchups(Reg_Season_Compact)

# Create training data set
# Merge Team1 and Team2 Averages with Tourney_Compact by Season and Team ID
# Merge seed information for each team1, team2 per season
training_df <- Tourney_Compact %>%
  inner_join(Team_Avgs, by = c("Season","Team1" = "TeamID")) %>%
  inner_join(Team_Avgs, by = c("Season","Team2" = "TeamID"), suffix = c("_Team1","_Team2")) %>% 
  inner_join(Adjusted_Standings, by = c("Season","Team1" = "Team")) %>%
  inner_join(Adjusted_Standings, by = c("Season","Team2" = "Team"), suffix = c("_Team1","_Team2")) %>% 
  inner_join(Tourney_History, by = c("Season","Team1" = "Team")) %>%
  inner_join(Tourney_History, by = c("Season","Team2" = "Team"), suffix = c("_Team1","_Team2")) %>% 
  inner_join(Tourney_Seeds, by = c("Season","Team1" = "TeamID")) %>%
  inner_join(Tourney_Seeds, by = c("Season", "Team2" = "TeamID"), suffix = c("_Team1","_Team2")) %>%
  inner_join(SOS, by = c("Season","Team1" = "Team")) %>%
  inner_join(SOS, by = c("Season", "Team2" = "Team"), suffix = c("_Team1","_Team2")) %>%
  inner_join(Teams %>% select('TeamID','TeamName'), by = c("Team1" = "TeamID")) %>%
  inner_join(Teams %>% select('TeamID','TeamName'), by = c("Team2" = "TeamID"), suffix = c("_Team1","_Team2")) %>%
  inner_join(ConfNames, by = c("Season","Team1" = "TeamID")) %>%
  inner_join(ConfNames, by = c("Season","Team2" = "TeamID"), suffix = c("_Team1","_Team2")) %>% 
  inner_join(Massey, by = c("Season","Team1" = "TeamID")) %>% 
  inner_join(Massey, by = c("Season","Team2" = "TeamID"), suffix = c("_Team1","_Team2")) %>% 
  inner_join(Coaches, join_by(Season == Season, Team1 == TeamID, DayNum >= FirstDayNum, DayNum <= LastDayNum)) %>%
  inner_join(Coaches, join_by(Season == Season, Team2 == TeamID, DayNum >= FirstDayNum, DayNum <= LastDayNum),
            suffix = c("_Team1","_Team2")) %>%
  select(-FirstDayNum_Team1, -FirstDayNum_Team2,-LastDayNum_Team1,
         -LastDayNum_Team2) %>%
  left_join(coaches_reg_season, by = c("Season","Team1" = "Team","CoachName_Team1" = "CoachName")) %>% 
  left_join(coaches_reg_season, by = c("Season","Team2" = "Team","CoachName_Team2" = "CoachName"), suffix = c("_Team1","_Team2")) %>% 
  inner_join(coaches_tourney, by = c("Season","Team1" = "Team","CoachName_Team1" = "CoachName")) %>% 
  inner_join(coaches_tourney, by = c("Season","Team2" = "Team","CoachName_Team2" = "CoachName"), suffix = c("_Team1","_Team2")) %>% 
  mutate_at(vars("Seed_Team1","Seed_Team2"), as.numeric) %>%
  mutate(Round = unlist(mapply(round_finder, Region_Team1, Region_Team2, Seed_Team1, Seed_Team2, 
                               SIMPLIFY = TRUE), use.names = FALSE),
         Seed_Diff = Seed_Team1 - Seed_Team2,
         RegionSeed_Team1 = case_when(Round > 0 ~ substr(RegionSeed_Team1, start = 1, stop = 3), TRUE ~ RegionSeed_Team1),
         RegionSeed_Team2 = case_when(Round > 0 ~ substr(RegionSeed_Team2, start = 1, stop = 3), TRUE ~ RegionSeed_Team2),
         Team1_Double_Digit_Seed = case_when(Seed_Team1 >= 10 ~ 1, TRUE ~ 0),
         Team2_Double_Digit_Seed = case_when(Seed_Team2 >= 10 ~ 1, TRUE ~ 0)) %>% 
  inner_join(Seasons %>% select(-DayZero), by = c("Season","Region_Team1" = "Region")) %>%
  inner_join(Seasons, by = c("Season","Region_Team2" = "Region"), suffix = c("_Team1","_Team2")) %>%
  left_join(Game_Cities, by = c("Season","DayNum","Team1","Team2")) %>% 
  left_join(Cities_Enriched %>% select("CityId","City","LatHost","LngHost"), by = c("CityID" = "CityId")) %>%
  inner_join(Tourney_Hosts %>% select("Season","Slot","Host","lat","lng","Round","Team1","Team2"),
             by = c("Season","Round","Team1","Team2")) %>% rename("Host_Lat" = lat, "Host_Lng" = lng) %>%
  mutate(Host = gsub(" ", "_", tolower(str_remove_all(coalesce(City, Host), fixed(".")))),
         Host_Lat = coalesce(LatHost, Host_Lat),
         Host_Lng = coalesce(LngHost, Host_Lng)) %>% dplyr::select(-LatHost, -LngHost) %>% 
  inner_join(Teams_Location, by = c("Team1" = "Team_Id")) %>%
  inner_join(Teams_Location, by = c("Team2" = "Team_Id"), suffix = c("_Team1","_Team2")) %>% 
  mutate(Team1_Dist = distHaversine(matrix(c(lng_Team1, lat_Team1), nrow = nrow(.), ncol = 2),
                                    matrix(c(Host_Lng, Host_Lat), nrow = nrow(.), ncol = 2), 
                                    r = 3963.2),
         Team2_Dist = distHaversine(matrix(c(lng_Team2, lat_Team2), nrow = nrow(.), ncol = 2),
                                    matrix(c(Host_Lng, Host_Lat), nrow = nrow(.), ncol = 2), 
                                    r = 3963.2),
         Team1_Power = case_when(ConfAbbrev_Team1 %in% c('big_twelve','big_ten','acc','sec','big_east') ~ 1, TRUE ~ 0),
         Team2_Power = case_when(ConfAbbrev_Team2 %in% c('big_twelve','big_ten','acc','sec','big_east') ~ 1, TRUE ~ 0),
         Team1_Power = as.factor(Team1_Power), Team2_Power = as.factor(Team2_Power)) %>%
  left_join(conf_tourn_results, by = c("Season","ConfAbbrev_Team1" = "ConfAbbrev","Team1" = "Team")) %>%
  left_join(conf_tourn_results, by = c("Season","ConfAbbrev_Team2" = "ConfAbbrev","Team2" = "Team"), suffix = c("_Team1","_Team2")) %>% 
  arrange(Season, DayNum, Team2, Team1) %>% 
  tidyr::replace_na(list(conf_tourn_num_wins_Team1 = 0, conf_tourn_num_losses_Team1 = 0,
                         conf_tourn_win_pct_Team1 = 0.5, conf_champ_Team1 = 0, 
                         conf_tourn_num_wins_Team2 = 0, conf_tourn_num_losses_Team2 = 0,
                         conf_tourn_win_pct_Team2 = 0.5, conf_champ_Team2 = 0)) %>% 
  left_join(matchup, by = c("Season","Team1","Team2")) %>% 
  tidyr::replace_na(list(Num_Losses_Matchup_Team1 = 0, Num_Losses_Matchup_Team2 = 0,
                         Num_Wins_Matchup_Team1 = 0, Num_Wins_Matchup_Team2 = 0, 
                         Team1_Matchup_Win_Pct = 0.5, Team2_Matchup_Win_Pct = 0.5))

training_df <- training_df %>% 
  mutate(Career_Num_Wins_Since_1985_Team1 = case_when(is.na(Career_Num_Wins_Since_1985_Team1) & CoachName_Team1 == "kevin_nickelberry" ~ 127, TRUE ~ Career_Num_Wins_Since_1985_Team1),
         Career_Num_Losses_Since_1985_Team1 = case_when(is.na(Career_Num_Losses_Since_1985_Team1) & CoachName_Team1 == "kevin_nickelberry" ~ 235, TRUE ~ Career_Num_Losses_Since_1985_Team1),
         Career_Win_Pct_Since_1985_Team1 = case_when(is.na(Career_Win_Pct_Since_1985_Team1) & CoachName_Team1 == "kevin_nickelberry" ~ Career_Num_Wins_Since_1985_Team1 / (Career_Num_Wins_Since_1985_Team1 + Career_Num_Losses_Since_1985_Team1),
                                                     TRUE ~ Career_Num_Wins_Since_1985_Team1 / (Career_Num_Wins_Since_1985_Team1 + Career_Num_Losses_Since_1985_Team1)))

training_df <- get_team_diffs(training_df)

training_df <- training_df[order(training_df$Season, training_df$Round, training_df$Region_Name_Team1, training_df$Region_Name_Team2, 
                                 match(training_df$Seed_Team1, matchups)),]

#sort(colSums(is.na(training_df)))
#table(training_df$Season)


#training_df %>% filter(Season == 2023) %>% arrange(Round) %>% 
  #select(Round, Slot, TeamName_Team1, TeamName_Team2, Team1, Team2, 
         #RegionSeed_Team1, RegionSeed_Team2, Region_Name_Team1, Region_Name_Team2, 
         #Host) %>% View()
#########################################################################################
diff_cols <- colnames(training_df)[endsWith(x = colnames(training_df), suffix = "_Diff")]
team1_cols <- colnames(training_df)[endsWith(x = colnames(training_df), suffix = "_Team1")]
team1_cols <- c("Season","Slot","Round","Host","Host_Lat","Host_Lng",
               "Team1","Team2","TeamName_Team1","TeamName_Team2","Team1_Dist","Team2_Dist",
               "Seed_Team1", "RegionSeed_Team1","RegionSeed_Team2", "Region_Name_Team1","Region_Name_Team2", 
               team1_cols, diff_cols)
# Remove Team1_Victory
team1_cols <- team1_cols[team1_cols != 'Team1_Victory']

# Team2 colnames
team2_cols <- colnames(training_df)[endsWith(x = colnames(training_df), suffix = "_Team2")]
team2_cols <- c("Season","Slot","Round","Host","Host_Lat","Host_Lng",
               "Team1","Team2","TeamName_Team1","TeamName_Team2","Team1_Dist","Team2_Dist",
               "Seed_Team2","RegionSeed_Team1","RegionSeed_Team2","Region_Name_Team1","Region_Name_Team2", 
               team1_cols, diff_cols)

###################################################################################
# Machine Learning
vars <- diff_cols
vars2 <- c(vars, "Team1_Victory")
train <- training_df[training_df$Season %in% c(seq(2003,2023)),] 
train_response <- train[, c("Team1_Victory","Season")]
train_continuous <- train[, vars2]

test <- training_df[training_df$Season %in% c(2023) & training_df$Round > 0,] 
test_response <- test[, c("Team1_Victory","Season")]
test_continuous <- test[, vars2]

mod <- run_model(vars, "rf", tuneLength = 15, k_fold = 5, TRUE)

answer <- mod[[2]]
hist(answer$Pred_Prob)
mean(answer$Pred_Prob)

# Look at games for which I predicted incorrectly
wrong <- test[which(answer$True != answer$Pred_Outcome),c("Season","TeamName_Team1","TeamName_Team2","Seed_Team1","Seed_Team2","Round","Team1_Victory")]
wrong$upset <- ifelse(wrong$Team1_Victory == 1 & wrong$Seed_Team1 > wrong$Seed_Team2, 1, 
                      ifelse(wrong$Team1_Victory == 0 & wrong$Seed_Team1 < wrong$Seed_Team2, 1, 0))
d <- answer[answer$True != answer$Pred_Outcome,]
cbind(wrong, d$Pred_Outcome, d$Pred_Prob) %>% View()
dim(wrong)[1]


right <- test[which(answer$True == answer$Pred_Outcome),c("Season","TeamName_Team1","TeamName_Team2","Seed_Team1","Seed_Team2","Round","Team1_Victory")]
right$upset <- ifelse(right$Team1_Victory == 1 & right$Seed_Team1 > right$Seed_Team2, 1, 
                      ifelse(right$Team1_Victory == 0 & right$Seed_Team1 < right$Seed_Team2, 1, 0))
d <- answer[answer$True == answer$Pred_Outcome,]
cbind(right, d$Pred_Outcome, d$Pred_Prob) %>% View()
dim(right)[1]

result_mens <- Bracket_Sim(2024, 10000, "RF")
bracket <- Normalize_Sim(result_mens[[1]], 10000)

#kaggle_mens <- do.call("rbind", result_mens[[2]])
kaggle_mens <- bind_rows(result_mens[[2]])

#colnames(bracket) = c("Season","Region","Seed","Team","Top 32","Sweet 16","Elite 8","Final 4","Final","Champion"); bracket
kable(bracket, row.names = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, position = "left", fixed_thead = T) %>%
  footnote(symbol = "Based on 500 Tournament Simulations") %>%
  scroll_box(width = "100%", height = "520px") %>%
  save_kable(file = paste0("2023_GLMNET_Bracket_Simulation.html"))

# Make Kaggle Predictions, trained with unstandardized data only
kaggle_preds_16 <- kaggle_predictions(mod, 'xgbTree', 2016, 2016, vars, names = T)
kaggle_preds_17 <- kaggle_predictions(mod, 'xgbTree', 2017, 2017, vars, names = T)
kaggle_preds_18 <- kaggle_predictions(mod, 'xgbTree', 2018, 2018, vars, names = T)
kaggle_preds_19 <- kaggle_predictions(mod, 'xgbTree', 2019, 2019, vars, names = T)
kaggle_preds_21 <- kaggle_predictions(mod, 'xgbTree', 2021, 2021, vars, names = T) %>%
  distinct()
kaggle_preds <- rbind(kaggle_preds_16, kaggle_preds_17, kaggle_preds_18,
                      kaggle_preds_19, kaggle_preds_21) %>% 
  dplyr::select(ID, Pred)

################################################################################
kaggle_preds_mens_rf <- kaggle_predictions(mod, 'rf', 2024, 2024, vars, names = T)

kaggle_preds_mens_rf %>% 
  filter(Team1_Name == "North Carolina")

write.csv(kaggle_preds_mens_rf, "kaggle_preds_mens_rf.csv", row.names = F)

######################################################################################
kaggle_preds %>% filter(Team1_Name %in% c('Texas Tech','Tennessee'), # Tenn, Tenn, Tenn
                        Team2_Name %in% c('Tennessee','Texas Tech'))

#####################################################################################
# NN
vars2 <- c(vars, "Team1_Victory")

# Standardize, if wanted
preProcess <- preProcess(train1[, vars], method = c("center","scale"))
new_train1 <- predict(preProcess, train1[, vars])
new_train1$Team1_Victory <- train1$Team1_Victory
new_train1$Team1_Name <- train1$Team1_Name
new_train1$Team2_Name <- train1$Team2_Name
new_train1$Season <- train1$Season
new_train1$Seed_Team1 <- train1$Seed_Team1
new_train1$Seed_Team2 <- train1$Seed_Team2
new_train1$Round <- train1$Round

# Use new_train1 instead of train1 for standardized version
training_data <- train1[train1$Season %in% c(seq(2003,2018)),] 
training_response <- training_data[, c("Team1_Victory","Season")]
training_continuous <- training_data[, vars2]

# Use new_train1 instead of train1 for standardized version
testing_data <- train1[train1$Season %in% c(2019) & train1$Round > 0,] 
testing_response <- testing_data[, c("Team1_Victory","Season")]
testing_continuous <- testing_data[, vars2]

nn <- run_nn(vars2, num_epochs = 100)
nn[[2]]

answer <- nn[[3]]
hist(answer$Pred_Prob)
mean(answer$Pred_Prob)

# Look at games for which I predicted incorrectly
wrong <- testing_data[which(answer$True != answer$Pred_Outcome),c("Season","Team1_Name","Team2_Name","Seed_Team1","Seed_Team2","Round","Team1_Victory")]
wrong$upset <- ifelse(wrong$Team1_Victory == 1 & wrong$Seed_Team1 > wrong$Seed_Team2, 1, 
                      ifelse(wrong$Team1_Victory == 0 & wrong$Seed_Team1 < wrong$Seed_Team2, 1, 0))
d <- answer[answer$True != answer$Pred_Outcome,]
cbind(wrong, d$Pred_Outcome, d$Pred_Prob)
dim(wrong)[1]

NN <- nn[[1]]
test5 <- Bracket_Sim_NN(2019, 200)
bracket <- Normalize_Sim(test5, 200)
colnames(bracket)[1:4] = c("Season","Region","Seed","Team"); bracket
kable(bracket, row.names = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, position = "left", fixed_thead = T) %>%
  footnote(symbol = "Based on 500 Tournament Simulations") %>%
  scroll_box(width = "100%", height = "520px")
# Make Kaggle Predictions, trained with unstandardized data only
kaggle_nn <- kaggle_predictions(NN, 'nn', 2015, 2019, vars, min = F, names = F); 
kaggle_nn %>% filter(Team1_Name %in% c('UC Irvine','Kansas St'),
                          Team2_Name %in% c('UC Irvine','Kansas St')
) 
###########################################################################
# compare all caret models 
model_list <- list(glmnet = GLMNET, rf = rF, gbm = gbm, xgboost = xgboost)
resamples <- resamples(model_list)
summary(resamples)
dotplot(resamples, conf.level = 0.75, main = '3-Fold CV Error Caret Model Comparison')

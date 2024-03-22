# Format options
options(kableExtra.auto_format = FALSE)
options(knitr.table.format = "html")

# set working directory to load in functions
setwd("C:/Users/rusla/OneDrive/MarchMadness/March-Madness-Predictions/Code/Women")

# load in functions files
source('functions_W.R')
source('bracket_sim_functions_w.R')

# set working directory to load in data files
setwd("C:/Users/rusla/OneDrive/MarchMadness/March-Madness-Predictions/Data")

# Load in Kaggle Data
Seasons <- read.csv("WSeasons.csv") %>% 
  pivot_longer(cols = starts_with("Region"), names_to = "Region", names_prefix = "Region", 
               values_to = "Region_Name", values_drop_na = TRUE)
Reg_Season_Detailed <- read.csv("WRegularSeasonDetailedResults.csv")
Reg_Season_Compact <- read.csv("WRegularSeasonCompactResults.csv") %>% 
  mutate(Wadj_win = case_when(WLoc ==  'H' ~ 0.6, WLoc == 'N' ~ 1, TRUE ~ 1.6),
         Ladj_win = case_when(WLoc ==  'H' ~ 0.6, WLoc == 'N' ~ 1, TRUE ~ 1.6),
         WTeamID_Won = 1, 
         LTeamID_Lost = 1,
         Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team1_Victory = case_when(WTeamID == Team1 ~ 1, TRUE ~ 0),
         Team1_Loss = case_when(Team1_Victory == 1 ~ 0, TRUE ~ 1),
         Team2_Victory = Team1_Loss, Team2_Loss = Team1_Victory)
Teams <- read.csv("WTeams.csv")
Tourney_Detailed <- read.csv("WNCAATourneyDetailedResults.csv")
Tourney_Compact <- read.csv("WNCAATourneyCompactResults.csv")  %>%
  mutate(Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team1_Victory = case_when(WTeamID == Team1 ~ 1, TRUE ~ 0),
         WTeamID_Won = 1, 
         LTeamID_Lost = 1)
Tourney_Seeds <- read.csv("WNCAATourneySeeds.csv") %>%
  rename('RegionSeed' = Seed) %>%
  mutate(Region = as.character(substr(RegionSeed, 1, 1)),
         Seed = as.character(substr(RegionSeed, 2, 3)))
Tourney_Slots = read.csv("WNCAATourneySlots.csv")
Submission <- read.csv("SampleSubmission2024.csv")
Game_Cities <- read.csv("WGameCities.csv") %>% 
  mutate(Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID))) %>%
  dplyr::select(-WTeamID, -LTeamID)
Cities_Enriched <- read.csv("CitiesEnriched.csv")
ConfNames <- read.csv("WTeamConferences.csv")

# Calculate adjusted wins, adjusted losses, adjusted win %
Adjusted_Standings <- get_adjusted_wins(Reg_Season_Compact)

# Create more advanced metrics for regular season detailed set
Team_Avgs <- get_reg_season_avg_team_stats(Reg_Season_Detailed)

# Take into account for num tournament games for each team prior to current tournament
Tourney_History <- past_tourney_history(Tourney_Compact)

SOS <- strength_of_schedule(Reg_Season_Compact)

matchup <- get_season_matchups(Reg_Season_Compact)


# Creat all pairwise matchups data set for bracket simulation
all_years_submission <- get_all_pairwise_matchups(training_df, 2010, 2024)
#table(all_years_submission$Season)
#all_years_submission %>% filter(Season == 2023) %>% View()

shelve_df <- all_years_submission %>% dplyr::select(-pred) %>% 
  inner_join(Team_Avgs, by = c("Season","Team1" = "TeamID")) %>%
  inner_join(Team_Avgs, by = c("Season","Team2" = "TeamID"), suffix = c("_Team1","_Team2")) %>% 
  inner_join(Adjusted_Standings, by = c("Season","Team1" = "Team")) %>%
  inner_join(Adjusted_Standings, by = c("Season","Team2" = "Team"), suffix = c("_Team1","_Team2")) %>% 
  left_join(Tourney_History, by = c("Season","Team1" = "Team")) %>%
  left_join(Tourney_History, by = c("Season","Team2" = "Team"), suffix = c("_Team1","_Team2")) %>% 
  inner_join(Tourney_Seeds, by = c("Season","Team1" = "TeamID")) %>%
  inner_join(Tourney_Seeds, by = c("Season", "Team2" = "TeamID"), suffix = c("_Team1","_Team2")) %>%
  inner_join(SOS, by = c("Season","Team1" = "Team")) %>%
  inner_join(SOS, by = c("Season", "Team2" = "Team"), suffix = c("_Team1","_Team2")) %>%
  inner_join(Teams %>% select('TeamID','TeamName'), by = c("Team1" = "TeamID")) %>%
  inner_join(Teams %>% select('TeamID','TeamName'), by = c("Team2" = "TeamID"), suffix = c("_Team1","_Team2")) %>%
  inner_join(ConfNames, by = c("Season","Team1" = "TeamID")) %>%
  inner_join(ConfNames, by = c("Season","Team2" = "TeamID"), suffix = c("_Team1","_Team2")) %>% 
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
  left_join(matchup, by = c("Season","Team1","Team2")) %>% 
  tidyr::replace_na(list(Num_Losses_Matchup_Team1 = 0, Num_Losses_Matchup_Team2 = 0,
                         Num_Wins_Matchup_Team1 = 0, Num_Wins_Matchup_Team2 = 0, 
                         Team1_Matchup_Win_Pct = 0.5, Team2_Matchup_Win_Pct = 0.5,
                         num_tourney_games_since_1998_Team2 = 0,
                         num_tourney_advanced_since_1998_Team2 = 0,
                         num_tourney_sweet_16_since_1998_Team2 = 0,
                         num_tourney_elite_8_since_1998_Team2 = 0,
                         num_tourney_final_4_since_1998_Team2 = 0,
                         num_tourney_games_since_1998_Team1 = 0,
                         num_tourney_advanced_since_1998_Team1 = 0,
                         num_tourney_sweet_16_since_1998_Team1 = 0,
                         num_tourney_elite_8_since_1998_Team1 = 0,
                         num_tourney_final_4_since_1998_Team1 = 0))

shelve_df <- get_slots(shelve_df)

shelve_df <- shelve_df %>%
  mutate(Team1_Power = case_when(ConfAbbrev_Team1 %in% c('big_twelve','big_ten','acc','sec','big_east') ~ 1, TRUE ~ 0),
         Team2_Power = case_when(ConfAbbrev_Team2 %in% c('big_twelve','big_ten','acc','sec','big_east') ~ 1, TRUE ~ 0),
         Team1_Power = as.factor(Team1_Power), Team2_Power = as.factor(Team2_Power)) %>%
  arrange(Season, Team2, Team1)

shelve_df <- shelve_df[order(shelve_df$Season, shelve_df$Round, shelve_df$Region_Name_Team1, shelve_df$Region_Name_Team2, match(shelve_df$Seed_Team1, matchups)),]
shelve_df <- shelve_df[order(shelve_df$Season, match(shelve_df$Slot,slot_order)),]

#shelve_df$EarlyDayNum[is.na(shelve_df$EarlyDayNum)] <- 134
#shelve_df$LateDayNum[is.na(shelve_df$LateDayNum)] <- 135

shelve_df <- get_team_diffs(shelve_df) %>% filter(Slot != "Play_In")

#sort(colSums(is.na(shelve_df)))
#table(shelve_df$Season)

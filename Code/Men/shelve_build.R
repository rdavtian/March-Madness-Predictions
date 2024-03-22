#hoopR::espn_mbb_scoreboard(2022) %>% View()
#bigballR: %>% View()
#ncaahoopR::dict %>% View()
#ncaahoopR:: %>% View()

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
Tourney_Slots = read.csv("MNCAATourneySlots.csv")
Submission <- read.csv("SampleSubmission2023.csv")
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

# Creat all pairwise matchups data set for bracket simulation
all_years_submission <- get_all_pairwise_matchups(training_df, 2003, 2024)
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
  inner_join(Massey, by = c("Season","Team1" = "TeamID")) %>% 
  inner_join(Massey, by = c("Season","Team2" = "TeamID"), suffix = c("_Team1","_Team2")) %>%
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
  inner_join(Seasons, by = c("Season","Region_Team2" = "Region"), suffix = c("_Team1","_Team2"))

shelve_df <- get_slots(shelve_df)

shelve_df <- shelve_df %>% 
  left_join(Tourney_Seed_Round_Slots %>% dplyr::select(Round, GameSlot, EarlyDayNum, LateDayNum) %>% distinct(), 
            by = c("Round","Slot"="GameSlot")) %>%
  left_join(Coaches %>% group_by(Season, TeamID) %>% filter(FirstDayNum == max(FirstDayNum)), 
             join_by(Season == Season, Team1 == TeamID, EarlyDayNum >= FirstDayNum)) %>%
  left_join(Coaches %>% group_by(Season, TeamID) %>% filter(FirstDayNum == max(FirstDayNum)), 
             join_by(Season == Season, Team2 == TeamID, EarlyDayNum >= FirstDayNum),
  suffix = c("_Team1","_Team2")) %>%
  select(-FirstDayNum_Team1, -FirstDayNum_Team2,-LastDayNum_Team1, -LastDayNum_Team2) %>%
  left_join(coaches_reg_season, by = c("Season","Team1" = "Team","CoachName_Team1" = "CoachName")) %>% 
  left_join(coaches_reg_season, by = c("Season","Team2" = "Team","CoachName_Team2" = "CoachName"), suffix = c("_Team1","_Team2")) %>% 
  left_join(coaches_tourney, by = c("Season","Team1" = "Team","CoachName_Team1" = "CoachName")) %>% 
  left_join(coaches_tourney, by = c("Season","Team2" = "Team","CoachName_Team2" = "CoachName"), suffix = c("_Team1","_Team2")) %>%
  left_join(Game_Cities %>% select(-CRType), by = c("Season","EarlyDayNum" = "DayNum","Team1","Team2")) %>% 
  left_join(Game_Cities %>% select(-CRType), by = c("Season","LateDayNum" = "DayNum","Team1","Team2")) %>% 
  mutate(CityID = coalesce(CityID.x, CityID.y)) %>% select(-CityID.x,-CityID.y) %>%
  left_join(Cities_Enriched %>% select("CityId","City","LatHost","LngHost"), by = c("CityID" = "CityId")) %>%
  left_join(Tourney_Hosts %>% select("Season","Slot","Host","lat","lng","Round"),
             by = c("Season","Round","Slot")) %>% rename("Host_Lat" = lat, "Host_Lng" = lng) %>%
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
  arrange(Season, EarlyDayNum, Team2, Team1) %>% 
  tidyr::replace_na(list(conf_tourn_num_wins_Team1 = 0, conf_tourn_num_losses_Team1 = 0,
                         conf_tourn_win_pct_Team1 = 0.5, conf_champ_Team1 = 0, 
                         conf_tourn_num_wins_Team2 = 0, conf_tourn_num_losses_Team2 = 0,
                         conf_tourn_win_pct_Team2 = 0.5, conf_champ_Team2 = 0,
                         num_tourney_games_since_1985_Team1 = 0,
                         num_tourney_advanced_since_1985_Team1 = 0,
                         num_tourney_sweet_16_since_1985_Team1 = 0,
                         num_tourney_elite_8_since_1985_Team1 = 0,
                         num_tourney_final_4_since_1985_Team1 = 0,
                         num_tourney_games_since_1985_Team2 = 0,
                         num_tourney_advanced_since_1985_Team2 = 0,
                         num_tourney_sweet_16_since_1985_Team2 = 0,
                         num_tourney_elite_8_since_1985_Team2 = 0,
                         num_tourney_final_4_since_1985_Team2 = 0)) %>% 
  left_join(matchup, by = c("Season","Team1","Team2")) %>% 
  tidyr::replace_na(list(Num_Losses_Matchup_Team1 = 0, Num_Losses_Matchup_Team2 = 0,
                         Num_Wins_Matchup_Team1 = 0, Num_Wins_Matchup_Team2 = 0, 
                         Team1_Matchup_Win_Pct = 0.5, Team2_Matchup_Win_Pct = 0.5))

shelve_df <- shelve_df[order(shelve_df$Season, shelve_df$Round, shelve_df$Region_Name_Team1, shelve_df$Region_Name_Team2, match(shelve_df$Seed_Team1, matchups)),]
shelve_df <- shelve_df[order(shelve_df$Season, match(shelve_df$Slot,slot_order)),]

shelve_df$EarlyDayNum[is.na(shelve_df$EarlyDayNum)] <- 134
shelve_df$LateDayNum[is.na(shelve_df$LateDayNum)] <- 135

shelve_df <- shelve_df %>% 
  mutate(Career_Num_Wins_Since_1985_Team2 = case_when(is.na(Career_Num_Wins_Since_1985_Team2) & CoachName_Team2 == "kevin_nickelberry" ~ 127, TRUE ~ Career_Num_Wins_Since_1985_Team2),
         Career_Num_Losses_Since_1985_Team2 = case_when(is.na(Career_Num_Losses_Since_1985_Team2) & CoachName_Team2 == "kevin_nickelberry" ~ 235, TRUE ~ Career_Num_Losses_Since_1985_Team2),
         Career_Win_Pct_Since_1985_Team2 = case_when(is.na(Career_Win_Pct_Since_1985_Team2) & CoachName_Team2 == "kevin_nickelberry" ~ Career_Num_Wins_Since_1985_Team2 / (Career_Num_Wins_Since_1985_Team2 + Career_Num_Losses_Since_1985_Team2),
                                                     TRUE ~ Career_Num_Wins_Since_1985_Team2 / (Career_Num_Wins_Since_1985_Team2 + Career_Num_Losses_Since_1985_Team2)),
         Career_Num_Wins_Since_1985_Team1 = case_when(is.na(Career_Num_Wins_Since_1985_Team1) & CoachName_Team1 == "kevin_nickelberry" ~ 127, TRUE ~ Career_Num_Wins_Since_1985_Team1),
         Career_Num_Losses_Since_1985_Team1 = case_when(is.na(Career_Num_Losses_Since_1985_Team1) & CoachName_Team1 == "kevin_nickelberry" ~ 235, TRUE ~ Career_Num_Losses_Since_1985_Team1),
         Career_Win_Pct_Since_1985_Team1 = case_when(is.na(Career_Win_Pct_Since_1985_Team1) & CoachName_Team1 == "kevin_nickelberry" ~ Career_Num_Wins_Since_1985_Team1 / (Career_Num_Wins_Since_1985_Team1 + Career_Num_Losses_Since_1985_Team1),
                                                     TRUE ~ Career_Num_Wins_Since_1985_Team1 / (Career_Num_Wins_Since_1985_Team1 + Career_Num_Losses_Since_1985_Team1))) %>% 
  tidyr::replace_na(list(Career_Tourney_Wins_Since_1985_Team1 = 0, 
                         Career_Tourney_Losses_Since_1985_Team1 = 0,
                         Career_Tourney_Pct_Since_1985_Team1 = 0.5,
                         Career_Tourney_Wins_Since_1985_Team2 = 0,
                         Career_Tourney_Losses_Since_1985_Team2 = 0,
                         Career_Tourney_Pct_Since_1985_Team2 = 0.5))

shelve_df <- get_team_diffs(shelve_df) %>% 
  select(-CityID, -City) %>% filter(Slot != "Play_In")

#sort(colSums(is.na(shelve_df)))
#table(shelve_df$Season)

#shelve_df <- shelve_df %>% filter(Season < 2024)

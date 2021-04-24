# Informative Websites to visit
#https://www.masseyratings.com/ranks?s=cb&sym=cmp&top=4
#https://www.masseyratings.com/ranks?s=cb&dt=20200311
#https://www.usatoday.com/sports/ncaab/polls/ap-poll/2019-2020/2020-03-09/
#https://www.nbastuffer.com/nba-moneyball/
#https://statathlon.com/four-factors-basketball-success/

# Format options
options(kableExtra.auto_format = FALSE)
options(knitr.table.format = "html")
setwd("C:/Users/rusla/OneDrive/MarchMadness/March-Madness-Predictions/Code") # set working directory
source('functions.R'); source('bracket_sim_functions.R') # load in functions files
library(ggplot2) # load libraries needed
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

setwd("C:/Users/rusla/OneDrive/MarchMadness/March-Madness-Predictions/Stage1_2020")

# Load Data
Seasons <- read.csv("MSeasons.csv")
Reg_Season_Detailed <- read.csv("MRegularSeasonDetailedResults.csv")
Reg_Season_Compact <- read.csv("MRegularSeasonCompactResults.csv")
Teams <- read.csv("MTeams.csv")
Tourney_Detailed <- read.csv("MNCAATourneyDetailedResults.csv")
Tourney_Compact <- read.csv("MNCAATourneyCompactResults.csv")
Tourney_Seeds <- read.csv("MNCAATourneySeeds.csv")
Tourney_Slots = read.csv("MNCAATourneySlots.csv")
Submission <- read.csv("MSampleSubmissionStage2.csv")
Teams_Location = read.csv("Teams_Location.csv")
Tourney_Hosts = read.csv("TourneyHosts.csv")
Pomeroy = read.csv("KenPom.csv")
Coaches = read.csv("MTeamCoaches.csv")
Standings <- loadWorkbook('Teams_03_21_Rankings.xlsx')
Game_Cities = read.csv("MGameCities.csv")
Cities_Enriched = read.csv("CitiesEnriched.csv")
ConfTournament = read.csv("MConferenceTourneyGames.csv")
ConfNames = read.csv("MTeamConferences.csv")
Tourney_Seed_Round_Slots <- read.csv('MNCAATourneySeedRoundSlots.csv')
Tourney_Seed_Round_Slots <- Tourney_Seed_Round_Slots %>%
  rename('RegionSeed' = Seed, 'Round' = 'GameRound',
         'EarlyDaynum' = EarlyDayNum, 'LateDaynum' = LateDayNum) %>%
  mutate(Region = as.character(substr(RegionSeed, 1, 1)),
         Seed = as.character(substr(RegionSeed, 2, 3)),
         GameSlot = as.character(GameSlot))

# Calculate adjusted wins, adjusted losses, adjusted win %
Reg_Season_Compact <- Reg_Season_Compact %>% 
  mutate(Wadj_wins = case_when(WLoc ==  'H' ~ 0.6, WLoc == 'N' ~ 1, 
                               TRUE ~ 1.6),
         Ladj_wins = case_when(WLoc ==  'H' ~ 0.6,WLoc == 'N' ~ 1, 
                               TRUE ~ 1.6),
         Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team1_Victory = case_when(WTeamID == Team1 ~ 1,
                                   TRUE ~ 0))
Win <- Reg_Season_Compact %>%
  group_by(Season, WTeamID) %>%
  summarise(Adj_Wins = sum(Wadj_wins), .groups = "drop") %>%
  rename('Team' = WTeamID)
Lose <- Reg_Season_Compact %>% 
  group_by(Season, LTeamID) %>%
  summarise(Adj_Losses = sum(Ladj_wins), .groups = "drop") %>%
  rename('Team' = LTeamID)
Adjusted_Standings <- Win %>% 
  full_join(Lose, by = c("Season","Team")) %>%
  filter(Season >= 2003) %>%
  tidyr::replace_na(list(Adj_Wins = 0, Adj_Losses = 0)) %>%
  mutate(Adj_Win_Perc = Adj_Wins / (Adj_Wins + Adj_Losses))

# Extract regular season rank, wins, losses, win %, read in excel worksheets
sheetNames <- sheets(Standings)
for(i in 1:length(sheetNames))
{
  assign(sheetNames[i],readWorkbook(Standings,sheet = i))
}
Standings <- rbind(rank03,rank04,rank05,rank06,rank07,
                   rank08,rank09,rank10,rank11,rank12,
                   rank13,rank14,rank15,rank16,rank17,
                   rank18,rank19,rank21)
Standings <- Standings %>%
  tidyr::separate(col = 'W-L', into = c("Wins", "Losses"), sep = "-") %>%
  mutate(Rank = as.numeric(Rank), 
         Wins = as.numeric(Wins),
         Losses = as.numeric(Losses),
         PreSeason_Top25 = as.factor(PreSeason_Top25)) %>%
  mutate(Win_Perc = Wins / (Wins + Losses)) %>%
  rename('Mean_Rank' = Mean) %>% dplyr::select(-Team_Name)

# Add standings, rank, win % to regular season compact
Reg_Season_Compact <- Reg_Season_Compact %>%
  inner_join(Standings[,c("Season","Team_Id","Win_Perc","Mean_Rank","PreSeason_Top25")], 
             by = c("Season","Team1" = "Team_Id")) %>%
  inner_join(Standings[,c("Season","Team_Id","Win_Perc","Mean_Rank","PreSeason_Top25")], 
             by = c("Season","Team2" = "Team_Id")) %>%
  rename('Team1_Win_Perc' = Win_Perc.x, 'Team2_Win_Perc' = Win_Perc.y,
         'Team1_Rank' = Mean_Rank.x, 'Team2_Rank' = Mean_Rank.y,
         'Team1_PreSeason_Top25' = PreSeason_Top25.x,  'Team2_PreSeason_Top25' = PreSeason_Top25.y)

# Create more advanced metrics for regular season detailed set
Reg_Season_Detailed <- Reg_Season_Detailed %>%
  mutate(WNum_Poss = (WFGA - WOR) + WTO + (0.44*WFTA),
         LNum_Poss = (LFGA - LOR) + LTO + (0.44*LFTA),
         WOff_Rating = (WScore / WNum_Poss) * 100,
         LOff_Rating = (LScore / LNum_Poss) * 100,
         WDef_Rating = (LScore / LNum_Poss) * 100,
         LDef_Rating = (WScore / WNum_Poss) * 100,
         WNet_Eff = WOff_Rating - LOff_Rating,
         LNet_Eff = LOff_Rating - WOff_Rating,
         WMargin_Adj = 100*((WScore - LScore) / WNum_Poss),
         LMargin_Adj = 100*((LScore - WScore) / LNum_Poss),
         WShoot_Eff = 100*(WScore / (WFGA + 0.44*WFTA)),
         LShoot_Eff = 100*(LScore / (LFGA + 0.44*LFTA)),
         WScore_Opp = ((WFGA + 0.44*WFTA) / (WScore)),
         LScore_Opp = ((LFGA + 0.44*LFTA) / (LScore)),
         WEfg_Pct = 100*(((WFGM - WFGM3) + (1.5*WFGM3)) / (WFGA)),
         LEfg_Pct = 100*(((LFGM - LFGM3) + (1.5*LFGM3)) / (LFGA)),
         WPie = ((WScore + WFGM + WFTM - WFGA - WFTA + WDR + 0.5*WOR + WAst + WStl + WBlk - WPF - WTO) / 
           ((WScore + WFGM + WFTM - WFGA - WFTA + WDR + 0.5*WOR + WAst + WStl + WBlk - WPF - WTO) + 
           (LScore + LFGM + LFTM - LFGA - LFTA + LDR + 0.5*LOR + LAst + LStl + LBlk - LPF - LTO))),
         LPie = ((LScore + LFGM + LFTM - LFGA - LFTA + LDR + 0.5*LOR + LAst + LStl + LBlk - LPF - LTO) / 
                   ((WScore + WFGM + WFTM - WFGA - WFTA + WDR + 0.5*WOR + WAst + WStl + WBlk - WPF - WTO) + 
                      (LScore + LFGM + LFTM - LFGA - LFTA + LDR + 0.5*LOR + LAst + LStl + LBlk - LPF - LTO))),
         WTie = 100*(WPie / (WPie + LPie)),
         LTie = 100*(LPie / (LPie + WPie)),
         WOR_perc = 100*(WOR / (WOR + LDR)),
         LOR_perc = 100*(LOR / (LOR + WDR)),
         WDR_perc = 100*(WDR / (WDR + LOR)),
         LDR_perc = 100*(LDR / (LDR + WOR)),
         WFT_Perc = 100*(WFTM / WFTA),
         LFT_Perc = 100*(LFTM / LFTA),
         WTO_Perc = 100*(WTO / WNum_Poss),
         LTO_Perc = 100*(LTO / LNum_Poss),
         WTS_Perc = 100 * (WScore / (2 * (WFGA + 0.44*WFTA))),
         LTS_Perc = 100 * (LScore / (2 * (LFGA + 0.44*LFTA))),
         W3PT_Perc = 100 * (WFGM3 / WFGA3),
         L3PT_Perc = 100 * (LFGM3 / LFGA3),
         WAstRatio = 100 * (WAst / (WFGA + (0.44 * WFTA) + WAst + WTO)),
         LAstRatio = 100 * (LAst / (LFGA + (0.44 * LFTA) + LAst + LTO)),
         WReb_Perc = 100*((WDR + WOR) / (WDR + WOR + LDR + LOR)),
         LReb_Perc = 100*((LDR + LOR) / (LDR + LOR + WDR + WOR)),
         WFour_Factor = (0.4*WEfg_Pct)+(0.25*WTO_Perc)+(0.2*WOR_perc)+(0.15*(WFTA / WFGA)),
         LFour_Factor = (0.4*LEfg_Pct)+(0.25*LTO_Perc)+(0.2*LOR_perc)+(0.15*(LFTA / LFGA)),
         WBLK_Perc = 100*(WBlk / (LFGA - LFGA3)),
         LBLK_Perc = 100*(LBlk / (WFGA - WFGA3)),
         Wmargin = case_when(NumOT > 0 ~ as.numeric(1),
                             WScore - LScore > 17 ~ as.numeric(17),
                             TRUE ~ as.numeric(WScore - LScore)),
         Lmargin = case_when(NumOT > 0 ~ as.numeric(1),
                             LScore - WScore < -17 ~ as.numeric(-17),
                             TRUE ~ as.numeric(LScore - WScore)))

# Extract region played and seeds of team in tourney_seeds
Tourney_Seeds <- Tourney_Seeds %>%
  rename('RegionSeed' = Seed) %>%
  mutate(Region = as.character(substr(RegionSeed, 1, 1)),
         Seed = as.character(substr(RegionSeed, 2, 3)))

# Clean Tourney_Compact data, convert winning, losing team into team 1 and team 2 with 
# team 1 as the larger team id and team 2 as the smaller team id in a given matchup
Tourney_Compact <- Tourney_Compact %>%
  mutate(Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team1_Victory = case_when(WTeamID == Team1 ~ 1,
                                   TRUE ~ 0))

# Combine regular season detailed stats for winning teams
list_of_vars <- c("Season", "DayNum", "NumOT",
                 names(Reg_Season_Detailed[startsWith(names(Reg_Season_Detailed), "W")]))
Winner_History <- get_columns(Reg_Season_Detailed, list_of_vars) %>% 
  dplyr::select(-WLoc) %>% mutate(Victory = 1)

# Combine regular season detailed stats for losing teams
list_of_vars = c("Season", "DayNum", "NumOT",
                 names(Reg_Season_Detailed[startsWith(names(Reg_Season_Detailed), "L")]))
Loser_History <- get_columns(Reg_Season_Detailed, list_of_vars) %>% mutate(Victory = 0)

# Rename columns so they match in order to combine all winner, loser observations into 1 data set
names(Winner_History) = c("Season","Daynum","Team","Score","Numot","fgmade","fgattempt","fgm_three","fga_three",
                          "ftmade","ftattempted","offreb","defreb","ast","turnovers","steals","blocks",
                          "pfouls","num_poss","off_rating","def_rating","net_eff","margin_adj","shoot_eff",
                          "score_opp","efg_perc","pie","tie","offreb_perc","defreb_perc","ft_perc","turnover_perc",
                          "true_shooting_perc","three_pt_perc","ast_ratio","reb_perc","four_factor","blk_perc",
                          "margin","victory")

names(Loser_History) = c("Season","Daynum","Team","Score","Numot","fgmade","fgattempt","fgm_three","fga_three",
                         "ftmade","ftattempted","offreb","defreb","ast","turnovers","steals","blocks",
                         "pfouls","num_poss","off_rating","def_rating","net_eff","margin_adj","shoot_eff",
                         "score_opp","efg_perc","pie","tie","offreb_perc","defreb_perc","ft_perc","turnover_perc",
                         "true_shooting_perc","three_pt_perc","ast_ratio","reb_perc","four_factor","blk_perc",
                         "margin","victory")
# Combine losing and winning team stats together into 1 data set
Team_History <- rbind(Winner_History, Loser_History)

# Aggregate or group by season and team. Find stat averages per team per season
Team_Avgs <- Team_History %>% group_by(Season, Team) %>% summarise_all(mean) %>% 
  dplyr::select(-Daynum, -Numot)
######################################################################################################
# Create training data set
# Merge Team1 and Team2 Averages with Tourney_Compact by Season and Team ID
train1 <- Tourney_Compact %>% dplyr::select(Season, DayNum, Team1, Team2, Team1_Victory) %>%
  inner_join(Team_Avgs, by = c("Season","Team1" = "Team")) %>%
  inner_join(Team_Avgs, by = c("Season","Team2" = "Team"),
             suffix = c("_Team1_Avg","_Team2_Avg")) %>%
  dplyr::select(-victory_Team1_Avg, -victory_Team2_Avg)
  
# Merge seed information for each team1, team2 per season
train1 <- train1 %>%
  inner_join(Tourney_Seeds, by = c("Season","Team1" = "TeamID")) %>%
  inner_join(Tourney_Seeds, by = c("Season", "Team2" = "TeamID"),
             suffix = c("_Team1","_Team2")) %>%
  mutate(Seed_Team1 = as.numeric(Seed_Team1),
         Seed_Team2 = as.numeric(Seed_Team2),
         Seed_Diff = Seed_Team1 - Seed_Team2)

train1$Round <- unlist(mapply(roundFinder,train1$Region_Team1,
                             train1$Region_Team2,
                             train1$Seed_Team1,
                             train1$Seed_Team2, SIMPLIFY = TRUE), use.names = FALSE)

upsets <- subset(train1, train1$Seed_Diff > 0 & train1$Team1_Victory == 1)
upsets <- rbind(upsets, subset(train1, train1$Seed_Diff < 0 & train1$Team1_Victory == 0))
accuracy = 1 - (nrow(upsets) / nrow(train1)); accuracy

# Plotting seed differences vs daynum
#ggplot(aes(x = upsets$DayNum, y = abs(upsets$Seed_Diff)), data =  upsets) +
  #geom_jitter(aes(colour = DayNum)) + geom_smooth(method = "lm")
#######################################################################################
## Get Region Names
# Team 1
W1 <- sqldf("SELECT train1.*, Seasons.Regionw AS Team1_Region_Name 
            FROM train1 JOIN Seasons ON train1.Season = Seasons.Season 
            AND train1.Region_Team1 = 'W'")
X1 <- sqldf("SELECT train1.*, Seasons.Regionx AS Team1_Region_Name 
            FROM train1 JOIN Seasons ON train1.Season = Seasons.Season 
            AND train1.Region_Team1 = 'X'")
Y1 <- sqldf("SELECT train1.*, Seasons.Regiony AS Team1_Region_Name 
            FROM train1 JOIN Seasons ON train1.Season = Seasons.Season 
            AND train1.Region_Team1 = 'Y'")
Z1 <- sqldf("SELECT train1.*, Seasons.Regionz AS Team1_Region_Name 
            FROM train1 JOIN Seasons ON train1.Season = Seasons.Season 
            AND train1.Region_Team1 = 'Z'")
train1 <- rbind(W1, X1, Y1, Z1)
#Team 2
W2 = sqldf("SELECT train1.*, Seasons.Regionw AS Team2_Region_Name 
           FROM train1 JOIN Seasons ON train1.Season = Seasons.Season 
           AND train1.Region_Team2 = 'W'")
X2 = sqldf("SELECT train1.*, Seasons.Regionx AS Team2_Region_Name 
           FROM train1 JOIN Seasons ON train1.Season = Seasons.Season 
           AND train1.Region_Team2 = 'X'")
Y2 = sqldf("SELECT train1.*, Seasons.Regiony AS Team2_Region_Name 
           FROM train1 JOIN Seasons ON train1.Season = Seasons.Season 
           AND train1.Region_Team2 = 'Y'")
Z2 = sqldf("SELECT train1.*, Seasons.Regionz AS Team2_Region_Name 
           FROM train1 JOIN Seasons ON train1.Season = Seasons.Season 
           AND train1.Region_Team2 = 'Z'")

train1 <- rbind(W2, X2, Y2, Z2)
train1 <- train1[order(train1$Season,train1$DayNum,train1$Team2,train1$Team1),]

# Edit region seed names
for (i in 1:dim(train1)[1])
{
  if (train1$Round[i] >= 1)
  {
    train1$RegionSeed_Team1[i] = substr(train1$RegionSeed_Team1[i], start = 1, stop = 3) 
    train1$RegionSeed_Team2[i] = substr(train1$RegionSeed_Team2[i], start = 1, stop = 3) 
  }
  else{
    train1$RegionSeed_Team1[i] = train1$RegionSeed_Team1[i]
    train1$RegionSeed_Team2[i] = train1$RegionSeed_Team2[i]
  }
}
# create slots
train1$slot <- creating_slots()

# Add team1, team2 official school names
train1 <- train1 %>%
  inner_join(Teams[,c('TeamID','TeamName')], by = c("Team1" = "TeamID")) %>%
  inner_join(Teams[,c('TeamID','TeamName')], by = c("Team2" = "TeamID")) %>%
  rename("Team1_Name" = TeamName.x, "Team2_Name" = TeamName.y)

# Team1, team2 conference names
train1 <- train1 %>%
  inner_join(ConfNames, by = c("Season","Team1" = "TeamID")) %>%
  inner_join(ConfNames, by = c("Season","Team2" = "TeamID")) %>%
  rename("Team1_Conf" = ConfAbbrev.x, "Team2_Conf" = ConfAbbrev.y)

# Get alternative city ids available since 2010 only
Game_Cities <- Game_Cities %>% filter(CRType == "NCAA") 
Game_Cities <- Game_Cities %>%
  mutate(Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID))) %>%
  dplyr::select(-WTeamID, -LTeamID, -CRType)
train1 <- train1 %>%
  left_join(Game_Cities, by = c("Season","DayNum","Team1","Team2"))

# Merge alt city, alt lat, alt lng, altitudes data
train1 <- train1 %>%
  left_join(Cities_Enriched[, c("CityId","City","LatHost","LngHost")], by = c("CityID" = "CityId"))

# Add host city and locations to each tournament match in training data 
Tourney_Hosts$Slot <- as.character(Tourney_Hosts$Slot)
train1 <- train1 %>%
  inner_join(Tourney_Hosts[, c("Season","Slot","Host","lat","lng","Round","Team1","Team2")],
             by = c("Season","Round","Team1","Team2")) %>%
  rename("Host_Lat" = lat, "Host_Lng" = lng, "slot2" = "Slot")

train1 <- train1 %>%
  mutate(City = as.character(City),
         Host = as.character(Host)) %>%
  mutate(Host_City = coalesce(City, Host),
         Host_Lat2 = coalesce(LatHost,Host_Lat),
         Host_Lng2 = coalesce(LngHost,Host_Lng)) %>%
  dplyr::select(-City, -LatHost, -LngHost, -Host, -Host_Lat, -Host_Lng) %>%
  rename("Host_Lat" = Host_Lat2, "Host_Lng" = Host_Lng2) %>%
  mutate(Host_City = tolower(Host_City),
         Host_City = gsub(" ", "_", Host_City))

# Add actual team locations (lat, lng coordinates) into training data 
train1 <- train1 %>%
  inner_join(Teams_Location, by = c("Team1" = "Team_Id")) %>%
  rename("Team1_Lat" = lat, "Team1_Lng" = lng) %>%
  inner_join(Teams_Location, by = c("Team2" = "Team_Id")) %>%
  rename("Team2_Lat" = lat, "Team2_Lng" = lng)

## Find distances from each host city to each team using the spherical formula
# Team1, Host
p1 = matrix(c(train1$Team1_Lng, train1$Team1_Lat), nrow = nrow(train1), ncol = 2)
p2 = matrix(c(train1$Host_Lng, train1$Host_Lat), nrow = nrow(train1), ncol = 2)
train1$Team1_Dist <- distHaversine(p1,p2, r = 3963.2)
# Team2, Host
p3 = matrix(c(train1$Team2_Lng, train1$Team2_Lat), nrow = nrow(train1), ncol = 2)
p4 = matrix(c(train1$Host_Lng, train1$Host_Lat), nrow = nrow(train1), ncol = 2)
train1$Team2_Dist <- distHaversine(p3,p4, r = 3963.2)

# Identify conference champions
ConfTournament <- ConfTournament %>%
  mutate(Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team1_Victory = case_when(Team1 == WTeamID ~ 1, TRUE ~ 0),
         Team2_Victory = case_when(Team2 == WTeamID ~ 1, TRUE ~ 0)) 

team1 <- ConfTournament %>% 
  group_by(Season, ConfAbbrev, Team1) %>%
  summarise(total_wins_team1 = sum(Team1_Victory),
            total_losses_team1 = sum(Team2_Victory), .groups = "drop")  %>%
  rename("Team" = Team1)
team2 <- ConfTournament %>% 
  group_by(Season, ConfAbbrev, Team2) %>%
  summarise(total_wins_team2 = sum(Team2_Victory),
            total_losses_team2 = sum(Team1_Victory), .groups = "drop")  %>%
  rename("Team" = Team2)

team_games <- team1 %>% full_join(team2, by = c("Season","Team","ConfAbbrev")) %>% 
  mutate(total_wins_team1 = tidyr::replace_na(total_wins_team1, 0),
         total_wins_team2 = tidyr::replace_na(total_wins_team2, 0),
         total_losses_team1 = tidyr::replace_na(total_losses_team1, 0),
         total_losses_team2 = tidyr::replace_na(total_losses_team2, 0),
         total_wins = total_wins_team1 + total_wins_team2,
         total_losses = total_losses_team1 + total_losses_team2) %>% 
  dplyr::select(Season, ConfAbbrev, Team, total_wins, total_losses)

Conf_Champs <- sqldf("SELECT DISTINCT Season, ConfAbbrev, Team, 1 AS Conf_Champ 
               FROM team_games WHERE 
               (Season, ConfAbbrev, total_wins, total_losses) IN  
               (SELECT Season, ConfAbbrev, MAX(total_wins) AS total_wins, 
               MIN(total_losses) AS total_losses FROM team_games
               GROUP BY Season, ConfAbbrev)") %>%
  mutate(ConfAbbrev = as.factor(ConfAbbrev))

train1 <- train1 %>% 
  left_join(Conf_Champs, by = c("Season","Team1_Conf" = "ConfAbbrev","Team1" = "Team")) %>%
  left_join(Conf_Champs, by = c("Season","Team2_Conf" = "ConfAbbrev","Team2" = "Team")) %>%
  rename(Team1_ConfChamp = "Conf_Champ.x",
         Team2_ConfChamp = "Conf_Champ.y") %>%
  tidyr::replace_na(list(Team1_ConfChamp = 0, Team2_ConfChamp = 0)) %>%
  mutate(Team1_Conf = as.factor(Team1_Conf), Team2_Conf = as.factor(Team2_Conf),
         Team1_ConfChamp = as.factor(Team1_ConfChamp),Team2_ConfChamp = as.factor(Team2_ConfChamp))

# Penalize for being double digit seed
train1$Team1_Penalize = as.factor(ifelse(train1$Seed_Team1 >= 10, 1, 0))
train1$Team2_Penalize = as.factor(ifelse(train1$Seed_Team2 >= 10, 1, 0))

# Take into account for num tournament games for each team prior to current tournament
team1 <- Tourney_Compact %>% 
  group_by(Team1, Season) %>%
  summarise(total_games_team1 = n(), .groups = "drop")  %>%
  rename("Team" = Team1)
team2 <- Tourney_Compact %>% 
  group_by(Team2, Season) %>%
  summarise(total_games_team2 = n(),.groups = "drop")  %>%
  rename("Team" = Team2)

team_games <- team1 %>% full_join(team2, by = c("Season","Team")) %>% 
  mutate(total_games_team1 = tidyr::replace_na(total_games_team1, 0),
         total_games_team2 = tidyr::replace_na(total_games_team2, 0),
         num_games = total_games_team1 + total_games_team2) %>% 
  dplyr::select(Season, Team, num_games) %>% 
  arrange(Team, Season) %>%
  mutate(num_games_since_1985 = cumsum(num_games)) %>%
  dplyr::select(Season, Team, num_games_since_1985)
team_games <- team_games %>%
  arrange(Team, Season) %>%
  group_by(Team) %>%
  mutate(num_games_since_1985 = dplyr::lag(num_games_since_1985, n = 1, default = 0))

train1 <- train1 %>%
  inner_join(team_games[, c("Team","Season","num_games_since_1985")], 
             by = c("Team1" = "Team","Season")) %>%
  inner_join(team_games[, c("Team","Season","num_games_since_1985")], 
             by = c("Team2" = "Team","Season")) %>%
  rename("Team1_Num_Games" = num_games_since_1985.x, 
         "Team2_Num_Games" = num_games_since_1985.y)

# Pull in reg. season win/loss, win%, and team rankings
train1 <- train1 %>% 
  inner_join(Standings, by = c("Season","Team1" = "Team_Id")) %>%
  inner_join(Standings, by = c("Season","Team2" = "Team_Id")) %>%
  rename('Team1_Wins' = Wins.x,'Team1_Losses' = Losses.x,'Team1_Rank' = Rank.x,
         'Team1_Mean_Rank' = Mean_Rank.x,'Team1_Win_Perc' = Win_Perc.x,
         'Team2_Wins' = Wins.y,'Team2_Losses' = Losses.y,'Team2_Rank' = Rank.y,
         'Team2_Mean_Rank' = Mean_Rank.y,'Team2_Win_Perc' = Win_Perc.y,
         'Team1_PreSeason_Top25' = PreSeason_Top25.x, 'Team2_PreSeason_Top25' = PreSeason_Top25.y)

# Pull in adjusted win %
train1 <- train1 %>%
  inner_join(Adjusted_Standings, by = c("Season","Team1" = "Team")) %>%
  inner_join(Adjusted_Standings, by = c("Season","Team2" = "Team")) %>%
  rename('Team1_Adj_Wins' = Adj_Wins.x,'Team1_Adj_Losses' = Adj_Losses.x,
         'Team1_Adj_Win_Perc' = Adj_Win_Perc.x, 
         'Team2_Adj_Wins' = Adj_Wins.y,'Team2_Adj_Losses' = Adj_Losses.y,
         'Team2_Adj_Win_Perc' = Adj_Win_Perc.y)

# Create SOS (strength of schedule)
Team1_SOS <- Reg_Season_Compact %>%
  group_by(Season, Team1) %>%
  summarise(Team1_Num_Games = n(), 
            Team1_SOS = mean(Team2_Win_Perc, na.rm = T), .groups = "drop") %>%
  rename('Team' = 'Team1')
Team2_SOS <- Reg_Season_Compact %>%
  group_by(Season, Team2) %>%
  summarise(Team2_Num_Games = n(), 
            Team2_SOS = mean(Team1_Win_Perc, na.rm = T), .groups = "drop") %>%
  rename('Team' = 'Team2')
SOS <- Team1_SOS %>% 
  full_join(Team2_SOS, by = c("Season","Team")) %>%
  tidyr::replace_na(list(Team1_Num_Games = 0, Team2_Num_Games = 0,
                         Team1_SOS = 0, Team2_SOS = 0)) %>%
  mutate(SOS = ((Team1_Num_Games / (Team1_Num_Games + Team2_Num_Games)) * Team1_SOS) + 
           ((Team2_Num_Games / (Team1_Num_Games + Team2_Num_Games)) * Team2_SOS)) %>%
  dplyr::select(Season, Team, SOS)

train1 <- train1 %>% 
  inner_join(SOS, by = c("Season","Team1" = "Team")) %>%
  inner_join(SOS, by = c("Season","Team2" = "Team")) %>%
  rename('Team1_SOS' = SOS.x, 'Team2_SOS' = SOS.y)

# Create Number of Top 50 Wins Variable
Team1_Top_Wins <- Reg_Season_Compact %>%
  filter(Team2_Rank <= 50 & Team1_Victory == 1) %>%
  group_by(Season, Team1) %>%
  summarise(Team1_Top_50_Wins = n(),.groups = "drop") %>%
  rename('Team' = Team1)
Team2_Top_Wins <- Reg_Season_Compact %>%
  filter(Team1_Rank <= 50 & Team1_Victory == 0) %>%
  group_by(Season, Team2) %>%
  summarise(Team2_Top_50_Wins = n(), .groups = "drop") %>%
  rename('Team' = Team2)
Top_50_Wins <- Team1_Top_Wins %>% 
  full_join(Team2_Top_Wins, by = c("Season","Team")) %>% 
  tidyr::replace_na(list(Team1_Top_50_Wins = 0, Team2_Top_50_Wins = 0)) %>%
  mutate(Top_50_Wins = Team1_Top_50_Wins + Team2_Top_50_Wins) %>%
  dplyr::select(Season, Team, Top_50_Wins)

train1 <- train1 %>% 
  left_join(Top_50_Wins, by = c("Season","Team1" = "Team")) %>%
  left_join(Top_50_Wins, by = c("Season","Team2" = "Team")) %>%
  rename('Team1_Top_50_Wins' = Top_50_Wins.x, 'Team2_Top_50_Wins' = Top_50_Wins.y) %>%
  tidyr::replace_na(list(Team1_Top_50_Wins = 0, Team2_Top_50_Wins = 0))

# Identify number of Bad Losses for teams that to lower ranked teams 
Team1_Bad_Losses <- Reg_Season_Compact %>%
  filter(Team2_Rank - Team1_Rank > 1, 
         Team1_Victory == 0) %>%
  group_by(Season, Team1) %>%
  summarise(Team1_Bad_Losses = n(), .groups = "drop") %>%
  rename('Team' = Team1)
Team2_Bad_Losses <- Reg_Season_Compact %>%
  filter(Team1_Rank - Team2_Rank > 1, 
         Team1_Victory == 1) %>%
  group_by(Season, Team2) %>%
  summarise(Team2_Bad_Losses = n(), .groups = "drop") %>%
  rename('Team' = Team2)
Bad_Losses <- Team1_Bad_Losses %>% 
  full_join(Team2_Bad_Losses, by = c("Season","Team")) %>% 
  tidyr::replace_na(list(Team1_Bad_Losses = 0, Team2_Bad_Losses = 0)) %>%
  mutate(Bad_Losses = Team1_Bad_Losses + Team2_Bad_Losses) %>%
  dplyr::select(Season, Team, Bad_Losses)

train1 <- train1 %>% 
  left_join(Bad_Losses, by = c("Season","Team1" = "Team")) %>%
  left_join(Bad_Losses, by = c("Season","Team2" = "Team")) %>%
  rename('Team1_Bad_Losses' = Bad_Losses.x, 'Team2_Bad_Losses' = Bad_Losses.y) %>%
  tidyr::replace_na(list(Team1_Bad_Losses = 0, Team2_Bad_Losses = 0))

# Power Conference (1/0) dummy variable for whether or not team is a power conference team
train1 <- train1 %>%
  mutate(Team1_Power = case_when(Team1_Conf %in% c('big_twelve','big_ten','acc','sec','big_east') ~ 1,
                                 TRUE ~ 0),
         Team2_Power = case_when(Team2_Conf %in% c('big_twelve','big_ten','acc','sec','big_east') ~ 1,
                                 TRUE ~ 0),
         Team1_Power = as.factor(Team1_Power), Team2_Power = as.factor(Team2_Power))
# Create differences of all averaged stats between both teams
# (team1 - team2)
for (i in (seq(6, 40, 1)))
{
  train1[,128 + i] <- train1[i] - train1[i+35]
  colnames(train1)[128 + i] = paste0(gsub("Team.*$", "", colnames(train1)[128 + i]),"diff")
}
train1$Team1_Name <- as.character(train1$Team1_Name)
train1$Team2_Name <- as.character(train1$Team2_Name)
train1 <- train1 %>%
  mutate(dist_diff = Team1_Dist - Team2_Dist,
         rank_diff = Team1_Mean_Rank - Team2_Mean_Rank,
         sos_diff = Team1_SOS - Team2_SOS,
         top_50_diff = Team1_Top_50_Wins - Team2_Top_50_Wins,
         win_perc_diff = Team1_Win_Perc - Team2_Win_Perc,
         adj_win_perc_diff = Team1_Adj_Win_Perc - Team2_Adj_Win_Perc,
         adj_win_diff = Team1_Adj_Wins - Team2_Adj_Wins,
         win_diff = Team1_Wins - Team2_Wins,
         bad_losses_diff = Team1_Bad_Losses - Team2_Bad_Losses,
         team_power_diff = as.numeric(as.character(Team1_Power)) - as.numeric(as.character(Team2_Power)),
         preseason_top_25_diff = as.numeric(as.character(Team1_PreSeason_Top25)) - 
           as.numeric(as.character(Team1_PreSeason_Top25)))
###############################################################################################
# Creat all pairwise matchups data set for bracket simulation
all_years_submission <- get_all_pairwise_matchups(2003, 2021)
kaggle1 <- all_years_submission %>% dplyr::select(-pred) %>%
  inner_join(Team_Avgs, by = c("Season","Team1" = "Team")) %>%
  inner_join(Team_Avgs, by = c("Season","Team2" = "Team"),
             suffix = c("_Team1_Avg","_Team2_Avg")) %>%
  dplyr::select(-victory_Team1_Avg, -victory_Team2_Avg)

# Merge seed information for each team1, team2 per season
kaggle1 <- kaggle1 %>%
  inner_join(Tourney_Seeds, by = c("Season","Team1" = "TeamID")) %>%
  inner_join(Tourney_Seeds, by = c("Season", "Team2" = "TeamID"),
             suffix = c("_Team1","_Team2")) %>%
  mutate(Seed_Team1 = as.numeric(Seed_Team1),
         Seed_Team2 = as.numeric(Seed_Team2),
         Seed_Diff = Seed_Team1 - Seed_Team2)

kaggle1$Round <- unlist(mapply(roundFinder,kaggle1$Region_Team1,
                              kaggle1$Region_Team2,
                              kaggle1$Seed_Team1,
                              kaggle1$Seed_Team2, SIMPLIFY = TRUE), use.names = FALSE)
#######################################################################################
## Get Region Names
# Team 1
W1 <- sqldf("SELECT kaggle1.*, Seasons.Regionw AS Team1_Region_Name 
            FROM kaggle1 JOIN Seasons ON kaggle1.Season = Seasons.Season 
            AND kaggle1.Region_Team1 = 'W'")
X1 <- sqldf("SELECT kaggle1.*, Seasons.Regionx AS Team1_Region_Name 
            FROM kaggle1 JOIN Seasons ON kaggle1.Season = Seasons.Season 
            AND kaggle1.Region_Team1 = 'X'")
Y1 <- sqldf("SELECT kaggle1.*, Seasons.Regiony AS Team1_Region_Name 
            FROM kaggle1 JOIN Seasons ON kaggle1.Season = Seasons.Season 
            AND kaggle1.Region_Team1 = 'Y'")
Z1 <- sqldf("SELECT kaggle1.*, Seasons.Regionz AS Team1_Region_Name 
            FROM kaggle1 JOIN Seasons ON kaggle1.Season = Seasons.Season 
            AND kaggle1.Region_Team1 = 'Z'")
kaggle1 <- rbind(W1, X1, Y1, Z1)
#Team 2
W2 <- sqldf("SELECT kaggle1.*, Seasons.Regionw AS Team2_Region_Name 
           FROM kaggle1 JOIN Seasons ON kaggle1.Season = Seasons.Season 
           AND kaggle1.Region_Team2 = 'W'")
X2 <- sqldf("SELECT kaggle1.*, Seasons.Regionx AS Team2_Region_Name 
           FROM kaggle1 JOIN Seasons ON kaggle1.Season = Seasons.Season 
           AND kaggle1.Region_Team2 = 'X'")
Y2 <- sqldf("SELECT kaggle1.*, Seasons.Regiony AS Team2_Region_Name 
           FROM kaggle1 JOIN Seasons ON kaggle1.Season = Seasons.Season 
           AND kaggle1.Region_Team2 = 'Y'")
Z2 <- sqldf("SELECT kaggle1.*, Seasons.Regionz AS Team2_Region_Name 
           FROM kaggle1 JOIN Seasons ON kaggle1.Season = Seasons.Season 
           AND kaggle1.Region_Team2 = 'Z'")

kaggle1 <- rbind(W2, X2, Y2, Z2)
kaggle1 <- kaggle1[order(kaggle1$Season,kaggle1$Team2,kaggle1$Team1),]

# Edit region seed names
for (i in 1:dim(kaggle1)[1])
{
  if (kaggle1$Round[i] >= 1)
  {
    kaggle1$RegionSeed_Team1[i] = substr(kaggle1$RegionSeed_Team1[i], start = 1, stop = 3) 
    kaggle1$RegionSeed_Team2[i] = substr(kaggle1$RegionSeed_Team2[i], start = 1, stop = 3) 
  }
  else{
    kaggle1$RegionSeed_Team1[i] = kaggle1$RegionSeed_Team1[i]
    kaggle1$RegionSeed_Team2[i] = kaggle1$RegionSeed_Team2[i]
  }
}
# create slots
kaggle1$slot <- creating_slots_kaggle()

# Add Created Daynum
daynum <- Tourney_Seed_Round_Slots[, c('Round','GameSlot','EarlyDaynum','LateDaynum')]
daynum <- daynum %>% distinct()
kaggle1 <- kaggle1 %>%
  left_join(daynum, by = c('Round','slot' = 'GameSlot'))
kaggle1$EarlyDaynum[is.na(kaggle1$EarlyDaynum)] <- 134
kaggle1$LateDaynum[is.na(kaggle1$LateDaynum)] <- 135
#################################################################################
# Add team1, team2 official school names
kaggle1 <- kaggle1 %>%
  inner_join(Teams[,c('TeamID','TeamName')], by = c("Team1" = "TeamID")) %>%
  inner_join(Teams[,c('TeamID','TeamName')], by = c("Team2" = "TeamID")) %>%
  rename("Team1_Name" = TeamName.x, "Team2_Name" = TeamName.y)

# Team1, team2 conference names
kaggle1 <- kaggle1 %>%
  inner_join(ConfNames, by = c("Season","Team1" = "TeamID")) %>%
  inner_join(ConfNames, by = c("Season","Team2" = "TeamID")) %>%
  rename("Team1_Conf" = ConfAbbrev.x, "Team2_Conf" = ConfAbbrev.y)

# Get alternative city ids available since 2010 only
kaggle1 <- kaggle1 %>%
  left_join(Game_Cities, by = c("Season","Team1","Team2"))

# Merge alt city, alt lat, alt lng, altitudes data
kaggle1 <- kaggle1 %>%
  left_join(Cities_Enriched[, c("CityId","City","LatHost","LngHost")], by = c("CityID" = "CityId"))

# Add host city and locations to each tournament match in training data 
Tourney_Hosts$Slot <- as.character(Tourney_Hosts$Slot)
Tourney_Hosts$Slot[Tourney_Hosts$Round == 0] = "Play_In"
kaggle1 <- kaggle1 %>%
  inner_join(Tourney_Hosts[, c("Season","Slot","Host","lat","lng")],
             by = c("Season","slot" = "Slot")) %>%
  rename("Host_Lat" = lat, "Host_Lng" = lng) %>% distinct()

kaggle1 <- kaggle1 %>%
  mutate(City = as.character(City),
         Host = as.character(Host)) %>%
  mutate(Host_City = coalesce(City, Host),
         Host_Lat2 = coalesce(LatHost,Host_Lat),
         Host_Lng2 = coalesce(LngHost,Host_Lng)) %>%
  dplyr::select(-City, -LatHost, -LngHost, -Host, -Host_Lat, -Host_Lng) %>%
  rename("Host_Lat" = Host_Lat2, "Host_Lng" = Host_Lng2) %>%
  mutate(Host_City = tolower(Host_City),
         Host_City = gsub(" ", "_", Host_City))

# Add actual team locations (lat, lng coordinates) into training data 
kaggle1 <- kaggle1 %>%
  inner_join(Teams_Location, by = c("Team1" = "Team_Id")) %>%
  rename("Team1_Lat" = lat, "Team1_Lng" = lng) %>%
  inner_join(Teams_Location, by = c("Team2" = "Team_Id")) %>%
  rename("Team2_Lat" = lat, "Team2_Lng" = lng)

## Find distances from each host city to each team using the spherical formula
# Team1, Host
p1 = matrix(c(kaggle1$Team1_Lng, kaggle1$Team1_Lat), nrow = nrow(kaggle1), ncol = 2)
p2 = matrix(c(kaggle1$Host_Lng, kaggle1$Host_Lat), nrow = nrow(kaggle1), ncol = 2)
kaggle1$Team1_Dist <- distHaversine(p1,p2, r = 3963.2)
# Team2, Host
p3 = matrix(c(kaggle1$Team2_Lng, kaggle1$Team2_Lat), nrow = nrow(kaggle1), ncol = 2)
p4 = matrix(c(kaggle1$Host_Lng, kaggle1$Host_Lat), nrow = nrow(kaggle1), ncol = 2)
kaggle1$Team2_Dist <- distHaversine(p3,p4, r = 3963.2)

# Identify conference champions
ConfTournament <- ConfTournament %>%
  mutate(Team1 = pmax(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team2 = pmin(as.numeric(WTeamID), as.numeric(LTeamID)),
         Team1_Victory = case_when(Team1 == WTeamID ~ 1, TRUE ~ 0),
         Team2_Victory = case_when(Team2 == WTeamID ~ 1, TRUE ~ 0)) 

team1 <- ConfTournament %>% 
  group_by(Season, ConfAbbrev, Team1) %>%
  summarise(total_wins_team1 = sum(Team1_Victory),
            total_losses_team1 = sum(Team2_Victory), .groups = "drop")  %>%
  rename("Team" = Team1)
team2 <- ConfTournament %>% 
  group_by(Season, ConfAbbrev, Team2) %>%
  summarise(total_wins_team2 = sum(Team2_Victory),
            total_losses_team2 = sum(Team1_Victory), .groups = "drop")  %>%
  rename("Team" = Team2)

team_games <- team1 %>% full_join(team2, by = c("Season","Team","ConfAbbrev")) %>% 
  mutate(total_wins_team1 = tidyr::replace_na(total_wins_team1, 0),
         total_wins_team2 = tidyr::replace_na(total_wins_team2, 0),
         total_losses_team1 = tidyr::replace_na(total_losses_team1, 0),
         total_losses_team2 = tidyr::replace_na(total_losses_team2, 0),
         total_wins = total_wins_team1 + total_wins_team2,
         total_losses = total_losses_team1 + total_losses_team2) %>% 
  dplyr::select(Season, ConfAbbrev, Team, total_wins, total_losses)

Conf_Champs <- sqldf("SELECT DISTINCT Season, ConfAbbrev, Team, 1 AS Conf_Champ 
               FROM team_games WHERE 
               (Season, ConfAbbrev, total_wins, total_losses) IN  
               (SELECT Season, ConfAbbrev, MAX(total_wins) AS total_wins, 
               MIN(total_losses) AS total_losses FROM team_games
               GROUP BY Season, ConfAbbrev)") %>%
  mutate(ConfAbbrev = as.factor(ConfAbbrev))

kaggle1 <- kaggle1 %>% 
  left_join(Conf_Champs, by = c("Season","Team1_Conf" = "ConfAbbrev","Team1" = "Team")) %>%
  left_join(Conf_Champs, by = c("Season","Team2_Conf" = "ConfAbbrev","Team2" = "Team")) %>%
  rename(Team1_ConfChamp = "Conf_Champ.x", Team2_ConfChamp = "Conf_Champ.y",) %>%
  tidyr::replace_na(list(Team1_ConfChamp = 0, Team2_ConfChamp = 0)) %>%
  mutate(Team1_ConfChamp = as.factor(Team1_ConfChamp),
         Team2_ConfChamp = as.factor(Team2_ConfChamp))

# Penalize for being double digit seed
kaggle1$Team1_Penalize = as.factor(ifelse(kaggle1$Seed_Team1 >= 10, 1, 0))
kaggle1$Team2_Penalize = as.factor(ifelse(kaggle1$Seed_Team2 >= 10, 1, 0))

# Take into account for number of tournament games for each team
team1 <- Tourney_Compact %>% 
  group_by(Team1, Season) %>%
  summarise(total_games_team1 = n(), .groups = "drop")  %>%
  rename("Team" = Team1)
team2 <- Tourney_Compact %>% 
  group_by(Team2, Season) %>%
  summarise(total_games_team2 = n(), .groups = "drop")  %>%
  rename("Team" = Team2)

team_games <- team1 %>% full_join(team2, by = c("Season","Team")) %>% 
  mutate(total_games_team1 = tidyr::replace_na(total_games_team1, 0),
         total_games_team2 = tidyr::replace_na(total_games_team2, 0),
         num_games = total_games_team1 + total_games_team2) %>% 
  dplyr::select(Season, Team, num_games) %>% 
  arrange(Team, Season) %>%
  mutate(num_games_since_1985 = cumsum(num_games)) %>%
  dplyr::select(Season, Team, num_games_since_1985)
team_games <- team_games %>%
  arrange(Team, Season) %>%
  group_by(Team) %>%
  mutate(num_games_since_1985 = dplyr::lag(num_games_since_1985, n = 1, default = 0))

kaggle1 <- kaggle1 %>%
  left_join(team_games[, c("Team","Season","num_games_since_1985")], 
             by = c("Team1" = "Team","Season")) %>%
  left_join(team_games[, c("Team","Season","num_games_since_1985")], 
             by = c("Team2" = "Team","Season")) %>%
  rename("Team1_Num_Games" = num_games_since_1985.x, 
         "Team2_Num_Games" = num_games_since_1985.y)

# Pull in reg. season win/loss, win%, and team rankings
kaggle1 <- kaggle1 %>% 
  inner_join(Standings, by = c("Season","Team1" = "Team_Id")) %>%
  inner_join(Standings, by = c("Season","Team2" = "Team_Id")) %>%
  rename('Team1_Wins' = Wins.x,'Team1_Losses' = Losses.x,'Team1_Rank' = Rank.x,
         'Team1_Mean_Rank' = Mean_Rank.x,'Team1_Win_Perc' = Win_Perc.x,
         'Team2_Wins' = Wins.y,'Team2_Losses' = Losses.y,'Team2_Rank' = Rank.y,
         'Team2_Mean_Rank' = Mean_Rank.y,'Team2_Win_Perc' = Win_Perc.y,
         'Team1_PreSeason_Top25' = PreSeason_Top25.x, 'Team2_PreSeason_Top25' = PreSeason_Top25.y)

# Pull in adjusted win %
kaggle1 <- kaggle1 %>%
  inner_join(Adjusted_Standings, by = c("Season","Team1" = "Team")) %>%
  inner_join(Adjusted_Standings, by = c("Season","Team2" = "Team")) %>%
  rename('Team1_Adj_Wins' = Adj_Wins.x,'Team1_Adj_Losses' = Adj_Losses.x,
         'Team1_Adj_Win_Perc' = Adj_Win_Perc.x, 
         'Team2_Adj_Wins' = Adj_Wins.y,'Team2_Adj_Losses' = Adj_Losses.y,
         'Team2_Adj_Win_Perc' = Adj_Win_Perc.y)

# Create SOS (strength of schedule)
kaggle1 <- kaggle1 %>% 
  inner_join(SOS, by = c("Season","Team1" = "Team")) %>%
  inner_join(SOS, by = c("Season","Team2" = "Team")) %>%
  rename('Team1_SOS' = SOS.x, 'Team2_SOS' = SOS.y)

# Top 50 Wins
kaggle1 <- kaggle1 %>% 
  left_join(Top_50_Wins, by = c("Season","Team1" = "Team")) %>%
  left_join(Top_50_Wins, by = c("Season","Team2" = "Team")) %>%
  rename('Team1_Top_50_Wins' = Top_50_Wins.x, 'Team2_Top_50_Wins' = Top_50_Wins.y) %>%
  tidyr::replace_na(list(Team1_Top_50_Wins = 0, Team2_Top_50_Wins = 0))

# Create Number of Bad Losses where a team lost to a lower ranked team
kaggle1 <- kaggle1 %>% 
  left_join(Bad_Losses, by = c("Season","Team1" = "Team")) %>%
  left_join(Bad_Losses, by = c("Season","Team2" = "Team")) %>%
  rename('Team1_Bad_Losses' = Bad_Losses.x, 'Team2_Bad_Losses' = Bad_Losses.y) %>%
  tidyr::replace_na(list(Team1_Bad_Losses = 0, Team2_Bad_Losses = 0))

# Power Conference (0/1) dummy variable for whether team is from power conference
kaggle1 <- kaggle1 %>%
  mutate(Team1_Power = case_when(Team1_Conf %in% c('big_twelve','big_ten','acc','sec','pac_twelve','big_east','pac_ten') ~ 1,
                                 TRUE ~ 0),
         Team2_Power = case_when(Team2_Conf %in% c('big_twelve','big_ten','acc','sec','pac_twelve','big_east','pac_ten') ~ 1,
                                 TRUE ~ 0),
         Team1_Power = as.factor(Team1_Power), Team2_Power = as.factor(Team2_Power))

# Set NA's equal to 0 & order data set by season, round, regions, and matchup like a normal bracket
kaggle1$Team1_Num_Games <- ifelse(is.na(kaggle1$Team1_Num_Games) == T, 0, kaggle1$Team1_Num_Games)
kaggle1$Team2_Num_Games <- ifelse(is.na(kaggle1$Team2_Num_Games) == T, 0, kaggle1$Team2_Num_Games)
kaggle1 <- kaggle1[order(kaggle1$Season, kaggle1$Round, kaggle1$Team1_Region_Name, kaggle1$Team2_Region_Name, 
                         match(kaggle1$Seed_Team1, matchups)),]
slot_order <- c("Play_In","R1W1","R1W8","R1W5","R1W4","R1W6","R1W3","R1W7","R1W2",
               "R1X1","R1X8","R1X5","R1X4","R1X6","R1X3","R1X7","R1X2",
               "R1Y1","R1Y8","R1Y5","R1Y4","R1Y6","R1Y3","R1Y7","R1Y2",
               "R1Z1","R1Z8","R1Z5","R1Z4","R1Z6","R1Z3","R1Z7","R1Z2",
               "R2W1","R2W4","R2W3","R2W2","R2X1","R2X4","R2X3","R2X2",
               "R2Y1","R2Y4","R2Y3","R2Y2","R2Z1","R2Z4","R2Z3","R2Z2",
               "R3W1","R3W2","R3X1","R3X2","R3Y1","R3Y2","R3Z1","R3Z2",
               "R4W1","R4X1","R4Y1","R4Z1","R5WX","R5YZ","R6CH")
kaggle1 <- kaggle1[order(kaggle1$Season, match(kaggle1$slot,slot_order)),]
kaggle1$Team1_Name <- as.character(kaggle1$Team1_Name)
kaggle1$Team2_Name <- as.character(kaggle1$Team2_Name)

# Create differences of all averaged stats between both teams (team1 - team2)
for (i in (seq(5, 39, 1)))
{
  kaggle1[,130 + i] <- kaggle1[i] - kaggle1[i+35]
  colnames(kaggle1)[130 + i] = paste0(gsub("Team.*$", "", colnames(kaggle1)[130 + i]),"diff")
}

kaggle1 <- kaggle1 %>%
  mutate(dist_diff = Team1_Dist - Team2_Dist,
         rank_diff = Team1_Mean_Rank - Team2_Mean_Rank,
         sos_diff = Team1_SOS - Team2_SOS,
         top_50_diff = Team1_Top_50_Wins - Team2_Top_50_Wins,
         win_perc_diff = Team1_Win_Perc - Team2_Win_Perc,
         adj_win_perc_diff = Team1_Adj_Win_Perc - Team2_Adj_Win_Perc,
         adj_win_diff = Team1_Adj_Wins - Team2_Adj_Wins,
         win_diff = Team1_Wins - Team2_Wins,
         bad_losses_diff = Team1_Bad_Losses - Team2_Bad_Losses,
         team_power_diff = as.numeric(as.character(Team1_Power)) - as.numeric(as.character(Team2_Power)),
         preseason_top_25_diff = as.numeric(as.character(Team1_PreSeason_Top25)) - 
           as.numeric(as.character(Team1_PreSeason_Top25)))
##############################################################################################
####################################################################################
# Visualization, Correlations
correlation_plot(data = train1, x_var = "Seed_Diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "rank_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "net_eff_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "margin_adj_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "top_50_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "margin_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "tie_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "pie_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "sos_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.2, binary = T)
correlation_plot(data = train1, x_var = "win_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "off_rating_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "adj_win_perc_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "def_rating_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "Score_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "blocks_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "blk_perc_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "reb_perc_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "offreb_perc_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "dist_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "ast_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "four_factor_diff", y_var = "Team1_Victory", 
                 x_coord = 3, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "turnovers_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "pfouls_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "ast_ratio_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "efg_perc_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "score_opp_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "true_shooting_perc_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "bad_losses_diff", y_var = "Team1_Victory", 
                 x_coord = -4, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "shoot_eff_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "offreb_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "defreb_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "Team1_Dist", y_var = "Team1_Victory", 
                 x_coord = 1000, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "Team2_Dist", y_var = "Team1_Victory", 
                 x_coord = 1000, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "steals_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "three_pt_perc_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "ft_perc_diff", y_var = "Team1_Victory", 
                 x_coord = 5, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "defreb_perc_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "fgm_three_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.5, binary = T)
correlation_plot(data = train1, x_var = "num_poss_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.3, binary = T)
correlation_plot(data = train1, x_var = "fga_three_diff", y_var = "Team1_Victory", 
                 x_coord = 0, y_coord = 0.5, binary = T)
########################################################################################
# Set variable names used in modeling into a variable

vars <- c("sos_diff","pie_diff","bad_losses_diff","pfouls_diff","fga_three_diff",
          "dist_diff","ft_perc_diff","top_50_diff","adj_win_diff","four_factor_diff",
          "blocks_diff","turnovers_diff","Score_diff","Team1_Power","Team2_Power",
          "reb_perc_diff","ast_ratio_diff","steals_diff","offreb_perc_diff",
          "rank_diff","Seed_Diff","Team1_ConfChamp","Team2_ConfChamp")

# Modeling
# Fix for Round 0
# Input remaining missing values with column mean
train1$DayNum <- ifelse(train1$Round == 0, 134, train1$DayNum)
train1 <- train1[order(train1$Season, train1$Round, train1$Team1_Region_Name, train1$Team2_Region_Name, 
                       match(train1$Seed_Team1, matchups)),]
train1 <- tidyr::replace_na(train1, 
          as.list(colMeans(train1[,c("ft_perc_Team1_Avg","ft_perc_Team2_Avg","ft_perc_diff")],na.rm=T)))
kaggle1 <- tidyr::replace_na(kaggle1, 
           as.list(colMeans(kaggle1[,c("ft_perc_Team1_Avg","ft_perc_Team2_Avg","ft_perc_diff")],na.rm=T)))
#########################################################################################
# Identify all columns for team1 and for team2 to use in bracket simulation
# Team1 colnames
diff_cols1 = colnames(train1)[endsWith(x = colnames(train1), suffix = "Diff")]
diff_cols2 = colnames(train1)[endsWith(x = colnames(train1), suffix = "diff")]
team1_cols_1 = colnames(train1)[endsWith(x = colnames(train1), suffix = "Team1_Avg")]
team1_cols_2 = colnames(train1)[startsWith(x = colnames(train1), prefix = "Team1_")]
team1_cols = c("Season","slot","Round","Host_City","Host_Lat","Host_Lng",
               "Team1","Team2","Team1_Name","Team2_Name","Team1_Dist","Team2_Dist",
               "Seed_Team1", team1_cols_1,team1_cols_2,diff_cols1,diff_cols2,"Team1_PreSeason_Top25")
# Remove Team1_Victory
team1_cols = team1_cols[team1_cols != 'Team1_Victory']

# Team2 colnames
team2_cols_1 = colnames(train1)[endsWith(x = colnames(train1), suffix = "Team2_Avg")]
team2_cols_2 = colnames(train1)[startsWith(x = colnames(train1), prefix = "Team2_")]
team2_cols = c("Season","slot","Round","Host_City","Host_Lat","Host_Lng",
               "Team1","Team2","Team1_Name","Team2_Name","Team1_Dist","Team2_Dist",
               "Seed_Team2", team2_cols_1,team2_cols_2,diff_cols1,diff_cols2,"Team2_PreSeason_Top25")
#############################################################################
# Penalized Logit (Lasso, Ridge, Elastic Net)
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

# Use new_train1 instead of train1 for standardized versionn
training_data <- train1[train1$Season %in% c(seq(2003,2016)),]
training_response <- training_data[, c("Team1_Victory","Season")]
training_continuous <- training_data[, vars]

# Use new_train1 instead of train1 for standardized version
testing_data <- train1[(train1$Season %in% c(2017,2018,2019)) & train1$Round > 0,]
testing_response <- testing_data[, c("Team1_Victory","Season")]
testing_continuous <- testing_data[, vars]
logit <- run_penalized_logit(vars, alpha = 0.1, min = T, k_fold = 3) # alpha = 0.5, min = T

answer <- logit[[2]]
hist(answer$Pred_Prob)
mean(answer$Pred_Prob)
cv.out <- logit[[1]]
lambda_min <- cv.out$lambda.min
lambda_1se <- cv.out$lambda.1se

# Look at games for which I predicted incorrectly
wrong <- testing_data[which(answer$True != answer$Pred_Outcome),c("Season","Team1_Name","Team2_Name","Seed_Team1","Seed_Team2","Round","Team1_Victory")]
wrong$upset <- ifelse(wrong$Team1_Victory == 1 & wrong$Seed_Team1 > wrong$Seed_Team2, 1, 
                      ifelse(wrong$Team1_Victory == 0 & wrong$Seed_Team1 < wrong$Seed_Team2, 1, 0))
d <- answer[answer$True != answer$Pred_Outcome,]
cbind(wrong, d$Pred_Outcome, d$Pred_Prob)
dim(wrong)[1]

# store and exponentiate out betas to look coefficient estimates in terms of odds
results <- tidy_coef(logit[[1]], min = T, glmnet = F)
results <- results[results[,2] != 0,]
variables <- results[,order("estimate")]
betas <- sort(results[,2], decreasing = T)
model <- as.data.frame(cbind(betas, variables))
model[,"betas"] <- as.numeric(as.character(model[,"betas"]))
model[,"variables"] <- as.character(model[, "variables"])
head(model, 50); tail(model,15)
model; exp(model$betas)

# run bracket simulation
test_1se <- Bracket_Sim_Penalized(2019, 200, lambda = lambda_1se)
bracket_1se <- Normalize_Sim(test_1se, 200)
colnames(bracket_1se)[1:4] = c("Season","Region","Seed","Team"); bracket_1se
kable(bracket_1se, row.names = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, position = "left", fixed_thead = T) %>%
  footnote(symbol = "Based on 500 Tournament Simulations") %>%
  scroll_box(width = "100%", height = "520px") %>%
  save_kable(file = paste0("2021_GLMNET_Conservative_Bracket_Simulation.html"))

test_min <- Bracket_Sim_Penalized(2021, 1000, lambda = lambda_min)
bracket_min <- Normalize_Sim(test_min, 1000)
colnames(bracket_min)[1:4] = c("Season","Region","Seed","Team"); bracket_min
kable(bracket_min, row.names = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, position = "left", fixed_thead = T) %>%
  footnote(symbol = "Based on 500 Tournament Simulations") %>%
  scroll_box(width = "100%", height = "520px") %>%
  save_kable(file = paste0("2021_GLMNET_Aggressive_Bracket_Simulation.html"))

# Make Kaggle Predictions, trained with unstandardized data only
setwd("C:/Users/rusla/OneDrive/MarchMadness/March-Madness-Predictions/Preds/Preds21")
kaggle_logit_min <- kaggle_predictions(logit, 'logit', 2019, 2021, vars, min = T, names = T)
kaggle_logit_1se <- kaggle_predictions(logit, 'logit', 2021, 2021, vars, min = F, names = T)
kaggle_logit_1se %>% filter(Team1_Name %in% c('Michigan St','BYU'),
                          Team2_Name %in% c('Michigan St','BYU')
) 
kaggle_logit_min %>% filter(Team1_Name %in% c('Gonzaga','Iowa'),
                          Team2_Name %in% c('Iowa','Gonzaga')
) 
kaggle_logit_min %>% filter(Team1_Name %in% c('Gonzaga','Alabama'),
                          Team2_Name %in% c('Alabama','Gonzaga')
) 
kaggle_logit_1se %>% filter(Team1_Name %in% c('Drake','USC'),
                          Team2_Name %in% c('USC','Drake')
) 
kaggle_logit_min %>% filter(Team1_Name %in% c('Houston','Baylor'),
                          Team2_Name %in% c('Baylor','Houston')
) 
kaggle_logit_min %>% filter(Team1_Name %in% c('Alabama','Houston'),
                          Team2_Name %in% c('Houston','Alabama')
) 
kaggle_logit_min %>% filter(Team1_Name %in% c('Texas Tech','Utah St'),
                          Team2_Name %in% c('Utah St','Texas Tech')
) 
kaggle_logit_1se %>% filter(Team1_Name %in% c('Auburn','Kansas'),
                     Team2_Name %in% c('Auburn','Kansas')
)
################################################################
# GLMNET, carets version of lasso/ridge/elastic net
vars2 <- c(vars, "Team1_Victory")
training_data <- train1[train1$Season %in% c(seq(2003,2016)),] 
training_response <- training_data[, c("Team1_Victory","Season")]
training_continuous <- training_data[, vars2]

testing_data <- train1[train1$Season %in% c(2017,2018,2019) & train1$Round > 0,] 
testing_response <- testing_data[, c("Team1_Victory","Season")]
testing_continuous <- testing_data[, vars2]

glmnetGrid <- expand.grid(alpha = seq(0, 1, 0.1),
  lambda = seq(0.0001, 1, length = 100))

glmnet <- run_glmnet(vars, glmnetGrid, k_fold = 3)

answer <- glmnet[[2]]
hist(answer$Pred_Prob)
mean(answer$Pred_Prob)

# Look at games for which I predicted incorrectly
wrong <- testing_data[which(answer$True != answer$Pred_Outcome),c("Season","Team1_Name","Team2_Name","Seed_Team1","Seed_Team2","Round","Team1_Victory")]
wrong$upset <- ifelse(wrong$Team1_Victory == 1 & wrong$Seed_Team1 > wrong$Seed_Team2, 1, 
                      ifelse(wrong$Team1_Victory == 0 & wrong$Seed_Team1 < wrong$Seed_Team2, 1, 0))
d <- answer[answer$True != answer$Pred_Outcome,]
cbind(wrong, d$Pred_Outcome, d$Pred_Prob)
dim(wrong)[1]

results <- tidy_coef(glmnet[[1]], min = F, glmnet = T)
results <- results[results[,2] != 0,]
variables <- results[,order("estimate")]

betas <- sort(results[,2], decreasing = T)
model <- as.data.frame(cbind(betas, variables))
model[,"betas"] <- as.numeric(as.character(model[,"betas"]))
model[,"variables"] <- as.character(model[, "variables"])
head(model, 50); tail(model,15)
model; exp(model$betas)

GLMNET <- glmnet[[1]]
test2 <- Bracket_Sim_GLMNET(2021, 1000)
bracket <- Normalize_Sim(test2, 1000)
colnames(bracket)[1:4] = c("Season","Region","Seed","Team"); bracket
kable(bracket, row.names = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, position = "left", fixed_thead = T) %>%
  footnote(symbol = "Based on 1000 Tournament Simulations") %>%
  scroll_box(width = "100%", height = "520px") %>%
  save_kable(file = paste0("2021_GLMNET_Normal_Bracket_Simulation.html"))

# Make Kaggle Predictions, trained with unstandardized data only
kaggle_glmnet <- kaggle_predictions(GLMNET, 'glmnet', 2021, 2021, vars, names = T); kaggle_glmnet
kaggle_glmnet %>% filter(Team1_Name %in% c('West Virginia','Morehead St'),
                     Team2_Name %in% c('Morehead St','West Virginia')
) 
kaggle_glmnet %>% filter(Team1_Name %in% c('Villanova','Winthrop'),
                     Team2_Name %in% c('Winthrop','Villanova')
) 
kaggle_glmnet %>% filter(Team1_Name %in% c('Florida','Virginia Tech'),
                     Team2_Name %in% c('Virginia Tech','Florida')
) 
kaggle_glmnet %>% filter(Team1_Name %in% c('VCU','Oregon '),
                     Team2_Name %in% c('Oregon','VCU')
) 
kaggle_glmnet %>% filter(Team1_Name %in% c('Oregon St','Tennessee'),
                     Team2_Name %in% c('Tennessee','Oregon St')
) 
kaggle_glmnet %>% filter(Team1_Name %in% c('Liberty','Oklahoma St'),
                     Team2_Name %in% c('Oklahoma St','Liberty')
) 
kaggle_glmnet %>% filter(Team1_Name %in% c('Texas Tech','Michigan'),
                     Team2_Name %in% c('Texas Tech','Michigan')
) 
kaggle_glmnet %>% filter(Team1_Name %in% c('Auburn','Kansas'),
                     Team2_Name %in% c('Auburn','Kansas')
)
##################################################################################################
# Random Forest
vars2 <- c(vars, "Team1_Victory")
training_data <- train1[train1$Season %in% c(seq(2003,2019)),] 
training_response <- training_data[, c("Team1_Victory","Season")]
training_continuous <- training_data[, vars2]

testing_data <- train1[train1$Season %in% c(2019) & train1$Round > 0,] 
testing_response <- testing_data[, c("Team1_Victory","Season")]
testing_continuous <- testing_data[, vars2]

rf <- run_random_forest(vars, 500, k_fold = 3)

answer <- rf[[2]]
hist(answer$Pred_Prob)
mean(answer$Pred_Prob)

# Look at games for which I predicted incorrectly
wrong <- testing_data[which(answer$True != answer$Pred_Outcome),c("Season","Team1_Name","Team2_Name","Seed_Team1","Seed_Team2","Round","Team1_Victory")]
wrong$upset <- ifelse(wrong$Team1_Victory == 1 & wrong$Seed_Team1 > wrong$Seed_Team2, 1, 
                      ifelse(wrong$Team1_Victory == 0 & wrong$Seed_Team1 < wrong$Seed_Team2, 1, 0))
d <- answer[answer$True != answer$Pred_Outcome,]
cbind(wrong, d$Pred_Outcome, d$Pred_Prob)
dim(wrong)[1]

rF <- rf[[1]]
test2 <- Bracket_Sim_RF(2021, 1000)
bracket <- Normalize_Sim(test2, 1000)
colnames(bracket)[1:4] = c("Season","Region","Seed","Team"); bracket
kable(bracket, row.names = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, position = "left", fixed_thead = T) %>%
  footnote(symbol = "Based on 1000 Tournament Simulations") %>%
  scroll_box(width = "100%", height = "520px") %>%
  save_kable(file = paste0("2021_RF_Bracket_Simulation.html"))

# Make Kaggle Predictions, trained with unstandardized data only
kaggle_rf <- kaggle_predictions(rf, 'rf', 2021, 2021, vars, names = T); kaggle_rf
kaggle_rf %>% filter(Team1_Name %in% c('UC Irvine','Kansas St'),
                          Team2_Name %in% c('UC Irvine','Kansas St')
) 
kaggle_rf %>% filter(Team1_Name %in% c('Wisconsin','Oregon'),
                          Team2_Name %in% c('Wisconsin','Oregon')
) 
kaggle_rf %>% filter(Team1_Name %in% c('Marquette','Murray St'),
                          Team2_Name %in% c('Marquette','Murray St')
) 
kaggle_rf %>% filter(Team1_Name %in% c('VCU','UCF'),
                          Team2_Name %in% c('VCU','UCF')
) 
kaggle_rf %>% filter(Team1_Name %in% c('Texas Tech','Virginia'),
                          Team2_Name %in% c('Texas Tech','Virginia')
) 
kaggle_rf %>% filter(Team1_Name %in% c('Texas Tech','Gonzaga'),
                          Team2_Name %in% c('Texas Tech','Gonzaga')
) 
kaggle_rf %>% filter(Team1_Name %in% c('Texas Tech','Michigan'),
                          Team2_Name %in% c('Texas Tech','Michigan')
) 
kaggle_rf %>% filter(Team1_Name %in% c('Auburn','Kansas'),
                     Team2_Name %in% c('Auburn','Kansas')
)
##################################################################################################
# Gradient Boosted Model
vars2 <- c(vars, "Team1_Victory")
training_data <- train1[train1$Season %in% c(seq(2013,2016)),]
training_response <- training_data[, c("Team1_Victory","Season")]
training_continuous <- training_data[, vars2]

testing_data <- train1[train1$Season %in% c(2017,2018,2019) & train1$Round > 0,]
testing_response <- testing_data[, c("Team1_Victory","Season")]
testing_continuous <- testing_data[, vars2]

gbmGrid <- expand.grid(n.trees=c(10,20,50,100,500,1000),
                       shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10,20),
                       interaction.depth=c(1,3,5))

GBM <- run_gbm(vars, gbmGrid, k_fold = 3)
answer <- GBM[[2]]
hist(answer$Pred_Prob)
mean(answer$Pred_Prob)

# Look at games for which I predicted incorrectly
wrong <- testing_data[which(answer$True != answer$Pred_Outcome),c("Season","Team1_Name","Team2_Name","Seed_Team1","Seed_Team2","Round","Team1_Victory")]
wrong$upset <- ifelse(wrong$Team1_Victory == 1 & wrong$Seed_Team1 > wrong$Seed_Team2, 1, 
                      ifelse(wrong$Team1_Victory == 0 & wrong$Seed_Team1 < wrong$Seed_Team2, 1, 0))
d <- answer[answer$True != answer$Pred_Outcome,]
cbind(wrong, d$Pred_Outcome, d$Pred_Prob)
dim(wrong)[1]

gbm <- GBM[[1]]
test3 <- Bracket_Sim_GBM(2021, 500)
bracket <- Normalize_Sim(test3, 500)
colnames(bracket)[1:4] = c("Season","Region","Seed","Team"); bracket
kable(bracket, row.names = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, position = "left", fixed_thead = T) %>%
  footnote(symbol = "Based on 500 Tournament Simulations") %>%
  scroll_box(width = "100%", height = "520px") %>%
  save_kable(file = paste0("2021_GBM_Bracket_Simulation.html"))

# Make Kaggle Predictions, trained with unstandardized data only
kaggle_gbm <- kaggle_predictions(gbm, 'gbm', 2021, 2021, vars, names = T); kaggle_gbm
kaggle_gbm %>% filter(Team1_Name %in% c('UC Irvine','Kansas St'),
                          Team2_Name %in% c('UC Irvine','Kansas St')
) 
kaggle_gbm %>% filter(Team1_Name %in% c('Wisconsin','Oregon'),
                          Team2_Name %in% c('Wisconsin','Oregon')
) 
kaggle_gbm %>% filter(Team1_Name %in% c('Marquette','Murray St'),
                          Team2_Name %in% c('Marquette','Murray St')
) 
kaggle_gbm %>% filter(Team1_Name %in% c('VCU','UCF'),
                          Team2_Name %in% c('VCU','UCF')
) 
kaggle_gbm %>% filter(Team1_Name %in% c('Texas Tech','Virginia'),
                          Team2_Name %in% c('Texas Tech','Virginia')
) 
kaggle_gbm %>% filter(Team1_Name %in% c('Texas Tech','Gonzaga'),
                          Team2_Name %in% c('Texas Tech','Gonzaga')
) 
kaggle_gbm %>% filter(Team1_Name %in% c('Texas Tech','Michigan'),
                          Team2_Name %in% c('Texas Tech','Michigan')
) 
kaggle_gbm %>% filter(Team1_Name %in% c('Auburn','Kansas'),
                      Team2_Name %in% c('Auburn','Kansas')
) 
#####################################################################################
# XGBoost
vars2 <- c(vars, "Team1_Victory")
training_data <- train1[train1$Season %in% c(seq(2003,2016)),]
training_response <- training_data[, c("Team1_Victory","Season")]
training_continuous <- training_data[, vars2]

testing_data <- train1[train1$Season %in% c(2017,2018,2019) & train1$Round > 0,]
testing_response <- testing_data[, c("Team1_Victory","Season")]
testing_continuous <- testing_data[, vars2]

parametersGrid <- expand.grid(eta = c(0.02), colsample_bytree=c(0.7), max_depth=c(3), gamma = c(1,10),
                              nrounds=c(50,100,150,200), min_child_weight=c(10,20,40), subsample = c(0.35,0.5))

XGBoost <- run_xgboost(vars, parametersGrid, k_fold = 3)
answer <- XGBoost[[2]]
hist(answer$Pred_Prob)
mean(answer$Pred_Prob)

# Look at games for which I predicted incorrectly
wrong <- testing_data[which(answer$True != answer$Pred_Outcome),c("Season","Team1_Name","Team2_Name","Seed_Team1","Seed_Team2","Round","Team1_Victory")]
wrong$upset <- ifelse(wrong$Team1_Victory == 1 & wrong$Seed_Team1 > wrong$Seed_Team2, 1, 
                      ifelse(wrong$Team1_Victory == 0 & wrong$Seed_Team1 < wrong$Seed_Team2, 1, 0))
d <- answer[answer$True != answer$Pred_Outcome,]
cbind(wrong, d$Pred_Outcome, d$Pred_Prob)
dim(wrong)[1]

xgboost <- XGBoost[[1]]
test4 <- Bracket_Sim_XGBoost(2021, 1000)
bracket <- Normalize_Sim(test4, 1000)
colnames(bracket)[1:4] = c("Season","Region","Seed","Team"); bracket
kable(bracket, row.names = F) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, position = "left", fixed_thead = T) %>%
  footnote(symbol = "Based on 1000 Tournament Simulations") %>%
  scroll_box(width = "100%", height = "520px") %>%
  save_kable(file = paste0("2021_XGBOOST_Bracket_Simulation.html"))

# Make Kaggle Predictions, trained with unstandardized data only
kaggle_xgboost <- kaggle_predictions(xgboost, 'xgboost', 2021, 2021, vars, min = F, names = T); 
kaggle_xgboost %>% filter(Team1_Name %in% c('Gonzaga','Michigan'),
                          Team2_Name %in% c('Michigan','Gonzaga')
                          ) 
kaggle_xgboost %>% filter(Team1_Name %in% c('Michigan','Alabama'),
                          Team2_Name %in% c('Michigan','Alabama')
) 
kaggle_xgboost %>% filter(Team1_Name %in% c('Alabama','Alabama'),
                          Team2_Name %in% c('Gonzaga','Gonzaga')
) 
kaggle_xgboost %>% filter(Team1_Name %in% c('VCU','UCF'),
                          Team2_Name %in% c('VCU','UCF')
) 
kaggle_xgboost %>% filter(Team1_Name %in% c('Texas Tech','Virginia'),
                          Team2_Name %in% c('Texas Tech','Virginia')
) 
kaggle_xgboost %>% filter(Team1_Name %in% c('Texas Tech','Gonzaga'),
                          Team2_Name %in% c('Texas Tech','Gonzaga')
) 
kaggle_xgboost %>% filter(Team1_Name %in% c('Texas Tech','Michigan'),
                          Team2_Name %in% c('Texas Tech','Michigan')
) 
kaggle_xgboost %>% filter(Team1_Name %in% c('Auburn','Kansas'),
                      Team2_Name %in% c('Auburn','Kansas')
)
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
kaggle_nn %>% filter(Team1_Name %in% c('Wisconsin','Oregon'),
                          Team2_Name %in% c('Wisconsin','Oregon')
) 
kaggle_nn %>% filter(Team1_Name %in% c('Marquette','Murray St'),
                          Team2_Name %in% c('Marquette','Murray St')
) 
kaggle_nn %>% filter(Team1_Name %in% c('VCU','UCF'),
                          Team2_Name %in% c('VCU','UCF')
) 
kaggle_nn %>% filter(Team1_Name %in% c('Texas Tech','Virginia'),
                          Team2_Name %in% c('Texas Tech','Virginia')
) 
kaggle_nn %>% filter(Team1_Name %in% c('Texas Tech','Gonzaga'),
                          Team2_Name %in% c('Texas Tech','Gonzaga')
) 
kaggle_nn %>% filter(Team1_Name %in% c('Texas Tech','Michigan'),
                          Team2_Name %in% c('Texas Tech','Michigan')
) 
kaggle_nn %>% filter(Team1_Name %in% c('Auburn','Kansas'),
                          Team2_Name %in% c('Auburn','Kansas')
)
###########################################################################
# compare all caret models 
model_list <- list(glmnet = GLMNET, rf = rF, gbm = gbm, xgboost = xgboost)
resamples <- resamples(model_list)
summary(resamples)
dotplot(resamples, conf.level = 0.75, main = '3-Fold CV Error Caret Model Comparison')

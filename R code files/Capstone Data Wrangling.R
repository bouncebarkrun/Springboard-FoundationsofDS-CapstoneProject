# Import data packages
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")
install.packages("countrycode")

#Load data packages
library("dplyr")
library("readr")
library("tidyr")
library("countrycode")

#Import data sets
Master <- read_csv("~/Desktop/Master.csv")
Scoring <- read_csv("~/Desktop/Scoring.csv")
Draft <- read_csv("~/Desktop/DraftData.csv")
Teams <- read_csv("~/Desktop/Teams.csv")
Goalies <- read_csv("~/Desktop/Goalies.csv")

#Convert datasets to a tbl class for easier viewing
tbl_df(Master)
tbl_df(Scoring)
tbl_df(Draft)
tbl_df(Teams)
tbl_df(Goalies)

#Start cleaning up column names to be more easily understood
colnames(Master) <- c("Player_ID", "Coach_ID", "HallFame_ID", "First_Name", "Last_Name", "Name_Notes", "Given_Name", "Nickname", "Height", "Weight", "Shooting_Hand", "legendsID", "ihdbID", "hrefID", "FirstNHLseason", "LastNHLseason", "FirstWHAseason", "LastWHAseason", "Position_Played", "Birth_Year", "Birth_Month", "Birth_Day", "Birth_Country", "Birth_State", "Birth_City", "Death_Year", "Death_Month", "Death_Day", "Death_Country", "Death_State", "Death_City")
colnames(Scoring) <- c("Player_ID", "Year", "Stint", "Team_ID", "League_ID", "Position", "Games_Played", "Goals", "Assists", "Points", "Penalty_Minutes", "Plus_Minus", "PP_Goals", "PP_Assists", "SH_Goals", "SH_Assists", "Gamewinning_Goals", "GameTying_Goals", "Shots", "PS_Games", "PS_Goals", "PS_Assists", "PS_Points", "PS_PenaltyMin", "PS_PlusMinus", "PS_PowerplayG", "PS_PowerplayA", "PS_ShorthandedG", "PS_ShorthandedA", "PS_GamewinningG", "PS_Shots")
colnames(Draft) <- c("Draft_Pick", "Draft_Year", "Draft_Team", "Player", "Draft_Age", "LastYearPlayed", "Amateur_Team")
colnames(Teams) <- c("Year", "League_ID", "Team_ID", "FranchiseID", "Conference_ID", "Division_ID", "SeasonEnd_Rank", "Playoff_Result", "Team_Total_Games", "Team_Wins", "Team_Losses", "Team_Ties", "Team_OT_Losses", "Team_Points", "Team_ShootoutWins", "Team_ShootoutLosses", "Team_GoalsFor", "Team_GoalsAgainst", "TeamName", "Team_PenaltyMin", "Team_BenchMinors", "Team_PPG", "Team_PPC", "Team_SHA", "Team_PKG", "Team_PKC", "Team_SHF")
colnames(Goalies) <-c("Player_ID", "Year","GoalieStint", "GoalieTeam_ID", "GoalieLeague_ID", "GoalieGames_Played", "GoalieMinutes", "GoalieWins","GoalieLosses", "GoalieTies_OTLoss", "GoalieEmptynetgoals", "GoalieShutouts", "GoalieGoals_Against", "GoalieShots_Against", "GoaliePostGP", "GoaliePostMin", "GoaliePostW",  "GoaliePostL", "GoaliePostT", "GoaliePostENG", "GoaliePostSHO", "GoaliePostGA", "GoaliePostSA")

#Separate player name and Player_ID from each other in Draft database. Then separate Amateur Team and Amateur League.
Draft <- separate(Draft, Player, into = c("Player", "Player_ID"), sep = "/")
Draft <- separate(Draft, Amateur_Team, into = c("AmateurTeam", "AmateurLeague"), sep = "[(]")
Draft$AmateurLeague <- gsub("[)]", "", Draft$AmateurLeague)

#Join datasets
MergedData <- merge(Master, Scoring, by = "Player_ID")
MergedData2 <- full_join(MergedData, Goalies, by =c("Player_ID", "Year"))
AllData <- merge(MergedData2, Draft, by = "Player_ID")

#In the player dataset, birth and death dates are divided into 3 columns each. It makes sense to combine them into one and set the data as a date.
AllData$BirthDate <- as.Date(with(AllData, paste(Birth_Year, Birth_Month, Birth_Day,sep="-")), "%Y-%m-%d")
AllData$DeathDate <- as.Date(with(AllData, paste(Death_Year, Death_Month, Death_Day, sep="-")), "%Y-%m-%d")

#Subset the dataset to the period of interest (the 2000-2011 seasons)
PlayerSubset <- subset(AllData, Year >= 2000)
TeamSubset <- subset(Teams, Year >= 2000)

#Remove some unnecessary columns
PlayerData <- select(PlayerSubset, -Birth_Year, -Birth_Month, -Birth_Day, -Death_Year, -Death_Month, -Death_Day, -Position, -GoalieStint, -Player, -ihdbID, -League_ID, -Coach_ID, -HallFame_ID, -Name_Notes, -Given_Name, -Nickname, -legendsID, -hrefID, -FirstWHAseason, -LastWHAseason, -LastYearPlayed, -PS_Goals, -PS_Assists, -PS_Points, -PS_Games, -PS_PenaltyMin, -PS_PlusMinus, -PS_PowerplayG, -PS_PowerplayA, -PS_ShorthandedG, -PS_ShorthandedA, -PS_GamewinningG, -PS_Shots, -Death_Country, -Death_State, -Death_City, -GoalieTeam_ID, -GoalieLeague_ID, -GoalieTies_OTLoss, -GoalieEmptynetgoals, -GoaliePostGP, -GoaliePostMin, -GoaliePostW, -GoaliePostL, -GoaliePostT, -GoaliePostENG, -GoaliePostSHO, -GoaliePostGA, -GoaliePostSA)
TeamData <- select(TeamSubset, -League_ID, -FranchiseID, -Team_PPC, -Team_SHA, -Team_PKG, -Team_PKC, -Team_SHF)

#Convert multiple fields to factors, integers and numeric
playercols <- c("Position_Played", "Team_ID", "Birth_Country", "Birth_City", "Birth_State", "Shooting_Hand", "Draft_Team", "AmateurTeam")
PlayerData[playercols] <- lapply(PlayerData[playercols], factor)
teamfactor <- c("Team_ID", "Conference_ID", "Division_ID", "Playoff_Result", "TeamName")
TeamData[teamfactor] <- lapply(TeamData[teamfactor], factor)
teamnum <- c("Team_OT_Losses", "Team_ShootoutWins", "Team_ShootoutLosses")
TeamData[teamnum] <- lapply(TeamData[teamnum], as.numeric)

#Look at new datasets
summary(PlayerData)
summary(TeamData)

#Assess NA values
is.na(PlayerData)
is.na(TeamData)

#NAs in certain fields are acceptable as they aren't applicable to some players: death statistics (many players haven't died as yet)
#Scoring statistics are NA where players or teams did not contribute to these stats during a season. These will be replaced with a 0.
NAcols <- c("Games_Played", "Goals", "Assists", "Points", "Penalty_Minutes", "Plus_Minus", "PP_Goals", "PP_Assists", "SH_Goals", "SH_Assists", "Gamewinning_Goals", "GameTying_Goals", "Shots")
PlayerData[NAcols][is.na(PlayerData[NAcols])] <- 0
NAteam <- c("Team_Ties", "Team_ShootoutWins", "Team_ShootoutLosses")
TeamData[NAteam][is.na(TeamData[NAteam])] <- 0

#Add a Playoff column to capture if teams made the playoffs with a simple 0 or 1
TeamData <- mutate(TeamData, Playoffs = as.numeric(TeamData$Playoff_Result != "NA"))
PlayoffRes <- c("Playoffs")
TeamData[PlayoffRes][is.na(TeamData[PlayoffRes])] <- 0

#Look at the final cleaned up datasets
summary(PlayerData)
summary(TeamData)

#Merge the cleaned datasets by year and team
FinalData <- merge(PlayerData, TeamData, by=c("Team_ID", "Year"))

##Create some new variables to help with data analysis.

#Use Draft Pick variable to approximate Draft Round (30 teams in the NHL during these years means ~30 picks per round).
FinalData$Draft_Round <- cut(FinalData$Draft_Pick, 
                        breaks = c(-Inf, 30, 60, 90, 120, 150, 180, 210, Inf), 
                        labels = c("1", "2", "3", "4", "5", "6", "7", "8"), 
                        right = TRUE)

#Add some variables that calculate player contribution to team totals.
FinalData <- mutate(FinalData, GoalsPerGame = Goals/Games_Played)
FinalData <- mutate(FinalData, PointsPerGame = Points/Games_Played)
FinalData <- mutate(FinalData, ShotsPerGame = Shots/Games_Played)
FinalData <- mutate(FinalData, PercentGoals = Goals/Team_GoalsFor * 100)
FinalData <- mutate(FinalData, PercentGames = Games_Played/Team_Total_Games * 100)
FinalData <- mutate(FinalData, YearsExperience = Year - FirstNHLseason)

#Add a birth region converted from birth country.
FinalData$BirthRegion <- countrycode(FinalData$Birth_Country, 'country.name', 'continent', warn = TRUE, custom_dict = NULL,
                                  custom_match = NULL, origin_regex = FALSE)

#Save the cleaned up datasets
write.csv(FinalData, file = "~/Desktop/FinalData_clean.csv")


# Import data packages
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")

#Load data packages
library("dplyr")
library("readr")
library("tidyr")

#Import data sets
Master <- read_csv("~/Desktop/Master.csv")
Scoring <- read_csv("~/Desktop/Scoring.csv")

#Convert datasets to a tbl class for easier manipulation
tbl_df(Master)
tbl_df(Scoring)

#Look at structure of files
glimpse(Master)
# Master consists of 7761 observations of 31 variables. As this dataset includes basic demographic information for every player in the NHL from 1909 to 2011, this makes sense.
glimpse(Scoring)
# Scoring consists of 45,967 observations of 31 variables. As this dataset captures year-by-year scoring for each NHL player in the dataset, it is understandable that there are more observations that the number of players who make up the Master dataset.

#Start cleaning up column names to be more easily understood
colnames(Master) <- c("Player_ID", "Coach_ID", "HallFame_ID", "First_Name", "Last_Name", "Name_Notes", "Given_Name", "Nickname", "Height", "Weight", "Shooting_Hand", "legendsID", "ihdbID", "hrefID", "FirstNHLseason", "LastNHLseason", "FirstWHAseason", "LastWHAseason", "Position_PLayed", "Birth_Year", "Birth_Month", "Birth_Day", "Birth_Country", "Birth_State", "Birth_City", "Death_Year", "Death_Month", "Death_Day", "Death_Country", "Death_State", "Death_City")
colnames(Scoring) <- c("Player_ID", "Season", "Stint", "Team_ID", "League_ID", "Position_Played", "Games_Played", "Goals", "Assists", "Points", "Penalty_Minutes", "Plus_Minus", "PP_Goals", "PP_Assists", "SH_Goals", "SH_Assists", "Gamewinning_Goals", "GameTying_Goals", "Shots", "PS_Games", "PS_Goals", "PS_Assists", "PS_Points", "PS_PenaltyMin", "PS_PlusMinus", "PS_PowerplayG", "PS_PowerplayA", "PS_ShorthandedG", "PS_ShorthandedA", "PS_GamewinningG", "PS_Shots")

#Join datasets
MergedData <- merge(Master, Scoring, by = "Player_ID")

#Filter the dataset to the period of interest (the 2000-2011 seasons)
PlayerData <- subset(MergedData, Season >= 2000)

#Start some cleanup
PlayerData$Position_Played <- as.factor(PlayerData$Position_Played)
PlayerData$Team_ID <- as.factor(PlayerData$Team_ID)
PlayerData$Birth_Country <- as.factor(PlayerData$Birth_Country)

#Look at new dataset
head(PlayerData)
str(PlayerData)
class(PlayerData) 
summary(PlayerData)

#Assess NA values
is.na(PlayerData)
#NAs in certain fields are acceptable as they aren't applicable to some players: Coach_ID, HallofFame_ID, Name_Notes, Given_Name, Nickname, legendsID, FirstWHAseason, LastWHAseason, death statistics (many players haven't died as yet)
#Need to investigate further NA values in FirstNHLseason, LastNHLseason, Birth_State.
#Scoring statistics are NA where players did not contribute to these stats during a season. Will replace them with 0.



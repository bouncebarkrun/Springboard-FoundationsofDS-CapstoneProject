## Load data packages that are needed
library(dplyr)
library(ggplot2)
library(readr)
library(countrycode)

## Import the data.  Fix an extra column that keeps importing from the CSV. Add a birth region converted from birth country.
Hockey <- read_csv("~/Desktop/FinalData_clean.csv")
Hockey <- select(Hockey, -X1)
Hockey$BirthRegion <- countrycode(Hockey$Birth_Country, 'country.name', 'continent', warn = TRUE, custom_dict = NULL,
                                  custom_match = NULL, origin_regex = FALSE)

## Create a subset that only includes the first instance of each player. This way analysis of certain attributes is not skewed by multiple seasons of play by a player.
ByPlayer <- Hockey[match(unique(Hockey$Player_ID), Hockey$Player_ID),]

## Look at the main dataset.
summary(Hockey)

##Look at the statistics for a subset of characteristics.
summary(ByPlayer[,c("Height", "Weight", "FirstNHLseason", "LastNHLseason", "Draft_Pick","Draft_Year", "Draft_Age", "Draft_Round", "Draft_Pick")])

Draftroundbycountry <- ByPlayer %>% group_by(Birth_Country) %>% summarize(mean=mean(Draft_Round))
Draftagebycountry <- ByPlayer %>% group_by(Birth_Country) %>% summarize(mean=mean(Draft_Age))

ggplot(ByPlayer) + geom_col(aes(x = BirthRegion, y = Draft_Pick, fill = BirthRegion))


ggplot(Hockey, aes(x = ShotsPerGame, y = Goals)) + geom_point()
ggplot(Hockey, aes(x = Penalty_Minutes, y = Goals)) + geom_point()

ggplot(ByPlayer, aes(x = Draft_Round)) + geom_bar()
ggplot(ByPlayer, aes(x = Draft_Pick)) + geom_histogram(binwidth = 1)

YearsPlayed = ByPlayer$LastNHLseason - ByPlayer$FirstNHLseason
ggplot(ByPlayer, aes(x = Draft_Round, y = YearsPlayed)) + geom_jitter() + geom_smooth(method = lm)

       
ggplot(Hockey, aes(x = Year, y=Team_GoalsFor, col = Playoffs)) + geom_point()
ggplot(Hockey, aes(x = Year, y=PercentGoals, col = Draft_Round)) + geom_jitter()
ggplot(Hockey, aes(x = Draft_Pick, y=GoalsPerGame, col=Playoffs)) + geom_point(alpha = 0.5, position = "jitter") + geom_smooth(method = lm)
ggplot(Hockey, aes(x = Draft_Pick, y=GoalsPerGame, col=BirthRegion)) + geom_point(alpha = 0.5) 

posn.jd <- position_jitterdodge(0.5, 0, 0.6)
ggplot(Hockey, aes(x = Draft_Age, y = Goals, color = Draft_Round)) + 
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + 
  facet_grid(. ~ Playoffs)
ggplot(Hockey, aes(x = Draft_Round, y = Goals, color = BirthRegion)) + 
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + 
  facet_grid(. ~ Playoffs)



boxplot(Draft_Round~BirthRegion,data=ByPlayer)

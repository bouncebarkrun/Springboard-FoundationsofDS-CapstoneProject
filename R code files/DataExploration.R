## Load data packages that are needed
library(dplyr)
library(ggplot2)
library(readr)
library(countrycode)

## Import the data.  Fix an extra column that keeps importing from the CSV. 
Hockey <- read_csv("~/Desktop/FinalData_clean.csv")
Hockey <- select(Hockey, -X1)


## Create a subset that only includes the first instance of each player. This way analysis of certain attributes is not skewed by multiple seasons of play by a player.
ByPlayer <- Hockey[match(unique(Hockey$Player_ID), Hockey$Player_ID),]

## Look at the main dataset.
summary(Hockey)

##Look at the statistics for a subset of player characteristics.
summary(ByPlayer[,c("Height", "Weight", "FirstNHLseason", "LastNHLseason", "Draft_Pick","Draft_Year", "Draft_Age", "Draft_Round", "Draft_Pick")])

##Create a table of mean Draft_Round and Draft_Age by Birth_Country
Draftroundbycountry <- ByPlayer %>% group_by(Birth_Country) %>% summarize(mean=mean(Draft_Round))
Draftagebycountry <- ByPlayer %>% group_by(Birth_Country) %>% summarize(mean=mean(Draft_Age))

##Begin by plotting basic scoring statistics.
ggplot(Hockey, aes(x = ShotsPerGame, y = Goals)) + geom_point()
ggplot(Hockey, aes(x = Penalty_Minutes, y = Goals)) + geom_point()
##As one might expect, it appears the more shots a player takes, the more goals he scores.
##The relationship between time spent in the penalty box and goals is less clear (one might expect
## more time in the box leaves less time for goals).

##Then a look at the distribution of draft position of the players in this dataset.
ggplot(ByPlayer, aes(x = Draft_Round)) + geom_bar()
ggplot(ByPlayer, aes(x = Draft_Pick)) + geom_histogram()
##Of the players in the years 2000-2012, it appears to be skewed in one direction, that of earlier
##drafted players.  It may be that players drafted in earlier rounds play longer in the league
##and therefore are more highly represented.

##To further investigate that point, we make a comparison between draft round and years in the NHL
##for players in this dataset.
YearsPlayed = ByPlayer$LastNHLseason - ByPlayer$FirstNHLseason
ggplot(ByPlayer, aes(x = Draft_Round, y = YearsPlayed)) + geom_jitter() + geom_smooth(method = lm)
##It does indeed appear that players from earlier draft rounds may have somewhat longer NHL careers.

##Next is a simple comparison of how many players were drafted from different geographical regions.
ggplot(ByPlayer, aes(x = BirthRegion)) + geom_histogram(stat="count")
## Clearly the Americas source most NHL players followed by Europe.

##Are players from some regions drafted earlier or later on average?
ggplot(ByPlayer, aes(BirthRegion, Draft_Round)) + geom_boxplot()

##It might be expected that teams who score more goals in a year are more likely to go to the playoffs.
ggplot(Hockey, aes(x = Year, y=Team_GoalsFor, col = Playoffs)) + geom_point()
## This graph seems to indicate such, though there are cases where the opposite is true.

##Do players who are drafted in earlier rounds score a greater percentage of their teams' goals?
ggplot(Hockey, aes(x = Year, y=PercentGoals, col = Draft_Round)) + geom_jitter()
##It is difficult to tell from this graph. but maybe?

##Do players drafted in earlier rounds score more goals per game? Does this affect their team 
##making the playoffs? Is there any difference based on their region of origin?
ggplot(Hockey, aes(x = Draft_Pick, y=GoalsPerGame, col=Playoffs)) + geom_point(alpha = 0.5, position = "jitter") + geom_smooth(method = lm)
ggplot(Hockey, aes(x = Draft_Pick, y=GoalsPerGame, col=BirthRegion)) + geom_point(alpha = 0.5) 
## It does look like perhaps the trend is for more goals per game from higher-drafted players but 
##it's not clear if this impacts making the playoffs.  The impact of birth region is not entirely clear
##though it seems possibly higher-drafted European players score a greater number of goals per game.

##Does draft age impact goal scoring?
posn.jd <- position_jitterdodge(0.5, 0, 0.6)
ggplot(Hockey, aes(x = Draft_Age, y = Goals, color = Draft_Round)) + 
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + 
  facet_grid(. ~ Playoffs)
##Players drafted younger may score more goals, on teams that do and don't make the playoffs, 
##if they were players drafted in early rounds.

##Does there appear to be any difference in goal scoring, as a function of round drafted, when compared
##between teams that do and don't make the playoffs and looking at birth region?
ggplot(Hockey, aes(x = Draft_Round, y = Goals, color = BirthRegion)) + 
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + 
  facet_grid(. ~ Playoffs)
##No clear difference is seen.




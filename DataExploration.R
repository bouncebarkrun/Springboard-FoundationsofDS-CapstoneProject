Hockey <- read_csv("~/Desktop/FinalData_clean.csv")
summary(Hockey)
sapply(Hockey, mean, na.rm=TRUE)


Hockey %>% group_by(BirthRegion) %>% summarize(mean=mean(Draft_Round), sum=sum(Draft_Round))
Hockey %>% group_by(Birth_Country) %>% summarize(mean=mean(GoalsPerGame), sum=sum(GoalsPerGame))

ggplot(Hockey) + geom_col(aes(x = Birth_Country, y = Draft_Pick, fill = Birth_Country))


ggplot(Hockey, aes(x = ShotsPerGame, y = Goals)) + geom_point()
ggplot(Hockey, aes(x = Penalty_Minutes, y = Goals)) + geom_point() + geom_smooth(method = lm)

ggplot(Hockey, aes(x = Draft_Round)) + geom_bar()
ggplot(Hockey, aes(x = Draft_Pick)) + geom_histogram(binwidth = 1)

YearsPlayed = Hockey$LastNHLseason - Hockey$FirstNHLseason
ggplot(Hockey, aes(x = Draft_Round, y = YearsPlayed)) + geom_jitter()

       
ggplot(Hockey, aes(x = Year, y=GoalsPerGame, col = Playoffs)) + geom_point(alpha = 0.5, position = "jitter")
ggplot(Hockey, aes(x = Year, y=PercentGames, col = Draft_Round)) + geom_jitter()
ggplot(Hockey, aes(x = Draft_Pick, y=GoalsPerGame, col=Playoffs)) + geom_point(alpha = 0.5, position = "jitter")
ggplot(Hockey, aes(x = Draft_Pick, y=GoalsPerGame, col=BirthRegion)) + geom_point(alpha = 0.5)


posn.jd <- position_jitterdodge(0.5, 0, 0.6)
ggplot(Hockey, aes(x = Draft_Age, y = Goals, color = Draft_Round)) + 
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + 
  facet_grid(. ~ Playoffs)
ggplot(Hockey, aes(x = Draft_Round, y = Goals, color = BirthRegion)) + 
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + 
  facet_grid(. ~ Playoffs)

Hockey$BirthRegion <- countrycode(Hockey$Birth_Country, 'country.name', 'continent', warn = TRUE, custom_dict = NULL,
            custom_match = NULL, origin_regex = FALSE)

boxplot(Draft_Round~BirthRegion,data=Hockey)

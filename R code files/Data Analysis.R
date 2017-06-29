#Import needed packages
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(randomForest)
library(caret)
library(e1071)
library(caTools)
library(rpart)
library(ROCR)
library(pscl)

#Import and refine dataset
FullData <- read.csv("~/Desktop/FinalData_clean.csv")
str(FullData)
FullData$Playoffs <- as.factor(FullData$Playoffs)
FullData$Birth_Country <-as.character(FullData$Birth_Country)
FullData$Position_Played <-as.character(FullData$Position_Played)

Question1 <- select(FullData, -X1, -X, -X.1, -Team_ID, -First_Name, -Last_Name, -Birth_State, -Birth_City, -Death_Country, -Death_State, -Death_City, -Stint, -AmateurTeam, -SeasonEnd_Rank, -TeamName)

#Prepare for analysis by setting seed 
set.seed(1234)

#Shuffle row indices and randomly order data
rows <- sample(nrow(Question1))
Question1 <- Question1[rows, ]

# Identify row to split on: split
split <- round(nrow(Question1) * 0.8)

# Create train
train <- Question1[1:split,]

# Create test
test <- Question1[(split+1):nrow(Question1),]


#Logistic regression model
Logmodel <- glm(Playoffs ~ Shooting_Hand + YearsExperience + BirthRegion + Games_Played + Goals + Assists + Points + Penalty_Minutes + Plus_Minus + Shots + GoalsPerGame + ShotsPerGame + PointsPerGame + PercentGoals + PercentGames + Draft_Pick + Draft_Round + Draft_Age, family="binomial", train)
summary(Logmodel)
# Predict on test: p
p <- predict(Logmodel, test, type="response")

# Calculate class probabilities: p_class
p_class <-
  ifelse(p > 0.5,
         "0",
         "1"
  )

# Create confusion matrix
confusionMatrix(p_class, test[["Playoffs"]])

# Calculate class probabilities: p_class
p_class2 <-
  ifelse(p > 0.9,
         "0",
         "1"
  )

# Create confusion matrix
confusionMatrix(p_class2, test[["Playoffs"]])

anova(Logmodel, test="Chisq")

pr <- prediction(p, test$Playoffs)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Classification/regression tree model
fit <- rpart(Playoffs ~ Shooting_Hand + YearsExperience+ BirthRegion + Games_Played + Goals + Assists + Points + Penalty_Minutes + Plus_Minus + Shots + GoalsPerGame + ShotsPerGame + PointsPerGame + PercentGoals + PercentGames + Draft_Pick + Draft_Round + Draft_Age + AmateurLeague, data=train,
             method="class")
plot(fit)
text(fit)

predict(fit, test, type = "class")
summary(fit)

#randomforest
fit5 <- randomForest(Playoffs ~ Shooting_Hand + YearsExperience + BirthRegion + Games_Played + Goals + Assists + Points + Penalty_Minutes + Plus_Minus + Shots + GoalsPerGame + ShotsPerGame + PointsPerGame + PercentGoals + PercentGames + Draft_Pick + Draft_Round + Draft_Age + AmateurLeague, data=train,
                    importance=TRUE, 
                    ntree=200, na.action=na.exclude)
varImpPlot(fit5)
predict(fit5, test)
summary(fit5)
fit6 <- cforest(Playoffs ~ Shooting_Hand + YearsExperience + BirthRegion + Games_Played + Goals + Assists + Points + Penalty_Minutes + Plus_Minus + Shots + GoalsPerGame + ShotsPerGame + PointsPerGame + PercentGoals + PercentGames + Draft_Pick + Draft_Round + Draft_Age + AmateurLeague, data=train, 
               controls=cforest_unbiased(ntree=200, mtry=3))
predict(fit6, test, OOB=TRUE, type = "response")
summary(fit6)





### Question 2: Differences between Over and Underperforming Draft Picks
Mod1 <- lm(Team_Wins ~ Draft_Pick + Draft_Round + GoalsPerGame + PointsPerGame + ShotsPerGame + PercentGoals + PercentGames,data=FullData)
summary(Mod1)

Mod2 <- lm(Team_Wins ~ Draft_Pick + Draft_Round,data=FullData)
summary(Mod2)

TopDraftPicks  <- subset(FullData, Draft_Round <= 2)
BottomDraftPicks  <- subset(FullData, Draft_Round >= 7)

t.test(TopDraftPicks$GoalsPerGame, BottomDraftPicks$GoalsPerGame) 
t.test(TopDraftPicks$PercentGoals, BottomDraftPicks$PercentGoals) 


MadePlayoffs <- subset(FullData, Playoffs == 1)
NoPlayoffs <- subset(FullData, Playoffs == 0)

t.test(MadePlayoffs$Draft_Pick, NoPlayoffs$Draft_Pick)
t.test(MadePlayoffs$Draft_Round, NoPlayoffs$Draft_Round)

Question2 <- rbind(TopDraftPicks, BottomDraftPicks)
Question2 <- mutate(Question2, OverUnder = as.numeric(Question2$Draft_Round <=2))
OverUnder <- c("OverUnder")
Question2[OverUnder][is.na(Question2[OverUnder])] <- 0

Logmodel2 <- glm(OverUnder ~ Height + Weight + Shooting_Hand + Position_Played + BirthRegion + Draft_Team + Draft_Age + AmateurLeague, family="binomial", Question2)
summary(Logmodel2)

anova(Logmodel2, test="Chisq")


#Import needed packages
library(readr)
library(dplyr)
library(caret)
library(ROCR)
library(rpart)
library(randomForest)
library(party)

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

#Create the formula
formula <- Playoffs ~ Shooting_Hand + YearsExperience + BirthRegion + Games_Played + Goals + Assists + Points + Penalty_Minutes + Plus_Minus + Shots + GoalsPerGame + ShotsPerGame + PointsPerGame + PercentGoals + PercentGames + Draft_Pick + Draft_Round + Draft_Age

#Logistic regression model
Logmodel <- glm(formula, family="binomial", train)
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
Class <- rpart(formula, data=train,
             method="class", na.action=na.rpart)
plot(Class)
text(Class)

predict(Class, test, type = "class")
summary(Class)

#randomforest
RandomFor <- randomForest(formula, data=train,
                    importance=TRUE, 
                    ntree=200, na.action=na.exclude)
varImpPlot(RandomFor)
predict(RandomFor, test)
summary(RandomFor)

#Conditional Random Forest
CondForest <- cforest(formula, data=train, 
               controls=cforest_unbiased(ntree=200, mtry=3))
pred <- summary(predict(CondForest, test, OOB=TRUE, type = "response"))


### Question 2: Differences between Over and Underperforming Draft Picks
Lin1 <- lm(Team_Wins ~ Draft_Pick + Draft_Round + GoalsPerGame + PointsPerGame + ShotsPerGame + PercentGoals + PercentGames,data=FullData)
summary(Lin1)

Lin2 <- lm(Team_Wins ~ Draft_Pick + Draft_Round,data=FullData)
summary(Lin2)

TopDraftPicks  <- subset(FullData, Draft_Round <= 2)
BottomDraftPicks  <- subset(FullData, Draft_Round >= 7)

t.test(TopDraftPicks$GoalsPerGame, BottomDraftPicks$GoalsPerGame) 
t.test(TopDraftPicks$PercentGoals, BottomDraftPicks$PercentGoals) 

Question2 <- rbind(TopDraftPicks, BottomDraftPicks)
Question2 <- mutate(Question2, OverUnder = as.numeric(Question2$Draft_Round <=2))
OverUnder <- c("OverUnder")
Question2[OverUnder][is.na(Question2[OverUnder])] <- 0

#Logistic regression model
PerfModel <- glm(OverUnder ~ Height + Weight + Shooting_Hand + Position_Played + BirthRegion + Draft_Team + Draft_Age + AmateurLeague, family="binomial", Question2)
summary(PerfModel)

anova(PerfModel, test="Chisq")


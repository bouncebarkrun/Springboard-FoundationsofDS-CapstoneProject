
# load libraries
library(mlbench)
library(caret)
library(readr)
library(rcart)

FullData <- read.csv("C:/Users/mmcnamara/Desktop/FinalData_clean.csv")
str(FullData)
FullData$Playoffs <- as.factor(FullData$Playoffs)
FullData$Birth_Country <-as.character(FullData$Birth_Country)
FullData$Position_Played <-as.character(FullData$Position_Played)

dataset <- FullData

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
preProcess=c("center", "scale")

#Create the formula
formula <- Playoffs ~ Shooting_Hand + YearsExperience + BirthRegion + Games_Played + Goals + Assists + Points + Penalty_Minutes + Plus_Minus + Shots + GoalsPerGame + ShotsPerGame + PointsPerGame + PercentGoals + PercentGames + Draft_Pick + Draft_Round + Draft_Age

# Logistic Regression
set.seed(seed)
fit.glm <- train(formula, data=dataset, method="glm", trControl=control, na.action=na.pass)
# CART
set.seed(seed)
fit.cart <- train(formula, data=dataset, method="rpart", trControl=control, na.action = na.pass)
# C5.0
set.seed(seed)
fit.c50 <- train(formula, data=dataset, method="C5.0", trControl=control, na.action = na.pass)
# Bagged CART
set.seed(seed)
fit.treebag <- train(formula, data=dataset, method="treebag", trControl=control, na.action = na.pass)
# Random Forest
set.seed(seed)
fit.rf <- train(formula, data=dataset, method="rf", trControl=control, na.action = na.omit)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(formula, data=dataset, method="gbm", trControl=control, verbose=FALSE, na.action = na.pass)



results <- resamples(list(logistic=fit.glm, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results, metric="Accuracy")

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

summary(fit.c50)
summary(fit.cart)
summary(fit.gbm)
summary(fit.glm)
summary(fit.treebag)
summary(fit.rf)
plot(fit.rf)

### Question 2: Differences between Over and Underperforming Draft Picks

TopDraftPicks  <- subset(FullData, Draft_Round <= 2)
BottomDraftPicks  <- subset(FullData, Draft_Round >= 7)

Question2 <- rbind(TopDraftPicks, BottomDraftPicks)
Question2 <- mutate(Question2, OverUnder = as.numeric(Question2$Draft_Round <=2))
OverUnder <- c("OverUnder")
Question2[OverUnder][is.na(Question2[OverUnder])] <- 0

#Logistic regression model
PerfModel <- glm(OverUnder ~ Height + Weight + Shooting_Hand + Position_Played + BirthRegion + Draft_Team + Draft_Age + AmateurLeague, family="binomial", Question2)
summary(PerfModel)

anova(PerfModel, test="Chisq")



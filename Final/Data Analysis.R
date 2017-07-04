#R code necessary to analyze NHL data to address questions for capstone project

# load libraries
library(mlbench)
library(caret)
library(readr)
library(rpart)
library(dplyr)
library(C50)
library(plyr)
library(ipred)
library(e1071)
library(ROCR)
library(randomForest)

#Read data and ensure variables are correct format
FullData <- read.csv("~/Desktop/FinalData_clean.csv")
str(FullData)
FullData$Playoffs <- as.factor(FullData$Playoffs)
FullData$Birth_Country <-as.character(FullData$Birth_Country)
FullData$Position_Played <-as.character(FullData$Position_Played)

#Rename data
dataset <- FullData

#Set trainControl and seed.  In this case, all models will be run with three separate 10-fold cross-validations as the resampling scheme.
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7

#Create the formula to evaluate the variables that may influence Playoffs
formula <- Playoffs ~ Shooting_Hand + YearsExperience + Goals + Assists + Penalty_Minutes + Shots + GoalsPerGame + ShotsPerGame + PointsPerGame + PercentGoals + PercentGames + Draft_Pick + Draft_Round + Draft_Age + SavePercentage + GoalieShutouts + GoalieMinutes

# Logistic Regression Model
set.seed(seed)
fit.glm <- train(formula, data=dataset, method="glm", trControl=control, na.action=na.pass, preProcess=c("center", "scale"))
print(fit.glm)

# Classification And Regression Tree Model
set.seed(seed)
fit.cart <- train(formula, data=dataset, method="rpart", trControl=control, na.action = na.pass, preProcess=c("center", "scale"))
print(fit.cart)

# C5.0 (a method that constructs classifiers expressed as decision trees)
set.seed(seed)
fit.c50 <- train(formula, data=dataset, method="C5.0", trControl=control, na.action = na.pass, preProcess=c("center", "scale"))
print(fit.c50)

# Bagged CART (a bootstrap aggregating algorithm)
set.seed(seed)
fit.treebag <- train(formula, data=dataset, method="treebag", trControl=control, na.action = na.pass, preProcess=c("center", "scale"))
print(fit.treebag)

# Random Forest
set.seed(seed)
fit.rf <- train(formula, data=dataset, method="rf", trControl=control, na.action = na.roughfix, preProcess=c("center", "scale"))
print(fit.rf)

# Stochastic Gradient Boosting (Generalized Boosted Modeling - a model that constructs additive regression models on repeated subsamples of the data)
set.seed(seed)
fit.gbm <- train(formula, data=dataset, method="gbm", trControl=control, verbose=FALSE, na.action = na.pass, preProcess=c("center", "scale"))
print(fit.gbm)

# Compile the resamples results from the models
results <- resamples(list(logistic=fit.glm, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
   
# Compare method accuracy
summary(results, metric="Accuracy")

# Boxplot comparison of methods
bwplot(results)
# Dot-plot comparison of methods
dotplot(results)

#Some further investigation of the important features in two of the models
summary(fit.glm)
varImp(fit.rf)

#Try to re-create the logistic regression model more simply to address potential overfitting, choosing the highest importance variables from RF
set.seed(seed)
Logmodel <- glm(formula, family="binomial", dataset)
summary(Logmodel)
anova(Logmodel, test ="Chisq")


### Question 2: Differences between Over and Underperforming Draft Picks

#Determine average goals
summary(FullData$Goals)

#Set Over and Under Achievers: Players who were drafted early and score less than average goals & players who were drafted late who score more than average
Under  <- subset(FullData, Draft_Round <= 2 & Goals <=6.62 & Position_Played != "G")
Over <- subset(FullData, Draft_Round >= 7 & Goals >=6.62 )

#Create one data file with a variable to identify over or underachieverfs
Question2 <- rbind(Under, Over)
Question2 <- mutate(Question2, OverUnder = as.numeric(Question2$Draft_Round <=2))
OverUnder <- c("OverUnder")
Question2[OverUnder][is.na(Question2[OverUnder])] <- 0
Question2$OverUnder <- as.factor(Question2$OverUnder)

#Logistic regression model
set.seed(seed)
PerfModel <- glm(OverUnder ~ Height + Weight + Position_Played + Draft_Team + Draft_Age + AmateurLeague, family="binomial", Question2)
summary(PerfModel)

#Further evaluate model components
anova(PerfModel, test="Chisq")


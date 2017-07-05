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

# Logistic Regression Model: This is a regression model used when the dependent variable is categorical, as with the Playoffs variable (0 or 1).
set.seed(seed)
fit.glm <- train(formula, data=dataset, method="glm", trControl=control, na.action=na.pass, preProcess=c("center", "scale"))
print(fit.glm)

# Classification And Regression Tree, or CART, Model: This type of model develops classification trees to predict the categorical outcome.
set.seed(seed)
fit.cart <- train(formula, data=dataset, method="rpart", trControl=control, na.action = na.pass, preProcess=c("center", "scale"))
print(fit.cart)

# C5.0: This is a method that constructs classifiers expressed as decision trees.
set.seed(seed)
fit.c50 <- train(formula, data=dataset, method="C5.0", trControl=control, na.action = na.pass, preProcess=c("center", "scale"))
print(fit.c50)

# Bagged CART: This is another classification and regression model that uses bagging for the model averaging.
set.seed(seed)
fit.treebag <- train(formula, data=dataset, method="treebag", trControl=control, na.action = na.pass, preProcess=c("center", "scale"))
print(fit.treebag)

# Random Forest: This type of model constructs many decision trees simultaneously to determine the most likely outcome.
set.seed(seed)
fit.rf <- train(formula, data=dataset, method="rf", trControl=control, na.action = na.roughfix, preProcess=c("center", "scale"))
print(fit.rf)

# Stochastic Gradient Boosting: This is also known as Generalized Boosted Modeling and is a model that constructs additive regression models on repeated subsamples of the data.
set.seed(seed)
fit.gbm <- train(formula, data=dataset, method="gbm", trControl=control, verbose=FALSE, na.action = na.pass, preProcess=c("center", "scale"))
print(fit.gbm)

# Compile the resamples results from the models
results <- resamples(list(logistic=fit.glm, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
   
# Compare method accuracy
summary(results, metric="Accuracy")

# Boxplot comparison of methods
bwplot(results, metric="Accuracy")
# Dot-plot comparison of methods
dotplot(results, metric="Accuracy")

#The random forest model has the highest mean and median accuracy (while logistic regression hits the highest maximum).
varImp(fit.rf)

#Plot the mean decrease gini to see the variable importance
varImpPlot(fit.rf)

#Evaluate the model by building a ROC curve
pred=predict(fit.rf,type = "prob")
perf = prediction(pred[,2], dataset$Playoffs)
pred2 = performance(perf, "tpr","fpr")
plot(pred2,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

### Question 2: Differences between Over and Underperforming Draft Picks

#Determine average goals
summary(FullData$Goals)

#Set Over and Under Achievers: Players who were drafted early and score less than average goals & players who were drafted late who score more than average
Under  <- subset(FullData, Draft_Round <= 2 & Goals <=6.62 & Position_Played != "G")
Over <- subset(FullData, Draft_Round >= 7 & Goals >=6.62 )

#Create one data file with a variable to identify over or underachieverfs
Question2 <- rbind(Under, Over)
Question2 <- mutate(Question2, OverUnder = as.numeric(Question2$Draft_Round >=7))
OverUnder <- c("OverUnder")
Question2[OverUnder][is.na(Question2[OverUnder])] <- 0
Question2$OverUnder <- as.factor(Question2$OverUnder)

#Logistic regression model
set.seed(seed)
PerfModel <- glm(OverUnder ~ Height + Weight + Position_Played + Draft_Team + BirthRegion + Draft_Age + AmateurLeague, family="binomial", Question2)
summary(PerfModel)

#Further evaluate model components
anova(PerfModel, test="Chisq")


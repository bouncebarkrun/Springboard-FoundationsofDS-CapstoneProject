#Import needed packages
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(ranger)

#Import and refine dataset
FullData <- read.csv("C:/Users/mmcnamara/Desktop/FinalData_clean.csv")
FullData <- select(FullData, -X1, -X)

#Prepare for analysis by setting seed 
set.seed(1234)

#Shuffle row indices and randomly order data
rows <- sample(nrow(FullData))
FullData <- FullData[rows, ]

# Identify row to split on: split
split <- round(nrow(FullData) * 0.8)

# Create train
train <- FullData[1:split,]

# Create test
test <- FullData[(split+1):nrow(FullData),]

summary(train)
summary(test)

#Logistic regression model
Logmodel <- glm(Playoffs ~ Shooting_Hand + YearsExperience + Position_Played + Birth_Country + BirthRegion + Games_Played + Goals + Assists + Points + Penalty_Minutes + Plus_Minus + Shots + GoalsPerGame + ShotsPerGame + PointsPerGame + PercentGoals + PercentGames + Draft_Pick + Draft_Round + Draft_Age + AmateurTeam + AmateurLeague, family="binomial", train)

# Predict on test: p
p <- predict(Logmodel, test, type="response")
summary(p)

#Random forest model

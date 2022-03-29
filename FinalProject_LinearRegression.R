rm(list = ls())
setwd("/Users/jseeb/Downloads")


library(caret)
library(tidyr)

movies.df <- read.csv("IMDb movies.csv")
ratings.df <- read.csv("IMDb ratings.csv")
#title_principals.df <- read.csv("title_principals.csv")
#names.df <- read.csv("IMDb names.csv")



# Data Preparation for Linear Regression
imdb.df <- merge(movies.df, ratings.df, by = "imdb_title_id")
imdb.df <- imdb.df[imdb.df$year > 2010 & movies.df$country == "USA", ]
imdb.df$budget <- gsub("[a-zA-Z$ ]", "", imdb.df$budget)
imdb.df$budget <- as.numeric(imdb.df$budget)
imdb.df$usa_gross_income <- gsub("[a-zA-Z$ ]", "", imdb.df$usa_gross_income)
imdb.df$usa_gross_income <- as.numeric(imdb.df$usa_gross_income)
imdb.df <- imdb.df[c("weighted_average_vote", "director", "writer", "production_company", "budget", "duration")]
imdb.df <- drop_na(imdb.df, 'budget', 'duration')
imdb.df <- imdb.df[imdb.df$weighted_average_vote > 5, ]
imdb.df 

# select variables for regression
selected.var <- c("weighted_average_vote","budget", "duration")

# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:dim(imdb.df)[1]), dim(imdb.df)[1]*0.6)  
train.df <- imdb.df[train.index, selected.var]
valid.df <- imdb.df[-train.index, selected.var]

# use lm() to run a linear regression of weighted_average_vote on all 11 predictors in the training set. 
imdb.lm <- lm(weighted_average_vote ~ ., data = train.df)


#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(imdb.lm)   # Get model summary

# Plot barchart of coefficients
library(lattice)
barchart(imdb.lm$coefficients)

# Standardized coefficients
# install.packages("QuantPsyc")
library(QuantPsyc)
imdb.lm.s<-lm.beta(imdb.lm)
barchart(imdb.lm.s)

# Make predictions on a new set. 
# install.packages("forecast") # Package installation is required for the first time
library(forecast)
imdb.lm.pred <- predict(imdb.lm, valid.df)
some.residuals <- valid.df$weighted_average_vote[1:20] - imdb.lm.pred[1:20]
data.frame("Predicted" = imdb.lm.pred[1:20], "Actual" = valid.df$weighted_average_vote[1:20],
           "Residual" = some.residuals)
options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(imdb.lm.pred, valid.df$weighted_average_vote)

# Visually check residuals
imdb.lm.pred <- predict(imdb.lm, valid.df)
all.residuals <- valid.df$weighted_average_vote - imdb.lm.pred
# The majority of residual values fall into [-1406, 1406]
length(all.residuals[which(all.residualsa > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")


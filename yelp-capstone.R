# Data Science Capstone Project
# Data set: Yelp Dataset Challenge - Round 6
# Author: Zeydy Ortiz, Ph. D.

# Libraries needed
library(jsonlite)
library(caTools)
library(randomForest)
library(caret)

# Read original data and flatten
biz <- stream_in(file("yelp_academic_dataset_business.json"))
biz <- toJSON(biz)
biz <- fromJSON(biz, flatten=TRUE)
saveRDS (biz, "biz.rds")
biz <- readRDS("biz.rds")

# Analyze data for Restaurants
rest <- biz[grep("Restaurants", biz$categories),]

# flatten `attributes.Accepts Credit Cards`
AcceptsCC <- logical(length(rest$`attributes.Accepts Credit Cards`))
for (i in 1: length(rest$`attributes.Accepts Credit Cards`)) { 
  if (lengths(rest$`attributes.Accepts Credit Cards`[i])== 0) { AcceptsCC[i] = NA } 
  else { AcceptsCC[i] <- unlist(rest$`attributes.Accepts Credit Cards`[i])}
}
rest$`attributes.Accepts Credit Cards` <- AcceptsCC

# removing variables that are mostly NA
for (i in 105:14) {
  if(sum(is.na(rest[,i]))>0.9*length(rest[,1])) rest[,i] <- NULL # remove that column
} # ended with 66 variables instead of 105
rest$neighborhoods <- NULL
rest$type <- NULL
rest$categories <- NULL

rest_df <- data.frame(rest)
# make NAs into Unk factor
for (i in 11:63) {
  if (i < 25) rest_df[,i] <- substr(rest_df[,i],1,2)
  rest_df[,i][is.na(rest_df[,i])] <- 'Unk'
  rest_df[,i] <- as.factor(rest_df[,i])
}

# remove irrelevant columns
rest_df$business_id <- NULL
rest_df$full_address <- NULL
rest_df$city <- NULL
rest_df$name <- NULL
rest_df$longitude <- NULL
rest_df$latitude <- NULL

# convert to factors
rest_df$open <- as.factor(rest_df$open)
rest_df$state <- as.factor(rest_df$state)
all_rest <- rest_df
saveRDS(all_rest, "all_rest.rds")
rest_df <- readRDS("all_rest.rds")

# Split data set
set.seed(123)

split = sample.split(rest_df$open, SplitRatio = 0.7)
train = subset(rest_df, split==TRUE)
test = subset(rest_df, split==FALSE)

# Build random forest
set.seed(456)
restRF <- randomForest(open~., data=train)
restRF$confusion
#baseline for training set
table(train$open)

predRF = predict(restRF, newdata=test)
table(predRF, test$open)
table(test$open)

varImpPlot(restRF, n.var=20, main="Top 20 Factors - All Restaurants")
all_restRF <- restRF
saveRDS(all_restRF, "all_restRF.rds")

all_train <- train
all_test <- test

saveRDS(all_train, "all_train.rds")
saveRDS(all_test, "all_test.rds")

# get only data for AZ
train <- all_train[all_train$state=="AZ",]
train$state <- NULL

test <- all_test[all_test$state=="AZ",]
test$state <- NULL

# Build random forest
set.seed(456)
restRF <- randomForest(open~., data=train)
restRF$confusion
#baseline for training set
table(train$open)

predRF = predict(restRF, newdata=test)
table(predRF, test$open)
table(test$open)

varImpPlot(restRF, n.var=20, main="Top 20 Factors - AZ Restaurants")
AZ_restRF <- restRF
saveRDS(AZ_restRF, "AZ_restRF.rds")

# get only data for QC
train <- all_train[all_train$state=="QC",]
train$state <- NULL

test <- all_test[all_test$state=="QC",]
test$state <- NULL

# Build random forest
set.seed(456)
restRF <- randomForest(open~., data=train)
restRF$confusion
#baseline for training set
table(train$open)

predRF = predict(restRF, newdata=test)
table(predRF, test$open)
table(test$open)

varImpPlot(restRF, n.var=20, main="Top 20 Factors - QC Restaurants")
QC_restRF <- restRF
saveRDS(QC_restRF, "QC_restRF.rds")

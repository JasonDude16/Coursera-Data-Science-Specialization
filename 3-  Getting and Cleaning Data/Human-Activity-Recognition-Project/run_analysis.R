library(readr); library(plyr); library(magrittr); library(dplyr)

setwd("./UCI HAR Dataset")

subject_test <- read.table("./test/subject_test.txt", col.names = "SubID")
subject_train <- read.table("./train/subject_train.txt", col.names = "SubID")

x_test <- read.table("./test/X_test.txt", header = F)
x_train <- read.table("./train/X_train.txt", header = F)

y_test <- read.table("./test/y_test.txt", col.names = "Condition")
y_train <- read.table("./train/y_train.txt", col.names = "Condition")

# CHANGE FACTOR LEVELS FROM NUMERIC TO CHAR
y_test$Condition <- 
    mapvalues(x = y_test$Condition, 
              from = c(1, 2, 3, 4, 5, 6), 
              to = c("Walking", "Walking Upstairs", "Walking Downstairs",
                     "Sitting", "Standing", "Laying"))

y_train$Condition <- 
    mapvalues(x = y_train$Condition, 
              from = c(1, 2, 3, 4, 5, 6), 
              to = c("Walking", "Walking Upstairs", "Walkiing Downstairs",
                     "Sitting", "Standing", "Laying"))

# IMPORT FEATURES LIST AND SEPARATE NUMERIC VALUES FROM CHAR VALUES
features <- read.delim("./features.txt", head = F, string = F)
features <- features %>% tidyr::separate(V1, into = c("n", "c"), sep = " ")
features <- (unname(unlist(features[,2])))

# MAP FEATURES TO COLUMN NAMES
colnames(x_test) <- features
colnames(x_train) <- features

# ADD TEST/TRAIN COLUMN, COMBINE DATASETS
data_test <- cbind(subject_test, y_test, x_test)
data_test$Type <- "Test"
data_test <- data_test[c(1,564,2:563)]
rm(subject_test, y_test, x_test)

data_train <- cbind(subject_train, y_train, x_train)
data_train$Type <- "Train"
data_train <- data_train[c(1,564,2:563)]
rm(subject_train, y_train, x_train, features)

data <- rbind(data_train, data_test)
rm(data_train, data_test)

# SELECT ONLY THE MEAN() AND STD() COLUMNS 
data_mean_std <- 
data %>% select(SubID, Type, Condition, contains("mean()"), contains("std()"))

# SUMMARISE DATA WHICH IS GROUPED BY SUBID AND CONDITION
data_summarized <- 
data_mean_std %>% 
    select(!Type) %>% 
    group_by(SubID, Condition) %>% 
    summarise_all(mean)

head(data_summarized)[1:5]
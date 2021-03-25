setwd("C:/Users/mhenn/Documents/Programming/Academic/BC2407 Medilytics")
library(data.table)
library(caret)
library(ROSE)
library(dplyr)
library(performanceEstimation)
library(caTools)

source("functions.R") #load functions from functions.R file for cleaner file contents


data <- readData("FinalCleanedData.csv", "CHCKDNY2")
data <- normalizeData(data)

# 31 independent variables (no pregnancy)
# 11 dependent variables

################################################################################
# Using ROSE for sampling (ONLY RUN THIS SECTION) -------------------------------
ctrl <- trainControl(method = "cv", allowParallel = FALSE, sampling = 'rose')
getdata <- sample.split(Y = data$DISEASE, SplitRatio = 0.975)
maindata <- subset(data, getdata == FALSE)
# use 2.5% of the data to come up with the model - q smart of me hehe :)
train_test_split <- sample.split(Y = maindata$DISEASE, SplitRatio = 0.7)
train <- subset(maindata, train_test_split == TRUE)
test <- subset(maindata, train_test_split == FALSE)

train_x <- subset(train, select = -c(DISEASE))
train_y <- train$DISEASE

test_x <- subset(test, select = -c(DISEASE))
test_y <- test$DISEASE

pure_x <- subset(data, select = -c(DISEASE))
pure_y <- data$DISEASE
##########################################################################
# DMwR SMOTE train and test sets (ONLY RUN THIS SECTION) -----------------------------------
# Load data from csv file
ctrl <- trainControl(method = "cv")

train <- readData("smotetrain_DMwR.csv", "MICHD")
test <- readData("smotetest_DMwR.csv", "MICHD")

train_x <- subset(train, select = -c(DISEASE))
train_y <- train$DISEASE

test_x <- subset(test, select = -c(DISEASE))
test_y <- test$DISEASE

pure_x <- subset(data, select = -c(DISEASE))
pure_y <- data$DISEASE
##########################################################################
# PE SMOTE train and test sets (ONLY RUN THIS SECTION) -------------------

ctrl <- trainControl(method = "cv", search = 'random')

getdata <- sample.split(Y = data$DISEASE, SplitRatio = 0.85) # use 10% of the data total
maindata <- subset(data, getdata == FALSE)
train_test_split <- sample.split(Y = maindata$DISEASE, SplitRatio = 0.7)
trainset.ori <- subset(maindata, train_test_split == TRUE)
testset.ori <- subset(maindata, train_test_split == FALSE)

train <- smote(DISEASE ~ ., data = trainset.ori,
                        perc.over = 1, perc.under = 2)
test <- smote(DISEASE ~ ., data = testset.ori,
                       perc.over = 1, perc.under = 2)

train_x <- subset(train, select = -c(DISEASE))
train_y <- train$DISEASE

test_x <- subset(test, select = -c(DISEASE))
test_y <- test$DISEASE

pure_x <- subset(data, select = -c(DISEASE))
pure_y <- data$DISEASE
###########################################################################

###########################################################################

# Training the NNET Model -------------------------------------------------
ctrl <- trainControl(method = "cv", search = 'grid')
nnet_grid <- expand.grid(.decay = c(0.5, 0.2, 0.1),.size = c(3, 5, 10))
model_nnet <- train(train_x, train_y, method = 'nnet', metric = "Accuracy",
                    trControl = ctrl, tunegrid = nnet_grid, maxit = 300)

?train()

saveRDS(model_nnet, "PE_kidney_smote80_model.rds")

###########################################################################

# Load model from file? ----------------------------------------------------
# model_nnet <- readRDS("PE_SMOTE_model.rds")
# model_nnet <- readRDS("roseNNETModel.rds")

###########################################################################

# Predicting Results ------------------------------------------------------
results_train <- predict(model_nnet, newdata = train_x)
confusion_matrix_train <- confusionMatrix(results_train, train_y)
confusion_matrix_train$table

results_test <- predict(model_nnet, newdata = test_x)
confusion_matrix_test <- confusionMatrix(results_test, test_y)
confusion_matrix_test$table

results_all <- predict(model_nnet, newdata = pure_x)
confusion_matrix_all <- confusionMatrix(results_all, pure_y)
confusion_matrix_all$table

cat(" Accuracy of train model is: ", confusion_matrix_train$overall["Accuracy"]*100, '\n',
    "FNR Train %: ", confusion_matrix_train$table[1,2]/(confusion_matrix_train$table[1,2] + confusion_matrix_train$table[2,2])*100, '\n',
    "Accuracy of test model is: ", confusion_matrix_test$overall["Accuracy"]*100, '\n',
    "FNR Test %: ", confusion_matrix_test$table[1,2]/(confusion_matrix_test$table[2,2] + confusion_matrix_test$table[1,2])*100, '\n',
    "Accuracy of model on overall data: ", confusion_matrix_all$overall["Accuracy"]*100, '\n',
    "FNR All %: ", confusion_matrix_all$table[1,2]/(confusion_matrix_all$table[1,2] + confusion_matrix_all$table[2,2])*100)
###########################################################################

# Test Model Data Results ----------------------------------------------------


# CHCKDNY2 - 85% data ; default settings
# Accuracy of train model is:  73.1888 
# FNR Train %:  22.17344 
# Accuracy of test model is:  71.54731 
# FNR Test %:  25.06394 
# Accuracy of model on overall data:  67.78464 
# FNR All %:  25.24775

# CHCKDNY2 - 85% data ; search = 'random'
# Accuracy of train model is:  77.36004 
# FNR Train %:  21.89901 
# Accuracy of test model is:  69.56522 
# FNR Test %:  31.07417 
# Accuracy of model on overall data:  71.07261 
# FNR All %:  30.73289

# CHCKDNY2 - 85% data ; search = 'random', maxit = 200
# Accuracy of train model is:  82.10757 
# FNR Train %:  17.23381 
# Accuracy of test model is:  67.00767 
# FNR Test %:  36.82864 
# Accuracy of model on overall data:  69.28545 
# FNR All %:  35.06568

# CHCKDNY2 - 85% data; search = 'grid', maxit = 200
# Accuracy of train model is:  73.21625 
# FNR Train %:  22.11855 
# Accuracy of test model is:  71.54731 
# FNR Test %:  25.06394 
# Accuracy of model on overall data:  67.79186 
# FNR All %:  25.24775

# CHCKDNY2 - 85% data; search = 'grid', maxit = 100
# nnet_grid <- expand.grid(.decay = c(0.5, 0.2, 0.1),.size = c(3, 5, 10))
# Accuracy of train model is:  77.11306 
# FNR Train %:  20.1427 
# Accuracy of test model is:  71.54731 
# FNR Test %:  27.10997 
# Accuracy of model on overall data:  69.67929 
# FNR All %:  26.87255

# CHCKDNY2 - 85% data; search = 'grid', maxit = 200
# nnet_grid <- expand.grid(.decay = c(0.5, 0.2, 0.1),.size = c(3, 5, 10))
# Accuracy of train model is:  77.19539 
# FNR Train %:  19.92316 
# Accuracy of test model is:  71.61125 
# FNR Test %:  26.9821 
# Accuracy of model on overall data:  69.59754 
# FNR All %:  26.74579
# Best Settings for MINIMIZED FNR -----------------------------------------



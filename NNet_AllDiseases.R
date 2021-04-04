setwd("C:/Users/mhenn/Documents/Programming/Academic/BC2407 Medilytics")
library(data.table)
library(caret)
library(dplyr)
library(performanceEstimation)
library(caTools)

source("functions.R")

runNNetModel <- function(path, chosen_disease) {
  
  # ensure reproducibility
  set.seed(2407) 
  
  # initialize and load data
  data <- readData(path, chosen_disease)
  data <- normalizeData(data)
  
  
  # getdata <- sample.split(Y = data$DISEASE, SplitRatio = 0.95) # use 15% of the data total
  # maindata <- subset(data, getdata == FALSE)
  train_test_split <- sample.split(Y = data$DISEASE, SplitRatio = 0.7)
  trainset.ori <- subset(data, train_test_split == TRUE)
  testset.ori <- subset(data, train_test_split == FALSE)
  


# Generate and save SMOTE Data --------------------------------------------
  # train <- smote(DISEASE ~ ., data = trainset.ori,
                 # perc.over = 1, perc.under = 2)
  # write.csv(train, paste('SMOTEData/train_', chosen_disease, '.csv', sep = ""), row.names = FALSE)
  # 

# Load SMOTE data ---------------------------------------------------------

  train <- readDataOnly(paste('SMOTEData/', chosen_disease, '.csv', sep = ""))

  test <- testset.ori
  
  ##############################################################################
  
  train_x <- subset(train, select = -c(DISEASE))
  train_y <- train$DISEASE
  
  test_x <- subset(test, select = -c(DISEASE))
  test_y <- test$DISEASE
  
  pure_x <- subset(data, select = -c(DISEASE))
  pure_y <- data$DISEASE
  
  # Training the NNET Model -------------------------------------------------
  # ctrl <- trainControl(method = "cv", search = 'random')
  # # # nnet_grid <- expand.grid(size = seq(from = 10, to = 20, by = 1), decay = seq(from = 0.1, to = 0.5, by = 0.1))
  # model_nnet <- train(train_x, train_y, method = 'nnet', metric = "Accuracy",
  #                     trControl = ctrl, tuneLength = 20, maxit = 200, MaxNWts = 4000) # increase to 200
  # 
  # saveRDS(model_nnet, paste("Models/", "NNet_", chosen_disease, ".rds", sep = ""))


  # Load NNet from file instead ---------------------------------------------

  model_nnet <- readRDS(paste("Models/", "NNet_", chosen_disease, ".rds", sep = ""))

  # Getting results from model ----------------------------------------------
  results_train <- predict(model_nnet, newdata = train_x)
  confusion_matrix_train <- confusionMatrix(results_train, train_y)
  confusion_matrix_train$table
  
  results_test <- predict(model_nnet, newdata = test_x)
  confusion_matrix_test <- confusionMatrix(results_test, test_y)
  confusion_matrix_test$table
  
  results_all <- predict(model_nnet, newdata = pure_x)
  confusion_matrix_all <- confusionMatrix(results_all, pure_y)
  confusion_matrix_all$table

  new_row <- data.frame(chosen_disease, confusion_matrix_train$overall["Accuracy"],
                        confusion_matrix_train$table[2,2]/(confusion_matrix_train$table[2,2] + confusion_matrix_train$table[1,2]),
                        confusion_matrix_test$overall["Accuracy"],
                        confusion_matrix_test$table[2,2]/(confusion_matrix_test$table[2,2] + confusion_matrix_test$table[1,2]),
                        confusion_matrix_all$overall["Accuracy"],
                        confusion_matrix_all$table[2,2]/(confusion_matrix_all$table[2,2] + confusion_matrix_all$table[1,2]))
  return(new_row)
}

NNetResults <- data.table('Disease Name' = character(),
                          'Train Accuracy' = numeric(),
                          'Recall (Train)' = numeric(),
                          'Test Accuracy' = numeric(),
                          'Recall (Test)' = numeric(),
                          'Overall Accuracy' = numeric(),
                          'Recall (All)' = numeric())

disease_list = c("MICHD", "CHCCOPD2", "CHCKDNY2", "CVDSTRK3", "DIABETE4")
for (disease in disease_list) {
  new_row <- runNNetModel("FinalCleanedData.csv", disease)
  NNetResults <- rbindlist(list(NNetResults, new_row), use.names = FALSE)
}
print("Completed sequence - NNet")
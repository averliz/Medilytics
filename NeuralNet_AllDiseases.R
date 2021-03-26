library(data.table)
library(caret)
library(dplyr)
library(performanceEstimation)
library(caTools)

source("functions.R")
set.seed(1)

runNeuralNetModel <- function(path, chosen_disease) {
  
  data <- readData(path, chosen_disease)
  data <- normalizeData(data)
  
  getdata <- sample.split(Y = data$DISEASE, SplitRatio = 0.7) # use 10% of the data total
  maindata <- subset(data, getdata == FALSE)
  train_test_split <- sample.split(Y = maindata$DISEASE, SplitRatio = 0.7)
  trainset.ori <- subset(maindata, train_test_split == TRUE)
  testset.ori <- subset(maindata, train_test_split == FALSE)
  
  trainset <- smote(DISEASE ~ ., data = trainset.ori,
                    perc.over = 1, perc.under = 2)
  testset <- smote(DISEASE ~ ., data = testset.ori,
                   perc.over = 1, perc.under = 2)
  summary(testset)
  
  ################################################################################
  
  # Get Dummies - Training Data --------------------------------------------------
  dummies <- dummyVars(DISEASE ~ ., data = trainset, sep = '_', fullRank = TRUE)
  train_x <- predict(dummies, newdata = trainset)
  train_x <- data.frame(train_x)
  train_y <- as.numeric(levels(trainset$DISEASE))[trainset$DISEASE]
  train_x$DISEASE <- train_y
  train <- train_x
  ################################################################################
  
  
  # Get Dummies - Test Data ------------------------------------------------------
  dummies <- dummyVars(DISEASE ~ ., data = testset, sep = '_', fullRank = TRUE)
  test_x <- predict(dummies, newdata = testset)
  test_x <- data.frame(test_x)
  test_y <- as.numeric(levels(testset$DISEASE))[testset$DISEASE]
  test_x$DISEASE <- test_y
  test <- test_x
  ################################################################################
  
  # Get Dummies - ALL Data ------------------------------------------------------
  dummies <- dummyVars(DISEASE ~ ., data = data, sep = '_', fullRank = TRUE)
  all_x <- predict(dummies, newdata = data)
  all_x <- data.frame(all_x)
  all_y <- as.numeric(data$DISEASE)
  all_x$DISEASE <- all_y
  all <- all_x
  ################################################################################
  model_neuralnet <- neuralnet(DISEASE ~ ., data = train, hidden = c(3,1), err.fct="ce",
                               linear.output=FALSE, threshold = 0.1, lifesign = "full",
                               stepmax = 1000000) 
  # saveRDS(model_neuralnet, "neuralnet_smote.rds")
  
  # predict trainset
  nn_results_train <- compute(model_neuralnet, subset(train, select = -c(DISEASE)))
  results_train <- data.frame(actual = train$DISEASE, prediction = nn_results_train$net.result)
  roundedresults<-sapply(results_train,round,digits=0)
  roundedresultsdf <- data.frame(roundedresults)
  attach(roundedresultsdf, warn.conflicts = FALSE)
  results_train_data <- table(actual, prediction)
  correct_preds <- results_train_data[1] + results_train_data[4]
  total <- results_train_data[1] + results_train_data[2] + results_train_data[3] + results_train_data[4]
  accuracy_train <- correct_preds/total
  fnr_train <- results_train_data[2]/(results_train_data[2]+results_train_data[4])
  
  # predict testset
  nn_results_test <- compute(model_neuralnet, subset(test, select = -c(DISEASE)))
  results_test <- data.frame(actual = test$DISEASE, prediction = nn_results_test$net.result)
  roundedresults <- sapply(results_test, round, digits = 0)
  roundedresultsdf <- data.frame(roundedresults)
  attach(roundedresultsdf, warn.conflicts = FALSE)
  results_test_data <- table(actual, prediction)
  correct_preds <- results_test_data[1] + results_test_data[4]
  total <- results_test_data[1] + results_test_data[2] + results_test_data[3] + results_test_data[4]
  accuracy_test <- correct_preds/total
  fnr_test <- results_test_data[2]/(results_test_data[2]+results_test_data[4])
  
  # predict entire dataset
  nn_results_all <- compute(model_neuralnet, subset(all, select = -c(DISEASE)))
  results_all <- data.frame(actual = all$DISEASE, prediction = nn_results_all$net.result)                              
  roundedresults <- sapply(results_all, round, digits = 0)
  roundedresultsdf <- data.frame(roundedresults)
  attach(roundedresultsdf, warn.conflicts = FALSE)
  results_all_data <- table(actual, prediction)
  correct_preds <- results_all_data[1] + results_all_data[4]
  total <- results_all_data[1] + results_all_data[2] + results_all_data[3] + results_all_data[4]
  accuracy_all <- correct_preds/total
  fnr_all <- results_all_data[2]/(results_all_data[2]+results_all_data[4])
  
  new_row <- data.frame(chosen_disease, accuracy_train, fnr_train, accuracy_test, fnr_test,
               accuracy_all, fnr_all)
  return(new_row)
}


NeuralNetResults <- data.table('Disease Name' = character(),
                          'Train Accuracy' = numeric(),
                          'FNR (Train)' = numeric(),
                          'Test Accuracy' = numeric(),
                          'FNR (Test)' = numeric(),
                          'Overall Accuracy' = numeric(),
                          'FNR (All)' = numeric())

disease_list = c("MICHD", "CHCCOPD2", "CHCKDNY2", "CVDSTRK3", "DIABETE4")
for (disease in disease_list) {
  new_row <- runNeuralNetModel("FinalCleanedData.csv", disease)
  NeuralNetResults <- rbindlist(list(NeuralNetResults, new_row), use.names = FALSE)
}



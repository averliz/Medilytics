library(data.table)
library(caTools)
library(car)
library(dplyr)
library(performanceEstimation) # for SMOTE
library(ggplot2)
library(caret)
# setwd("C:/Users/mhenn/Documents/Programming/Academic/BC2407 Medilytics")
# setwd("C:/Users/jimmy/NTU/BC2407/Project")
set.seed(2407)

source("functions.R") # Load in the functions 

### Define function to run a logreg model based on a chosen disease ###
# chosen_disease is the selected predictor variable
runLogRegModel <- function(chosen_disease) {
  # restore original data to initial, unaltered state
  data <- readData("FinalCleanedData.csv", chosen_disease)
 
  # create train and test sets with equal proportions of 1's and 0's using 
  train_test_split <- sample.split(data$DISEASE, SplitRatio = 0.7)
  trainset.ori <- subset(data, train_test_split == T)
  testset.ori <- subset(data, train_test_split == F)
  
  ### SMOTE ### - highest accuracy rate. 
  trainset <- smote(DISEASE ~ ., data = trainset.ori,
                    perc.over = 1,k = sqrt(nrow(trainset.ori)), perc.under = 2)
  # write.csv(trainset, paste("SmotedData/", chosen_disease, "_trainset.csv",sep = ""), row.names = FALSE)
  # trainset <- readDataOnly(paste("SmotedData/", chosen_disease, "_trainset.csv",sep = ""))
  # testset <- readDataOnly(paste("SmotedData/", chosen_disease, "_testset_unseen.csv",sep = ""))
  
  testSplitRatio <- ((3/7)*nrow(trainset))/nrow(testset.ori)
  testset_split <- sample.split(testset.ori$DISEASE, SplitRatio = testSplitRatio)
  testset <- subset(testset.ori, testset_split == T)
  # write.csv(testset, paste("SmotedData/", chosen_disease, "_testset_unseen.csv",sep = ""), row.names = FALSE)

  # To check the class distribution of disease in trainset and testset 
  print(table(trainset$DISEASE))
  print(prop.table(table(trainset$DISEASE)))
  print(table(testset$DISEASE))
  print(prop.table(table(testset$DISEASE)))

  # Let us fit all the variable into the Logistic Regression Model
  fitAll <- glm(DISEASE ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH +
                  HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA +
                  RENTHOM1 + VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 +
                  HTM4 + DEAF + BLIND + RFSMOK3 + RFDRHV7 +
                  TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + GRENDA1 + FRNCHDA +
                  POTADA1 + VEGEDA2 + HIVRISK5 + RACE + STATE + AGE,
                  data = trainset, family = "binomial" )
  
  # Let us conduct backward stepwise regression to obtain the optimised model
  stepwise_analysis <- step(fitAll, direction = "backward")
  formula(stepwise_analysis) # quite a few variables were removed
  
  # Build the Logistic Regression Model using the optimised model
  op.LogReg <- glm(formula = stepwise_analysis[["formula"]], data = trainset, family = "binomial")
  # saveRDS(op.LogReg, paste("Models/", chosen_disease, "_LogReg.RDS",sep = ""))
  # op.LogReg <- readRDS(paste("Models/", chosen_disease, "_LogReg.RDS",sep = ""))
  
  # model prediction on train set
  probTrain <- predict.glm(op.LogReg, type = 'response')
  threshold <- optimum_threshold_glm(probTrain, trainset$DISEASE)
  predTrain <- as.factor(ifelse(probTrain > threshold, 1, 0))
  train_cf <- confusionMatrix(predTrain, trainset$DISEASE, positive = "1")
  train_accuracy <- train_cf$overall[1]
  print(train_accuracy)
  recall_train <- train_cf$byClass["Recall"]

  # model prediction on test set scaled 
  probTest <- predict.glm(op.LogReg, newdata = testset, type = 'response')
  predTest <- as.factor(ifelse(probTest > threshold, 1, 0))
  test_cf <- confusionMatrix(predTest, testset$DISEASE, positive = "1")
  test_accuracy <- test_cf$overall[1]
  print(test_accuracy)
  recall_test <- test_cf$byClass["Recall"]

  # Predicting on entire dataset
  probOverall <- predict.glm(op.LogReg, newdata = data, type = 'response')
  predOverall <- as.factor(ifelse(probOverall > threshold, 1, 0))
  # Checking classification accuracy
  overall_cf <- confusionMatrix(predOverall, data$DISEASE, positive = "1")
  overall_accuracy <- overall_cf$overall[1]
  print(overall_accuracy)
  recall_overall <- overall_cf$byClass["Recall"]
  
  cat(" Disease being analyzed is:", chosen_disease
      ,'\n',"Accuracy on Trainset:", train_accuracy
      ,'\n',"Recall (Train):", recall_train
      ,'\n',"Accuracy on Testset:", test_accuracy
      ,'\n',"Recall (Test):", recall_test
      ,'\n',"Accuracy on entire dataset:", overall_accuracy
      ,'\n',"Recall (Overall):", recall_overall)
  new_row <- data.frame(chosen_disease, 
                        train_accuracy,
                        recall_train,
                        test_accuracy,
                        recall_test,
                        overall_accuracy,
                        recall_overall)
  return(new_row)
  
}
# Create the empty table to hold all the data
LogRegResults <- data.table('Disease Name' = character(),
                            'Train Accuracy' = numeric(),
                            'Recall (Train)' = numeric(),
                            'Test Accuracy' = numeric(),
                            'Recall (Test)' = numeric(),
                            'Overall Accuracy' = numeric(),
                            'Recall (Overall)' = numeric())

# list of diseases to parse through the model
disease_list = c("MICHD","CHCCOPD2", "CHCKDNY2", "CVDSTRK3", "DIABETE4")

for (disease in disease_list) {
  new_row <- runLogRegModel(disease)
  LogRegResults <- rbindlist(list(LogRegResults, new_row), use.names = FALSE)
}

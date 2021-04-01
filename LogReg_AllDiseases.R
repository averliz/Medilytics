library(data.table)
library(caTools)
library(car)
library(dplyr)
library(performanceEstimation) # for SMOTE
library(randomForest)
library(ROSE) # for Rose 
library(shapper)
library(ggplot2)
library(caret)
# setwd("C:/Users/mhenn/Documents/Programming/Academic/BC2407 Medilytics")
setwd("C:/Users/jimmy/NTU/BC2407/Project")
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
  # trainset <- smote(DISEASE ~ ., data = trainset.ori,
  #                   perc.over = 1,k = 5, perc.under = 2)
  # write.csv(trainset, paste("SmotedData/", chosen_disease, "_trainset_pe.csv",sep = ""), row.names = FALSE)
  # testset.smote <- smote(DISEASE ~ ., data = testset.ori,
  #                  perc.over = 1, k = 5, perc.under = 2)
  # write.csv(testset.smote, paste("SmotedData/", chosen_disease, "_testset_pe.csv",sep = ""), row.names = FALSE)

  trainset <- readDataOnly(paste("SmotedData/", chosen_disease, "_trainset_pe.csv",sep = ""))
  testset.smote <- readDataOnly(paste("SmotedData/", chosen_disease, "_testset_pe.csv",sep = ""))
  
  testSplitRatio <- ((3/7)*nrow(trainset))/nrow(testset.ori)
  print(testSplitRatio)
  testset_split <- sample.split(testset.ori$DISEASE, SplitRatio = testSplitRatio)
  testset.scaled <- subset(testset.ori, testset_split == T)
  print(table(trainset$DISEASE))
  print(prop.table(table(trainset$DISEASE)))
  print(table(testset.smote$DISEASE))
  print(prop.table(table(testset.smote$DISEASE)))
  print(table(testset.scaled$DISEASE))
  print(prop.table(table(testset.scaled$DISEASE)))

  fitAll <- glm(DISEASE ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH + 
                  HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA + 
                  RENTHOM1 + VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 + 
                  HTM4 + DEAF + BLIND + RFSMOK3 + RFDRHV7 + 
                  TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + GRENDA1 + FRNCHDA + 
                  POTADA1 + VEGEDA2 + HIVRISK5 + RACE + STATE + AGE, data = trainset, family = "binomial" )
  
  stepwise_analysis <- step(fitAll, direction = "backward")
  formula(stepwise_analysis) # quite a few variables were removed
  
  # first model: logistic regression
  m1 <- glm(formula = stepwise_analysis[["formula"]], data = trainset, family = "binomial")
  saveRDS(m1, paste("Models/", chosen_disease, "_LogReg.RDS",sep = ""))
  # m1 <- readRDS(paste("Models/", chosen_disease, "_LogReg.RDS",sep = ""))
  
  # model prediction on train set
  probTrain <- predict.glm(m1, type = 'response')
  # threshold <- 0.5
  threshold <- optimum_threshold_glm(probTrain, trainset$DISEASE)
  predTrain <- as.factor(ifelse(probTrain > threshold, 1, 0))
  train_cf <- confusionMatrix(predTrain, trainset$DISEASE)
  train_accuracy <- train_cf$overall[1]
  print(train_accuracy)
  fnr_train <- fnr(train_cf)
   
  
  # model prediction on test set Smoted 
  # prob <- predict.glm(m1, newdata = testset, type = 'response')
  # y.hat <- ifelse(prob > threshold, 1, 0)
  # table(testset$DISEASE, y.hat, deparse.level = 2)
  # test_accuracy <- mean(y.hat == testset$DISEASE) # 74.2% accuracy on testset
  # plot(testset$MICHD,prob, data = testset)
  # print(plot(testset$MICHD,prob, data = testset))
  probTestSmote <- predict.glm(m1, newdata = testset.smote, type = 'response')
  predTestSmote <- as.factor(ifelse(probTestSmote > threshold, 1, 0))
  test_smote_cf <- confusionMatrix(predTestSmote, testset.smote$DISEASE)
  test_smote_accuracy <- test_smote_cf$overall[1]
  print(test_smote_accuracy)
  fnr_test_smote <- fnr(test_smote_cf)

  # model prediction on test set scaled 
  probTestScaled <- predict.glm(m1, newdata = testset.scaled, type = 'response')
  predTestScaled <- as.factor(ifelse(probTestScaled > threshold, 1, 0))
  test_scaled_cf <- confusionMatrix(predTestScaled, testset.scaled$DISEASE)
  test_scaled_accuracy <- test_scaled_cf$overall[1]
  print(test_scaled_accuracy)
  fnr_test_scaled <- fnr(test_scaled_cf)


  
  # model prediction on entire dataset
  # prob <- predict.glm(m1, newdata = data, type = 'response')
  # y.hat <- ifelse(prob > threshold, 1, 0)
  # table(data$DISEASE, y.hat, deparse.level = 2)
  # overall_accuracy <- mean(y.hat == data$DISEASE) # 73% overall accuracy

  # Predicting on entire dataset
  probOverall <- predict.glm(m1, newdata = data, type = 'response')
  predOverall <- as.factor(ifelse(probOverall > threshold, 1, 0))
  # Checking classification accuracy
  overall_cf <- confusionMatrix(predOverall, data$DISEASE)
  overall_accuracy <- overall_cf$overall[1]
  print(overall_accuracy)
  fnr_overall <- fnr(overall_cf)
  
  cat(" Disease being analyzed is:", chosen_disease
      ,'\n',"Accuracy on Trainset:", train_accuracy
      ,'\n',"False Negative Rate (Trainset):", fnr_train
      ,'\n',"Accuracy on Testset_Smote:", test_smote_accuracy
      ,'\n',"False Negative Rate (Testset_Smote):", fnr_test_smote
      ,'\n',"Accuracy on Testset_Scaled:", test_scaled_accuracy
      ,'\n',"False Negative Rate (Testset_Scaled):", fnr_test_scaled
      ,'\n',"Accuracy on entire dataset:", overall_accuracy
      ,'\n',"False Negative Rate (Overall):", fnr_overall)
  new_row <- data.frame(chosen_disease, 
                        train_accuracy,
                        fnr_train,
                        test_smote_accuracy,
                        fnr_test_smote,
                        test_scaled_accuracy,
                        fnr_test_scaled,
                        overall_accuracy,
                        fnr_overall)
  return(new_row)
  
}
# Create the empty table to hold all the data
LogRegResults <- data.table('Disease Name' = character(),
                            'Train Accuracy' = numeric(),
                            'FNR (Train)' = numeric(),
                            'Test (SMOTE) Accuracy' = numeric(),
                            'FNR (Test SMOTE)' = numeric(),
                            'Test (Scaled) Accuracy' = numeric(),
                            'FNR (Test Scaled)' = numeric(),
                            'Overall Accuracy' = numeric(),
                            'FNR (Overall)' = numeric())
# list of diseases to parse through the model
disease_list = c("MICHD", "CHCCOPD2", "CHCKDNY2", "CVDSTRK3", "DIABETE4")

for (disease in disease_list) {
  new_row <- runLogRegModel(disease)
  LogRegResults <- rbindlist(list(LogRegResults, new_row), use.names = FALSE)
}



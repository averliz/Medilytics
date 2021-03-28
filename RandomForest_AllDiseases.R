library(data.table)
library(caTools)
library(car)
library(dplyr)
library(performanceEstimation) # for SMOTE
library(randomForest)
library(ROSE) # for Rose 
library(caret)
# setwd("C:/Users/mhenn/Documents/Programming/Academic/BC2407 Medilytics")
setwd("C:/Users/jimmy/NTU/BC2407/Project")
set.seed(2407)

source("functions.R") # Load in the functions 

### Define function to run a logreg model based on a chosen disease ###
# chosen_disease is the selected predictor variable
runRFModel <- function(chosen_disease) {
  # restore original data to initial, unaltered state
  data <- readData("FinalCleanedData.csv", chosen_disease)
  
  
  # Get the relevant columns for the chosen disease prediction (all predictors)
  
  interest_cols <- c("DISEASE", "SEXVAR", "GENHLTH", "PHYS14D", "MENT14D", "POORHLTH", 
                     "HLTHPLN1", "PERSDOC2", "MEDCOST", "CHECKUP1", "MARITAL", "EDUCA", 
                     "RENTHOM1", "VETERAN3", "EMPLOY1", "CHLDCNT", "INCOME2", "WTKG3", 
                     "HTM4", "DEAF", "BLIND", "RFSMOK3", "RFDRHV7", 
                     "TOTINDA", "STRFREQ", "FRUTDA2", 'FTJUDA2', "GRENDA1", "FRNCHDA", 
                     "POTADA1", "VEGEDA2", "HIVRISK5" )
  
  data_subset <- data[, ..interest_cols]

  # create train and test sets with equal proportions of 1's and 0's using 
  train_test_split <- sample.split(data_subset$DISEASE, SplitRatio = 0.7)
  trainset.ori <- subset(data_subset, train_test_split == T)
  testset.ori <- subset(data_subset, train_test_split == F)
  
  ### SMOTE ### - highest accuracy rate. 
  trainset <- smote(DISEASE ~ ., data = trainset.ori,
                    perc.over = 1,k = 5, perc.under = 2)

  testset.smote <- smote(DISEASE ~ ., data = testset.ori,
                   perc.over = 1, k = 5, perc.under = 2)

  # trainset <- readDataOnly(paste("SmotedData/", chosen_disease, "_trainset_pe.csv",sep = ""))
  # testset.smote <- readDataOnly(paste("SmotedData/", chosen_disease, "_testset_pe.csv",sep = ""))
  
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
  set.seed(2407)
 
  # Tune the Random Forest Model - 
  # Use TuneRF to obtain the optimum RF model 
  #   "doBest = TRUE" -> returns the model 
  set.seed(2407)
  # op.rf <- tuneRF(x = trainset[,c(2:32)],
  #             y = trainset$DISEASE,
  #             ntreeTry = 1000,
  #             mtryStart = 5,
  #             stepFactor = 1.5,
  #             improve    = 0.01,
  #             doBest = TRUE
  # )
  
  
  # optimum m-try (tuneRF)
  # MICHD - mtry = 4 
  # CHCCOPD2 - mtry = 5
  # CHCKDNY2 - mtry = 4
  # CVDSTRK3 - mtry = 5 
  # DIABETE4 - mtry = 4
  

  # op.rf <-randomForest(DISEASE ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH +
  #                 HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA +
  #                 RENTHOM1 + VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 +
  #                 HTM4 + DEAF + BLIND + RFSMOK3 + RFDRHV7 +
  #                 TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + GRENDA1 + FRNCHDA +
  #                 POTADA1 + VEGEDA2 + HIVRISK5, data = trainset,
  #                 mtry=op.mtry, importance=T,ntree=500)
  
  # saveRDS(op.rf, paste("Models/", chosen_disease, "_RF_.rds",sep = ""))
  op.rf <- readRDS(paste("RF_Models/", chosen_disease, "_RF_.rds",sep = ""))
  
  
  #Evaluate variable importance
  importance(op.rf)
  varImpPlot(op.rf)

  # Predicting on train set
  predTrain <- predict(op.rf, trainset)
  # Checking classification accuracy
  train_cf <- confusionMatrix(predTrain, trainset$DISEASE)
  train_accuracy <- train_cf$overall[1]
  print(train_accuracy)
  fnr_train <- fnr(train_cf)
    
  # Predicting on test set - smote
  predTestSmote <- predict(op.rf, testset.smote)
  # Checking classification accuracy
  test_smote_cf <- confusionMatrix(predTestSmote, testset.smote$DISEASE)
  test_smote_accuracy <- test_smote_cf$overall[1]
  print(test_smote_accuracy)
  fnr_test_smote <- fnr(test_smote_cf)

  # Predicting on test set - scaled
  predTestScaled <- predict(op.rf, testset.scaled)
  # Checking classification accuracy
  test_scaled_cf <- confusionMatrix(predTestScaled, testset.scaled$DISEASE)
  test_scaled_accuracy <- test_scaled_cf$overall[1]
  print(test_scaled_accuracy)
  fnr_test_scaled <- fnr(test_scaled_cf)

  # Predicting on entire dataset
  predOverall <- predict(op.rf, data)
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
RandForestResults <- data.table('Disease Name' = character(),
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

# Start the clock!
ptm <- proc.time()

for (disease in disease_list) {
  new_row <- runRFModel(disease)
  RandForestResults <- rbindlist(list(RandForestResults, new_row), use.names = FALSE)
}

# Stop the clock
proc.time() - ptm

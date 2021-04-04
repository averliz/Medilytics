library(data.table)
library(caTools)
library(car)
library(dplyr)
library(performanceEstimation) # for SMOTE
library(randomForest)
library(ggplot2)
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
  
    # create train and test sets with equal proportions of 1's and 0's using 
  train_test_split <- sample.split(data$DISEASE, SplitRatio = 0.7)
  trainset.ori <- subset(data, train_test_split == T)
  testset.ori <- subset(data, train_test_split == F)
  
  # SMOTE ### - highest accuracy rate.
  # trainset <- smote(DISEASE ~ ., data = trainset.ori,
  #                   perc.over = 1,k = sqrt(nrow(trainset.ori)), perc.under = 2)
  # write.csv(trainset, paste("SmotedData/", chosen_disease, "knn_trainset_pe.csv",sep = ""), row.names = FALSE)

  trainset <- readDataOnly(paste("SmotedData/", chosen_disease, "knn_trainset_pe.csv",sep = ""))
  
  testSplitRatio <- ((3/7)*nrow(trainset))/nrow(testset.ori)
  print(testSplitRatio)
  testset_split <- sample.split(testset.ori$DISEASE, SplitRatio = testSplitRatio)
  testset <- subset(testset.ori, testset_split == T)
  
  # To check the class distribution of disease in trainset and testset 
  print(table(trainset$DISEASE))
  print(prop.table(table(trainset$DISEASE)))
  print(table(testset$DISEASE))
  print(prop.table(table(testset$DISEASE)))
  set.seed(2407)
 
  # Tune the Random Forest Model - 
  # Use TuneRF to obtain the optimum RF model 
    # mtry <- tuneRF(x = trainset[,c(2:36)],
  #             y = trainset$DISEASE,
  #             ntreeTry = 2000,
  #             mtryStart = 5,
  #             stepFactor = 1.5,
  #             improve    = 1e-05
  # )
  # op.mtry <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

# op.rf <-randomForest(DISEASE ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH +
#                 HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA +
#                 RENTHOM1 + VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 +
#                 HTM4 + DEAF + BLIND + RFSMOK3 + RFDRHV7 +
#                 TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + GRENDA1 + FRNCHDA +
#                 POTADA1 + VEGEDA2 + HIVRISK5, data = trainset,
#                 mtry=op.mtry, importance=T,ntree=1000) # CHANGE ntree
  
  # saveRDS(op.rf, paste("Models/", chosen_disease, "knn_RF_.rds",sep = ""))
  op.rf <- readRDS(paste("Models/", chosen_disease, "knn_RF_.rds",sep = ""))
  # op.rf <- readRDS(paste("Models/", chosen_disease, "_RF_.rds",sep = ""))
  
  # Let us look at the variable importance of each of the disease
  var_imp_df <- importance(op.rf) %>% 
    data.frame() %>% 
    mutate(feature = row.names(.)) 
  
  # Viewing the variable importance in a plot
  varImptPlot <- ggplot(var_imp_df, aes(x = reorder(feature, MeanDecreaseAccuracy ), 
                         y = MeanDecreaseAccuracy )) +
    geom_bar(stat='identity') +
    coord_flip() +
    theme_classic() +
    labs(
      x     = chosen_disease,
      y     = "MeanDecreaseAccuracy",
      title = paste("Variable Importance for ", chosen_disease, sep = "")
    )
  print(varImptPlot)
  # ggsave(paste("RFPlots/", chosen_disease, "_VarImpPlot.png",sep = ""))
  
  # Predicting on train set
  predTrain <- predict(op.rf, trainset)
  # Checking classification accuracy
  train_cf <- confusionMatrix(predTrain, trainset$DISEASE)
  train_accuracy <- train_cf$overall[1]
  print(train_accuracy)
  fnr_train <- fnr(train_cf)
    
  # Predicting on test set - scaled to keep the 3:7 ratio with the trainset
  predTest <- predict(op.rf, testset)
  # Checking classification accuracy
  test_cf <- confusionMatrix(predTest, testset$DISEASE)
  test_accuracy <- test_cf$overall[1]
  print(test_accuracy)
  fnr_test <- fnr(test_cf)

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
      ,'\n',"Accuracy on Testset:", test_accuracy
      ,'\n',"False Negative Rate (Testset):", fnr_test
      ,'\n',"Accuracy on entire dataset:", overall_accuracy
      ,'\n',"False Negative Rate (Overall):", fnr_overall)
  new_row <- data.frame(chosen_disease, 
                        train_accuracy,
                        fnr_train,
                        test_accuracy,
                        fnr_test,
                        overall_accuracy,
                        fnr_overall)
  return(new_row)
  
}
# Create the empty table to hold all the data
RandForestResults <- data.table('Disease Name' = character(),
                            'Train Accuracy' = numeric(),
                            'FNR (Train)' = numeric(),
                            'Test  Accuracy' = numeric(),
                            'FNR (Test)' = numeric(),
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

readSmoteData <- function(path, chosen_disease) {
  data <- fread(path)
  
  all_variables <- names(data)
  
  numeric_variables <- c("POORHLTH",
                         "WTKG3", 
                         "HTM4",
                         "STRFREQ",
                         "FRUTDA2",
                         "FTJUDA2",
                         "GRENDA1",
                         "FRNCHDA",
                         "POTADA1",
                         "VEGEDA2"
  )
  
  factor_variables <- setdiff(all_variables, numeric_variables)
  
  for (variable in factor_variables) {
    set(data, j = variable, value = as.factor(data[[variable]]))
  }
  
  data <- data %>% 
    rename(
      'DISEASE' = chosen_disease
    )
  
  return(data)
}

data <- readSmoteData("FinalCleanedData.csv", 'MICHD')

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
# trainset <- smote(DISEASE ~ ., data = trainset.ori,
#                   perc.over = 1,k = 5, perc.under = 2)
trainset <- readSmoteData("smotetrain_PE.csv", "MICHD")
# testset.smote <- smote(DISEASE ~ ., data = testset.ori,
#                        perc.over = 1, k = 5, perc.under = 2)
testset.smote <- readSmoteData("smotetest_PE.csv", "MICHD")
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

rf1 <- randomForest(DISEASE ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH + 
                      HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA + 
                      RENTHOM1 + VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 + 
                      HTM4 + DEAF + BLIND + RFSMOK3 + RFDRHV7 + 
                      TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + GRENDA1 + FRNCHDA + 
                      POTADA1 + VEGEDA2 + HIVRISK5, data = trainset,  importance= T)

print(rf1)

# Tune the Random Forest Model - find optimal mtry and optimal ntrees
set.seed(2407)
mtry <- tuneRF(x = trainset[,c(2:32)], 
               y = trainset$DISEASE, 
               ntreeTry = 1000, 
               mtryStart = 5, 
               stepFactor = 1.5,
               improve    = 0.01,   # to not show real-time progress 
               doBest = TRUE
)
mtry
op.mtry <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(op.mtry)

op.rf <-randomForest(DISEASE ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH + 
                       HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA + 
                       RENTHOM1 + VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 + 
                       HTM4 + DEAF + BLIND + RFSMOK3 + RFDRHV7 + 
                       TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + GRENDA1 + FRNCHDA + 
                       POTADA1 + VEGEDA2 + HIVRISK5, data = trainset, 
                     mtry=op.mtry, importance=T,ntree=500)
print(op.rf)
#Evaluate variable importance
importance(op.rf)
varImpPlot(op.rf)
op.rf <- mtry

# Predicting on train set
predTrain <- predict(op.rf, trainset)
# Checking classification accuracy
table(predTrain, trainset$DISEASE)
train_accuracy <- mean(predTrain == trainset$DISEASE) 
print(train_accuracy)

# Predicting on test set - smote
predTestSmote <- predict(op.rf, testset.smote)
# Checking classification accuracy
table(predTestSmote, testset.smote$DISEASE)
test_smote_accuracy <- mean(predTestSmote == testset.smote$DISEASE) 
print(test_smote_accuracy)
confusionMatrix(predTestSmote, testset.smote$DISEASE)

# Predicting on test set - scaled
predTestScaled <- predict(op.rf, testset.scaled)
# Checking classification accuracy
table(predTestScaled, testset.scaled$DISEASE)
test_scaled_accuracy <- mean(predTestScaled == testset.scaled$DISEASE) 
print(test_scaled_accuracy)
confusionMatrix(predTestScaled, testset.scaled$DISEASE)

# Predicting on entire dataset
predOverall <- predict(op.rf, data)
# Checking classification accuracy
table(predOverall, data$DISEASE)
overall_accuracy <- mean(predOverall == data$DISEASE) 
print(overall_accuracy)
confusionMatrix(predOverall, data$DISEASE)


cat(" Disease being analyzed is:", chosen_disease
    ,'\n',"Accuracy on Trainset:", train_accuracy
    ,'\n',"Accuracy on Testset_Smote:", test_accuracy
    ,'\n',"Accuracy on Testset_Scaled:", test_accuracy
    ,'\n',"Accuracy on entire dataset:", overall_accuracy)
new_row <- data.frame(chosen_disease, 
                      train_accuracy, 
                      test_smote_accuracy,
                      test_scaled_accuracy,
                      overall_accuracy)
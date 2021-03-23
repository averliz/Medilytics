library(data.table)
library(caTools)
library(car)
library(dplyr)
# library(DMwR) # for SMOTE
library(performanceEstimation)
library(ROSE)
library(randomForest)
setwd("C:/Users/jimmy/NTU/BC2407/Project")
set.seed(2407)

### Read in data and factorize the relevant variables ###
resetData <- function() {
  data <- fread("FinalCleanedData.csv")
  
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
  return(data)
}

data <- resetData()

interest_cols <- c("MICHD", "SEXVAR", "GENHLTH", "PHYS14D", "MENT14D", "POORHLTH",
                   "HLTHPLN1", "PERSDOC2", "MEDCOST", "CHECKUP1", "MARITAL", "EDUCA",
                   "RENTHOM1", "VETERAN3", "EMPLOY1", "CHLDCNT", "INCOME2", "WTKG3",
                   "HTM4", "DEAF", "BLIND", "RFSMOK3", "RFDRHV7",
                   "TOTINDA", "STRFREQ", "FRUTDA2", 'FTJUDA2', "GRENDA1", "FRNCHDA",
                   "POTADA1", "VEGEDA2", "HIVRISK5" )

data_subset <- data[, ..interest_cols]

# Let's split the data into train and test set
summary(data_subset$MICHD)  
MICHD.table <- table(data_subset$MICHD)
round(prop.table(MICHD.table), digits = 2) # The data is very skewed 


train_test_split <- sample.split(data_subset$MICHD, SplitRatio = 0.7)
trainset.ori <- subset(data_subset, train_test_split == T)
testset.ori <- subset(data_subset, train_test_split == F)
summary(trainset.ori$MICHD)
prop.table(table(trainset.ori$MICHD))
summary(testset.ori$MICHD)
prop.table(table(testset.ori$MICHD))

# # SMOTE - train- test split 
# library(DMwR)

smote.trainset <- smote(MICHD ~ ., data = trainset.ori,
                        perc.over = 1, perc.under = 2)
smote.testset <- smote(MICHD ~ ., data = testset.ori,
                       perc.over = 1, perc.under = 2)
table(smote.trainset$MICHD)
table(smote.testset$MICHD)
prop.table(table(smote.trainset$MICHD))
prop.table(table(smote.testset$MICHD))


rf1 <- randomForest(MICHD ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH + 
                     HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA + 
                     RENTHOM1 + VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 + 
                     HTM4 + DEAF + BLIND + RFSMOK3 + RFDRHV7 + 
                     TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + GRENDA1 + FRNCHDA + 
                     POTADA1 + VEGEDA2 + HIVRISK5, data = smote.trainset, 
                   ntree = 1000, importance = T)

rf1

# Predicting on train set
predTrain <- predict(rf, smote.trainset, type = "class")
# Checking classification accuracy
table(predTrain, smote.trainset$MICHD) 

# Predicting on Validation set
predTest <- predict(rf, smote.testset, type = "class")
# Checking classification accuracy
mean(predTest == smote.testset$MICHD)    
table(predTest,smote.testset$MICHD)

# To check important variables
var.impt.rf1 <- importance(rf1)
var.impt.rf1

varImpPlot(rf1, type = 1)


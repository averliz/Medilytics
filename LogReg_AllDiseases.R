library(data.table)
library(caTools)
library(car)
library(dplyr)
setwd("C:/Users/mhenn/Documents/Programming/Academic/BC2407 Medilytics")
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

### Define function to run a logreg model based on a chosen disease ###
# chosen_disease is the selected predictor variable
runLogRegModel <- function(chosen_disease) {
  # restore original data to initial, unaltered state
  data <- resetData()
  # Rename selected variable in data.table to 'DISEASE'
  data <- data %>% 
    rename(
      'DISEASE' = chosen_disease
    )
  
  
  # Get the relevant columns for the chosen disease prediction (all predictors)
  
  interest_cols <- c("DISEASE", "SEXVAR", "GENHLTH", "PHYS14D", "MENT14D", "POORHLTH", 
                  "HLTHPLN1", "PERSDOC2", "MEDCOST", "CHECKUP1", "MARITAL", "EDUCA", 
                  "RENTHOM1", "VETERAN3", "EMPLOY1", "CHLDCNT", "INCOME2", "WTKG3", 
                  "HTM4", "DEAF", "BLIND", "RFSMOK3", "RFDRHV7", 
                  "TOTINDA", "STRFREQ", "FRUTDA2", 'FTJUDA2', "GRENDA1", "FRNCHDA", 
                  "POTADA1", "VEGEDA2", "HIVRISK5" )
  
  data_subset <- data[, ..interest_cols]
  positiveDiseaseData <- data_subset[DISEASE == 1]
  negativeDiseaseData <- data_subset[DISEASE == 0][sample(.N, nrow(positiveDiseaseData))]
  # for logistic regression, it is important to obtain an unbiased set of data
  # this means we obtain a set of data with equal proportions of positive and
  # negative disease status
  disease_data <- merge(positiveDiseaseData, negativeDiseaseData, all = TRUE)
  
  # create training and testing sets with an equal number of positive and negative stroke cases
  train <- sample.split(Y = disease_data$DISEASE, SplitRatio = 0.7)
  trainset <- subset(disease_data, train == T)
  testset <- subset(disease_data, train == F)
  
  fitAll <- glm(DISEASE ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH + 
                  HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA + 
                  RENTHOM1 + VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 + 
                  HTM4 + DEAF + BLIND + RFSMOK3 + RFDRHV7 + 
                  TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + GRENDA1 + FRNCHDA + 
                  POTADA1 + VEGEDA2 + HIVRISK5, data = trainset, family = "binomial" )
  
  stepwise_analysis <- step(fitAll, direction = "backward")
  formula(stepwise_analysis) # quite a few variables were removed
  
  # first model: logistic regression
  m1 <- glm(formula = stepwise_analysis[["formula"]], data = trainset, family = "binomial")
  
  
  # model prediction on train set
  prob <- predict(m1, type = 'response')
  threshold <- 0.5
  y.hat <- ifelse(prob > threshold, 1, 0)
  table(trainset$DISEASE, y.hat, deparse.level = 2)
  train_accuracy <- mean(y.hat == trainset$DISEASE) # 74% accuracy on trainset
  
  
  # model prediction on test set
  prob <- predict.glm(m1, newdata = testset, type = 'response')
  y.hat <- ifelse(prob > threshold, 1, 0)
  table(testset$DISEASE, y.hat, deparse.level = 2)
  test_accuracy <- mean(y.hat == testset$DISEASE) # 74.2% accuracy on testset
  
  # model prediction on entire dataset
  prob <- predict.glm(m1, newdata = data, type = 'response')
  y.hat <- ifelse(prob > threshold, 1, 0)
  table(data$DISEASE, y.hat, deparse.level = 2)
  overall_accuracy <- mean(y.hat == data$DISEASE) # 73% overall accuracy
  
  cat(" Disease being analyzed is:", chosen_disease
      ,'\n',"Accuracy on Trainset:", train_accuracy
      ,'\n',"Accuracy on Testset:", test_accuracy
      ,'\n',"Accuracy on entire dataset:", overall_accuracy)
  new_row <- data.frame(chosen_disease, train_accuracy, test_accuracy, overall_accuracy)
  return(new_row)
  
}
# Create the empty table to hold all the data
LogRegResults <- data.table('Disease Name' = character(),
                            'Train Accuracy' = numeric(),
                            'Test Accuracy' = numeric(),
                            'Overall Accuracy' = numeric())

# list of diseases to parse through the model
disease_list = c("MICHD", "CHCCOPD2", "CHCKDNY2", "CVDSTRK3", "DIABETE4")

for (disease in disease_list) {
  new_row <- runLogRegModel(disease)
  LogRegResults <- rbindlist(list(LogRegResults, new_row), use.names = FALSE)
}






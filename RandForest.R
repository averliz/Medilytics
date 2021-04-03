library(data.table)
library(caTools)
library(car)
library(dplyr)
library(performanceEstimation) # for SMOTE
library(randomForest)
library(ROSE) # for Rose 
library(caret)
library(ggplot2)
library(ggcorrplot)
# setwd("C:/Users/mhenn/Documents/Programming/Academic/BC2407 Medilytics")
setwd("C:/Users/jimmy/NTU/BC2407/Project")
set.seed(2407)
source("functions.R")

# x <- table(subset(data$RFDRHV7, data$DISEASE == 1 | data$AGE >50 ))
# x_data <- data[which(data$DISEASE == 1 & data$STATE == ""), ]
# table(data$DISEASE)
# table(x_data$DISEASE)
# table(x_data$RFDRHV7)
# prop.table(table(x_data$RFDRHV7))
# 
# prop.table(x)
# 
# y <- table(data$RFDRHV7)
# y
# prop.table(y)


# Cor plots 
# df_num <- select_if(data, is.numeric)
# df_num$DISEASE <- data$DISEASE
# df_factor <- data %>% select("DISEASE", "RACE" )
# 
# race_name <- c("White", "Hispanic", "Black", "Asian", "Multiracial", "American Indian", "Native Hawaiian", "Other")
# # data_test <- data
# i = 1
# for (name in race_name) {
#     data[RACE == i, RACE := race_name[i]]
#     i = i + 1
# }
# 
# 
# model.matrix(~0+., data=df_factor) %>% 
#     cor(use="pairwise.complete.obs") %>% 
#     ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
# 
# mm <- model.matrix(~0+., data=df_factor) %>% 
#          cor(use="pairwise.complete.obs")
# mm 
# 
# # calulate the correlations
# r <- cor(df_num, use="complete.obs")
# round(r,2)
# ggcorrplot(r)

chosen_disease <- "MICHD"
# restore original data to initial, unaltered state
data <- readData("FinalCleanedData.csv", chosen_disease)

# create train and test sets with equal proportions of 1's and 0's using 
train_test_split <- sample.split(data$DISEASE, SplitRatio = 0.7)
trainset.ori <- subset(data, train_test_split == T)
testset.ori <- subset(data, train_test_split == F)

## SMOTE ### - highest accuracy rate.
# trainset <- smote(DISEASE ~ ., data = trainset.ori,
                  # perc.over = 1,k = 6, perc.under = 2)
# write.csv(trainset, paste("SmotedData/", chosen_disease, "_kNN_trainset_pe.csv",sep = ""), row.names = FALSE)

# trainset <- readDataOnly(paste("SmotedData/", chosen_disease, "_trainset_pe.csv",sep = ""))
trainset <- readDataOnly(paste("SmotedData/", chosen_disease, "knn391_trainset_pe.csv",sep = ""))


testSplitRatio <- ((3/7)*nrow(trainset))/nrow(testset.ori)
print(testSplitRatio)
testset_split <- sample.split(testset.ori$DISEASE, SplitRatio = testSplitRatio)
testset.scaled <- subset(testset.ori, testset_split == T)
print(table(trainset$DISEASE))
print(prop.table(table(trainset$DISEASE)))
print(table(testset.scaled$DISEASE))
print(prop.table(table(testset.scaled$DISEASE)))
set.seed(2407)

# Tune the Random Forest Model - 
# Use TuneRF to obtain the optimum RF model 
#   "doBest = TRUE" -> returns the model 
set.seed(2407)
mtry <- tuneRF(x = trainset[,c(2:36)],
               y = trainset$DISEASE,
               ntreeTry = 2000,
               mtryStart = 15,
               stepFactor = 1,
               improve    = 0.01
)
op.mtry <- mtry[mtry[, 2] == min(mtry[, 2]), 1]


# optimum m-try (tuneRF)
# MICHD - mtry = 4 
# CHCCOPD2 - mtry = 5
# CHCKDNY2 - mtry = 4
# CVDSTRK3 - mtry = 5 
# DIABETE4 - mtry = 4


op.rf <-randomForest(DISEASE ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH +
                         HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA +
                         RENTHOM1 + VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 +
                         HTM4 + DEAF + BLIND + RFSMOK3 + RFDRHV7 +
                         TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + GRENDA1 + FRNCHDA +
                         POTADA1 + VEGEDA2 + HIVRISK5, data = trainset,
                     mtry=op.mtry, importance=T,ntree=1000) # CHANGE ntree

saveRDS(op.rf, paste("Models/", chosen_disease, "_RF_.rds",sep = ""))
# op.rf <- readRDS(paste("Models/", chosen_disease, "_RF_.rds",sep = ""))


#Evaluate variable importance
importance(op.rf)
# make dataframe from importance() output
var_imp_df <- importance(op.rf) %>% 
    data.frame() %>% 
    mutate(feature = row.names(.)) 

# plot Var Importance 
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
ggsave(paste("RFPlots/", chosen_disease, "_VarImpPlot.png",sep = ""))


# Predicting on train set
predTrain <- predict(op.rf, trainset)
# Checking classification accuracy
train_cf <- confusionMatrix(predTrain, trainset$DISEASE)
train_accuracy <- train_cf$overall[1]
print(train_accuracy)
fnr_train <- fnr(train_cf)

# Predicting on test set - Unscaled 
predTest <- predict(op.rf, testset.ori)
# Checking classification accuracy
test_cf <- confusionMatrix(predTest, testset.ori$DISEASE)
test_accuracy <- test_cf$overall[1]
print(test_accuracy)
fnr_test <- fnr(test_cf)

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
    ,'\n',"Accuracy on Testset:", test_accuracy
    ,'\n',"False Negative Rate (Testset):", fnr_test
    ,'\n',"Accuracy on Testset_Scaled:", test_scaled_accuracy
    ,'\n',"False Negative Rate (Testset_Scaled):", fnr_test_scaled
    ,'\n',"Accuracy on entire dataset:", overall_accuracy
    ,'\n',"False Negative Rate (Overall):", fnr_overall
    ,'\n',"Optimum m_try", op.mtry)
new_row <- data.frame(chosen_disease, 
                      train_accuracy,
                      fnr_train,
                      test_accuracy,
                      fnr_test,
                      test_scaled_accuracy,
                      fnr_test_scaled,
                      overall_accuracy,
                      fnr_overall,
                      op.mtry)

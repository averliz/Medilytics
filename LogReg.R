library(car)
library(data.table)
library(skimr)
library(caTools)
library(dplyr)
library(alookr) # for split_by
library(caret)
library(e1071)

setwd("C:/Users/jimmy/NTU/BC2407/Project")

########## Importing the data ##########
data <- fread("FinalCleanedData.csv")

# Set the variables to their appropriate data type 
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

# perform sanity check on variables - make sure factor levels are accurate
str(data)

data <- na.omit(data) # all in all, there are 230423 observations with no NA values in them at all


# 'SEXVAR','GENHLTH','PHYS14D','MENT14D','POORHLTH','HLTHPLN1',
# 'PERSDOC2','MEDCOST','CHECKUP1','MARITAL','EDUCA','RENTHOM1',
# 'VETERAN3','EMPLOY1','CHLDCNT','INCOME2','WTKG3','HTM4','PREGNANT',
# 'DEAF','BLIND','RFSMOK3','RFDRHV7','TOTINDA','STRFREQ','FRUTDA2',
# 'FTJUDA2','GRENDA1','FRNCHDA','POTADA1','VEGEDA2','HIVRISK5'


########## Logistic regression Model ##########
##### Preparing the data - train test split #####
# Let us try to predict MICHD - Presence of Heart Disease (HD)
library(alookr)
library(ROSE)

# Let's set the seed for repeatability
set.seed(2407)

# Let's split the data into train and test set
summary(data$MICHD)  
MICHD.table <- table(data$MICHD)
round(prop.table(MICHD.table), digits = 2) # The data is very skewed 
barplot(prop.table(MICHD.table), 
        col = rainbow(2),
        ylim = c(0,1),
        main = "MICHD Distribution") # Visual representation of skewness

train_test_split <- sample.split(data$MICHD, SplitRatio = 0.7)
trainset.ori <- subset(data, train_test_split == T)
testset.ori <- subset(data, train_test_split == F)
summary(trainset.ori$MICHD)
prop.table(table(trainset.ori$MICHD))
summary(testset.ori$MICHD)
prop.table(table(testset.ori$MICHD))

# ROSE - train test split 
rose.trainset <- ROSE(MICHD ~ ., data = trainset.ori,seed = 2407)$data
table(rose.trainset$MICHD)
rose.testset <- ROSE(MICHD ~ ., data = testset.ori,seed = 2407)$data
table(rose.testset$MICHD)

prop.table(table(rose.trainset$MICHD))
prop.table(table(rose.testset$MICHD))

# SMOTE - train- test split 
library(DMwR)

smote.trainset <- SMOTE(MICHD ~ ., data = trainset.ori, 
                        perc.over = 100, perc.under = 200)
smote.testset <- SMOTE(MICHD ~ ., data = testset.ori, 
                        perc.over = 100, perc.under = 200)
table(smote.trainset$MICHD)
table(smote.testset$MICHD)
prop.table(table(smote.trainset$MICHD))
prop.table(table(smote.testset$MICHD))


MICHD.split <- data %>% 
  split_by(MICHD, seed = 2407 )
 
# under-sampling with random seed
under <- MICHD.split %>%
  sampling_target(seed = 2407)

under %>% count(MICHD)

under.split <- under %>% 
  split_by(MICHD, seed =2407)

under %>% count(MICHD)

under.trainset <- under.split %>% 
  extract_set(set = "train")
under.trainset %>% count(MICHD)

under.testset <- under.split %>% 
  extract_set(set = "test")

# Check the proportion of cases in trainset and testset - undersampling
summary(under.trainset$MICHD)
prop.table(table(under.trainset$MICHD))
summary(under.testset$MICHD)
prop.table(table(under.testset$MICHD))

# oversampling 
over <- MICHD.split %>% 
  sampling_target(method = "ubOver", seed = 2407)

over %>% count(MICHD)

over.split <- over %>% 
  split_by(MICHD, seed = 2407)

over.trainset <- over.split %>% 
  extract_set(set = "train")
over.trainset %>% count(MICHD)

over.testset <- over.split %>% 
  extract_set(set = "test")

# Check the proportion of cases in trainset and testset - oversampling
summary(over.trainset$MICHD)
prop.table(table(over.trainset$MICHD))
summary(over.testset$MICHD)
prop.table(table(over.testset$MICHD))


##### Logistic Regression #####
# Use GLM to do regression 
# Under sampling 
m1_under <- glm(MICHD ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH 
  + HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA + RENTHOM1 + 
  VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 + HTM4 + PREGNANT + DEAF + 
  BLIND + RFSMOK3 + RFDRHV7 + TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + 
  GRENDA1 + FRNCHDA + POTADA1 + VEGEDA2 + HIVRISK5, 
  data = under.trainset, family = "binomial")

m1_over <- glm(MICHD ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH 
                + HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA + RENTHOM1 + 
                  VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 + HTM4 + PREGNANT + DEAF + 
                  BLIND + RFSMOK3 + RFDRHV7 + TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + 
                  GRENDA1 + FRNCHDA + POTADA1 + VEGEDA2 + HIVRISK5, 
                data = over.trainset, family = "binomial")

m1_rose <- glm(MICHD ~ SEXVAR + GENHLTH + PHYS14D + MENT14D + POORHLTH 
               + HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA + RENTHOM1 + 
                 VETERAN3 + EMPLOY1 + CHLDCNT + INCOME2 + WTKG3 + HTM4 + PREGNANT + DEAF + 
                 BLIND + RFSMOK3 + RFDRHV7 + TOTINDA + STRFREQ + FRUTDA2 + FTJUDA2 + 
                 GRENDA1 + FRNCHDA + POTADA1 + VEGEDA2 + HIVRISK5, 
               data = rose.trainset, family = "binomial")

#Take a look at model coefficients and add odds ratio for interpretability
summary(m1_under)
summary(m1_over)
summary(m1_rose)

# Let us now use stepwise regression to optimise the model 
# om1 - optimised model for model 1 
# stepwise_analysis <- step(m1_heart, direction = "backward")
# om1_heart_call <- stepwise_analysis[["call"]]
# om1_heart <- glm(formula = om1_heart_call[["formula"]], data = trainset,
#                  family = "binomial")

# Is there a difference in the optimised model vs original model?
# Let us take a look at the number of predictors. 

# Number of predictors for original model - m1_heart
length(coef(m1_heart)) - 1  #-1 for intercept || 66 Predictors

# Number of predictors for optimised model - om1_heart
length(coef(om1_heart)) - 1  #-1 for intercept || 54 Predictors

# Let us take a look at the odds ratio of the model 
# OR <- exp(coef(om1_heart))
# OR
OR_under <- exp(coef(m1_under))
OR_under
OR_over <- exp(coef(m1_over))
OR_over
# Interesting note: GENHLTH 1-5 increasing in odds ratio 

# Let us take a look at the collinearity.    
# vif(om1_heart) # Note all var < 5. No issue of collinearity.
vif(m1_under)
vif(m1_over)

##### Prediction on trainset #####
## undersampling ##
# Let us now make predictions using testing set - m1_under
# Output the probability from the logistic function for all cases in the data.
pred_under_train <- predict(m1_under, type = 'response')
# Set empirical threshold and create confusion matrix 
threshold_under <- sum(under.trainset$MICHD == 1)/length(under.trainset$MICHD)
MICHD.hat_under <- ifelse(pred_under_train > threshold_under, 1, 0)
table_under <- table(under.trainset$MICHD, MICHD.hat_under)
table_under
prop.table(table_under)

## Oversampling ##
# Let us now make predictions using testing set - m1_over
pred_over_train <- predict(m1_over, type = 'response')
threshold_over <- sum(over.trainset$MICHD == 1)/length(over.trainset$MICHD)
MICHD.hat_over <- ifelse(pred_over_train > threshold_over, 1, 0)
table_over <- table(over.trainset$MICHD, MICHD.hat_over)
table_over
prop.table(table_over)

# Overall Accuracy if same misclassification costs
mean(MICHD.hat_under == under.trainset$MICHD)
mean(MICHD.hat_over == over.trainset$MICHD)

## ROSE ##
pred_rose_train <- predict(m1_rose, type = 'response')
threshold_rose <- sum(rose.trainset$MICHD == 1)/length(rose.trainset$MICHD)
MICHD.hat_rose <- ifelse(pred_rose_train > threshold_rose, 1, 0)
table_rose <- table(rose.trainset$MICHD, MICHD.hat_rose)
table_rose
prop.table(table_rose)

##### Prediction on testset #####

## Undersampling ##
prob.under.test <- predict(m1_under, newdata = under.testset, type = 'response')
predict.under.MICHD.test <- ifelse(prob.under.test > threshold_under, 1, 0)
under.con_matrix.test <- table(under.testset$MICHD, predict.under.MICHD.test)
under.con_matrix.test
prop.table(under.con_matrix.test)
# Test set accuracy 
mean(predict.under.MICHD.test == under.testset$MICHD) 

## Oversampling ##
prob.over.test <- predict(m1_over, newdata = over.testset, type = 'response')
predict.over.MICHD.test <- ifelse(prob.over.test > threshold_over, 1, 0)
over.con_matrix.test <- table(over.testset$MICHD, predict.over.MICHD.test)
over.con_matrix.test
prop.table(over.con_matrix.test)
# Test set accuracy 
mean(predict.over.MICHD.test == over.testset$MICHD) 

## ROSE ## 
prob.rose.test <- predict(m1_rose, newdata = rose.testset, type = 'response')
predict.rose.MICHD.test <- ifelse(prob.rose.test > threshold_rose, 1, 0)
rose.con.matrix.test <- table(rose.testset$MICHD, predict.rose.MICHD.test)
rose.con.matrix.test
prop.table(rose.con.matrix.test)
# Test set accuracy 
mean(predict.rose.MICHD.test == rose.testset$MICHD)


roc.curve(under.testset$MICHD, prob.under.test)
roc.curve(over.testset$MICHD, prob.over.test)
roc.curve(rose.testset$MICHD, prob.rose.test)
over.holdout <- ROSE.eval(MICHD ~ ., data = over.testset, 
                          learner = glm, method.assess = "holdout", 
                          control.learner=list(family=binomial), seed=1)
over.holdout
ROSE.holdout <- ROSE.eval(MICHD ~ ., data = rose.testset, 
                          learner = glm, method.assess = "holdout", 
                          control.learner=list(family=binomial), seed=1)
ROSE.holdout

# Overall accuracy 
prob <- predict.glm(m1_over, newdata = data, type = 'response')
y.hat <- ifelse(prob > threshold, 1, 0)
table(data$MICHD, y.hat, deparse.level = 2)
overall_accuracy <- mean(y.hat == data$MICHD) # 73% overall accuracy
overall_accuracy

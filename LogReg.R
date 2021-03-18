# Lets try to predict RFHYPE5
# RFHYPE5 - High BP present?
library(car)
library(data.table)
library(skimr)
setwd("C:/Users/jimmy/NTU/BC2407/Project")
data <- fread("CleanedData_medical_nonmedical.csv")

for (col in names(data)) {
  set(data, j = col, value = as.factor(data[[col]]))
}
data[,EXRACT11:=NULL]
skim(data)

# SEXVAR + GENHLTH + PHYSHLTH + MENTHLTH + POORHLTH + HLTHPLN1 + PERSDOC2 + 
#   MEDCOST + CHECKUP1 + MARITAL + EDUCA + RENTHOM1 + VETERAN3 + EMPLOY1 + 
#   CHILDREN + INCOME2 + WEIGHT2 + HEIGHT3 + PREGNANT + DEAF + BLIND + 
#   SMOKE100 + SMOKDAY2 + LASTSMK2 + USENOW3 + ALCDAY5 + AVEDRNK3 + DRNK3GE5 + 
#   EXERANY2 + EXRACT11 + STRENGTH + FRUIT2 + FRUITJU2 + FVGREEN1 + FRENCHF1 + 
#   POTATOE1 + VEGETAB2 + HIVRISK5
# Fitting all non-medical data to predict 
mydata <- na.omit(data)


non_medical_data_names  <- c('RFHYPE5','SEXVAR','GENHLTH','PHYSHLTH','MENTHLTH','POORHLTH','HLTHPLN1','PERSDOC2','MEDCOST','CHECKUP1','MARITAL','EDUCA','RENTHOM1','VETERAN3','EMPLOY1','CHILDREN','INCOME2','WEIGHT2','HEIGHT3','DEAF','BLIND','SMOKE100','USENOW3','ALCDAY5','EXERANY2','STRENGTH','FRUIT2','FRUITJU2','FVGREEN1','FRENCHF1','POTATOE1','VEGETAB2','HIVRISK5')

non_medical_data <- data[, ..non_medical_data_names]
skim(non_medical_data)

mydata_nonMedical <- na.omit(non_medical_data)

fitAll <- glm(RFHYPE5~SEXVAR + GENHLTH + PHYSHLTH + MENTHLTH + POORHLTH + 
               HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA + 
               RENTHOM1 + VETERAN3 + EMPLOY1 + CHILDREN + INCOME2 + WEIGHT2 + 
               HEIGHT3 + DEAF + BLIND + SMOKE100 + USENOW3 + ALCDAY5 + 
               EXERANY2 + STRENGTH + FRUIT2 + FRUITJU2 + FVGREEN1 + FRENCHF1 + 
               POTATOE1 + VEGETAB2 + HIVRISK5, data = mydata_nonMedical, family = "binomial" )
# summary(fitAll)
stepwise_analysis <- step(fitAll, direction = "backward")
formula(stepwise_analysis)
summary(stepwise_analysis)

# Defining the most optimised model
optimised_model_1 <- stepwise_analysis[["call"]]
# op1 <- glm(formula = RFHYPE5 ~ SEXVAR + GENHLTH + MENTHLTH + POORHLTH + 
#              HLTHPLN1 + PERSDOC2 + MEDCOST + CHECKUP1 + MARITAL + EDUCA + 
#              RENTHOM1 + VETERAN3 + EMPLOY1 + CHILDREN + INCOME2 + WEIGHT2 + 
#              HEIGHT3 + DEAF + BLIND + SMOKE100 + USENOW3 + ALCDAY5 + EXERANY2 + 
#              STRENGTH + FRUIT2 + FRUITJU2 + FVGREEN1 + FRENCHF1 + POTATOE1 + 
#              VEGETAB2 + HIVRISK5, family = "binomial", data = mydata_nonMedical)

vif(op1)

#======== TRAIN-TEST SPLIT (ONLY SPLIT IF REQUIRED) ===========
library(caTools)
# Generate a random number sequence that can be reproduced to verify results.
set.seed(100)
summary(mydata_nonMedical$RFHYPE5)

# 70% trainset. 
train <- sample.split(Y = mydata_nonMedical$RFHYPE5, SplitRatio = 0.7)
trainset <- subset(mydata_nonMedical, train == T)
testset <- subset(mydata_nonMedical, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$XX)
summary(testset$XX)

skim(mydata_nonMedical)







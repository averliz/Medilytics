library(tidyverse)
library(data.table)
library(dplyr)
library(skimr)

setwd("C:/Users/jimmy/NTU/BC2407/Project")
data <- fread('dataset.csv') # very taxing procedure

relevantcols <- fread('RelevantColumns.csv', ) # a .csv of the original file, with the redundant rows removed
relevantVariableNames <- c(relevantcols$`SAS Variable Name`)
as.data.frame(relevantVariableNames)

actual_data <- data[, ..relevantVariableNames]

disease_analysis_names <- c('BPHIGH4','TOLDHI2','CHOLCHK2', 'CVDINFR4', 'CVDCRHD4',
                     'CVDSTRK3', 'ASTHMA3', 'CHCSCNCR', 'CHCOCNCR', 'CHCCOPD2',
                     'CHCKDNY2', 'DIABETE4', 'HAVARTH4', 'PREDIAB1')

# 'SMOKDAY2','LASTSMK2' <- these 2 var is reliant removed as it SMOKE100 is enough
# 'AVEDRNK3','DRNK3GE5' <- ALCDAY5 is sufficient to det. if patient drinks 


medical <- data[,..disease_analysis_names]
skim(medical)

# JQ - 'BPHIGH4','TOLDHI2','CHOLCHK2', 'CVDINFR4', 'CVDCRHD4', 'CVDSTRK3', 'ASTHMA3', 'CHCSCNCR', 'CHCOCNCR', 'CHCCOPD2', 'CHCKDNY2', 'DIABETE4', 'HAVARTH4', 'PREDIAB1'
# Jeremy - 'SEXVAR','GENHLTH','PHYSHLTH','POORHLTH','HLTHPLN1','PERSDOC2','MEDCOST','CHECKUP1'
# Manika - 'MARITAL','EDUCA','RENTHOM1','VETERAN3','EMPLOY1','CHILDREN', 'INCOME2','WEIGHT2'
# Shao Jing - 'HEIGHT3','PREGNANT','DEAF','BLIND','SMOKE100', 'USENOW3','ALCDAY5','EXERANY2'
# JL - 'EXRACT11','STRENGTH','FRUIT2','FRUITJU2','FVGREEN1','FRENCHF1','POTATOE1', 'VEGETAB2','HIVRISK5'
demo_analysis_names <- c('SEXVAR','GENHLTH','PHYSHLTH','POORHLTH',
                         'HLTHPLN1','PERSDOC2','MEDCOST',
                         'CHECKUP1','MARITAL','EDUCA','RENTHOM1','VETERAN3','EMPLOY1','CHILDREN',
                         'INCOME2','WEIGHT2','HEIGHT3','PREGNANT','DEAF','BLIND','SMOKE100',
                         'USENOW3','ALCDAY5','EXERANY2',
                         'EXRACT11','STRENGTH','FRUIT2','FRUITJU2','FVGREEN1','FRENCHF1','POTATOE1',
                         'VEGETAB2','HIVRISK5')
non_medical <- data[,..demo_analysis_names]
skim(non_medical)

skim(actual_data)






library(dplyr)
library(data.table)

data <- fread('dataset.csv') # very taxing procedure
# summary(data)

relevantcols <- fread('RelevantColumns.csv') # a .csv of the original file, with the redundant rows removed
relevantVariableNames <- c(relevantcols$`SAS Variable Name`)
as.data.frame(relevantVariableNames)

actual_data <- data[, ..relevantVariableNames]

actual_data <- actual_data %>%
  rename(
    'RFHYPE5' = '_RFHYPE5',
    'RFCHOL2' = '_RFCHOL2',
    'MICHD' = '_MICHD',
    'CASTHM1' = '_CASTHM1',
    'DRDXAR2' = '_DRDXAR2',
  )


write.csv(actual_data, "CleanedData.csv")


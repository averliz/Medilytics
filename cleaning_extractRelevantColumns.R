# setwd("C:/Users/mhenn/Documents/Programming/Academic/BC2407 Medilytics/Manika")
library(data.table)

data <- fread('dataset.csv') # very taxing procedure
# summary(data)

relevantcols <- fread('RelevantColumns.csv') # a .csv of the original file, with the redundant rows removed
relevantVariableNames <- c(relevantcols$`SAS Variable Name`)
as.data.frame(relevantVariableNames)

actual_data <- data[, ..relevantVariableNames]


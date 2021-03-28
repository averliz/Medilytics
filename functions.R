library(dplyr)
library(data.table)
readData <- function(path, chosen_disease) {
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
  
  # factorizing relevant variables
  for (variable in factor_variables) {
    set(data, j = variable, value = as.factor(data[[variable]]))
  }
  
  # setting desired disease to be dependent variable
  data <- data %>% 
    rename(
      'DISEASE' = chosen_disease
    )
  
  interest_cols <- c("DISEASE", "SEXVAR", "GENHLTH", "PHYS14D", "MENT14D", "POORHLTH", 
                     "HLTHPLN1", "PERSDOC2", "MEDCOST", "CHECKUP1", "MARITAL", "EDUCA", 
                     "RENTHOM1", "VETERAN3", "EMPLOY1", "CHLDCNT", "INCOME2", "WTKG3", 
                     "HTM4", "DEAF", "BLIND", "RFSMOK3", "RFDRHV7", 
                     "TOTINDA", "STRFREQ", "FRUTDA2", 'FTJUDA2', "GRENDA1", "FRNCHDA", 
                     "POTADA1", "VEGEDA2", "HIVRISK5" )
  
  data <- data[, ..interest_cols]
  
  return(data)
}

readDataOnly <- function(path) {
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
  
  # factorizing relevant variables
  for (variable in factor_variables) {
    set(data, j = variable, value = as.factor(data[[variable]]))
  }
  
  return(data)
}

normalizeData <- function(data) {
  # normalizes all the data (necessary for a neural network)
  data[, WTKG3 := (WTKG3 - min(WTKG3))/(max(WTKG3) - min(WTKG3))]
  data[, HTM4 := (HTM4 - min(HTM4))/(max(HTM4) - min(HTM4))]
  data[, STRFREQ := (STRFREQ - min(STRFREQ))/(max(STRFREQ) - min(STRFREQ))]
  data[, FRUTDA2 := (FRUTDA2 - min(FRUTDA2))/(max(FRUTDA2) - min(FRUTDA2))]
  data[, FTJUDA2 := (FTJUDA2 - min(FTJUDA2))/(max(FTJUDA2) - min(FTJUDA2))]
  data[, GRENDA1 := (GRENDA1 - min(GRENDA1))/(max(GRENDA1) - min(GRENDA1))]
  data[, FRNCHDA := (FRNCHDA - min(FRNCHDA))/(max(FRNCHDA) - min(FRNCHDA))]
  data[, POTADA1 := (POTADA1 - min(POTADA1))/(max(POTADA1) - min(POTADA1))]
  data[, VEGEDA2 := (VEGEDA2 - min(VEGEDA2))/(max(VEGEDA2) - min(VEGEDA2))]
  data[, POORHLTH := (POORHLTH - min(POORHLTH))/(max(POORHLTH) - min(POORHLTH))]
  
  return(data)
}

fnr <- function(confusionMatrix) {
  rate <- 
    confusionMatrix$table[1,2]/(confusionMatrix$table[1,2] + confusionMatrix$table[2,2])
  return(rate)
}

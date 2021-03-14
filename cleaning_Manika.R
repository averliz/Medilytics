library(data.table)
data <- fread("CleanedData.csv") # this file is the file that has been processed by cleaning_extractRelevantColumns.R
summary(data)

# Analysis of variables 
# Manika - 'MARITAL','EDUCA','RENTHOM1','VETERAN3','EMPLOY1','CHILDREN', 'INCOME2','WEIGHT2'

### MARITAL STATUS ###

data[MARITAL == 9, MARITAL := NA] # converting all the 9 values to NA to allow for easier MICEing

data$MARITAL <- factor(data$MARITAL) # recognize marital status as a factor variable

summary(data$MARITAL)

### EDUCATION LEVEL ###

data[is.na(MARITAL)]

data[EDUCA == 9, EDUCA := NA]

data$EDUCA <- factor(data$EDUCA) # Education is also a factor variable

summary(data$EDUCA)

### OWN OR RENT HOME? ###

data[RENTHOM1 == 9 | RENTHOM1 == 7, RENTHOM1 := NA]

data$RENTHOM1 <- factor(data$RENTHOM1)

summary(data$RENTHOM1)

### VETERAN STATUS ###

# People that refuse to answer are possibly doing so out of pride - they probably never served

data[is.na(VETERAN3) | VETERAN3 == 7 | VETERAN3 == 9, VETERAN3 := 2]

data$VETERAN3 <- factor(data$VETERAN3)

### EMPLOYMENT STATUS ###

data[EMPLOY1 == 9, EMPLOY1 := NA]

data$EMPLOY1 <- factor(data$EMPLOY1)

summary(data$EMPLOY1)

### HOW MANY CHILDREN ###

data[CHILDREN == 88, CHILDREN := 0]

data[CHILDREN == 99, CHILDREN := NA]

data$CHILDREN <- factor(data$CHILDREN)

summary(data$CHILDREN) # is this variable considered numerical or categorical

### INCOME LEVEL ###

data[INCOME2 == 77 | INCOME2 == 99, INCOME2 := NA]

data$INCOME2 <- factor(data$INCOME2)

summary(data$INCOME2)

### WEIGHT ###

data[WEIGHT2 != 7777 & WEIGHT2 < 9000, WEIGHT2 := WEIGHT2/2.205] # converting all weights to kg

data[WEIGHT2 >= 9000 & WEIGHT2 != 9999, WEIGHT2 := WEIGHT2 - 9000] # correcting the kg records

data[WEIGHT2 == 7777 | WEIGHT2 == 9999, WEIGHT2 := NA ] # all those that refused/not asked are set to NA for easy MICEing

summary(data$WEIGHT2)

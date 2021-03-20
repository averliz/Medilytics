library('data.table')
library('dplyr')

### EXTRACTING RELEVANT COLUMNS ###
data <- fread("dataset.csv") # original dataset - massive file
relevantcols <- fread('RelevantColumns.csv', header = T) # a .csv of the original file, with the redundant rows removed
relevantVariableNames <- c(relevantcols$`SAS Variable Name`)
as.data.frame(relevantVariableNames)

filtered_data <- data[, ..relevantVariableNames]

filtered_data <- filtered_data %>%
  rename(
    'RFHYPE5' = '_RFHYPE5',
    'RFCHOL2' = '_RFCHOL2',
    'MICHD' = '_MICHD',
    'CASTHM1' = '_CASTHM1',
    'DRDXAR2' = '_DRDXAR2',
    'PHYS14D' = '_PHYS14D',
    'MENT14D' = '_MENT14D',
    'CHLDCNT' = '_CHLDCNT',
    'RFSMOK3' = '_RFSMOK3',
    'RFDRHV7' = '_RFDRHV7',
    'TOTINDA' = '_TOTINDA',
    'STRFREQ' = 'STRFREQ_',
    'FRUTDA2' = 'FRUTDA2_',
    'FTJUDA2' = 'FTJUDA2_',
    'GRENDA1' = 'GRENDA1_',
    'FRNCHDA' = 'FRNCHDA_',
    'POTADA1' = 'POTADA1_',
    'VEGEDA2' = 'VEGEDA2_'
    
  )

data <- filtered_data # reassign to data for easier processing later


# RFHYPE5
# RFCHOL2
# MICHD
# CVDSTRK3
# CASTHM1
# CHCSCNCR
# CHCOCNCR
# CHCCOPD2
# CHCKDNY2
# DIABETE4
# DRDXAR2

### CLEANING MEDICAL COLUMNS ###

### RFHYPE5 ###
data[RFHYPE5 == 1, RFHYPE5 := 0]
data[RFHYPE5 == 2, RFHYPE5 := 1]
data[RFHYPE5 == 9, RFHYPE5 := NA]

### RFCHOL2 ###
data[RFCHOL2 == 1, RFCHOL2 := 0]
data[RFCHOL2 == 2, RFCHOL2 := 1]
data[RFCHOL2 == 9, RFCHOL2 := NA]

### MICHD ###
data[MICHD == 2, MICHD := 0]
data[MICHD == 9, MICHD := NA]

### _CASTHM1 ###
data[CASTHM1 == 1, CASTHM1 := 0]
data[CASTHM1 == 2, CASTHM1 := 1]
data[CASTHM1 == 9, CASTHM1 := NA]


### _DRDXAR2 ###
data[DRDXAR2 == 2, DRDXAR2 := 0]

### DIABETES ###
# for diabetes, it seems like a positive diabetes result for pregnant
# women translates as a false positive
# Therefore, 1 will be coded as Yes, while 2, 3, 4 are coded as No
# 7 and 9 are coded to NA as per normal
data[DIABETE4 == 3 | DIABETE4 == 4 | DIABETE4 == 2, DIABETE4 := 0]
data[DIABETE4 == 7 | DIABETE4 == 9, DIABETE4 := NA]

### KIDNEY DISEASE ###
data[CHCKDNY2 == 2, CHCKDNY2 := 0] # No
data[CHCKDNY2 == 7 | CHCKDNY2 == 9, CHCKDNY2 := NA]

### CVDSTRK3 ###
# (Ever told) you had a stroke.
data[CVDSTRK3 == 2, CVDSTRK3 := 0] # No
data[CVDSTRK3 == 7 | CVDSTRK3 == 9, CVDSTRK3 := NA] # Refused/Unsure


### CHCSCNCR ###
# (Ever told) you had skin cancer?
data[CHCSCNCR == 2, CHCSCNCR := 0] # No
data[CHCSCNCR == 7 | CHCSCNCR == 9, CHCSCNCR := NA] # Refused/Unsure

### CHCOCNCR ###
# (Ever told) you had any other types of cancer?
data[CHCOCNCR == 2, CHCOCNCR := 0] # No
data[CHCOCNCR == 7 | CHCOCNCR == 9, CHCOCNCR := NA] # Refused/Unsure

### CHCCOPD2 ###
# (Ever told) (you had) chronic obstructive pulmonary disease, 
# C.O.P.D., emphysema or chronic bronchitis?
data[CHCCOPD2 == 2, CHCCOPD2 := 0] # No
data[CHCCOPD2 == 7 | CHCCOPD2 == 9, CHCCOPD2 := NA] # Refused/Unsure

### CLEANING NON-MEDICAL COLUMNS ###

# Jeremy - 'SEXVAR','GENHLTH','PHYSHLTH','MENTHLTH','POORHLTH','HLTHPLN1','PERSDOC2','MEDCOST','CHECKUP1'

# converting all rows with 7 (Don't know) or 9 (Refused) 
# to NA to allow for easier MICEing
data[GENHLTH == 9 | GENHLTH == 7, GENHLTH := NA] 


### PHYS14D ###
# Convert value = 88 to 0 as NONE 
data[PHYS14D == 9, PHYS14D := NA]
data[PHYS14D == 1, PHYS14D := 0]
data[PHYS14D == 2, PHYS14D := 1]
data[PHYS14D == 3, PHYS14D := 2]

### MENT14D ### 
# Convert 88 to 0 -> no days with bad MENTH
data[MENT14D == 9, MENT14D := NA]
data[MENT14D == 1, MENT14D := 0]
data[MENT14D == 2, MENT14D := 1]
data[MENT14D == 3, MENT14D := 2]

### POORHLTH ###
# Convert value = 88 to 0 as NONE 
data[POORHLTH == 88, POORHLTH := 0]
# Convert POORHLTH NA values to 0 when PHYSHLTH and MENTHLTH == 0 
data[is.na(POORHLTH) & PHYS14D == 0 & MENT14D == 0, POORHLTH := 0]
# Convert POORHLTH 77 and 99 values to NA for MICE
data[POORHLTH == 77 | POORHLTH == 99, POORHLTH := NA]

### HLTHPLN1 ###
# converting all rows with 7 (Don't know) or 9 (Refused) 
# to NA to allow for easier MICEing
data[HLTHPLN1 == 2, HLTHPLN1 := 0] # convert NO health plan to 0
data[HLTHPLN1 == 9 | HLTHPLN1 == 7, HLTHPLN1 := NA] 

### PERSDOC2 ###
# Convert having no personal doc to 0 
data[PERSDOC2 == 3 , PERSDOC2 := 0]
# converting all rows with 7 (Don't know) or 9 (Refused) 
# to NA to allow for easier MICEing
data[PERSDOC2 == 9 | PERSDOC2 == 7, PERSDOC2 := NA] 


### MEDCOST ###
# converting all rows with 7 (Don't know) or 9 (Refused) 
# to NA to allow for easier MICEing
data[MEDCOST == 9 | MEDCOST == 7, MEDCOST := NA] 
data[MEDCOST == 2, MEDCOST := 0]


### CHECKUP1 ###
# Convert Never been for routine checkup to 0 
data[CHECKUP1 == 8 , CHECKUP1 := 0]

# converting all rows with 7 (Don't know) or 9 (Refused) 
# to NA to allow for easier MICEing
data[CHECKUP1 == 9 | CHECKUP1 == 7, CHECKUP1 := NA] 


# Analysis of variables 
# Manika - 'MARITAL','EDUCA','RENTHOM1','VETERAN3','EMPLOY1','CHILDREN', 'INCOME2','WEIGHT2'

### MARITAL STATUS ###
data[MARITAL == 9, MARITAL := NA] # converting all the 9 values to NA to allow for easier MICEing

### EDUCATION LEVEL ###
data[EDUCA == 9, EDUCA := NA]

### OWN OR RENT HOME? ###
data[RENTHOM1 == 9 | RENTHOM1 == 7, RENTHOM1 := NA]

### VETERAN STATUS ###
# People that refuse to answer are possibly doing so out of pride - they probably never served
data[VETERAN3 == 2, VETERAN3 := 0]
data[is.na(VETERAN3) | VETERAN3 == 7 | VETERAN3 == 9, VETERAN3 := 0]

### EMPLOYMENT STATUS ###
data[EMPLOY1 == 9, EMPLOY1 := NA]

### HOW MANY CHILDREN ###
data[CHLDCNT == 9, CHLDCNT := NA]

### INCOME LEVEL ###
data[INCOME2 == 77 | INCOME2 == 99, INCOME2 := NA]
### WEIGHT ###
# weight is already cleaned in the list

# Shao Jing - 'HEIGHT3','PREGNANT','DEAF','BLIND','SMOKE100', 'USENOW3','ALCDAY5','EXERANY2'

#HEIGHT3
# Height in meters is already pre-cleaned

#PREGNANT
#change not sure/refused to NA
data[PREGNANT == 2, PREGNANT := 0]
data[is.na(PREGNANT), PREGNANT := 0] # anyone that wasn't asked this question is undoubtedly not pregnant
data[PREGNANT == 7 | PREGNANT == 9, PREGNANT := NA] # no answer - NA

#DEAF
#change not sure/refused to NA
data[DEAF == 7 | DEAF == 9, DEAF := NA]
data[DEAF == 2, DEAF := 0] # YES to deaf is coded as 1; NO is coded as 0
#leave NA as it is (i guess? to confirm)

#BLIND
#change not sure/refused to NA
data[BLIND == 7 | BLIND == 9, BLIND := NA]
data[BLIND == 2, BLIND := 0] # YES to blind is coded as 1, NO is 0
#leave NA as it is (same as DEAF)

#RFSMOK3
data[RFSMOK3 == 9, RFSMOK3 := NA]
data[RFSMOK3 == 1, RFSMOK3 := 0]
data[RFSMOK3 == 2, RFSMOK3 := 1]
#predict NA using MICE

#ALCDAY5
data[RFDRHV7 == 9, RFDRHV7 := NA]
data[RFDRHV7 == 1, RFDRHV7 := 0] # no is encoded as 0 while yes is encoded as 1
data[RFDRHV7 == 2, RFDRHV7 := 1]
#predict NA using MICE

#EXERANY2
data[TOTINDA == 9, TOTINDA := NA]
data[TOTINDA == 2, TOTINDA := 0] # not exercising is encoded as 0, otherwise 1
#predict NA using MICE


# JL - 'STRENGTH','FRUIT2','FRUITJU2','FVGREEN1','FRENCHF1','POTATOE1', 'VEGETAB2','HIVRISK5'

# STRFREQ
# number of times engaged in strength training per week (3 decimal places are implied)
data[STRFREQ == 99000, STRFREQ := NA] #refused
data[,STRFREQ := STRFREQ/1000]

# FRUTDA2
# Fruit intake in times per day - 2 decimal places are implied
data[,FRUTDA2 := FRUTDA2/100]

# FTJUDA2
# Fruit juice intake in times per data - 2 decimal places are implied
data[,FTJUDA2 := FTJUDA2/100]

# GRENDA1
# Dark green vegetable intake in times per day - 2 decimal places are implied
data[,GRENDA1 := GRENDA1/100]

# FRNCHDA
# French Fry intake in times per day - 2 decimal places are implied
data[,FRNCHDA := FRNCHDA/100]

# POTADA1
# Potato servings per day - 2 decimal places are implied
data[,POTADA1 := POTADA1/100]

# VEGETAB2
# Other vegetable intake in times per day - 2 decimal places are implied
data[,VEGEDA2 := VEGEDA2/100]


# HIVRISK5 (Categorical / Binary)
data[HIVRISK5 == 9, HIVRISK5 := NA] #refused
data[HIVRISK5 == 7, HIVRISK5 := NA] #not sure

### FINAL CLEANING TO REMOVE ALL NA ROWS ###

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


write.csv(data, "FinalCleanedData.csv",row.names = FALSE) # write all the data to a .csv for analysis

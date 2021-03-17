library(tidyverse)
library(data.table)
library(dplyr)
library(skimr)

data <- fread("CleanedData.csv") 
summary(data)

######################### Cleaning of medical data #################

medical_data_names <- c('BPHIGH4','TOLDHI2','CHOLCHK2', 'CVDINFR4', 'CVDCRHD4',
                        'CVDSTRK3', 'ASTHMA3', 'CHCSCNCR', 'CHCOCNCR', 'CHCCOPD2',
                        'CHCKDNY2', 'DIABETE4', 'HAVARTH4', 'PREDIAB1')
summary(data[, ..medical_data_names])
skim(data[, ..medical_data_names])


### BPHIGH4 ###
summary(data$BPHIGH4)

# Consolidate all high BP regardless of if it is borderline or prehypertensive
data[BPHIGH4 == 1 | BPHIGH4 == 2 | BPHIGH4 == 4 , BPHIGH4 := 1] 

# Convert no High BP to 0 
data[BPHIGH4 == 3, BPHIGH4 := 0]

# Convert Not sure to NA
data[BPHIGH4 == 7 | BPHIGH4 == 9, BPHIGH4 := NA]

data$BPHIGH4 <- factor(data$BPHIGH4) # BPHIGH4 is a factor variable

summary(data$BPHIGH4)


### TOLDHI2 ###
summary(data$TOLDHI2)

# Convert No to 0
data[TOLDHI2 == 2, TOLDHI2 := 0]

# Convert Not sure or refused to answer to NA
data[TOLDHI2 == 7 | TOLDHI2 == 9, TOLDHI2 := NA]

data$TOLDHI2 <- factor(data$TOLDHI2) # TOLDHI2 is a factor variable

summary(data$TOLDHI2)


### CHOLCHK2 ###
summary(data$CHOLCHK2)











######################### Cleaning of non-medical data #################
# Analysis of variables 
# Jeremy - 'SEXVAR','GENHLTH','PHYSHLTH','MENTHLTH','POORHLTH','HLTHPLN1','PERSDOC2','MEDCOST','CHECKUP1'
demo_first_set_names <- 
  c('SEXVAR','GENHLTH','PHYSHLTH','MENTHLTH','POORHLTH','HLTHPLN1','PERSDOC2','MEDCOST','CHECKUP1')

summary(data[, ..demo_first_set_names])
skim(data[, ..demo_first_set_names])
# skim(data)


### SEXVAR ###
summary(data$SEXVAR) # No Na - no cleaning required 


### GENHLTH ###
summary(data$GENHLTH)

# converting all rows with 7 (Don't know) or 9 (Refused) 
# to NA to allow for easier MICEing
data[GENHLTH == 9 | GENHLTH == 7, GENHLTH := NA] 

summary(data$GENHLTH)


### PHYSHLTH ###

summary(data$PHYSHLTH)

# Convert value = 88 to 0 as NONE 
data[PHYSHLTH == 88, PHYSHLTH := 0]

# Convert 77 and 99 to NA for MICE 
data[PHYSHLTH == 77 | PHYSHLTH == 99, PHYSHLTH :=NA]

summary(data$PHYSHLTH)


### MENTHLTH ### 

summary(data$MENTHLTH)

# Convert 88 to 0 -> no days with bad MENTH
data[MENTHLTH == 88, MENTHLTH := 0]

# Convert 77 and 99 to NA for MICE
data[MENTHLTH == 77 | MENTHLTH == 99, MENTHLTH := NA]

summary(data$MENTHLTH)


### POORHLTH ###
summary(data$POORHLTH)

# Convert value = 88 to 0 as NONE 
data[POORHLTH == 88, POORHLTH := 0]

# Convert POORHLTH NA values to 0 when PHYSHLTH and MENTHLTH == 0 

data[is.na(POORHLTH) & PHYSHLTH == 0 & MENTHLTH == 0, POORHLTH := 0]

# Convert POORHLTH 77 and 99 values to NA for MICE
data[POORHLTH == 77 | POORHLTH == 99, POORHLTH := NA]

summary(data$POORHLTH)


### HLTHPLN1 ###
summary(data$HLTHPLN1)

# converting all rows with 7 (Don't know) or 9 (Refused) 
# to NA to allow for easier MICEing
data[HLTHPLN1 == 9 | HLTHPLN1 == 7, HLTHPLN1 := NA] 

summary(data$HLTHPLN1)


### PERSDOC2 ###
summary(data$PERSDOC2)

# Convert having no personal doc to 0 
data[PERSDOC2 == 3 , PERSDOC2 := 0]

# converting all rows with 7 (Don't know) or 9 (Refused) 
# to NA to allow for easier MICEing
data[PERSDOC2 == 9 | PERSDOC2 == 7, PERSDOC2 := NA] 

summary(data$PERSDOC2)


### MEDCOST ###
summary(data$MEDCOST)

# converting all rows with 7 (Don't know) or 9 (Refused) 
# to NA to allow for easier MICEing
data[MEDCOST == 9 | MEDCOST == 7, MEDCOST := NA] 

summary(data$MEDCOST)


### CHECKUP1 ###
summary(data$CHECKUP1)

# Convert Never been for routine checkup to 0 
data[CHECKUP1 == 8 , CHECKUP1 := 0]

# converting all rows with 7 (Don't know) or 9 (Refused) 
# to NA to allow for easier MICEing
data[CHECKUP1 == 9 | CHECKUP1 == 7, CHECKUP1 := NA] 

summary(data$CHECKUP1)


# Let us first convert all Variables to Factor variables
demo_first_set_names <- 
  c('SEXVAR','GENHLTH','PHYSHLTH','MENTHLTH',
    'POORHLTH','HLTHPLN1','PERSDOC2','MEDCOST','CHECKUP1')

skim(data[, ..demo_first_set_names])

# Let us look at the str of the variables 
str(data[, ..demo_first_set_names]) # all variables are numeric 

# Convert to Factor variables
for (col in demo_first_set_names) {
  set(data, j = col, value = as.factor(data[[col]]))
}

str(data[, ..demo_first_set_names]) # All are now factor variables 


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

# Shao Jing - 'HEIGHT3','PREGNANT','DEAF','BLIND','SMOKE100', 'USENOW3','ALCDAY5','EXERANY2'

#HEIGHT3
#change not sure/refused to NA
data[HEIGHT3 == 7777 | HEIGHT3 == 9999, HEIGHT3 := NA]
#change feet & inches to cm
#1 inch = 2.54 cm 1 feet = 30.48 cm
#note: codebook say 200 - 711 but there is someone who is 800 (8 feet tall), so upper limit put as 800
data[HEIGHT3 >= 200 & HEIGHT3 <= 800, HEIGHT3 := (HEIGHT3/100)*30.48 + (HEIGHT3%%100)*2.54]
#remove 9 from infront of cm
data[HEIGHT3 >= 9000 & HEIGHT3 <= 9998, HEIGHT3 := (HEIGHT3%%1000)]
#predict NA using MICE

#PREGNANT
#change not sure/refused to NA
data[PREGNANT == 7 | PREGNANT == 9, PREGNANT := NA]
#leave NA as it is (males etc)

#DEAF
#change not sure/refused to NA
data[DEAF == 7 | DEAF == 9, DEAF := NA]
#leave NA as it is (i guess? to confirm)

#BLIND
#change not sure/refused to NA
data[BLIND == 7 | BLIND == 9, BLIND := NA]
#leave NA as it is (same as DEAF)

#SMOKE100
#change not sure/refused to NA
data[SMOKE100 == 7 | SMOKE100 == 9, SMOKE100 := NA]
#predict NA using MICE

#USENOW3
#change not sure/refused to NA
data[USENOW3 == 7 | USENOW3 == 9, USENOW3 := NA]
#predict NA using MICE

#ALCDAY5
#change not sure/refused to NA
data[ALCDAY5 == 777 | ALCDAY5 == 999, ALCDAY5 := NA]
#change days per week to days per month as well
#note: 1 row of data with random value of 2 so treat it as 2 days per week
data[ALCDAY5 >= 101 & ALCDAY5 <= 107, ALCDAY5 := (ALCDAY5%%10)*4]
#change days per month to standard form
data[ALCDAY5 >= 201 & ALCDAY5 <= 230, ALCDAY5 := (ALCDAY5%%100)]
#predict NA using MICE

#EXERANY2
#change not sure/refused to NO (they probably did not exercise at all)
data[EXERANY2 == 7 | EXERANY2 == 9, EXERANY2 := 2]
#predict NA using MICE


#CATEGORICAL
#'PREGNANT','DEAF','BLIND','SMOKE100', 'USENOW3', 'EXERANY2'
# Convert to Factor variables
factorvars <- c('PREGNANT','DEAF','BLIND','SMOKE100', 'USENOW3', 'EXERANY2')
for (col in factorvars) {
  set(data, j = col, value = as.factor(data[[col]]))
}
str(data[,..factorvars])
#NUMERICAL
#'HEIGHT3','ALCDAY5'


# JL - 'EXRACT11','STRENGTH','FRUIT2','FRUITJU2','FVGREEN1','FRENCHF1','POTATOE1', 'VEGETAB2','HIVRISK5'

# EXRACT11 (Categorical)
# What type of physical activity or exercise did you spend the most time doing during the past month?

data[EXRACT11 == 99, EXRACT11 := NA] #refused
data[EXRACT11 == 77, EXRACT11 := NA] #unsure
data$EXRACT11 <- factor(data$EXRACT11)
summary(data$EXRACT11)

# STRENGTH
# During the past month, how many times per week or per month did you do physical activities or exercises 
# to STRENGTHEN your muscles?   [Do NOT count aerobic activities like walking, running, or bicycling. 
# Count activities using your own body weight like yoga, sit-ups or push-ups and those using weight machines, 
# free weights, or elastic bands.]

# unsure if this should be a categorical or numerical factor - likely need to split into week and month since
# 101-199: 1-99 times per week
# 201-299: 1-99 times per month
# 888: never (aka 0)

data[STRENGTH == 999, STRENGTH := NA] #refused
data[STRENGTH == 777, STRENGTH := NA] #not sure
data[STRENGTH == 200, STRENGTH := NA] #no meaning for this number
data$STRENGTH <- factor(data$STRENGTH)
summary(data$STRENGTH)

# FRUIT2
# Not including juices, how often did you eat fruit?
# I have no idea how to interpret these numbers, for now I just remove the refused and not sure records

data[FRUIT2 == 999, FRUIT2 := NA] #refused
data[FRUIT2 == 777, FRUIT2 := NA] #not sure
data$FRUIT2 <- factor(data$FRUIT2)
summary(data$FRUIT2)

# FRUITJU2
# Not including fruit-flavored drinks or fruit juices with added sugar, 
# how often did you drink 100% fruit juice such as apple or orange juice?
# same as FRUIT2, these numbers don't make sense

data[FRUITJU2 == 999, FRUITJU2 := NA] #refused
data[FRUITJU2 == 777, FRUITJU2 := NA] #not sure
data[FRUITJU2 == 200, FRUITJU2 := NA] #no meaning for this number
data$FRUITJU2 <- factor(data$FRUITJU2)
summary(data$FRUITJU2)

# FVGREEN1
# How often did you eat a green leafy or lettuce salad, with or without other vegetables?

data[FVGREEN1 == 999, FVGREEN1 := NA] #refused
data[FVGREEN1 == 777, FVGREEN1 := NA] #not sure
data$FVGREEN1 <- factor(data$FVGREEN1)
summary(data$FVGREEN1)


# FRENCHF1
# How often did you eat any kind of fried potatoes, including french fries, home fries, or hash browns?

data[FRENCHF1 == 999, FRENCHF1 := NA] #refused
data[FRENCHF1 == 777, FRENCHF1 := NA] #not sure
data[FRENCHF1 == 200, FRENCHF1 := NA] #no meaning for this number
data$FRENCHF1 <- factor(data$FRENCHF1)
summary(data$FRENCHF1)


# POTATOE1
# How often did you eat any other kind of potatoes, or sweet potatoes, 
# such as baked, boiled, mashed potatoes, or potato salad?

data[POTATOE1 == 999, POTATOE1 := NA] #refused
data[POTATOE1 == 777, POTATOE1 := NA] #not sure
data$POTATOE1 <- factor(data$POTATOE1)
summary(data$POTATOE1)


# VEGETAB2
# Not including lettuce salads and potatoes, how often did you eat other vegetables?

data[VEGETAB2 == 999, VEGETAB2 := NA] #refused
data[VEGETAB2 == 777, VEGETAB2 := NA] #not sure
data$VEGETAB2 <- factor(data$VEGETAB2)
summary(data$VEGETAB2)


# HIVRISK5 (Categorical / Binary)
# I am going to read you a list. When I am done, please tell me if any of the situations apply to you. 
# You do not need to tell me which one. You have injected any drug other than those prescribed for you in the past year. 
# You have been treated for a sexually transmitted disease or STD in the past year.  
# You have given or received money or drugs in exchange for sex in the past year.

data[HIVRISK5 == 9, HIVRISK5 := NA] #refused
data[HIVRISK5 == 7, HIVRISK5 := NA] #not sure
data$HIVRISK5 <- factor(data$HIVRISK5)
summary(data$HIVRISK5)
library(data.table)

data <- fread("CleanedData.csv") 
summary(data)

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



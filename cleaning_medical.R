data <- fread("CleanedData.csv")
library('data.table')

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


### RFHYPE5 ###
data[RFHYPE5 == 1, RFHYPE5 := 0]
data[RFHYPE5 == 2, RFHYPE5 := 1]
data[RFHYPE5 == 9, RFHYPE5 := NA]
data$RFHYPE5 <- factor(data$RFHYPE5)
summary(data$RFHYPE5)

### RFCHOL2 ###
data[RFCHOL2 == 1, RFCHOL2 := 0]
data[RFCHOL2 == 2, RFCHOL2 := 1]
data[RFCHOL2 == 9, RFCHOL2 := NA]
data$RFCHOL2 <- factor(data$RFCHOL2)
summary(data$RFCHOL2)

### MICHD ###
data[MICHD == 2, MICHD := 0]
data[MICHD == 9, MICHD := NA]
data$MICHD <- factor(data$MICHD)
summary(data$MICHD)

### _CASTHM1 ###
data[CASTHM1 == 1, CASTHM1 := 0]
data[CASTHM1 == 2, CASTHM1 := 1]
data[CASTHM1 == 9, CASTHM1 := NA]
data$CASTHM1 <- factor(data$CASTHM1)
summary(data$CASTHM1)


### _DRDXAR2 ###
data[DRDXAR2 == 2, DRDXAR2 := 0]
data$DRDXAR2 <- factor(data$DRDXAR2)
summary(data$DRDXAR2)

### DIABETES ###
# for diabetes, it seems like a positive diabetes result for pregnant
# women translates as a false positive
# Therefore, 1 will be coded as Yes, while 2, 3, 4 are coded as No
# 7 and 9 are coded to NA as per normal
data[DIABETE4 == 3 | DIABETE4 == 4 | DIABETE4 == 2, DIABETE4 := 0]
data[DIABETE4 == 7 | DIABETE4 == 9, DIABETE4 := NA]
data$DIABETE4 <- factor(data$DIABETE4)
summary(data$DIABETE4)

### KIDNEY DISEASE ###
data[CHCKDNY2 == 2, CHCKDNY2 := 0] # No
data[CHCKDNY2 == 7 | CHCKDNY2 == 9, CHCKDNY2 := NA]
data$CHCKDNY2 <- factor(data$CHCKDNY2)
summary(data$CHCKDNY2)

### CVDSTRK3 ###
# (Ever told) you had a stroke.
data[CVDSTRK3 == 2, CVDSTRK3 := 0] # No
data[CVDSTRK3 == 7 | CVDSTRK3 == 9, CVDSTRK3 := NA] # Refused/Unsure
data$CVDSTRK3 <- factor(data$CVDSTRK3) # TOLDHI2 is a factor variable
summary(data$CVDSTRK3)


### CHCSCNCR ###
# (Ever told) you had skin cancer?
data[CHCSCNCR == 2, CHCSCNCR := 0] # No
data[CHCSCNCR == 7 | CHCSCNCR == 9, CHCSCNCR := NA] # Refused/Unsure
data$CHCSCNCR <- factor(data$CHCSCNCR) # TOLDHI2 is a factor variable
summary(data$CHCSCNCR)

### CHCOCNCR ###
# (Ever told) you had any other types of cancer?
data[CHCOCNCR == 2, CHCOCNCR := 0] # No
data[CHCOCNCR == 7 | CHCOCNCR == 9, CHCOCNCR := NA] # Refused/Unsure
data$CHCOCNCR <- factor(data$CHCOCNCR) # TOLDHI2 is a factor variable
summary(data$CHCOCNCR)

### CHCCOPD2 ###
# (Ever told) (you had) chronic obstructive pulmonary disease, 
# C.O.P.D., emphysema or chronic bronchitis?
data[CHCCOPD2 == 2, CHCCOPD2 := 0] # No
data[CHCCOPD2 == 7 | CHCCOPD2 == 9, CHCCOPD2 := NA] # Refused/Unsure
data$CHCCOPD2 <- factor(data$CHCCOPD2) # TOLDHI2 is a factor variable
summary(data$CHCCOPD2)

write.csv(data, "CleanedData_medical.csv")

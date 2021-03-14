setwd("C:/Users/Lim Jun Liang/OneDrive/Desktop/Medilytics-master")
data <- fread("CleanedData.csv")
summary(data)

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








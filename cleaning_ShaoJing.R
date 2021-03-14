library(data.table)

data <- fread("CleanedData.csv") 
summary(data)

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


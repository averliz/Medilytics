library(tidyverse)
library(data.table)

setwd("C:/Users/jimmy/NTU/BC2407/Project")

full_data <- fread("dataset.csv")

# full_data

disease_analysis_names <- c('BPHIGH4','TOLDHI2','CHOLCHK2', 'CVDINFR4', 'CVDCRHD4', 
                     'CVDSTRK3', 'ASTHMA3', 'CHCSCNCR', 'CHCOCNCR', 'CHCCOPD2', 
                     'CHCKDNY2', 'DIABETE4', 'HAVARTH4', 'PREDIAB1')

full_data_tibble <- as_tibble(full_data)
disease_data <- full_data_tibble[,disease_analysis]

demo_analysis_names <- c('SEXVAR','GENHLTH','PHYSHLTH','POORHLTH','HLTHPLN1','PERSDOC2','MEDCOST',
'CHECKUP1','MARITAL','EDUCA','RENTHOM1','VETERAN3','EMPLOY1','CHILDREN',
'INCOME2','WEIGHT2','HEIGHT3','PREGNANT','DEAF','BLIND','SMOKE100',
'SMOKDAY2','LASTSMK2','USENOW3','ALCDAY5','AVEDRNK3','DRNK3GE5','EXERANY2',
'EXRACT11','STRENGTH','FRUIT2','FRUITJU2','FVGREEN1','FRENCHF1','POTATOE1',
'VEGETAB2','HIVRISK5')

demo_data <- full_data_tibble[,demo_analysis_names]

combined_names <- c('BPHIGH4','TOLDHI2','CHOLCHK2','CVDINFR4','CVDCRHD4',
                    'CVDSTRK3','ASTHMA3','CHCSCNCR','CHCOCNCR','CHCCOPD2',
                    'CHCKDNY2','DIABETE4','HAVARTH4','PREDIAB1','DEAF',
                    'GENHLTH','PHYSHLTH','POORHLTH','HLTHPLN1','PERSDOC2',
                    'MEDCOST','CHECKUP1','MARITAL','EDUCA','RENTHOM1',
                    'VETERAN3','EMPLOY1','CHILDREN','INCOME2','WEIGHT2',
                    'HEIGHT3','PREGNANT','DEAF','BLIND','SMOKE100','SMOKDAY2',
                    'LASTSMK2','USENOW3','ALCDAY5','AVEDRNK3','DRNK3GE5',
                    'EXERANY2','EXRACT11','STRENGTH','FRUIT2','FRUITJU2',
                    'FVGREEN1','FRENCHF1','POTATOE1','VEGETAB2','HIVRISK5')

combined_data <- full_data_tibble[,combined_names]

summary(combined_data["BPHIGH4"])

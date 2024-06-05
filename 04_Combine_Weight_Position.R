
### THIS SCRIPT'S PURPOSE IS TO COMBINE SPATIAL DATA WITH WEIGHT DATA 

library(dplyr)
library(tidyr)

rm(list=ls()) #clear the working environment 

weightDiffs <- read.csv("C:/Users/Rasek/Downloads/Archive 2/03_Data/Weight_Differences_all.csv") %>% as_tibble()
weightDiffs

#specify dates as dates
weightDiffs$WeightDate <- as.Date(weightDiffs$WeightDate, format="%Y-%m-%d")

#drop unused columns
weightDiffs <- weightDiffs %>% dplyr::select(Code, WeightDate, GroupRef, weightDiff, zWeightID, zWeightGroup)

colnames(weightDiffs) <- c("code", "date", "groupRef", "weightDiff", "zWeightID", "zWeightGroup")
weightDiffs

weightDiffs %>% group_by(date, code) %>% filter(n()>1) #no duplicates 

spatialData <- read.csv("C:/Users/Rasek/Downloads/Archive 2/01_Data/PositionDataAll_2023.09.04.csv") %>% as_tibble()
spatialData

#specify dates as dates
spatialData$date <- as.Date(spatialData$date, format="%Y-%m-%d")

positionAndWeight <- left_join(spatialData, weightDiffs) %>% drop_na(weightDiff) 
#gives us only 195 meerkat-days cases where we have both position and weight data 


#how many sessions? 
positionAndWeight %>% group_by(groupRef, format(date, "%Y-%m")) %>% summarise(N=n()) %>% arrange(groupRef,`format(date, "%Y-%m")`)
levels(as.factor(positionAndWeight$date))

#add session/group IDS
positionAndWeight <- positionAndWeight %>% mutate(session = case_when(
  format(date, "%Y")== "2017" ~ "hm17",
  groupRef=="134" & format(date, "%Y")=="2019" ~ "hm19",
  groupRef=="18" & format(date, "%Y")== "2019" ~ "l19",
  groupRef=="81" & format(date, "%Y")== "2021" ~ "zu21",
  groupRef=="156" & format(date, "%Y")== "2022" ~ "si22",
  groupRef=="152" & format(date, "%Y")== "2021" ~ "rw21",
  groupRef=="152" & format(date, "%Y")== "2022" ~ "rw22", 
  groupRef=="147" & format(date, "%Y")== "2021" ~ "nq21",
  groupRef=="147" & format(date, "%Y")== "2022" ~ "nq22"))

#check that all looks good 
positionAndWeight %>% group_by(code, session) %>% summarise(status=first(status2)) %>% arrange(session) %>% print(n=Inf)


write.csv(positionAndWeight, "01_Weight_And_Position/04_Data/positionAndWeight.csv", row.names = FALSE)

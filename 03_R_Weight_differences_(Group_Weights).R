setwd("C:/Users/Rasek/Dropbox/Analysis/raw_data")

#uncomment to run on Vlads laptop
#setwd("C:/Users/vdemartsev/Dropbox/Analysis/raw_data")


rm(list=ls()) #clear the working environment 

### DATA UPLOAD
Weights <- read.csv("C:/Users/Rasek/Downloads/Archive 2/03_Data/Group_Weights_all.csv")#Weights of NQ, SI, RW from 2017-2022.
Weights <- as_tibble(Weights) %>% dplyr::select(-c(X, RowRef))
Weights

#specify dates as dates 
Weights$WeightDate <- as.Date(Weights$WeightDate, format="%Y-%m-%d")
Weights

ID_codes <- read.csv("C:/Users/Rasek/Downloads/Archive 2/02_Data/Identity_codes_filtered.csv", sep = ",", row.names = FALSE)#Idendity codes (excel sheet) of individuals from different groups
ID_codes <- as_tibble(ID_codes) %>% dplyr::select(IndividID, Code)
ID_codes


Weights <- left_join(Weights, ID_codes)

#### now we are calculating the weight difference for each meerkat on each day

#first we will remove Evening weights as those are less relevant for us
Weights <- Weights %>% filter(WeightSession!="E")


#just grab cases with at least two weights between 2017 and 2022
Weights2 <- Weights %>% group_by(Code, WeightDate) %>% filter(n()==2) %>% filter(WeightDate > "2017-01-01" & WeightDate < "2023-01-01")

Weights3 <- Weights2 %>% 
  arrange(Code, WeightDate, desc(WeightSession)) %>% 
  mutate(weightDiff = Weight-lag(Weight)) %>% 
  filter(WeightSession=="L") %>%
  dplyr::select(Code, WeightDate, GroupRef, weightDiff)
Weights3

#add a scaled weight relative to average for each meerkat
#zWeightId is weight gain relative to self (i.e. was it a good day for me today relative to how i usually do?)
#zWeightGroup is weight gain relative to group on the same day (how good was my day compared to others in the group?)
#this controls for the effects of age on foraging success 

library(zoo)
#create a rolling mean for individual weight over the 11 weight sessions (+/- five sessions including the current one)
Weights3 <- Weights3 %>% 
  arrange(Code, WeightDate) %>% 
  ungroup() %>% group_by(Code) %>%
  mutate(meanWeightDiff = rollapply(weightDiff, 11, mean, align="center", fill=NA, partial=TRUE),
         sdWeightDiff = rollapply(weightDiff, 11, sd, align="center", fill=NA, partial=TRUE))

#calculate zWeightID by taking the z-score for each individual's weight gain relative to their average weight gain
Weights3 <- Weights3 %>%
  mutate(zWeightID = (weightDiff-meanWeightDiff)/sdWeightDiff)

#drop NAs
Weights3 <- Weights3 %>% filter(!is.na(weightDiff)) #only loses 2 observations 

#add the group mean for standardized weight gain 
Weights3 <- left_join(Weights3, 
          Weights3 %>% 
            group_by(WeightDate, GroupRef) %>% 
            summarise(weightDiffGroupAvg=mean(zWeightID),
                      weightDiffGroupSd=sd(zWeightID)))

#add standardized weight gain relative to group 
Weights3 <- Weights3 %>% mutate(
  zWeightGroup = (zWeightID - weightDiffGroupAvg)/weightDiffGroupSd)

write.csv(Weights3, "01_Weight_And_Position/03_Data/Weight_Differences_all.csv", row.names = FALSE)




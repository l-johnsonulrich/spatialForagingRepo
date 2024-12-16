## The purpose of this script is to calculate spatial position per day per meerkat 
## This script uses the spatial metrics calculated using the CCAS standardized method over a 10m discretization 
## It includes spatial metrics calculated every second (1 second fixes) 
## Turning angles calculated based on past and future 10m of movement per individual and relative to the centroid's past and future 10m of movement



library(dplyr)
library(tidyr)
library(ggplot2)

#load the data
load("00_Data/output/10m_2024-12-06.RData")

#convert to a tibble for easier handling
spatialMetrics <- spatialMetrics %>% as_tibble()
spatialMetrics

#check that data is present for all expected meerkats
#this code creates a list of each session and each meerkat 
points <- spatialMetrics %>% as_tibble() %>% group_by(session, format(date, "%Y"), indUniqID) %>% summarise(points=sum(!is.na(relX)))
points

#cleaning
spatialMetrics <- spatialMetrics %>% drop_na(relX)

#drop 2022-06-16, no data collected on this day
spatialMetrics <- spatialMetrics %>% filter(date!="2022-06-16")

#join with indiviudal info to add sex and original meerkat code 
spatialMetrics <- spatialMetrics %>% 
  left_join(allIndInfo %>% dplyr::select(code, DOB, sex, indUniqID=uniqueID), by="indUniqID")

#calculated distance moved per second
spatialMetrics <- spatialMetrics %>% arrange(code, t) %>% mutate(indDist = if_else(t-lag(t)==1, sqrt((lag(x) - x)^2 + (lag(y) - y)^2), NA))
spatialMetrics %>% dplyr::select(code, t, indDist)
summary(spatialMetrics$indDist)

#selecting just the metrics that we need for the position analysis
spatialMetrics2 <- spatialMetrics %>% 
  dplyr::select(session, date, t, tIdx, code, status, sex, DOB, relX, relY, relativeAngle, frontBackPosition, inFrontHalf, IndRankAlongMvmtAxis, indDist, indSpeedPast)
#relativeAngle is the meerkat's position relative to the centroid
#frontBackPosition = distance in meters from the front to the back 
#inFrontHalf = binary variable for front/back
#indAngleFromGroupMovement = same as relativeAngle 
#IndRankAlongMvmtAxis = ranked position along movement axis 
#relY = left right distance from the centroid
#relX = front back distance from the centroid


#add a position variable based on dividing the centroid into four quandrants
spatialMetrics2 <- spatialMetrics2 %>% mutate(position = case_when(
  abs(relativeAngle) < pi/4 ~ "front",
  abs(relativeAngle) > pi/4 & abs(relativeAngle) < 3*pi/4 ~ "side",
  abs(relativeAngle) > 3*pi/4 ~ "back"
))

#calculate the median distance to the centroid 
spatialMetrics2 <- left_join(spatialMetrics2, spatialMetrics2 %>% 
  mutate(distance=sqrt((relX)^2 + (relY)^2)) %>% 
  group_by(t) %>%
  summarise(median.dist=median(distance)))

#add individual distance from center and then create a second distance variable with center, front, side, and back 
spatialMetrics2 <- spatialMetrics2 %>% mutate(
  distance=sqrt((relX)^2 + (relY)^2),
  position2 = if_else(distance < median.dist, "center", position)
)

#grab first the total gps points for each morning 
sum1 <- spatialMetrics2 %>% 
  group_by(date, code, status, sex) %>% 
  summarise(totalPoints=n()) 
#grab next the gps points in each position 
sum2 <- spatialMetrics2 %>%
  group_by(date, code, position2) %>%
  summarise(points=n())
#join these together
sum3 <- left_join(sum2, sum1, by = c("date", "code"))
sum3 <- sum3 %>% mutate(P = points/totalPoints) #P is the proportion of time spent in each position 

#pivot wider so that proportion of time spent in each position is its own column 
spatialMetrics3 <- sum3 %>% dplyr::select(date, code, status, sex, position2, P) %>%  pivot_wider(id_cols = c(date, code, status, sex), names_from = position2, values_from = P)
spatialMetrics3

#now also grabbing a binary front/back variable 
#inFrontHalf is a binary variable 
sum4 <- spatialMetrics2 %>% group_by(date, code) %>% summarise(frontPoints=sum(inFrontHalf), totalPoints=n(), front.bin=frontPoints/totalPoints)
#add to main dataframe
spatialMetrics3 <- left_join(spatialMetrics3, sum4 %>% dplyr::select(date, code, front.bin))

library(scales)
#last, add a ranked position similar to previous publications
#frontBackPosition is a variable indicating meters along the front/back axis 
#IndRankAlongMvmtAxis is a ranked variable from -1 to 1 for which meerkat is furthest in front (1) vs back (-1)
sum5 <- spatialMetrics2 %>% group_by(date, code) %>%
  summarise(meanFrontBackPosition = mean(frontBackPosition, na.rm=TRUE), meanRank = mean(IndRankAlongMvmtAxis, na.rm=TRUE)) %>%
  ungroup() %>% group_by(date) %>% arrange(date, (meanFrontBackPosition)) %>%
  mutate(rank.order = row_number()-1, meanPositionRanked = rescale(rank.order, to=c(-1,1))) %>%
  dplyr::select(date, code, meanRank, meanPositionRanked)
sum5
#meanRank is simply the mean of the ranked position every second across the entire session
#meanPositionRanked takes the mean position (in meters) across the session and then ranks individuals in a group 

cor.test(sum5$meanRank, sum5$meanPositionRanked) #highly correlated

#add ranked metrics to the main dataframe
spatialMetrics3 <- left_join(spatialMetrics3, sum5)
spatialMetrics3


#add distance moved and speed 
sum6 <- spatialMetrics2 %>% group_by(date, code) %>%
  summarise(
    totalDist = sum(indDist, na.rm=TRUE), 
    secs=sum(!is.na(indDist)), 
    avgSpeed = mean(indSpeedPast, na.rm=TRUE),
    distPerSec = totalDist/secs)
sum6
spatialMetrics3 <- left_join(spatialMetrics3, sum6)






#check if these levels are accurate
levels(as.factor(spatialMetrics3$status))

#there is a dominant without a sex and two version of subadult, correcting these
spatialMetrics3 <- spatialMetrics3 %>% mutate(status = case_when(
  status == "Dominant" ~ paste0(status, sex),
  status == "Sub-adult" ~ "Sub-Adult",
  !is.na(status) ~ status
))

#next add a new status category with less levels
#pup is < 3 mo (not present in this dataset)
#juvenile is 3-60m
#sub-adult is 6mo to 1 year
#yearling is more than 1 year
#eldest sub is the oldest subordinate individual (usually yearling or older)
#adult is more than 2 years

spatialMetrics3 <- spatialMetrics3 %>% mutate(status2 = case_when(
  status == "DominantF" ~ "DominantF",
  status == "DominantM" ~ "DominantM",
  status == "Adult" | status == "Eldest Sub" | status == "Yearling" ~ "Subordinate", 
  status == "Sub-Adult" | status == "Juvenile" ~ "Juvenile"
)) 
#subordinate means older than one year but not a dominant
#juvenile is less than one year 


#add dob
allIndInfo$DOB2 <- as.Date(allIndInfo$DOB, format="%d.%m.%Y")
which(is.na(allIndInfo$DOB2))
allIndInfo[1,]$DOB2 <- as.Date("12.11.19", format="%d.%m.%y")
allIndInfo[7,]$DOB2 <- as.Date("30.11.2021", format="%d.%m.%Y") 
allIndInfo[19,]$DOB2 <- as.Date("02.07.2021", format="%d.%m.%Y")
allIndInfo[22,]$DOB2 <- as.Date("02.03.2022", format="%d.%m.%Y")
allIndInfo[23,]$DOB2 <- as.Date("02.03.2022", format="%d.%m.%Y")
allIndInfo[24,]$DOB2 <- as.Date("02.03.2022", format="%d.%m.%Y")
allIndInfo[27,]$DOB2 <- as.Date("06.02.2021", format="%d.%m.%Y")
allIndInfo[28,]$DOB2 <- as.Date("06.02.2021", format="%d.%m.%Y")
allIndInfo[29:31,]$DOB2 <- as.Date("06.02.2021", format="%d.%m.%Y")
allIndInfo[36:40,]$DOB2 <- as.Date("04/03/2022", format="%d/%m/%Y")

spatialMetrics3 <- spatialMetrics3 %>% left_join(allIndInfo %>% dplyr::select(code, dob=DOB2) %>% distinct(code, .keep_all=TRUE), by = "code")
spatialMetrics3 <- spatialMetrics3 %>% mutate(ageDays = as.numeric(date - dob))


spatialMetrics3 %>% group_by(date, code) %>% filter(n()>1) #no duplicates
#make NAs into 0 for 0 percent time spent in these positions 
spatialMetrics3 <- spatialMetrics3 %>% mutate(
  back = if_else(is.na(back), 0, back),
  center = if_else(is.na(center), 0, center))
drop_na(spatialMetrics3) #check to see if any NAs, all good 

write.csv(x=spatialMetrics3, file="01_Output/PositionDataAll_2024.12.06.csv", row.names=FALSE)

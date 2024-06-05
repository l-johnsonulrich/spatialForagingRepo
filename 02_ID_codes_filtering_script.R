#If you ask R to count number of rows, you just get the value, e.g 7 (for
#7rows)
#this script filters out the whole meerkat ID table and gets rid of all the
#intermediate codes (pup codes). We need the full identity-codes table as an input
#at the end we have a new table that only includes the latest code entries for each 
#meerkat ID


rm(list=ls()) #clear the working environment 

#reading the full table of ID codes
#ID_codes <- read.csv("01_Weight_And_Position/02_Data/Identity codes.csv")
ID_codes <- read.csv("C:/Users/Rasek/Downloads/Archive 2/02_Data/Identity codes.csv")
#making a vector of unique ID codes from the full table
IDs <- unique(ID_codes$IndividID)

#creating an empty data frame for filling up later with the data
adult_codes <- data.frame()

ID_codes$CodeDate <- as.Date(ID_codes$CodeDate, format="%d/%m/%Y")
ID_codes <- ID_codes %>% as_tibble() %>% arrange(IndividID, CodeDate)
ID_codes %>% group_by(IndividID) %>% filter(n()>1)

#this is a loop that reads one ID at a time and only saves the very last entry from the ID_codes table
#this way we are getting the latest CODE for each ID
#the code in curly brackets needs to be run together

ID_codes_filtered <- ID_codes %>% 
  group_by(IndividID) %>% 
  summarise(
    IndividID=last(IndividID),
    Code=last(Code))

#now we have the object adult codes that includes the ID and Code conversion 
#we can use it for filling up the codes in the weights data


#This script is now done. We saved the adult codes table as an excel table in 
#our working directory. This table will now go into R_Script_Weights

write.csv(ID_codes_filtered, "01_Weight_And_Position/02_Data/Identity_codes_filtered.csv")

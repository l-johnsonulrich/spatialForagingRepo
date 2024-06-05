library(tidyr)
library(dplyr)
library(glmmTMB)
library(ggplot2)
library(performance)
library(rptR)
library(sjPlot)
library(modelbased)
library(emmeans)
library(cowplot)
library(ggeffects)
library(extrafont)
library(viridis)
library(viridisLite)
library(scales)
library(RColorBrewer)
#install.packages("scales")
setwd("C:/Users/Rasek/Downloads/Archive 2/04_Data")
Position_and_weight <- read.csv("positionAndWeight.csv")
Position_and_weight$status3 <- ifelse(Position_and_weight$status %in% c("Yearling", "Adult", "Eldest Sub"), "Older individuals",
                                      ifelse(Position_and_weight$status %in% c("Sub-Adult", "Juvenile"), "Younger individuals",
                                             Position_and_weight$status))
#Adding columns (Rank and Age) to Position_and_weight dataframe
Position_and_weight2 <- Position_and_weight %>%
  mutate("Rank" = ifelse(status %in% c("DominantM", "DominantF"), "Dominant", "Subordinate"))
#Position_and_weight3 <- Position_and_weight2 %>%
#  mutate("Age" = ifelse(status2 == "Juvenile", "Juvenile", "Adult"))

Position_and_weight3 <- Position_and_weight2 %>%
  mutate(Age = case_when(
    status %in% c("Adult", "Eldest Sub", "Yearling", "DominantF", "DominantM") ~ "Adult",
    status %in% c("Juvenile", "Sub-Adult") ~ "Sub-Adult",
    TRUE ~ NA_character_  # Handle other cases if any
  ))


colnames(Position_and_weight3) [4] = "Sex"
Position_and_weight3 <- Position_and_weight3 %>% 
  rename(centre = center)
#Saving Position_and_weight3 as a csv file
#write.csv(Position_and_weight3, "C:/Users/Rasek/Downloads/Archive 2/05_data/positionAndWeight3.csv", row.names = FALSE)

#Analysis


#Consistency in Relative positions
#model results from m1-m4 are presented in Table A1
m1 <- glmmTMB(front ~ Rank +Age + Sex +  Rank*Sex+(1|code),data=Position_and_weight3)
summary(m1)
emmeans(m1, ~Rank*Sex)

m2 <- glmmTMB(back ~  Rank + Age+ Sex + Rank*Sex + (1|code),data=Position_and_weight3)
summary(m2)
emmeans(m2, ~Rank*Sex)

m3 <- glmmTMB(centre ~ Rank + Age+ Sex + Rank*Sex + (1|code),data=Position_and_weight3)
summary(m3)
emmeans(m3, ~Rank*Sex)

m4 <- glmmTMB(side ~  Rank + Age+ Sex + Rank*Sex+ (1|code),data=Position_and_weight3)
summary(m4)

#calculate the marginal means for plotting 
frontmm1 <- emmeans(m1, ~Rank*Sex) %>% as_tibble() %>% filter(Rank== "Dominant") %>% 
  mutate(Status= if_else(Sex=="F", "DominantF", "DominantM")) %>% 
  dplyr::select(!c("Rank","Sex")) %>% mutate(position= "front") 
backmm1 <- emmeans(m2, ~Rank*Sex) %>% as_tibble() %>% filter(Rank== "Dominant")%>% 
  mutate(Status= if_else(Sex=="F", "DominantF", "DominantM")) %>% 
  dplyr::select(!c("Rank","Sex")) %>% mutate(position = "back")
centremm1 <- emmeans(m3, ~Rank*Sex) %>% as_tibble() %>% filter(Rank== "Dominant") %>% 
  mutate(Status= if_else(Sex=="F", "DominantF", "DominantM")) %>% 
  dplyr::select(!c("Rank","Sex")) %>% mutate(position= "centre")
sidemm1 <- emmeans(m4, ~Rank*Sex) %>% as_tibble() %>% filter(Rank== "Dominant") %>% 
  mutate(Status= if_else(Sex=="F", "DominantF", "DominantM")) %>% 
  dplyr::select(!c("Rank","Sex")) %>% mutate(position= "side")

frontmm2 <- emmeans(m1, ~Rank*Age) %>% as_tibble() %>% filter(Rank== "Subordinate") %>% 
  mutate(Status= if_else(Age== "Adult", "Adult", "Sub-Adult")) %>% 
  dplyr::select(!c("Rank","Age")) %>%mutate(position= "front")  
backmm2 <- emmeans(m2, ~Rank*Age) %>% as_tibble() %>% filter(Rank== "Subordinate")%>% 
  mutate(Status= if_else(Age== "Adult", "Adult", "Sub-Adult")) %>% 
  dplyr::select(!c("Rank","Age")) %>% mutate(position= "back")
centremm2 <- emmeans(m3, ~Rank*Age) %>% as_tibble() %>% filter(Rank== "Subordinate") %>% 
  mutate(Status= if_else(Age== "Adult", "Adult", "Sub-Adult")) %>% 
  dplyr::select(!c("Rank","Age")) %>% mutate(position= "centre")
sidemm2 <- emmeans(m4, ~Rank*Age) %>% as_tibble() %>% filter(Rank== "Subordinate") %>% 
  mutate(Status= if_else(Age== "Adult", "Adult", "Sub-Adult"))%>% 
  dplyr::select(!c("Rank","Age")) %>% mutate(position= "side")

#Figure 1 Plot
#compile into one dataframe
positionMm <- add_row(frontmm1, backmm1)
positionMm <- add_row(positionMm, sidemm1)
positionMm <- add_row(positionMm, centremm1)
positionMm <- add_row(positionMm, frontmm2)
positionMm <- add_row(positionMm, backmm2)
positionMm <- add_row(positionMm, sidemm2)
positionMm <- add_row(positionMm, centremm2)

#add expected proportions 
positionMm <- positionMm %>% mutate(expected =case_when(
  position == "front" | position == "back" ~ 0.125, 
  position == "centre" ~ 0.5, 
  position == "side" ~ 0.25))

#add fonts for plotting 
font_import()
loadfonts()

#colors=c("#D73027", "#4575B4", "#FC8D59","#91BFDB", "#FEE090", "#E0F3F8")
#positionMm %>%
  #ggplot(aes(y = emmean, x = factor(Status, levels = c("DominantF", "DominantM", "Adult", "Sub-Adult")), fill = Status)) +
  #geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  #geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), position = position_dodge()) +
  #facet_wrap(~factor(position, levels = c("front", "centre", "side", "back"), labels= c("Front", "Centre", "Side", "Back")), scales = "free") +
   #Add x-axis and fill (legend key) labels
  #labs(x = "Status",y= "Estimated Marginal Mean Proportion", fill = "Key")+ 
  #theme_bw()+theme(legend.position='none', panel.grid.major = element_blank(), text=element_text(family="Times New Roman"))+ 
  #geom_hline(aes(yintercept=expected),linetype = "dashed", col = 'darkgrey')

#Figure_1
positionMm$Status <- ifelse(positionMm$Status == "Adult", "Older individuals", 
                    ifelse(positionMm$Status == "Sub-Adult", "Younger individuals", positionMm$Status))
# Define a colorblind-friendly palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","#CC79A7")
positionMm %>%
  ggplot(aes(y = emmean, x = factor(Status, levels = c("DominantF", "DominantM", "Older individuals", "Younger individuals")), fill = Status)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), position = position_dodge()) +
  facet_wrap(~factor(position, levels = c("front", "centre", "side", "back"), labels = c("Front", "Centre", "Side", "Back")), scales = "free") +
  # Add x-axis and fill (legend key) labels
  labs(x = "Status", y = "Estimated Marginal Mean Proportion", fill = "Key") + 
  theme_bw() + 
  theme(legend.position = 'none', 
        panel.grid.major = element_blank(), 
        text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 10, color= "black")) + # Adjust font size of x-axis labels
  scale_x_discrete(labels = label_wrap_gen(width = 5)) + # Adjust width as needed
  geom_hline(aes(yintercept = expected), linetype = "dashed", col = 'darkgrey') +
  scale_fill_manual(values = cbbPalette)


#Consistency model: Binary position, ranked location, average rank
#model summaries are presented in Table A2 
Front_binary <- glmmTMB(front.bin ~ Rank +Age + Sex +  Rank*Sex + (1|code),data=Position_and_weight3)
summary(Front_binary)
#DO NOT INTERPRET THE MAIN EFFECTS FOR FACTORS INVOLVED IN AN INTERACTION

#no significant pairwise comparisons 
emmeans(Front_binary, pairwise~Rank*Sex) 
#the significant interaction apepars to be driven by the dominant female spending more time in front
test(emmeans(Front_binary, ~Rank*Sex), null=0.5)


Mean_rank <- glmmTMB(meanRank ~ Rank +Age + Sex + Rank*Sex + (1|code),data=Position_and_weight3)
summary(Mean_rank)
#get the marginal effect of subordinate and male 
#nothing significant 
emmeans(Mean_rank, pairwise~Rank)
emmeans(Mean_rank, pairwise~Sex)
emmeans(Mean_rank, pairwise~Rank*Sex)



#repeatability models: Actual position
#these results are also presented as the R values in Table A1 with corresponding GLMMs
rpt_1 <- rpt(front ~ (1|code), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
             nboot = 100, npermut = 0)
print(rpt_1)
rpt_1_1 <- rpt(front ~ Rank+ Age+ Sex+ Rank*Sex+ (1|code), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
               nboot = 100, npermut = 0)
print(rpt_1_1)

rpt_2 <- rpt(back ~ (1|code), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
             nboot = 100, npermut = 0)
print(rpt_2)
rpt_2_2 <- rpt(back ~ Rank+ Age+ Sex +(1|code), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
               nboot = 100, npermut = 0)
print(rpt_2_2)

rpt_3 <- rpt(centre ~ (1|ID), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
             nboot = 100, npermut = 0)
print(rpt_3)
rpt_3_3 <- rpt(centre ~ Rank+ Age+ Sex +(1|code), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
               nboot = 100, npermut = 0)
print(rpt_3_3)
rpt_4 <- rpt(side ~ (1|ID), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
             nboot = 100, npermut = 0)
print(rpt_4)
rpt_4_4 <- rpt(side ~ Rank+ Age+ Sex+ (1|code), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
               nboot = 100, npermut = 0)
print(rpt_4_4)

#Repeatability models: Binary Position
#these R values are presented in Table A2 with corresponding GLMMs
rpt_1 <- rpt(front.bin ~ (1|code), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
             nboot = 100, npermut = 0)
print(rpt_1)
rpt_1_1 <- rpt(front.bin ~ Rank+ Age+ Sex+ Rank*Sex+ (1|code), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
               nboot = 100, npermut = 0)
print(rpt_1_1)

rpt_2 <- rpt(meanRank ~ (1|code), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
             nboot = 100, npermut = 0)
print(rpt_2)
rpt_2_2 <- rpt(meanRank ~ Rank+ Age+ Sex +(1|code), grname = "code", data = Position_and_weight3, datatype = "Gaussian", 
               nboot = 100, npermut = 0)
print(rpt_2_2)

#General linear mixed models
#zWeightID #appendix table A3 
zfront <- glmmTMB(zWeightID ~ front*Rank + front*Sex + front*Age+ Rank*Sex + (1|code), data= Position_and_weight3)
summary(zfront)


#Rank and sex: Dominant F and M
estimate_slopes(zfront, trend="front", by=c("Rank", "Sex"))
#Rank and Age: Adults and Juveniles (Subordinates)
estimate_slopes(zfront, trend="front", by=c("Rank", "Age"))

zback <- glmmTMB(zWeightID ~ back*Rank + front*Sex + back*Age+ Rank*Sex + (1|code), data= Position_and_weight3)
summary(zback)
#Rank and sex: Dominant F and M
estimate_slopes(zback, trend="back", by=c("Rank", "Sex"))
#Rank and Age: Adults and Juveniles (Subordinates)
estimate_slopes(zfront, trend="front", by=c("Rank", "Age"))

zcentre <- glmmTMB(zWeightID ~ centre*Rank + centre*Sex + centre*Age+ Rank*Sex + (1|code), data= Position_and_weight3)
summary(zcentre)
#Rank and sex: Dominant F and M
estimate_slopes(zcentre, trend="centre", by=c("Rank","Sex"))
#Rank and Age: Adults and Juveniles (Subordinates)
estimate_slopes(zfront, trend="front", by=c("Rank", "Age"))

zside <- glmmTMB(zWeightID ~ side*Rank + side*Sex + side*Age+ Rank*Sex+ (1|code), data= Position_and_weight3)
summary(zside)
#Rank and sex: Dominant F and M
estimate_slopes(zside, trend="side", by=c("Rank","Sex"))
#Rank and Age: Adults and Juveniles (Subordinates)
estimate_slopes(zside, trend="side", by=c("Rank", "Age"))

#these results are presented in Table A4
zfront.bin <- glmmTMB(zWeightID ~ front.bin*Rank + front.bin*Sex + front.bin*Age + Rank*Sex + (1|code), data= Position_and_weight3)
summary(zfront.bin)
#Rank and sex: Dominant F and M
estimate_slopes(zfront.bin, trend="front.bin", by=c("Rank","Sex"))
#Rank and Age: Adults and Juveniles (Subordinates)
estimate_slopes(zfront.bin, trend="front.bin", by=c("Rank", "Age"))

zmeanRank <- glmmTMB(zWeightID ~ meanRank*Rank + meanRank*Sex + meanRank*Age+Rank*Sex + (1|code), data= Position_and_weight3)
summary(zmeanRank)
#Rank and sex: Dominant F and M
estimate_slopes(zmeanRank, trend="meanRank", by=c("Rank", "Sex"))
#Rank and Age: Adults and Juveniles (Subordinates)
estimate_slopes(zmeanRank, trend="meanRank", by=c("Rank", "Age"))


#zWeightGroup
#these results are presented in Table A5
zfront1 <- glmmTMB(zWeightGroup ~ front*Rank + front*Sex + front*Age+ Rank*Sex+ (1|code), data= Position_and_weight3)
estimate_slopes(zfront1, trend="front", by=c("Rank", "Sex"))
estimate_slopes(zfront1, trend="front", by=c("Rank", "Age"))

zback1 <- glmmTMB(zWeightGroup ~ back*Rank + back*Sex + back*Age+Rank*Sex + (1|code), data= Position_and_weight3)
estimate_slopes(zback1, trend="back", by=c("Rank", "Sex"))
estimate_slopes(zback1, trend="back", by=c("Rank", "Age"))

zcentre1 <- glmmTMB(zWeightGroup ~ centre*Rank + centre*Sex + centre*Age+Rank*Sex +(1|code), data= Position_and_weight3)
estimate_slopes(zcentre1, trend="centre", by=c("Rank", "Sex"))
estimate_slopes(zcentre1, trend="centre", by=c("Rank", "Age"))

zside1 <- glmmTMB(zWeightGroup ~ side*Rank + side*Sex + side*Age+Rank*Sex+ (1|code), data= Position_and_weight3)
estimate_slopes(zside1, trend="side", by=c("Rank", "Sex"))
estimate_slopes(zside1, trend="side", by=c("Rank", "Age"))


#these results are presented in Table A6
zfront.bin1 <- glmmTMB(zWeightGroup ~ front.bin*Rank + front.bin*Sex + front.bin*Age + Rank*Sex + (1|code), data= Position_and_weight3)
estimate_slopes(zfront.bin1, trend="front.bin", by=c("Rank", "Sex"))
estimate_slopes(zfront.bin1, trend="front.bin", by=c("Rank", "Age"))

zmeanRank1 <- glmmTMB(zWeightGroup ~ meanRank*Rank + meanRank*Sex + meanRank*Age+Rank*Sex + (1|code), data= Position_and_weight3)
estimate_slopes(zmeanRank1, trend="meanRank", by=c("Rank", "Sex"))
estimate_slopes(zmeanRank1, trend="meanRank", by=c("Rank", "Age"))



#Scatterplots for spatial position and weight gain
#zWeightID Plots
Back <- ggplot(data = Position_and_weight3, aes(x = back, y = zWeightID)) + geom_point(position = "jitter") + geom_smooth(method = "lm")+ theme_bw()+ xlab("Back")+ ylab("Weight gain")
centre <- ggplot(data = Position_and_weight3, aes(x = centre, y = zWeightID)) + geom_point(position = "jitter") + geom_smooth(method = "lm")+ theme_bw()+ theme_bw()+ xlab("centre")+ ylab("Weight gain")
Front <- ggplot(data = Position_and_weight3, aes(x = front, y = zWeightID)) + geom_point(position = "jitter") + geom_smooth(method = "lm")+ theme_bw()+ theme_bw()+ xlab("Front")+ ylab("Weight gain")
Side <-  ggplot(data = Position_and_weight3, aes(x = side, y = zWeightID)) + geom_point(position = "jitter") + geom_smooth(method = "lm")+ theme_bw()+ theme_bw()+ xlab("Side")+ ylab("Weight gain")

#zWeightGroup
Back2 <- ggplot(data = Position_and_weight3, aes(x = back, y = zWeightGroup)) + geom_point(position = "jitter") + geom_smooth(method = "lm")+ theme_bw()+ xlab("Back")+ ylab("Weight gain")
centre2 <- ggplot(data = Position_and_weight3, aes(x = centre, y = zWeightGroup)) + geom_point(position = "jitter") + geom_smooth(method = "lm")+ theme_bw()+ theme_bw()+ xlab("centre")+ ylab("Weight gain")
Front2 <- ggplot(data = Position_and_weight3, aes(x = front, y = zWeightGroup)) + geom_point(position = "jitter") + geom_smooth(method = "lm")+ theme_bw()+ theme_bw()+ xlab("Front")+ ylab("Weight gain")
Side2 <-  ggplot(data = Position_and_weight3, aes(x = side, y = zWeightGroup)) + geom_point(position = "jitter") + geom_smooth(method = "lm")+ theme_bw()+ theme_bw()+ xlab("Side")+ ylab("Weight gain")


#Spatial position and weight gain Plots

#change subordinate to adult in the dataframe 
Position_and_weight3 <- Position_and_weight3 %>%
  mutate(status2 = ifelse(status2 == "Subordinate", "Adult", status2))

#Change the column name "status2" to "Rank_Age"
colnames(Position_and_weight3) [12] = "Rank_Age"

Position_and_weight3$status2 <- ifelse(Position_and_weight3$status2 == "Adult", "Older individuals", 
                    ifelse(Position_and_weight3$status2 == "Sub-Adult", "Younger individuals", Position_and_weight3$status2))
#zWeightID Plot
#Figure 2
tiff("title.tiff", width = 6, height = 4, units = "in", res = 600)

Position_and_weight3 %>%
  dplyr::select(zWeightID, status3, front, back, side, centre) %>%
  pivot_longer(
    cols = c("front", "back", "side", "centre"),
    names_to = "position",
    values_to = "proportionTime"
  ) %>%
  ggplot(aes(x = proportionTime, y = zWeightID)) +
  geom_point(size = 0.5, alpha = 0.5, color = "black") +
  geom_smooth(aes(color = factor(status3, labels = c("DominantF", "DominantM", "Older individual", "Younger individual"))), method = "lm", se = FALSE) +
  facet_wrap(
    ~factor(position, levels = c("front", "centre", "side", "back"), labels = c("Front", "Centre", "Side", "Back")),
    scales = "free"
  ) +
  labs(y = "Relative weight gain", x = "Proportion of time spent in Front, Centre, Side, & Back",
       color = "Status:") +
  scale_color_manual(values = cbbPalette) +  # Use manually assigned colors
  theme_bw() +
  theme(
    legend.position = "top", 
    legend.box = "horizontal", 
    legend.box.spacing = unit(0.2, "cm"),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    text = element_text(family = "Times New Roman")
  )

dev.off()

#ggsave("my_plot.pdf", width = 10, height = 8, dpi = 200)


#this analysis not used for publication, added to plots for presentations 
#Regression (r-squared values for all positions) This code
#can be edited: Replace zWeightID with zWeightGroup to get its
#r-squraed
regression_summaries <- Position_and_weight3 %>%
  dplyr::select(zWeightID, status2, front, back, side, centre) %>%
  pivot_longer(
    cols = c("front", "back", "side", "centre"),
    names_to = "position",
    values_to = "proportionTime"
  ) %>%
  group_by(position) %>%
  do(model = lm(zWeightID ~ proportionTime, data = .)) %>%
  summarise(r_squared = summary(model)$r.squared)
print(regression_summaries)

# Filter the data for 'side' position and 'Juvenile' status
juvenile_side_data <- Position_and_weight3 %>%
  dplyr::select(zWeightID, status2, side) %>%
  filter(status2 == "Juvenile")

# Perform linear regression for 'Juvenile' status and 'side' position
model_juvenile_side <- lm(zWeightID ~ side, data = juvenile_side_data)

# Get summary of the model
summary(model_juvenile_side)

# Extract R-squared value for 'Juvenile' status and 'side' position
r_squared_juvenile_side <- summary(model_juvenile_side)$r.squared
print(r_squared_juvenile_side)

#ggsave("Figure_3.png", width = 8, height = 6, dpi = 300)


##Figure 3. Mean rank: Dominant female and male plot
tiff("title.tiff", width = 6, height = 4, units = "in", res = 600)


Position_and_weight3 %>%
  dplyr::select(zWeightID, status3, meanRank) %>%
  ggplot(aes(x = meanRank, y = zWeightID)) +
  geom_point(size = 0.5, alpha = 0.5, color = "black") +
  geom_smooth(aes(color= factor(status3, labels=c("DominantF", "DominantM", "Older individuals", "Younger individuals"))), method = "lm", se = FALSE) +
  labs(y = "Relative weight gain", x = "Average ranked position within front-back location",
       color = "Status:") +
  scale_color_manual(values = cbbPalette) +  # Use manually assigned colors
  theme_bw() +
  theme(
    legend.position = "top", 
    legend.box = "horizontal", 
    legend.box.spacing = unit(0.2, "cm"),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    text = element_text(family = "Times New Roman")
  )


dev.off()

#Figure A1
#Plot: Function of interaction(Rank, Sex, Age)
desired_order <- c("front", "back", "centre", "side")

Position_and_weight3 %>%
  select(front, back, side, centre, Rank, Sex, Age) %>%
  pivot_longer(
    cols = c(front, back, side, centre),
    names_to = "position",
    values_to = "proportionTime"
  ) %>%
  group_by(Rank, Sex, Age, position) %>% 
  summarise(meanPropTime = mean(proportionTime)) %>%
  # Convert 'position' to a factor with desired order of levels
  mutate(position = factor(position, levels = desired_order)) %>%
  ggplot(aes(x = interaction(Rank, Sex, Age), y = meanPropTime, fill = position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  facet_grid(~position) +
  scale_x_discrete(labels = scales::label_parse()) +
  labs(
    x = "Interaction (Rank, Age, and Sex)",
    y = "EMM Proportion"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels
    panel.grid = element_blank()  # Remove grid lines
  )
#weight_diff is not a dataframe in this script... what is this code for?? 
#weight_diff$Code <- substr(weight_diff$Code, 1, 3)
#unique_groups <- unique(weight_diff$Code)

#cat("Unique Groups:", unique_groups, "\n")
#cat("Number of Unique Groups:", length(unique_groups), "\n")
#individuals_count <- table(weight_diff$Code)
#print(individuals_count)

#Calculating Repeatability for residual variance calculation
Position_and_weight3$code <- as.character(Position_and_weight3$code)
rpt_1 <- rpt(front ~ Rank+Age+Sex+ Rank*Sex+(1|code), grname = c("Fixed","code"), adjusted= FALSE, data = Position_and_weight3, datatype = "Gaussian", 
             nboot = 100, npermut = 0)
print(rpt_1)

Position_and_weight3$code <- as.character(Position_and_weight3$code)
rpt_2 <- rpt(back ~ Rank+Age+Sex+ Rank*Sex+(1|code), grname = c("Fixed","code"), adjusted= FALSE, data = Position_and_weight3, datatype = "Gaussian", 
             nboot = 100, npermut = 0)
print(rpt_2)

Position_and_weight3$code <- as.character(Position_and_weight3$code)
rpt_3 <- rpt(side ~ Rank+Age+Sex+ Rank*Sex+(1|code), grname = c("Fixed","code"), adjusted= FALSE, data = Position_and_weight3, datatype = "Gaussian", 
             nboot = 100, npermut = 0)
print(rpt_3)

Position_and_weight3$code <- as.character(Position_and_weight3$code)
rpt_4 <- rpt(centre ~ Rank+Age+Sex+ Rank*Sex+(1|code), grname = c("Fixed","code"), adjusted= FALSE, data = Position_and_weight3, datatype = "Gaussian", 
             nboot = 100, npermut = 0)
print(rpt_4)

rpt_1_1 <- rpt(front.bin ~ Rank+ Age+ Sex+ Rank*Sex+ (1|code), grname = c("Fixed","code"), data = Position_and_weight3, datatype = "Gaussian", 
               nboot = 100, npermut = 0)
print(rpt_1_1)

rpt_1_2 <- rpt(meanRank ~ Rank+ Age+ Sex+ Rank*Sex+ (1|code), grname = c("Fixed","code"), data = Position_and_weight3, datatype = "Gaussian", 
               nboot = 100, npermut = 0)
print(rpt_1_2)

rpt_1_3 <- rpt(meanPositionRanked ~ Rank+ Age+ Sex+ Rank*Sex+ (1|code), grname = c("Fixed","code"), data = Position_and_weight3, datatype = "Gaussian", 
               nboot = 100, npermut = 0)
print(rpt_1_3)

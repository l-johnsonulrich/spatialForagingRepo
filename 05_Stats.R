#The purpose of this script is to statistically analyze the effect of meerkat spatial position on weight gain. 

library(tidyr)
library(dplyr)
library(glmmTMB)
library(ggplot2)
library(performance)
library(rptR)
library(modelbased)
library(emmeans)
library(DHARMa)
library(viridis)


Position_and_weight <- read.csv("04_Output/positionAndWeight20241206.csv")
Position_and_weight <- as_tibble(Position_and_weight)
Position_and_weight


#########################       DATA PREP           #################################

levels(as.factor(Position_and_weight$status)) #original status variable from life history sheet used to help ID meerkats in the field
Position_and_weight %>% group_by(status) %>% summarise(N=n())
#only 4 juveniles (under 6 months) and only 14 adults

levels(as.factor(Position_and_weight$status2)) #collapses the status category to merge juveniles with subadults, and also merges eldest sub, adult, and yearlings
Position_and_weight %>% group_by(status2) %>% summarise(N=n())

#correct status to drop "Eldest Sub" as this category overlaps with age categories such as adult or yearling and is not biologically meaningful
#just re-verify based on ages as status is not always updated in group sheets on exact days
Position_and_weight <- Position_and_weight %>% mutate(status3 = case_when(
  status == "DominantF" ~ "dominantF",
  status == "DominantM" ~ "dominantM",
  ageDays < 30.5*3 ~ "pup", #less than 3 months pup
  ageDays < 30.5*6 ~ "juvenile", #less than 6 months juvenile
  ageDays < 365.25 ~ "subadult", #less than 1 year subadult
  ageDays < 365.25*2 ~ "yearling", #1 year of age is yearling
  ageDays > 365.25*2 ~ "adult", #more than two years adult 
  ))
Position_and_weight %>% group_by(status3) %>% summarise(N=n())
#only 4 juveniles and 11 adults 

#merge juveniles and subadults, yearlings and adults 
Position_and_weight <- Position_and_weight %>% mutate(status4 = case_when(
  status3 == "dominantF" ~ "dominantF",
  status3 == "dominantM" ~ "dominantM",
  status3 == "adult" | status3 == "yearling" ~ "adult",
  status3 == "subadult" | status3 == "juvenile" ~ "subadult"
))
Position_and_weight %>% group_by(status4) %>% summarise(N=n())

#create a binary metric for social rank independent of age or sex 
Position_and_weight <- Position_and_weight %>%
  mutate(rank = ifelse(status %in% c("DominantM", "DominantF"), "dominant", "subordinate"))
levels(as.factor(Position_and_weight$rank))
Position_and_weight %>% group_by(rank) %>% summarise(N=n())

#create a binary metric for age that is independent of rank or sex
#use 1 year as cut-off based on sexual maturity 
Position_and_weight <- Position_and_weight %>%
  mutate(age = case_when(
    status %in% c("Adult", "Eldest Sub", "Yearling", "DominantF", "DominantM") ~ "adult",
    status %in% c("Juvenile", "Sub-Adult") ~ "subadult"))
Position_and_weight %>% group_by(age) %>% summarise(N=n())
#using status4
Position_and_weight <- Position_and_weight %>%
  mutate(age2 = case_when(
    status4 %in% c("adult", "dominantF", "dominantM") ~ "adult",
    status4 %in% c("subadult") ~ "subadult"))
Position_and_weight %>% group_by(age2) %>% summarise(N=n())

levels(as.factor(Position_and_weight$session)) #9 groups 

#how many days per meerkats?
Position_and_weight %>% group_by(code) %>% summarise(N=n()) %>% ungroup() %>% summary()

#how many meerkats on any given day? 
Position_and_weight %>% group_by(date) %>% summarise(N=n()) %>% ungroup() %>% summary()



#############################            ANALYSIS      ###########################

##############     Consistency in Relative positions     ####################
#model results from m1-m4 are presented in Table A1
#main model investigating whether individual traits predict time spent in front
hist(Position_and_weight$front)
m1 <- glmmTMB(front ~ rank + age2 + sex + rank*sex +   (1|code) + (1|date) + (1|session), data=Position_and_weight)
summary(m1)
plot(simulateResiduals(fittedModel = m1, n = 1000))
icc(m1, by_group = TRUE)
#rptR package for estimating R values 
#null model only includes random effects
r1 <- rpt(front ~ (1|code) + (1|session) + (1|date), 
          grname = c("code"), 
          data = Position_and_weight, datatype = "Gaussian", 
          nboot = 1000, npermut = 0)
print(r1)
#full model including fixed effects 
r1.1 <- rpt(front ~ rank+ age2+ sex+ rank*sex+ (1|code) + (1|session) + (1|date), 
            grname = c("code"), 
            data = Position_and_weight, datatype = "Gaussian", 
            nboot = 1000, npermut = 0)
print(r1.1)
#more or less time than expected? (marginal means and t-test)
test(emmeans(m1, ~rank*sex), null= 0.125) #first look at the interaction
test(emmeans(m1, ~age2), null= 0.125) #additive effect of age 



#main model investigating whether individual traits predict time spent in back
hist(Position_and_weight$back)
m2 <- glmmTMB(back ~ rank + age2 + sex + rank*sex +   (1|code) + (1|session) + (1|date), data=Position_and_weight)
summary(m2)
plot(simulateResiduals(fittedModel = m2, n = 1000))
icc(m2, by_group = TRUE)
#rptR package for estimating R values 
#null model only includes random effects
r2 <- rpt(back ~ (1|code) + (1|session) + (1|date), 
          grname = c("code"), 
          data = Position_and_weight, datatype = "Gaussian", 
          nboot = 1000, npermut = 0)
print(r2)
#full model including fixed effects 
r2.1 <- rpt(back ~ rank + age2 + sex + rank*sex+ (1|code) + (1|session) + (1|date), 
            grname = c("code"), 
            data = Position_and_weight, datatype = "Gaussian", 
            nboot = 1000, npermut = 0)
print(r2.1)
#more or less time than expected? (marginal means and t-test)
test(emmeans(m2, ~sex*rank), null= 0.125) #first look at the interaction
test(emmeans(m2, ~age2), null= 0.125) #additive effect of age 


#main model investigating whether individual traits predict time spent in centre
hist(Position_and_weight$center)
m3 <- glmmTMB(center ~ rank + age2 + sex + rank*sex +   (1|code) + (1|session) + (1|date), data=Position_and_weight)
summary(m3)
plot(simulateResiduals(fittedModel = m3, n = 1000))
icc(m3, by_group = TRUE)
#rptR package for estimating R values 
#null model only includes random effects
r3 <- rpt(center ~ (1|code) + (1|session) + (1|date), 
          grname = c("code"), 
          data = Position_and_weight, datatype = "Gaussian", 
          nboot = 1000, npermut = 0)
print(r3)
#full model including fixed effects 
r3.1 <- rpt(center ~ rank + age2 + sex + rank*sex+ (1|code) + (1|session) + (1|date), 
            grname = c("code"), 
            data = Position_and_weight, datatype = "Gaussian", 
            nboot = 1000, npermut = 0)
print(r3.1)
#more or less time than expected? (marginal means and t-test)
test(emmeans(m3, ~sex*rank), null= 0.5) #first look at the interaction
test(emmeans(m3, ~age2), null= 0.5) #additive effect of age 

#main model investigating whether individual traits predict time spent in side
hist(Position_and_weight$side)
m4 <- glmmTMB(side ~ rank + age2 + sex + rank*sex +   (1|code) + (1|session) + (1|date), data=Position_and_weight)
summary(m4)
plot(simulateResiduals(fittedModel = m4, n = 1000))
icc(m4, by_group = TRUE)
#rptR package for estimating R values 
#null model only includes random effects
r4 <- rpt(side ~ (1|code) + (1|session) + (1|date), 
          grname = c("code"), 
          data = Position_and_weight, datatype = "Gaussian", 
          nboot = 1000, npermut = 0)
print(r4)
#full model including fixed effects 
r4.1 <- rpt(side ~ rank + age2 + sex + rank*sex+ (1|code) + (1|session) + (1|date), 
            grname = c("code"), 
            data = Position_and_weight, datatype = "Gaussian", 
            nboot = 1000, npermut = 0)
print(r4.1)
#more or less time than expected? (marginal means and t-test)
test(emmeans(m4, ~sex*rank), null= 0.25) #first look at the interaction
test(emmeans(m4, ~age2), null= 0.25) #additive effect of age 


###################       Calculate the predicted values for plotting       ############################

#plot predicted values and expected values 
intercepts = c("frontP" = 0.125, "backP" = 0.125, "centreP" = 0.5, "sideP" = 0.25)
# Define a colorblind-friendly palette

#Plot Figure 1
tiff("Figure1.tiff", width = 8, height = 6, units = 'in', res = 300)
#plot code
Position_and_weight %>% mutate(frontP = predict(m1), backP = predict(m2), centreP=predict(m3), sideP=predict(m4)) %>%
  dplyr::select(code, session, date, rank, sex, age2, frontP, backP, centreP, sideP) %>%
  mutate(status = case_when(
    rank == "dominant" ~ "Dominant",
    rank == "subordinate" & age2 =="adult" ~ "Older Subordinate", 
    rank == "subordinate" & age2 == "subadult" ~ "Younger Subordinate")) %>% 
  pivot_longer(cols = c(frontP, backP, centreP, sideP), names_to = "position", values_to = "proportion") %>%
  ggplot(aes(x=status, y=proportion, fill=factor(sex, labels=c("Female", "Male")))) +
  geom_violin(position = position_dodge(0.9)) +
  geom_boxplot(position = position_dodge(0.9), color="lightgray", alpha=.0, width=.1) +
  facet_wrap(~factor(position, levels=c("frontP", "backP", "sideP", "centreP"), labels=c("Front", "Back", "Side", "Center")), scales="free")+
  geom_hline(data = data.frame(position = names(intercepts), yintercept = intercepts), aes(yintercept = yintercept), linetype = "dashed", color = "gray") + 
  labs(x = "Social Status", y = "Estimated Marginal Mean Proportion", fill = "Sex") + 
  theme_bw() + 
  theme(legend.position = 'top', 
        panel.grid.major = element_blank(), 
        text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 8, color= "black")) + # Adjust font size of x-axis labels
  scale_x_discrete(labels = label_wrap_gen(width = 5)) +
  scale_color_manual(values = c("#440154FF", "#22A884FF"))+
  scale_fill_manual(values = c("#440154FF", "#22A884FF"))
dev.off()


############ Consistency model: Binary position and ranked position #############################
#model summaries are presented in Table A3 
hist(Position_and_weight$front.bin)
m5 <- glmmTMB(front.bin ~ rank + age2 + sex + rank*sex +   (1|code) + (1|session) + (1|date), data=Position_and_weight)
summary(m5)
plot(simulateResiduals(fittedModel = m5, n = 1000))
icc(m5, by_group = TRUE)
#rptR package for estimating R values 
#null model only includes random effects
r5 <- rpt(front.bin ~ (1|code) + (1|session) + (1|date), 
          grname = c("code"), 
          data = Position_and_weight, datatype = "Gaussian", 
          nboot = 1000, npermut = 0)
print(r5)
#full model including fixed effects 
r5.1 <- rpt(front.bin ~ rank + age2 + sex + rank*sex+ (1|code) + (1|session) + (1|date), 
            grname = c("code"), 
            data = Position_and_weight, datatype = "Gaussian", 
            nboot = 1000, npermut = 0)
print(r5.1)
#post hoc means 
m5_emmeans <- test(emmeans(m5, ~sex*rank*age2), null= 0.5) %>% as_tibble() %>% filter(!(rank == "dominant" & age2 == "subadult"))
m5_emmeans
test(emmeans(m5, ~sex*rank), null= 0.5)
test(emmeans(m5, ~age2), null= 0.5)

hist(Position_and_weight$meanRank)
m6 <- glmmTMB(meanRank ~ rank + age2 + sex + rank*sex +   (1|code) + (1|session) + (1|date), data=Position_and_weight)
summary(m6)
plot(simulateResiduals(fittedModel = m6, n = 1000))
icc(m6, by_group = TRUE)
#null model only includes random effects
r6 <- rpt(meanRank ~ (1|code) + (1|session) + (1|date), 
          grname = c("code"), 
          data = Position_and_weight, datatype = "Gaussian", 
          nboot = 1000, npermut = 0)
print(r6)
#full model including fixed effects 
r6.1 <- rpt(meanRank ~ rank + age2 + sex + rank*sex+ (1|code) + (1|session) + (1|date), 
            grname = c("code"), 
            data = Position_and_weight, datatype = "Gaussian", 
            nboot = 1000, npermut = 0)
print(r6.1)
#post hoc means 
summary(Position_and_weight$meanRank) #average/median is a bit above 0 indicating that meerkats tend to be a bit clumped towards the front
test(emmeans(m6, ~sex*rank), null= 0.13)
test(emmeans(m6, ~age2), null= 0.13)



#quick visual check to see if activity levels impact position 
ggplot(data=Position_and_weight, aes(x=totalDist, y=front)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(front ~ totalDist + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=distPerSec, y=front)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(front ~ distPerSec + (1|date) + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=avgSpeed, y=front)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(front ~ avgSpeed + (1|date) + (1|code), data=Position_and_weight))

ggplot(data=Position_and_weight, aes(x=totalDist, y=back)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(back ~ totalDist + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=distPerSec, y=back)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(back ~ distPerSec + (1|date) + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=avgSpeed, y=back)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(back ~ avgSpeed + (1|date) + (1|code), data=Position_and_weight))

ggplot(data=Position_and_weight, aes(x=totalDist, y=side)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(side ~ totalDist + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=distPerSec, y=side)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(side ~ distPerSec + (1|date) + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=avgSpeed, y=side)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(side ~ avgSpeed + (1|date) + (1|code), data=Position_and_weight))

ggplot(data=Position_and_weight, aes(x=totalDist, y=center)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(center ~ totalDist + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=distPerSec, y=center)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(center ~ distPerSec + (1|date) + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=avgSpeed, y=center)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(center ~ avgSpeed + (1|date) + (1|code), data=Position_and_weight))

ggplot(data=Position_and_weight, aes(x=totalDist, y=front.bin)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(front.bin ~ totalDist + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=distPerSec, y=front.bin)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(front.bin ~ distPerSec + (1|date) + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=avgSpeed, y=front.bin)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(front.bin ~ avgSpeed + (1|date) + (1|code), data=Position_and_weight))

ggplot(data=Position_and_weight, aes(x=totalDist, y=meanRank)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(meanRank ~ totalDist + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=distPerSec, y=meanRank)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(meanRank ~ distPerSec + (1|date) + (1|code), data=Position_and_weight))
ggplot(data=Position_and_weight, aes(x=avgSpeed, y=meanRank)) + geom_point() + geom_smooth(method="lm")
summary(glmmTMB(meanRank ~ avgSpeed + (1|date) + (1|code), data=Position_and_weight))








################ EFFECT OF POSITION ON WEIGHT GAIN #########################

#check to see if activity levels impact weight gain 
ggplot(data=Position_and_weight, aes(x=totalDist, y=zWeightID)) + geom_point() + geom_smooth(method="lm")
activityModel <- glmmTMB(zWeightID ~ totalDist + (1|date) + (1|code), data=Position_and_weight)
summary(activityModel)
ggplot(data=Position_and_weight, aes(x=distPerSec, y=zWeightID)) + geom_point() + geom_smooth(method="lm")
activityModel2 <- glmmTMB(zWeightID ~ distPerSec + (1|date) + (1|code), data=Position_and_weight)
summary(activityModel2)
ggplot(data=Position_and_weight, aes(x=avgSpeed, y=zWeightID)) + geom_point() + geom_smooth(method="lm")
activityModel3 <- glmmTMB(zWeightID ~ avgSpeed + (1|date) + (1|code), data=Position_and_weight)
summary(activityModel3)



#zWeightID #appendix table A5 
zfront <- glmmTMB(zWeightID ~ front*rank + front*age2 + front*sex + front:rank:sex +   (1|code) + (1|date) + (1|session), data= Position_and_weight)
summary(zfront)
plot(simulateResiduals(fittedModel = zfront, n = 1000))
icc(zfront, by_group = TRUE)
estimate_slopes(zfront, trend="front", by=c("sex", "rank"))
estimate_slopes(zfront, trend="front", by=c("age2"))

zback <- glmmTMB(zWeightID ~ back*rank + back*age2 + back*sex+ back:rank:sex +   (1|code) + (1|date) + (1|session), data= Position_and_weight)
summary(zback)
plot(simulateResiduals(fittedModel = zback, n = 1000))
icc(zback, by_group = TRUE)
estimate_slopes(zback, trend="back", by=c("sex", "rank"))
estimate_slopes(zback, trend="back", by=c("age2"))

zcentre <- glmmTMB(zWeightID ~ center*rank + center*age2 + center*sex + center:rank:sex +   (1|code) + (1|date) + (1|session), data= Position_and_weight)
summary(zcentre)
plot(simulateResiduals(fittedModel = zcentre, n = 1000))
icc(zcentre, by_group = TRUE)
estimate_slopes(zcentre, trend="center", by=c("sex", "rank"))
estimate_slopes(zcentre, trend="center", by=c("age2"))

zside <- glmmTMB(zWeightID ~ side*rank + side*age2 + side*sex + side:rank:sex +   (1|code) + (1|date) + (1|session), data= Position_and_weight)
summary(zside)
plot(simulateResiduals(fittedModel = zside, n = 1000))
icc(zside, by_group = TRUE)
estimate_slopes(zside, trend="side", by=c("sex", "rank"))
estimate_slopes(zside, trend="side", by=c("age2"))


####### PLOTTING ########### 

tiff("FigureRelativePositionWeight.tiff", width = 8, height = 6, units = 'in', res = 300)
Position_and_weight %>% 
  mutate(weight_front = predict(zfront), weight_back = predict(zback), weight_center = predict(zcentre), weight_side = predict(zside)) %>%
  dplyr::select(session, date, code, rank, age2, sex, weight_front, weight_back, weight_center, weight_side, location_front=front, location_back=back, location_center=center, location_side=side) %>%
  pivot_longer(
    cols = c(weight_front, weight_back, weight_center, weight_side, 
             location_front, location_back, location_center, location_side),
    names_to = c(".value", "loc"),
    names_pattern = "^(weight|location)_(.*)$"
  ) %>%
  rename(predictedWeight = weight, proportion = location, location = loc) %>%
  ggplot(aes(x=proportion, y=predictedWeight, color=factor(sex, labels=c("Female", "Male")))) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  facet_grid(factor(rank, labels=c("Dominant", "Subordinate"))*factor(age2, labels=c("Older", "Younger")) ~ factor(location, labels=c("Back", "Center", "Front", "Side")), scales="free_x") + 
  labs(x = "Proportion of Time", y = "Predicted Weight Gain", color = "Sex") + 
  theme_bw() + 
  theme(legend.position = 'top', 
        panel.grid.major = element_blank(), 
        text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 8, color= "black")) + # Adjust font size of x-axis labels
  scale_color_manual(values = c("#440154FF", "#22A884FF"))
dev.off()



####    Control Positions: these results are presented in Table A7 ###########
zfront.bin <- glmmTMB(zWeightID ~ front.bin*rank + front.bin*age2 + front.bin*sex + front.bin:rank:sex +   (1|code) + (1|date) + (1|session), data= Position_and_weight)
summary(zfront.bin)
plot(simulateResiduals(fittedModel = zfront.bin, n = 1000))
icc(zfront.bin, by_group = TRUE, tolerance = 1e-10)
estimate_slopes(zfront.bin, trend="front.bin", by=c("sex", "rank"))
estimate_slopes(zfront.bin, trend="front.bin", by=c("age2"))


hist(Position_and_weight$meanRank)
zmeanRank <- glmmTMB(zWeightID ~ meanRank*rank + meanRank*age2 + meanRank*sex + meanRank:rank:sex +   (1|code) + (1|date) + (1|session), data= Position_and_weight)
summary(zmeanRank)
plot(simulateResiduals(fittedModel = zmeanRank, n = 1000))
icc(zmeanRank, by_group = TRUE, tolerance = 1e-10)
estimate_slopes(zmeanRank, trend="meanRank", by=c("sex", "rank"))
estimate_slopes(zmeanRank, trend="meanRank", by=c("age2"))


tiff("Figure2.tiff", width = 8, height = 6, units = 'in', res = 300)
Position_and_weight %>% 
  mutate(predictedFrontBinary = predict(zfront.bin), 
         predictedRank = predict(zmeanRank)) %>%
  pivot_longer(
    cols = c(predictedFrontBinary, predictedRank),
    names_to = "position",
    values_to = "predicted") %>%
  ggplot(aes(x = ifelse(position == "predictedFrontBinary", front.bin, meanRank),
             y = predicted, color = sex, fill=sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(factor(age2, labels=c("Older", "Younger"))*factor(rank, labels=c("Dominant", "Subordinate")) ~ factor(position, labels=c("Binary Location", "Ranked Location")), scales = "free_x") + 
  labs(x = "Frontness Score", y = "Predicted Weight Gain", color = "Sex", fill="Sex") + 
  theme_bw() + 
  theme(legend.position = 'top', 
        panel.grid.major = element_blank(), 
        text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 8, color= "black")) + # Adjust font size of x-axis labels
  scale_color_manual(values = c("#440154FF", "#22A884FF"))+
  scale_fill_manual(values = c("#440154FF", "#22A884FF"))
dev.off()

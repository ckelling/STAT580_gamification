###
### Analysis of engagement with motivation
### Claire Kelling, 11/6/17
###

library(plyr)
library(readr)
library(dplyr)
stu_survey <- read_csv("~/GitHub/STAT580_gamification/survey & data/stu_survey.csv")
dat_clean <- read_csv("~/GitHub/STAT580_gamification/Database data/database_cleaned.csv")


#new definition of avg_assm
dat_clean$avg_assm <- dat_clean$total_assm/dat_clean$m30_n
#use stay_30m as the definition of avg_time


#now, I would like to match the two datasets and create a new dataset that has the
#gamification and gender for people who filled out the survey
merged_data <- left_join(stu_survey, dat_clean, by = c(loginId = "loginId"))

#taking out the first questions of the survey, which I'm not interested in
merged_data <- merged_data[,-c(2:30)]

#now, with this combined dataset, I would like to see if their is an effect of motivation when considering the engagement

####
#### WITH OUTLIERS
####
#    I complete this analysis first without game interaction with motviation, and then including!
#first, I will consider stay30m as my measure of engagement at first
fit1 <- aov(stay_30m ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
              female+maxLevel+game,data = merged_data)
summary(fit1) #significant variables: perfApp, teacher, maxlevel, game (no interactions are significant when included)
plot(fit1)

#now, I will consider average assessment as the response
fit2 <- aov(avg_assm ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
              female+maxLevel+game,data = merged_data)
summary(fit2) #significant variables: teacher, maxlevel (no interactions are significant when included)

# response = total assessment
fit3 <- aov(total_time ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
              female+maxLevel+game,data = merged_data)
summary(fit3) #significant variables: mastAvo, perfApp, teacher, gender, maxlevel (no interactions are significant when included)

#response = total time
fit4 <- aov(total_assm ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
              female+maxLevel+game,data = merged_data)
summary(fit4) #significant variables: mastAvo, teacher, maxlevel (no interactions are significant when included)

#things pretty drastically change based on which measure we are using for engagement!

#now I need to subset the data so that it no longer includes the outliers 
#   and re-run the analysis to see if I get the same results

wo_outliers <- "dataset that has removed outliers, code from Nik"

####
#### WITHOUT OUTLIERS
####
#first, I will consider stay30m as my measure of engagement at first
fit1a <- aov(stay_30m ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
               female+maxLevel+game,data = wo_outliers)
summary(fit1a) #perfApp is quite significant, as is the teacher and the maxLevel
plot(fit1a)
#now, I will consider average assessment
fit2a <- aov(stay_30m ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
               female+maxLevel+game,data = wo_outliers)
summary(fit2a)
# response = total assessment
fit3a <- aov(stay_30m ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
               female+maxLevel+game,data = wo_outliers)
summary(fit3a)
#response = total time
fit4a <- aov(stay_30m ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
               female+maxLevel+game,data = wo_outliers)
summary(fit4a)


##
## Additional question on effect on final exam score
##

#first, I will consider the subsetted data, so that I can include motivation as well, since they seem to be important
lm_fit <- lm(course_total ~ stay_30m + mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
               female+maxLevel+game, data = merged_data)
summary(lm_fit) #significant: mastApp, teacher, female, maxLevel
plot(lm_fit) #residuals actually look okay!

#now I would like to try the full dataset, where I cannot control for motivation
lm_fit2 <- lm(course_total ~ stay_30m +as.factor(teacher)+
                female+maxLevel+game, data = dat_clean)
summary(lm_fit2) #significant: everything except game.............
plot(lm_fit2) #residuals actually look okay, again!

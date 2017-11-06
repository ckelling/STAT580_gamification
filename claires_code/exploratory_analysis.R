###
### Exploratory Analysis of Cleaned Database Data
###
library(plyr)
library(readr)
library(dplyr)
stu_survey <- read_csv("~/GitHub/STAT580_gamification/survey & data/stu_survey.csv")
dat_clean <- read_csv("~/GitHub/STAT580_gamification/Database data/database_cleaned.csv")
#copy on my local computer
#dat_clean <- read_csv("C:/Users/ckell/OneDrive/Penn State/2017-2018/580/Gamification/stats consultant/Database data/database_cleaned.csv")

#next steps:
#    1) use dataset without outliers to test ANOVA of engagement
#    2) use dataset without outliers to test lin model for exam scores
#    3) include plots in write up
#    4) include results in write up
#    5) create table in write up to show which effects are significant with and without outliers


# looking at the distributions of some of our data
hist(dat_clean$visits)
hist(dat_clean$course_total)
hist(dat_clean$total_assm)
hist(dat_clean$total_time)
hist(dat_clean$avg_time)

#counts for data that is categorical
count(dat_clean, 'female')
count(dat_clean, 'game')
count(dat_clean, 'teacher')
count(dat_clean, 'eng_class')

#looking at possible correlations between variables
#data is highly correlated, most likely need to only consider some of these variables
cor(dat_clean$total_assm, dat_clean$total_time)
cor(dat_clean$total_assm, dat_clean$avg_time)
cor(dat_clean$total_time, dat_clean$avg_time)
cor(dat_clean$total_time, dat_clean$course_total)

#the definition of average assessment and average time were unclear
#we create our own definition for average assessment and use stay_30 as our 
#   definition for average time
#dat_clean$avg_assm <- dat_clean$total_assm/dat_clean$visits
dat_clean$avg_time2 <- dat_clean$total_time/dat_clean$visits
cor(dat_clean$avg_assm, dat_clean$avg_time)
cor(dat_clean$avg_time2, dat_clean$avg_assm)



######################################################################################
### Part 1:
### Introduction and t-test/ANOVA
######################################################################################

##how to use r for boxplot,barplot,scatterplot,t test and anova

#first download r studio


##how to import data : if you have an excel or cvs file then you can simply click on
##import dataset (on the environment window) -> from excel -> 
##browse (find the data whereever you have stored it) -> import

##once you import the data, it will come in your enironment under whatevername you had saved
##the excel file in.


##We have used the file name as d.
d ##this opens the data
head(d) ##this shows the first few observation of the data along with variable names
data=d ##this is how you can rename your data set if you want.

##Subsetting data into 2 datasets. first one is data set for treatment group and 
##the order one is for the control group


d1=data.frame(d[which(d$game==1),])
d0=data.frame(d[which(d$game==0),])

##plotting a boxplot

boxplot(total_assm~game,data=d, ylab="Total assessment", xlab="Game")
#if you want to use whole data

#or

boxplot(d0$total_assm,d1$total_assm,ylab="Total assessment", xlab="Game")
#if you want to use subset of the data

boxplot(m_avgtime~game,data=d, ylab="average time", xlab="Game")
#stay_30 is in mins, we calculated m_avgtime in secs

######similarly you can plot for all the variables wrt game

boxplot(course_total~female,data=d, ylab="Course total", xlab="Gender")
boxplot(course_total~teacher,data=d, ylab="Course total", xlab="Teacher")


##plotting scatterplots diagrams
plot(d$maxLevel,d$course_total, ylab="Course total", xlab="Max Level")
plot(d$stay_30m,d$course_total, ylab="Course total", xlab="Average time")

##if both the variables are continuous then you can plot a scatter plot but if one of them
##is categorial then you do boxplot

##Merging the data
t=merge(d,s,by="loginId") #this merges the two data set usings a common variable

##missing data
tt=d[!(d$loginId %in% t$loginId),] #this will only keep the values which are not common


##plotting histograms(This is a one dimensional plot)
hist(d$game,main = paste("Original Data for Game"),xlab = "Game")
hist(t$game.x,main = paste("Survey Data For Game"),xlab = "Game") 
hist(tt$game,main = paste("Missing Data For Game"),xlab = "Game")

hist(d$female,main = paste("Original Data for Gender"),xlab = "Gender")
hist(t$female,main = paste("Survey Data For Gender"),xlab = "Gender") 
hist(tt$female,main = paste("Missing Data for Gender"),xlab = "Gender")

##two Anova

a <- aov(total_assm ~ game + female, data=d) ###reponse~treatment 1 + treatment 2
summary(a)

a1 <- aov(m_avgtime ~ game + female, data=d)
summary(a1)


a2 <- aov(total_assm ~ game + teacher, data=d)
summary(a2)


a3 <- aov(m_avgtime ~ game + teacher, data=d)
summary(a3)

##t test

t.test(d$m_avgtime[which(d$game==0)],d$m_avgtime[which(d$game==1)],alternative = 'less')

#or

t.test(d0$m_avgtime,d1$m_avgtime,alternative = 'less')
##you just specify the two samples that you want to test



##non parametric t test
wilcox.test(d$m_avgtime[which(d$game==0)],d$m_avgtime[which(d$game==1)],alternative = 'less')

######################################################################################
### Part 2:
### Analysis of survey data, satisfaction
######################################################################################

######################## read data ###############################
database_cleaned=read.csv("~/Documents/STAT/STAT 580/Real Round/DataAnalysis/database_cleaned.csv")
summary(database_cleaned) # 2 missing values
data.without.missing.value=database_cleaned[which(database_cleaned$course_total!="NA"),]
data1=data.without.missing.value
rm(database_cleaned)
rm(data.without.missing.value)

stu_survey=read.csv("~/Documents/STAT/STAT 580/Real Round/DataAnalysis/stu_survey.csv")
summary(stu_survey) 

data2_original=stu_survey[,c(1,31:45)]
summary(data2_original)
rm(stu_survey)
# Because the loginId in data2_original is not ordered, which may cause
# incovenience in id identification, we rearrange our data by the order
# of loginID.
data2=data2_original[order(data2_original$loginId),]
rm(data2_original)
#####################################################################

############## match data1 and data2 by loginID #####################
data1$loginId
data2$loginId
sum(data2$loginId %in% data1$loginId) # 276 matched
# delete the not matched items in data2
data2_match=data2[which(data2$loginId %in% data1$loginId),] 
data1_match=data1[which(data1$loginId %in% data2$loginId),]
# combine two ordered dataset together
data_combine=cbind(data2_match,data1_match)
head(data_combine)
sum(data_combine[,1]!=data_combine[,17]) 
# check combination result in case there is any error.
# sum=0 implies no error.
data_final=data_combine[,c(1:16,19,20)]
colnames(data_final) <- c("loginID","mastApp","mastAvo","perfApp","perfAvo",paste0("sys_",1:11),"female","game")
#####################################################################

######################################################################
############# analysis between game and satisfaction #################
head(data_final)
data_final$loginID=as.factor(data_final$loginID)
data_final$game=as.factor(data_final$game)
data_final$female=as.factor(data_final$female)
summary(data_final)
attach(data_final)
### two group ###
data.game0=data_final[which(game==0),]
data.game1=data_final[which(game==1),]
########################## EDA ######################################
########### Boxplots for four aggregated measure variables ##########
par(mfrow=c(2,2))
boxplot(mastApp~game,data=data_final,xlab="Game",ylab="mastApp",main="Mastery Approach")
boxplot(mastAvo~game,data=data_final,xlab="Game",ylab="mastAvo",main="Mastery Avoidance")
boxplot(perfApp~game,data=data_final,xlab="Game",ylab="perfApp",main="Performance Approach")
boxplot(perfAvo~game,data=data_final,xlab="Game",ylab="perfAvo",main="Performance Avoidance")
########## correlation plot for the eleven survey questions #########
par(mfrow=c(1,1))
corrplot(cor(data_final[!is.na(data_final[,15]),6:16]))
################### Boxplots for system questions ###################
par(mfrow=c(4,3))
boxplot(data_final[,6]~game,data=data_final,xlab="Game",ylab="sys_1",main="Question 1")
boxplot(data_final[,7]~game,data=data_final,xlab="Game",ylab="sys_2",main="Question 2")
boxplot(data_final[,8]~game,data=data_final,xlab="Game",ylab="sys_3",main="Question 3")
boxplot(data_final[,9]~game,data=data_final,xlab="Game",ylab="sys_4",main="Question 4")
boxplot(data_final[,10]~game,data=data_final,xlab="Game",ylab="sys_5",main="Question 5")
boxplot(data_final[,11]~game,data=data_final,xlab="Game",ylab="sys_6",main="Question 6")
boxplot(data_final[,12]~game,data=data_final,xlab="Game",ylab="sys_7",main="Question 7")
boxplot(data_final[,13]~game,data=data_final,xlab="Game",ylab="sys_8",main="Question 8")
boxplot(data_final[,14]~game,data=data_final,xlab="Game",ylab="sys_9",main="Question 9")
boxplot(data_final[,15]~game,data=data_final,xlab="Game",ylab="sys_10",main="Question 10")
boxplot(data_final[,16]~game,data=data_final,xlab="Game",ylab="sys_11",main="Question 11")
######################################################################

############################# t-test #################################
summary(data.game0)
summary(data.game1)
round(apply(data.game0[,6:16],2,mean),2)-round(apply(data.game1[,6:16],2,mean),2)
round(mean(data.game0[!is.na(data.game0[,15]),15]),2)-round(mean(data.game1[!is.na(data.game1[,15]),15]),2)
################# multivariate two-sample t-test #####################
library(ICSNP)
Z1=as.matrix(data.game0[,c(6:14,16)])
Z2=as.matrix(data.game1[,c(6:14,16)])
Z12=rbind(Z1,Z2)
n1=142;n2=134
NK=c(n1,n2)
IV2  <- factor(rep(1:2, NK))
HotellingsT2(Z12 ~ IV2)
t.test(data.game0[!is.na(data.game0[,15]),15],data.game1[!is.na(data.game1[,15]),15],alternative="two.sided")
########################### Cronbach's alpha #########################
library(ltm)
data_final[which(data_final[,9]==1),9] <-5
data_final[which(data_final[,9]==2),9] <-4
data_final[which(data_final[,9]==4),9] <-2
data_final[which(data_final[,9]==5),9] <-1
cronbach.alpha(data_final[,c(6:14,16)],standardized=FALSE,na.rm=TRUE) # 0.716
cronbach.alpha(data_final[,c(6:16)],standardized=FALSE,na.rm=TRUE) # 0.702
########################## individual t-test #########################
k=7
t.test(data.game0[,k],data.game1[,k],alternative="less") # k= 6,7,8,10:16
k=9
t.test(data.game0[,k],data.game1[,k],alternative="greater")


######################################################################################
### Part 3:
### Analysis of engagement with motivation
######################################################################################

library(plyr)
library(readr)
library(dplyr)
library(lme4)
stu_survey <- read_csv("C:/Users/ckell/OneDrive/Penn State/2017-2018/580/Gamification/STAT580_gamification/survey & data/stu_survey.csv")
dat_clean <- read_csv("C:/Users/ckell/OneDrive/Penn State/2017-2018/580/Gamification/STAT580_gamification/Database data/database_cleaned.csv")


#new definition of avg_assm
dat_clean$avg_assm <- dat_clean$total_assm/dat_clean$m30_n
#use stay_30m as the definition of avg_time


#now, I would like to match the two datasets and create a new dataset that has the
#gamification and gender for people who filled out the survey
merged_data <- left_join(stu_survey, dat_clean, by = c(loginId = "loginId"))

#taking out the first questions of the survey, which I'm not interested in
merged_data <- merged_data[,-c(2:30)]

dat <- merged_data

#now, with this combined dataset, I would like to see if their is an effect of motivation when considering the engagement

####
#### WITH and WITHOUT OUTLIERS (after removing below)
####
#    I complete this analysis first without game interaction with motviation, and then including!
#first, I will consider stay30m as my measure of engagement at first
# (different without outliers)
# without outliers: perfApp, etacher, maxLevel but NOT game
fit1 <- aov(stay_30m ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
              female+maxLevel+game,data = merged_data)
summary(fit1) #significant variables: perfApp, teacher, maxlevel, game (no interactions are significant when included)
plot(fit1)

#now, I will consider average assessment as the response (different without outliers)
#without outliers: perfApp, teacher, maxLevel
fit2 <- aov(avg_assm ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
              female+maxLevel+game,data = merged_data)
summary(fit2) #significant variables: teacher, maxlevel (no interactions are significant when included)

# response = total time (same without outliers, gender less significant)
fit3 <- aov(total_time ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
              female+maxLevel+game,data = merged_data)
summary(fit3) #significant variables: mastAvo, perfApp, teacher, gender, maxlevel (no interactions are significant when included)

#response = total assessment (same without outliers)
fit4 <- aov(total_assm ~ mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
              female+maxLevel+game,data = merged_data)
summary(fit4) #significant variables: mastAvo, teacher, maxlevel (no interactions are significant when included)

#things pretty drastically change based on which measure we are using for engagement!

#now I need to subset the data so that it no longer includes the outliers 
#   and re-run the analysis to see if I get the same results

attach(merged_data)

####
#### WITHOUT OUTLIERS
####
# just repeat the above analysis
# reset in between the outlier tests
source("http://goo.gl/UUyEzD")
outlierKD(merged_data, total_time)
merged_data <- dat
outlierKD(merged_data, total_assm)
merged_data <- dat
outlierKD(merged_data, stay_30m)
merged_data <- dat
outlierKD(merged_data, avg_assm)



merged_data <- dat
outlierKD(merged_data, course_total)
##
## Additional question on effect on final exam score
##

#first, I will consider the subsetted data, so that I can include motivation as well, since they seem to be important
lm_fit <- lm(course_total ~ stay_30m + mastApp+mastAvo+perfApp+perfAvo+as.factor(teacher)+
               female+maxLevel+game, data = merged_data)
summary(lm_fit) #significant: mastApp, teacher, female, maxLevel (same without outliers)
plot(lm_fit) #residuals actually look okay!

#now I would like to try the full dataset, where I cannot control for motivation
lm_fit2 <- lm(course_total ~ stay_30m +as.factor(teacher)+
                female+maxLevel+game, data = dat_clean)
summary(lm_fit2) #significant: everything except game.............
plot(lm_fit2) #residuals actually look okay, again!


#now, I will fit a model with random effect, for the course total
#teacher will be included as a random effect
merged_data$teacher <- as.factor(merged_data$teacher)
fm_0 <- lmer(course_total ~ stay_30m + mastApp+mastAvo+perfApp+perfAvo+(1 |teacher)+
                   female+maxLevel, data = merged_data,REML =FALSE)
summary(fm_0)

fm_1 <- lmer(course_total ~ stay_30m + mastApp+mastAvo+perfApp+perfAvo+(1 |teacher)+
               female+maxLevel+game, data = merged_data,REML =FALSE)

anova(fm_0,fm_1)

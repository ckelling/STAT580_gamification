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
#####################################################################


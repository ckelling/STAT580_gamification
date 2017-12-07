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

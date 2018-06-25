# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Statistics")

# Install packages
install.packages("foreign")
install.packages("raster")
install.packages("MASS")
install.packages("moments")
install.packages("plotrix")

# Include packages
library(foreign)
library(raster)
library(MASS) 
library(moments)
library(plotrix)

# Read data file
piaac.df = read.dta("PIAACdk.dta")

head(piaac.df)
dim(piaac.df)
attach(piaac.df)

#---------------------------------------------------------------------------------------------#
# 3. UNVARIATE DESCRIPTION
#---------------------------------------------------------------------------------------------#

# 3.2 Quantitative variables

# AGE

# Table 
table(age)

# Max and min
max(age, na.rm = TRUE)
min(age, na.rm = TRUE)

# Mean and median
mean(age,na.rm = TRUE)
median(age,na.rm = TRUE)

# Dispersion measures
var(age)
sd(age)
cv(age)
range<-max(age)-min(age);range
IQR(age)

# Skewness
skewness(age)

# Quartiles and summary
quantile(age,probs = c(1/4,1/2,3/4))
quantile(age,probs = seq(0, 1, 0.25))
summary(age)

# Absolute frequencies
n1<-sum((age>=30)&(age<35));n1
n2<-sum((age>=35)&(age<40));n2
n3<-sum((age>=40)&(age<45));n3
n4<-sum((age>=45)&(age<50));n4
n5<-sum((age>=50)&(age<55));n5
n6<-sum((age>=55)&(age<60));n6

# Frequency table (absolute frequencies)
n<-sum(n1,n2,n3,n4,n5,n6);n
nj<-c(n1,n2,n3,n4,n5,n6);nj

# Relative frequencies
fj<-nj/sum(nj);fj

# Density Histogram 
hist(age,freq=FALSE,breaks=c(30,35,40,45,50,55,60),xlim=c(30,60),ylim=c(0,0.05),col="lightblue",main= "Age distribution",xlab="Age",prob=TRUE)
axis(1, at=c(30,35,40,45,50,55,60))

# Boxplot
boxplot(age,horizontal=T)

# HOURLY EARNINGS

# Table 
table(hourly_earnings)

# Max and min
max(hourly_earnings, na.rm = TRUE)
min(hourly_earnings, na.rm = TRUE)

# Mean and median
mean(hourly_earnings, na.rm = TRUE)
median(hourly_earnings, na.rm = TRUE)

# Dispersion measures
var(hourly_earnings, na.rm = TRUE)
sd(hourly_earnings, na.rm = TRUE)
cv(hourly_earnings, na.rm = TRUE)
range<-max(hourly_earnings, na.rm = TRUE)-min(hourly_earnings, na.rm = TRUE);range
IQR(hourly_earnings, na.rm = TRUE)

# Skewness
skewness(hourly_earnings,na.rm = TRUE)

# Quartiles and summary
quantile(hourly_earnings,probs = c(1/4,1/2,3/4), na.rm = TRUE)
quantile(hourly_earnings,probs = seq(0, 1, 0.25), na.rm = TRUE)
summary(hourly_earnings)

# Absolute frequencies
n1<-sum(hourly_earnings<100, na.rm = TRUE);n1
n2<-sum((hourly_earnings>=100)&(hourly_earnings<200), na.rm = TRUE);n2
n3<-sum((hourly_earnings>=200)&(hourly_earnings<300), na.rm = TRUE);n3
n4<-sum((hourly_earnings>=300)&(hourly_earnings<400), na.rm = TRUE);n4
n5<-sum(hourly_earnings>=400, na.rm = TRUE);n5

# Frequency table (absolute frequencies)
n<-sum(n1,n2,n3,n4,n5);n
nj<-c(n1,n2,n3,n4,n5);nj

# Relative frequencies
fj<-nj/sum(nj);fj

split.screen(c(1,2))

# Density Historgram
screen(1)
hist(earnings,probability=TRUE,histo = TRUE, col="light blue",border="white",xlim=c(0,750),breaks=100, main = "Histogram of earnings", xlab = "Hourly earnings")
lines(density(income),col="dark blue")

# Density Historgram
screen(2)
hist(hourly_earnings,freq=FALSE,ylim=c(0,0.010),xlim=c(500,4500),breaks=100,col="light blue",main= "Histogram of earnings",xlab="Hourly earnings",prob=TRUE)
axis(1, at=c(500,1500,2500,3500,4500))

good<-complete.cases(piaac.df$experience);
experience.modified<-piaac.df$experience[good];experience.modified

split.screen(c(1,3))
screen(1)
hist(experience.modified,freq=FALSE,breaks=c(0,5,10,15,20,25,30,35,40,45,47),ylim=c(0,0.04),col="light blue",main= "Histogram of experience",xlab="Years experience",ylab="Density",prob=TRUE)
axis(1, at=c(0,5,10,15,20,25,30,35,40,45,47))
lines(density(experience.modified),col="dark blue")
screen(2)
barplot(fj,col="lightblue",xlim=c(0,5),width=0.5,ylim=c(0,0.35),main = "Barblot of education",xlab="Years of schooling", ylab="Density")
screen(3)
hist(age,freq=FALSE,breaks=c(30,35,40,45,50,55,60),xlim=c(30,60),ylim=c(0,0.05),col="lightblue",main= "Histogram of age",xlab="Age",prob=TRUE)
axis(1, at=c(30,35,40,45,50,55,60))

# Boxplot
split.screen(c(1,2))
screen(1)
hist(earnings,probability=TRUE,histo = TRUE, col="light blue",border="white",xlim=c(0,750),breaks=100, main = "Histogram of earnings", xlab = "Hourly earnings")
lines(density(earnings),col="dark blue")
screen(2)
boxplot(hourly_earnings,horizontal=T,main="Boxplot of earnings",col="lightblue")
axis(1, at=c(0,1000,2000,3000,4000,4500))


# YEARS OF SCHOOLING

# Table 
table(years_schooling)

# Max and min
max(years_schooling, na.rm = TRUE)
min(years_schooling, na.rm = TRUE)

# Mean and median
mean(years_schooling, na.rm = TRUE)
median(years_schooling, na.rm = TRUE)

# Dispersion measures
var(years_schooling, na.rm = TRUE)
sd(years_schooling, na.rm = TRUE)
cv(years_schooling, na.rm = TRUE)
range<-max(years_schooling, na.rm = TRUE)-min(years_schooling, na.rm = TRUE);range
IQR(years_schooling, na.rm = TRUE)

# Skewness
skewness(years_schooling,na.rm = TRUE)

# Quartiles and summary
quantile(years_schooling,probs = c(1/4,1/2,3/4), na.rm = TRUE)
quantile(years_schooling,probs = seq(0, 1, 0.25), na.rm = TRUE)
summary(years_schooling)

# Relative frequencies
n<-length(years_schooling);n 
#the error from n leads to incorrect frequencies
fj<-table(years_schooling)/n;fj 

# Barplot (using relative freq.)
barplot(fj,col="lightblue",xlim=c(0,5),width=0.5,ylim=c(0,0.35),main = "Histogram of education",xlab="Years of schooling", ylab="Density")

# Boxplot
boxplot(years_schooling,horizontal=T,col="lightblue",main = "Education")

# WORK EXPERIENCE

# Table 
table(experience)

# Max and min
max(experience, na.rm = TRUE)
min(experience, na.rm = TRUE)

# Mean and median
mean(experience, na.rm = TRUE)
median(experience, na.rm = TRUE)

# Dispersion measures
var(experience, na.rm = TRUE)
sd(experience, na.rm = TRUE)
cv(experience, na.rm = TRUE)
range<-max(experience, na.rm = TRUE)-min(experience, na.rm = TRUE);range
IQR(experience, na.rm = TRUE)

# Skewness
skewness(experience,na.rm = TRUE)

# Quartiles and summary
quantile(experience,probs = c(1/4,1/2,3/4), na.rm = TRUE)
quantile(experience,probs = seq(0, 1, 0.25), na.rm = TRUE)
summary(experience)

# Absolute frequencies (intervals)
n1<-sum(experience<5, na.rm = TRUE);n1
n2<-sum((experience>=5)&(experience<10), na.rm = TRUE);n2
n3<-sum((experience>=10)&(experience<15), na.rm = TRUE);n3
n4<-sum((experience>=15)&(experience<20), na.rm = TRUE);n4
n5<-sum((experience>=20)&(experience<25), na.rm = TRUE);n5
n6<-sum((experience>=25)&(experience<30), na.rm = TRUE);n6
n7<-sum((experience>=30)&(experience<35), na.rm = TRUE);n7
n8<-sum((experience>=35)&(experience<40), na.rm = TRUE);n8
n9<-sum((experience>=40)&(experience<45), na.rm = TRUE);n9
n10<-sum(experience>=45, na.rm = TRUE);n10

# Frequency table (absolute frequencies)
n<-sum(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10);n
nj<-c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10);nj

# Relative frequencies
fj<-nj/sum(nj);fj

# Density Historgram
hist(experience,freq=FALSE,breaks=c(0,5,10,15,20,25,30,35,40,45,47),ylim=c(0,0.06),col="light blue",main= "Histogram of experience",xlab="Years experience",ylab="Density",prob=TRUE)
axis(1, at=c(0,5,10,15,20,25,30,35,40,45,47))

# Boxplot
boxplot(experience,horizontal=T)


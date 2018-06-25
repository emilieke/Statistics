# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Statistics")

# Install packages
install.packages("foreign")
install.packages("raster")
install.packages("MASS")
install.packages("moments")

# Include packages
library(foreign)
library(raster)
library(MASS) 
library(moments)

# Read data file
piaac.df = read.dta("PIAACdk.dta")

head(piaac.df)
dim(piaac.df)
attach(piaac.df)

# Split screen
split.screen(c(2,3))

# Exit split screen
dev.off()

# HOURLY EARNINGS

# Summary of hourly earnings
summary(hourly_earnings)

# Average hourly earnings given for each gender
tapply(hourly_earnings,as.factor(female_dummy),mean,na.rm = TRUE)
# Median hourly earnings given for each gender
tapply(hourly_earnings,as.factor(female_dummy),median,na.rm = TRUE)
# SD hourly earnings given for each gender
tapply(hourly_earnings,as.factor(female_dummy),sd,na.rm = TRUE)
# CV hourly earnings given for each gender
tapply(hourly_earnings,as.factor(female_dummy),cv,na.rm = TRUE)

# Average hourly earnings given for each citizenship
tapply(hourly_earnings,as.factor(immigrant_dummy),mean,na.rm = TRUE)
# Median hourly earnings given for each citizenship
tapply(hourly_earnings,as.factor(immigrant_dummy),median,na.rm = TRUE)
# SD hourly earnings given for each citizenship
tapply(hourly_earnings,as.factor(immigrant_dummy),sd,na.rm = TRUE)
# CV hourly earnings given for each citizenship
tapply(hourly_earnings,as.factor(immigrant_dummy),cv,na.rm = TRUE)

# Average hourly earnings given for each sector
tapply(hourly_earnings,as.factor(public_sector_dummy),mean,na.rm = TRUE)
# Median hourly earnings given for each sector
tapply(hourly_earnings,as.factor(public_sector_dummy),median,na.rm = TRUE)
# SD hourly earnings given for each sector
tapply(hourly_earnings,as.factor(public_sector_dummy),sd,na.rm = TRUE)
# CV hourly earnings given for each sector
tapply(hourly_earnings,as.factor(public_sector_dummy),cv,na.rm = TRUE)

# Histograms for hourly earnings given for each gender
split.screen(c(1,2))
screen(1)
hist(hourly_earnings[female_dummy==0],freq=FALSE,ylim=c(0,0.010),xlim=c(0,1000),breaks=50,col="light blue",main= "Male hourly earnings",xlab="Hourly earnings",prob=TRUE)
screen(2)
hist(hourly_earnings[female_dummy==1],freq=FALSE,ylim=c(0,0.010),xlim=c(0,1000),breaks=100,col="light cyan",main= "Female hourly earnings",xlab="Hourly earnings",prob=TRUE)

# Histograms for hourly earnings given for each citizenship
screen(1)
hist(hourly_earnings[immigrant_dummy==0],freq=FALSE,ylim=c(0,0.010),xlim=c(0,1000),breaks=100,col="light blue",main= "Native hourly earnings",xlab="Hourly earnings",prob=TRUE)
screen(2)
hist(hourly_earnings[immigrant_dummy==1],freq=FALSE,ylim=c(0,0.010),xlim=c(0,1000),breaks=50,col="light cyan",main= "Immigrant hourly earnings",xlab="Hourly earnings",prob=TRUE)

# Histograms for hourly earnings given for each sector
screen(1)
hist(hourly_earnings[public_sector_dummy==0],freq=FALSE,ylim=c(0,0.010),xlim=c(0,1000),breaks=50,col="light blue",main= "Private sector hourly earnings",xlab="Hourly earnings",prob=TRUE)
screen(2)
hist(hourly_earnings[public_sector_dummy==1],freq=FALSE,ylim=c(0,0.010),xlim=c(0,1000),breaks=100,col="light cyan",main= "Public sector hourly earnings",xlab="Hourly earnings",prob=TRUE)

# Boxplot hourly earnings given for each gender
boxplot(hourly_earnings[female_dummy==0],horizontal=T,main="Male hourly earnings",xlab="Hourly earnings",col="lightblue")
boxplot(hourly_earnings[female_dummy==1],horizontal=T,main="Female hourly earnings",xlab="Hourly earnings",col="lightcyan")

# Boxplot hourly earnings given for each citizenship
boxplot(hourly_earnings[immigrant_dummy==0],horizontal=T)
boxplot(hourly_earnings[immigrant_dummy==1],horizontal=T)

# Boxplot hourly earnings given for each sector
boxplot(hourly_earnings[public_sector_dummy==0],horizontal=T)
boxplot(hourly_earnings[public_sector_dummy==1],horizontal=T)


# YEARS SCHOOLING

# Summary of years of schooling
summary(years_schooling)

# Average years of schooling given for each gender
tapply(years_schooling,as.factor(female_dummy),mean,na.rm = TRUE)
# Median years of schooling given for each gender
tapply(years_schooling,as.factor(female_dummy),median,na.rm = TRUE)
# SD years of schooling given for each gender
tapply(years_schooling,as.factor(female_dummy),sd,na.rm = TRUE)
# CV years of schooling given for each gender
tapply(years_schooling,as.factor(female_dummy),cv,na.rm = TRUE)

# Average years of schooling given for each citizenship
tapply(years_schooling,as.factor(immigrant_dummy),mean,na.rm = TRUE)
# Median years of schooling given for each citizenship
tapply(years_schooling,as.factor(immigrant_dummy),median,na.rm = TRUE)
# SD years of schooling given for each citizenship
tapply(years_schooling,as.factor(immigrant_dummy),sd,na.rm = TRUE)
# CV years of schooling given for each citizenship
tapply(years_schooling,as.factor(immigrant_dummy),cv,na.rm = TRUE)

# Average years of schooling given for each sector
tapply(years_schooling,as.factor(public_sector_dummy),mean,na.rm = TRUE)
# Median years of schooling given for each sector
tapply(years_schooling,as.factor(public_sector_dummy),median,na.rm = TRUE)
# SD years of schooling given for each sector
tapply(years_schooling,as.factor(public_sector_dummy),sd,na.rm = TRUE)
# CV years of schooling given for each sector
tapply(years_schooling,as.factor(public_sector_dummy),cv,na.rm = TRUE)

# Relative frequencies
n_male<-length(years_schooling[female_dummy==0]);n_male 
fj_male<-table(years_schooling[female_dummy==0])/n_male;fj_male
n_female<-length(years_schooling[female_dummy==1]);n_female 
fj_female<-table(years_schooling[female_dummy==1])/n_female;fj_female 

# Barplot (using relative freq.)
screen(1)
barplot(fj_male,col="lightblue",ylim=c(0,0.5),main = "Male education",xlab="Years of schooling", ylab="Density")
screen(4)
barplot(fj_female,col="lightcyan",ylim=c(0,0.5),main = "Female education",xlab="Years of schooling", ylab="Density")

# Relative frequencies
n_native<-length(years_schooling[immigrant_dummy==0]);n_native #TODO this is not correct as it includes nan
fj_native<-table(years_schooling[immigrant_dummy==0])/n_native;fj_native #the error from n leads to incorrect frequencies
n_immigrant<-length(years_schooling[immigrant_dummy==1]);n_immigrant #TODO this is not correct as it includes nan
fj_immigrant<-table(years_schooling[immigrant_dummy==1])/n_immigrant;fj_immigrant #the error from n leads to incorrect frequencies

# Barplot (using relative freq.)
screen(2)
barplot(fj_native,col="lightblue",ylim=c(0,0.5),main = "Native education",xlab="Years of schooling", ylab="Density")
screen(5)
barplot(fj_immigrant,col="lightcyan",ylim=c(0,0.5),main = "Immigrant education",xlab="Years of schooling", ylab="Density")

# Relative frequencies
n_private<-length(years_schooling[public_sector_dummy==0]);n_private 
fj_private<-table(years_schooling[public_sector_dummy==0])/n_private;fj_private 
n_public<-length(years_schooling[public_sector_dummy==1]);n_public 
fj_public<-table(years_schooling[public_sector_dummy==1])/n_public;fj_public 

# Barplot (using relative freq.)
screen(3)
barplot(fj_private,col="lightblue",ylim=c(0,0.5),main = "Private sector educational",xlab="Years of schooling", ylab="Density")
screen(6)
barplot(fj_public,col="lightcyan",ylim=c(0,0.5),main = "Public sector educational",xlab="Years of schooling", ylab="Density")

# Boxplot years of schooling given for each gender
boxplot(years_schooling[female_dummy==0],horizontal=T,main="Male education",xlab="Years of schooling",col="lightblue")
boxplot(years_schooling[female_dummy==1],horizontal=T,main="Female education",xlab="Years of schooling",col="lightcyan")

# Boxplot years of schooling given for each citizenship
boxplot(years_schooling[immigrant_dummy==0],horizontal=T,main="Native education",xlab="Years schooling",col="lightblue")
boxplot(years_schooling[immigrant_dummy==1],horizontal=T,main="Immigrant education",xlab="Years schooling",col="lightcyan")

# Boxplot years of schooling given for each sector
boxplot(years_schooling[public_sector_dummy==0],horizontal=T,main="Private sector education",xlab="Years of schooling",col="lightblue")
boxplot(years_schooling[public_sector_dummy==1],horizontal=T,main="Public sector education",xlab="Years of schooling",col="lightcyan")

# EXPERIENCE

# Summary of years of working experience
summary(experience)

# Average experience given for each gender
tapply(experience,as.factor(female_dummy),mean,na.rm = TRUE)
# Median experience given for each gender
tapply(experience,as.factor(female_dummy),median,na.rm = TRUE)
# SD experience given for each gender
tapply(experience,as.factor(female_dummy),sd,na.rm = TRUE)
# CV experience given for each gender
tapply(experience,as.factor(female_dummy),cv,na.rm = TRUE)

# Average years of working experience given for each citizenship
tapply(experience,as.factor(immigrant_dummy),mean,na.rm = TRUE)
# Median years of working experience given for each citizenship
tapply(experience,as.factor(immigrant_dummy),median,na.rm = TRUE)
# SD years of working experience given for each citizenship
tapply(experience,as.factor(immigrant_dummy),sd,na.rm = TRUE)
# CV years of working experience given for each citizenship
tapply(experience,as.factor(immigrant_dummy),cv,na.rm = TRUE)

# Average years of working experience given for each sector
tapply(experience,as.factor(public_sector_dummy),mean,na.rm = TRUE)
# Median years of working experience given for each sector
tapply(experience,as.factor(public_sector_dummy),median,na.rm = TRUE)
# SD years of working experience given for each sector
tapply(experience,as.factor(public_sector_dummy),sd,na.rm = TRUE)
# CV years of working experience given for each sector
tapply(experience,as.factor(public_sector_dummy),cv,na.rm = TRUE)

# Histograms for years of working experience given for each gender
screen(1)
hist(experience[female_dummy==0],freq=F,main="Male experience",xlab="Years experience",ylim=c(0,0.04),xlim=c(0,50),col="lightblue",cex=2,prob=TRUE)
curve(dnorm(x, mean=mean(experience[female_dummy==0],na.rm=TRUE), sd=sd(experience[female_dummy==0],na.rm=TRUE)), add=TRUE)
screen(4)
hist(experience[female_dummy==1],freq=F,main="Female experience",xlab="Years experience",ylim=c(0,0.04),xlim=c(0,50),col="lightcyan",cex=2,prob=TRUE)
curve(dnorm(x, mean=mean(experience[female_dummy==1],na.rm=TRUE), sd=sd(experience[female_dummy==1],na.rm=TRUE)), add=TRUE)

# Histograms for years of working experience given for each citizenship
screen(2)
hist(experience[immigrant_dummy==0],freq=F,main="Native experience",xlab="Working experience",ylim=c(0,0.04),xlim=c(0,50),col="light blue",cex=2,prob=TRUE)
curve(dnorm(x, mean=mean(experience[immigrant_dummy==0],na.rm=TRUE), sd=sd(experience[immigrant_dummy==0],na.rm=TRUE)), add=TRUE)
screen(5)
hist(experience[immigrant_dummy==1],freq=F,main="Immigrant experience",xlab="Working experience",ylim=c(0,0.04),xlim=c(0,50),col="light cyan",cex=2,prob=TRUE)
curve(dnorm(x, mean=mean(experience[immigrant_dummy==1],na.rm=TRUE), sd=sd(experience[immigrant_dummy==1],na.rm=TRUE)), add=TRUE)

# Histograms for years of working experience given for each sector
screen(3)
hist(experience[public_sector_dummy==0],freq=F,main="Private sector experience",xlab="Working experience",ylim=c(0,0.04),xlim=c(0,50),col="light blue",cex=2,prob=TRUE)
curve(dnorm(x, mean=mean(experience[public_sector_dummy==0],na.rm=TRUE), sd=sd(experience[public_sector_dummy==0],na.rm=TRUE)), add=TRUE)
screen(6)
hist(experience[public_sector_dummy==1],freq=F,main="Public sector experience",xlab="Working experience",ylim=c(0,0.04),xlim=c(0,50),col="light cyan",cex=2,prob=TRUE)
curve(dnorm(x, mean=mean(experience[public_sector_dummy==1],na.rm=TRUE), sd=sd(experience[public_sector_dummy==1],na.rm=TRUE)), add=TRUE)


# Boxplot years of working experience given for each gender
boxplot(experience[female_dummy==0],horizontal=T,main="Male experience",xlab="Years experience",col="lightblue")
axis(1,at=c(0,10,20,30,40,47))
# TODO: how do i get 47 included on the x-axis?
boxplot(experience[female_dummy==1],horizontal=T,main="Female experience",xlab="Years experience",col="lightcyan")
axis(1,at=c(0,10,20,30,40,47))

# Boxplot years of working experience given for each citizenship
boxplot(experience[immigrant_dummy==0],horizontal=T,main="Native experience",xlab="Years experience",col="lightblue")
axis(1,at=c(0,10,20,30,40,47))
boxplot(experience[immigrant_dummy==1],horizontal=T,main="Immigrant experience",xlab="Years experience",col="lightcyan")
axis(1,at=c(0,10,20,30,40,47))

# Boxplot years of working experience given for each sector
screen(1)
boxplot(experience[public_sector_dummy==0],horizontal=T)
screen(2)
boxplot(experience[public_sector_dummy==1],horizontal=T)


# AGE

# Summary of age
summary(age)

# Average age given for each gender
tapply(age,as.factor(female_dummy),mean,na.rm = TRUE)
# Median age given for each gender
tapply(age,as.factor(female_dummy),median,na.rm = TRUE)
# SD age given for each gender
tapply(age,as.factor(female_dummy),sd,na.rm = TRUE)
# CV age given for each gender
tapply(age,as.factor(female_dummy),cv,na.rm = TRUE)

# Average age given for each citizenship
tapply(age,as.factor(immigrant_dummy),mean,na.rm = TRUE)
# Median age given for each citizenship
tapply(age,as.factor(immigrant_dummy),median,na.rm = TRUE)
# SD age given for each citizenship
tapply(age,as.factor(immigrant_dummy),sd,na.rm = TRUE)
# CV age given for each citizenship
tapply(age,as.factor(immigrant_dummy),cv,na.rm = TRUE)

# Average age given for each sector
tapply(age,as.factor(public_sector_dummy),mean,na.rm = TRUE)
# Median age given for each sector
tapply(age,as.factor(public_sector_dummy),median,na.rm = TRUE)
# SD age given for each sector
tapply(age,as.factor(immigrant_dummy),sd,na.rm = TRUE)
# CV age given for each sector
tapply(age,as.factor(immigrant_dummy),cv,na.rm = TRUE)


# Histograms for age given for each gender
screen(1)
hist(age[female_dummy==0],freq=F,breaks=c(30,35,40,45,50,55,60),main="Male age",xlab="Age",ylim=c(0,0.06),xlim=c(30,60),col="lightblue",cex=2,prob=TRUE)
#curve(dnorm(x, mean=mean(age[female_dummy==0],na.rm=TRUE), sd=sd(age[female_dummy==0],na.rm=TRUE)), add=TRUE)
screen(4)
hist(age[female_dummy==1],freq=F,breaks=c(30,35,40,45,50,55,60),main="Female age",xlab="Age",ylim=c(0,0.06),xlim=c(30,60),col="lightcyan",cex=2,prob=TRUE)
#curve(dnorm(x, mean=mean(age[female_dummy==1],na.rm=TRUE), sd=sd(age[female_dummy==1],na.rm=TRUE)), add=TRUE)

# Histograms for age given for each citizenship
screen(2)
hist(age[immigrant_dummy==0],freq=F,breaks=c(30,35,40,45,50,55,60),main="Native age",xlab="Age",ylim=c(0,0.06),xlim=c(30,60),col="light blue",cex=2,prob=TRUE)
#curve(dnorm(x, mean=mean(age[immigrant_dummy==0],na.rm=TRUE), sd=sd(age[immigrant_dummy==0],na.rm=TRUE)), add=TRUE)
screen(5)
hist(age[immigrant_dummy==1],freq=F,breaks=c(30,35,40,45,50,55,60),main="Immigrant age",xlab="Age",ylim=c(0,0.06),xlim=c(30,60),col="light cyan",cex=2,prob=TRUE)
#curve(dnorm(x, mean=mean(age[immigrant_dummy==1],na.rm=TRUE), sd=sd(age[immigrant_dummy==1],na.rm=TRUE)), add=TRUE)

# Histograms for age experience given for each sector
screen(3)
hist(age[public_sector_dummy==0],freq=F,breaks=c(30,35,40,45,50,55,60),main="Private sector age",xlab="Age",ylim=c(0,0.06),xlim=c(30,60),col="light blue",cex=2,prob=TRUE)
screen(6)
hist(age[public_sector_dummy==1],freq=F,breaks=c(30,35,40,45,50,55,60),main="Public sector age",xlab="Age",ylim=c(0,0.06),xlim=c(30,60),col="light cyan",cex=2,prob=TRUE)


# Boxplot age given for each gender
screen(1)
boxplot(age[female_dummy==0],horizontal=T)
screen(2)
boxplot(age[female_dummy==1],horizontal=T)

# Boxplot age given for each citizenship
screen(1)
boxplot(age[immigrant_dummy==0],horizontal=T)
screen(2)
boxplot(age[immigrant_dummy==1],horizontal=T)

# Boxplot age given for each sector
screen(1)
boxplot(age[public_sector_dummy==0],horizontal=T)
screen(2)
boxplot(age[public_sector_dummy==1],horizontal=T)


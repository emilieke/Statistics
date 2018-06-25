# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Statistics")

# Install packages
install.packages("foreign")
install.packages("raster")
install.packages("MASS")
install.packages("moments")
install.packages("flexsurv")
install.packages("fitdistrplus")
install.packages("ineq")
install.packages("hexbin")
install.packages("car")

# Include packages
library(foreign)
library(raster)
library(MASS) 
library(moments)
library(flexsurv)
library(fitdistrplus)
library(ineq)
library(hexbin)
library(car) 

# Read data file
piaac.df = read.dta("PIAACdk.dta")

head(piaac.df)
dim(piaac.df)
attach(piaac.df)

# EXPERIENCE AND HOURLY EARNINGS
mean(hourly_earnings, na.rm = TRUE)
mean(experience, na.rm = TRUE)

# Scatter plot
split.screen(c(1,2))
screen(1)
plot(experience,hourly_earnings,main = "Scatter plot of experience and earnings",ylab="Hourly earnings",xlab="Years experience",cex=1,lwd=1,xlim=c(0,50),ylim=c(0,4500),col=rgb(0,0,128,20,maxColorValue=250), pch=16)
abline(v=mean(experience, na.rm = TRUE),lty=2)
abline(h=mean(hourly_earnings, na.rm = TRUE),lty=2)
abline(lm(hourly_earnings~experience), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 
screen(2)
plot(experience,hourly_earnings,main = "Scatter plot of experience and earnings",ylab="Hourly earnings",xlab="Years experience",cex=1,lwd=1,xlim=c(0,50),ylim=c(0,750),col=rgb(0,0,128,20,maxColorValue=250), pch=16)
abline(v=mean(experience, na.rm = TRUE),lty=2)
abline(h=mean(hourly_earnings, na.rm = TRUE),lty=2)
abline(lm(hourly_earnings~experience), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 

plot(experience,hourly_earnings,main = "Scatter plot of experience and earnings",ylab="Hourly earnings",xlab="Years experience",cex=1,lwd=1,xlim=c(0,50),ylim=c(0,1600),col=rgb(0,0,128,20,maxColorValue=250), pch=16)
abline(v=mean(experience, na.rm = TRUE),lty=2)
abline(h=mean(hourly_earnings, na.rm = TRUE),lty=2)
abline(lm(hourly_earnings~experience), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 

# Log plots
split.screen(c(1,2))
screen(1)
plot(log(experience),hourly_earnings,main = "Scatter plot of log experience and earnings",ylab="Hourly earnings",xlab="log years experience",cex=1,lwd=1,xlim=c(0,4),ylim=c(0,750),col=rgb(0,0,128,20,maxColorValue=250), pch=16)
abline(v=mean(log(experience), na.rm = TRUE),lty=2)
abline(h=mean(hourly_earnings, na.rm = TRUE),lty=2)
abline(lm(hourly_earnings~log(experience)), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 
screen(2)
plot(experience,log(hourly_earnings),main = "Scatter plot of experience and log earnings",ylab="Log hourly earnings",xlab="Years experience",cex=1,lwd=1,xlim=c(0,50),ylim=c(4,7),col=rgb(0,0,128,20,maxColorValue=250), pch=16)
abline(v=mean(experience, na.rm = TRUE),lty=2)
abline(h=mean(log(hourly_earnings), na.rm = TRUE),lty=2)
abline(lm(log(hourly_earnings)~experience), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 

bin<-hexbin(experience, hourly_earnings, xbins=50) 
plot(bin, main="Hexagonal Binning")

# Covariance and Pearson's linear correlation coef. 
cov(experience,hourly_earnings, use = "pairwise.complete.obs")
cor(experience,hourly_earnings, use = "pairwise.complete.obs")

cov(log(experience),log(hourly_earnings), use = "pairwise.complete.obs")
cor(log(experience),log(hourly_earnings), use = "pairwise.complete.obs")

cov(log(experience),hourly_earnings, use = "pairwise.complete.obs")
cor(log(experience),hourly_earnings, use = "pairwise.complete.obs")

cov(experience,log(hourly_earnings), use = "pairwise.complete.obs")
cor(experience,log(hourly_earnings), use = "pairwise.complete.obs")

# HOURLY EARNINGS AND YEARS OF SCHOOLING
mean(years_schooling,na.rm = TRUE)
mean(hourly_earnings,na.rm = TRUE)

# Scatter plot
screen(1)
plot(years_schooling,hourly_earnings,main="Scatter plot of education and earnings",xlab="Years of schooling",ylab="Hourly earnings",col=rgb(0,0,128,20,maxColorValue=250), pch=16,cex=1,lwd=1)
abline(v=mean(years_schooling,na.rm = TRUE),lty=2)
abline(h=mean(hourly_earnings,na.rm = TRUE),lty=2)
abline(lm(hourly_earnings~years_schooling), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 
screen(2)
plot(years_schooling,hourly_earnings,main="Scatter plot of education and earnings",xlab="Years of schooling",ylab="Hourly earnings",ylim=c(0,750),col=rgb(0,0,128,20,maxColorValue=250), pch=16,cex=1,lwd=1)
abline(v=mean(years_schooling,na.rm = TRUE),lty=2)
abline(h=mean(hourly_earnings,na.rm = TRUE),lty=2)
abline(lm(hourly_earnings~years_schooling), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 

# Log plots
screen(1)
plot(log(years_schooling),hourly_earnings,main="Scatter plot of earnings and log education",xlab="Log years of schooling",ylab="Hourly earnings",col=rgb(0,0,128,20,maxColorValue=250),ylim=c(0,750),pch=16,cex=1,lwd=1)
abline(v=mean(log(years_schooling),na.rm = TRUE),lty=2)
abline(h=mean(hourly_earnings,na.rm = TRUE),lty=2)
abline(lm(hourly_earnings~log(years_schooling)), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 
screen(2)
plot(years_schooling,log(hourly_earnings),main="Scatter plot of log earnings and education",xlab="Years of schooling",ylab="Log hourly earnings",ylim=c(3,8),col=rgb(0,0,128,20,maxColorValue=250), pch=16,cex=1,lwd=1)
abline(v=mean(years_schooling,na.rm = TRUE),lty=2)
abline(h=mean(log(hourly_earnings),na.rm = TRUE),lty=2)
abline(lm(log(hourly_earnings)~years_schooling), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 


# Covariance and Pearson's linear correlation coef.
cov(years_schooling,hourly_earnings,use = "pairwise.complete.obs")
cor(years_schooling,hourly_earnings,use = "pairwise.complete.obs")

cov(log(years_schooling),hourly_earnings,use = "pairwise.complete.obs")
cor(log(years_schooling),hourly_earnings,use = "pairwise.complete.obs")

cov(years_schooling,log(hourly_earnings),use = "pairwise.complete.obs")
cor(years_schooling,log(hourly_earnings),use = "pairwise.complete.obs")

# HOURLY EARNINGS AND AGE
mean(age)
mean(hourly_earnings,na.rm = TRUE)

# Scatter plot
screen(1)
plot(age,hourly_earnings,main="Scatter plot of age and earnings",xlab="Age",ylab="Hourly earnings",col=rgb(0,0,128,20,maxColorValue=250), pch=16,cex=1,lwd=1)
abline(v=mean(age),lty=2)
abline(h=mean(hourly_earnings,na.rm = TRUE),lty=2)
abline(lm(hourly_earnings~age), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 
screen(2)
plot(age,hourly_earnings,main="Scatter plot of age and earnings",xlab="Age",ylab="Hourly earnings",col=rgb(0,0,128,20,maxColorValue=250), pch=16,cex=1,lwd=1,ylim=c(0,750))
abline(v=mean(age),lty=2)
abline(h=mean(hourly_earnings,na.rm = TRUE),lty=2)
abline(lm(hourly_earnings~age), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 

screen(1)
plot(log(age),hourly_earnings,main="Scatter plot of earnings and log age",xlab="Log age",ylab="Hourly earnings",col=rgb(0,0,128,20,maxColorValue=250), ylim=c(0,750),pch=16,cex=1,lwd=1)
abline(v=mean(log(age)),lty=2)
abline(h=mean(hourly_earnings,na.rm = TRUE),lty=2)
abline(lm(hourly_earnings~log(age)), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 
screen(2)
plot(age,log(hourly_earnings),main="Scatter plot of log earnings and age",xlab="Age",ylab="Log hourly earnings",col=rgb(0,0,128,20,maxColorValue=250), pch=16,cex=1,lwd=1,ylim=c(3,8))
abline(v=mean(age),lty=2)
abline(h=mean(log(hourly_earnings),na.rm = TRUE),lty=2)
abline(lm(log(hourly_earnings)~age), col=rgb(178,34,34,178,maxColorValue=178)) # regression line (y~x) 

# Covariance and Pearson's linear correlation coef.
cov(age,hourly_earnings,use = "pairwise.complete.obs")
cor(age,hourly_earnings,use = "pairwise.complete.obs")

cov(log(age),hourly_earnings,use = "pairwise.complete.obs")
cor(log(age),hourly_earnings,use = "pairwise.complete.obs")

cov(age,log(hourly_earnings),use = "pairwise.complete.obs")
cor(age,log(hourly_earnings),use = "pairwise.complete.obs")

# EXPERIENCE AND YEARS OF SCHOOLING
mean(years_schooling, na.rm = TRUE)
mean(experience, na.rm = TRUE)

# Scatter plot
plot(years_schooling,experience,main="Scatter plot of education and experience",xlab="Years schooling",ylab="Years experience",col=rgb(0,0,128,20,maxColorValue=250), pch=16,cex=1,lwd=1)
abline(v=mean(years_schooling, na.rm = TRUE),lty=2)
abline(h=mean(experience, na.rm = TRUE),lty=2)

# Covariance and Pearson's linear correlation coef.
cov(years_schooling,experience,use = "pairwise.complete.obs")
cor(years_schooling,experience,use = "pairwise.complete.obs")

# EXPERIENCE AND AGE
mean(age)
mean(experience, na.rm = TRUE)

# Scatter plot
plot(age,experience,main="Scatter plot of age and experience",xlab="Age",ylab="Years experience",col=rgb(0,0,128,20,maxColorValue=250), pch=16,cex=1,lwd=1)
abline(v=mean(age),lty=2)
abline(h=mean(experience,na.rm = TRUE),lty=2)

# Covariance and Pearson's linear correlation coef. between the two grades
cov(age,experience,use = "pairwise.complete.obs")
cor(age,experience,use = "pairwise.complete.obs")

# YEARS OF SCHOOLING AND AGE
mean(age)
mean(years_schooling,na.rm = TRUE)

# Scatter plot
plot(age,years_schooling,main="Scatter plot of age and education",xlab="Age",ylab="Years of Schooling",col=rgb(0,0,128,20,maxColorValue=250), pch=16,cex=1,lwd=1)
abline(v=mean(age),lty=2)
abline(h=mean(years_schooling,na.rm = TRUE),lty=2)

# Covariance and Pearson's linear correlation coef. between the two grades
cov(age,years_schooling)
cor(age,years_schooling)


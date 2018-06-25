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


# Include packages
library(foreign)
library(raster)
library(MASS) 
library(moments)
library(flexsurv)
library(fitdistrplus)
library(ineq)


# Read data file
piaac.df = read.dta("PIAACdk.dta")

head(piaac.df)
dim(piaac.df)
attach(piaac.df)

#---------------------------------------------------------------------------------------------#
# 7. MODEL SELECTION
#---------------------------------------------------------------------------------------------#

data("piaac.df", package = "fitdistrplus")
str(piaac.df)

good<-complete.cases(piaac.df$hourly_earnings);
income<-piaac.df$hourly_earnings[good];income


split.screen(c(1,3))
screen(1)
hist(earnings,probability=TRUE,histo = TRUE, col="light blue",border="white",xlim=c(0,750),breaks=100, main = "Histogram with empirical density", xlab = "Hourly earnings")
lines(density(income),col="dark blue")
screen(2)
# obtain empirical CDF values
earnings.ecdf = ecdf(earnings);earnings.ecdf
plot(earnings.ecdf, xlim=c(0,1500),col=rgb(0,0,128,50,maxColorValue=250),pch=16,cex=1,lwd=5,xlab = 'Sample Quantiles', ylab = 'CDF', main = 'Cumluative Distribution')
screen(3)
qqnorm(earnings,col=rgb(0,0,128,50,maxColorValue=250),pch=16,cex=1,lwd=1); qqline(earnings)

fit_g<-fitdist(income,"gamma");fit_g
fit_ln<-fitdist(income,"lnorm");fit_ln
fit_w<-fitdist(income,"weibull");fit_w
plot.legend <- c("weibull","lognormal","gamma")

par(mfrow = c(1, 3))

u=seq(0,800,length=251)
hist(income,probability=TRUE, col="light blue",border="white",xlim=c(0,750),breaks=150, main = "Histogram with theoretical densities", xlab = "Hourly earnings")
v_g <- dgamma(u, fit_g$estimate[1],fit_g$estimate[2])
v_ln <- dlnorm(u, fit_ln$estimate[1],fit_ln$estimate[2])
v_w <- dweibull(u, fit_w$estimate[1],fit_w$estimate[2])
lines(u,v_g,col="blue",lwd=1.5,cex=0.5)
lines(u,v_ln,col="dark blue",lwd=1.5,cex=0.5)
lines(u,v_w,col="dark cyan",lwd=1.5,cex=0.5)
legend(470,0.008,legend=c("weibull", "lognormal", "gamma"),fill=c("dark cyan","dark blue","blue"),border="white",bty = "n")

qqcomp(list(fit_w, fit_ln, fit_g), fitcol=c("dark cyan","dark blue","blue"),xlegend = "topleft", legendtext = plot.legend, cex=0.9)

cdfcomp(list(fit_w, fit_ln, fit_g), xlim=c(0,700),fitlty=c(1,1,1),xlab = "Hourly earnings",datacol="light blue",fitcol=c("dark cyan","dark blue","blue"),xlegend = "topleft", legendtext = plot.legend, cex=0.9)

ppcomp(list(fit_w, fit_ln, fit_g),fitcol=c("dark cyan","dark blue","blue"),xlegend = "topleft", legendtext = plot.legend, cex=0.8)

summary(fit_w)
summary(fit_g)
summary(fit_ln)

# Extra
n=length(income)
x=log(sort(income))
y=log((n:1)/n)
plot(x,y) 
plot(x,y,xlim = c(0,8))

plot(Lc(income))
lines(Lc.lognorm,param=1.5,col="red")
lines(Lc.lognorm,param=1.2,col="red")
lines(Lc.lognorm,param=.8,col="red")
lines(Lc.lognorm,param=.2,col="red")

plot(Lc(income))
lines(Lc.pareto,param=2,col="red")
lines(Lc.pareto,param=1.5,col="red")
lines(Lc.pareto,param=1.2,col="red")







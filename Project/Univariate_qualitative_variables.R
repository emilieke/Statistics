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

#---------------------------------------------------------------------------------------------#
# 3. UNVARIATE DESCRIPTION
#---------------------------------------------------------------------------------------------#

# 3.1 Qualitative variables

# GENDER

# Table
male<-table(male_dummy);male
female<-table(female_dummy);female

# Absolute frequencies
number_of_males<-sum(male_dummy==1);number_of_males
number_of_females<-sum(female_dummy==1);number_of_females
n<-sum(number_of_males,number_of_females);n
nj<-c(number_of_males,number_of_females);nj

# Relative frequencies
fj<-nj/sum(nj);fj

dev.off()
split.screen(c(1,3))

# Barplot (using relative freq.)
screen(1)
barplot(fj,names.arg = c("Male", "Female"),width=1,xlim=c(0,2.5),col=c("lightblue","lightcyan"),ylab="Density",ylim=c(0,1.0),main = "Gender")

# Barplot (using relative freq.)
screen(2)
barplot(fj_citizenship,names.arg = c("Native", "Immigrant"),width=1,xlim=c(0,2.5),col=c("lightblue","lightcyan"),ylab="Density",ylim=c(0,1.0),main = "Immigrant status")

# Barplot (using relative freq.)
screen(3)
barplot(fj_sector,names.arg = c("Private", "Public"),width=1,xlim=c(0,2.5),col=c("lightblue","lightcyan"),ylab="Density",ylim=c(0,1.0),main = "Working Sector")

# Piechart (using relative freq.)
pie(fj,main="Gender",names.arg = c("Male", "Female"),col=c("lightblue","lightcyan"))

# CITIZENSHIP

# Table
native<-table(native_dummy);native
immigrant<-table(immigrant_dummy);immigrant

# Absolute frequencies
number_of_natives<-sum(native_dummy==1);number_of_natives
number_of_immigrants<-sum(immigrant_dummy==1);number_of_immigrants
n<-sum(number_of_natives,number_of_immigrants);n
nj<-c(number_of_natives,number_of_immigrants);nj

# Relative frequencies
fj_citizenship<-nj/sum(nj);fj_citizenship

# Barplot (using relative freq.)
barplot(fj_citizenship,names.arg = c("Native", "Immigrant"),width=1,xlim=c(0,2.5),col=c("lightblue","lightcyan"),ylab="Density",ylim=c(0,1.0),main = "Immigrant status")

# Piechart (using relative freq.)
pie(fj,main=list("Citizenship", font = 1, cex=2))

# WORKING SECTOR

# Table
private<-table(private_sector_dummy);private
public<-table(public_sector_dummy);public

# Absolute frequencies
number_of_private<-sum(private_sector_dummy==1);number_of_private
number_of_public<-sum(public_sector_dummy==1);number_of_public
n<-sum(number_of_private,number_of_public);n
nj<-c(number_of_private,number_of_public);nj

# Relative frequencies
fj_sector<-nj/sum(nj);fj_sector

# Barplot (using relative freq.)
barplot(fj_sector,names.arg = c("Private", "Public"),width=1,xlim=c(0,2.5),col=c("lightblue","lightcyan"),ylab="Density",ylim=c(0,1.0),main = "Working Sector")

# Piechart (using relative freq.)
pie(fj,main=list("Working Sector", font = 1, cex=2))



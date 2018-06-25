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
# 4. BIVARIATE DESCRIPTION
#---------------------------------------------------------------------------------------------#

# 4.1 Both variables qualitative

# GENDER AND WORKING SECTOR

# Absolute frequency tables (marginals, cross-classified)
table(public_sector_dummy)
table(female_dummy)
table(public_sector_dummy,female_dummy)

# Sample size
n<-sum(table(table(public_sector_dummy)));n

# Relative frequency tables (marginals, cross-classified)
100*table(public_sector_dummy)/n
100*table(female_dummy)/n
100*table(female_dummy,public_sector_dummy)/n
prop.table(table(female_dummy,public_sector_dummy))*100

# Distribution of gender status given each working sector (conditional distribution)
cond<-prop.table(table(female_dummy,public_sector_dummy),1)*100
cond

# Grouped barplot of gender status given each working sector
barplot(t(cond),beside=T,main = "Grouped barplot for working sector",names.arg = c("Male", "Female"),col=c("lightblue","lightcyan"),ylab="Conditional Frec. (%)",ylim=c(0,100))
legend(4.5,100,legend=c("Private", "Public"),fill=c("lightblue","lightcyan"), bty = "n")

# CITIZENSHIP AND WORKING SECTOR

# Absolute frequency tables (marginals, cross-classified)
table(public_sector_dummy)
table(immigrant_dummy)
table(public_sector_dummy,immigrant_dummy)

# Sample size
n<-sum(table(table(public_sector_dummy)));n

# Relative frequency tables (marginals, cross-classified)
100*table(public_sector_dummy)/n
100*table(immigrant_dummy)/n
100*table(immigrant_dummy,public_sector_dummy)/n
prop.table(table(immigrant_dummy,public_sector_dummy))*100

# Distribution of citizenship given the working sector (conditional distribution)
cond<-prop.table(table(immigrant_dummy,public_sector_dummy),1)*100
cond

# Grouped barplot of citizenship given the working sector
barplot(t(cond),beside=T,main = "Working sector",names.arg = c("Native", "Immigrant"),col=c("lightblue","lightcyan"),ylab="Conditional Frec. (%)",ylim=c(0,100))
legend(4.5,100,legend= c("Private", "Public"),fill=c("lightblue","lightcyan"), bty = "n")

# CITIZENSHIP AND GENDER

# Absolute frequency tables (marginals, cross-classified)
table(female_dummy)
table(immigrant_dummy)
table(female_dummy,immigrant_dummy)

# Sample size
n<-sum(table(table(female_dummy)));n

# Relative frequency tables (marginals, cross-classified)
100*table(female_dummy)/n
100*table(immigrant_dummy)/n
100*table(immigrant_dummy,female_dummy)/n
prop.table(table(immigrant_dummy,female_dummy))*100

# Distribution of gender given the citizenship (conditional distribution)
cond<-prop.table(table(immigrant_dummy,female_dummy),1)*100
cond

# Grouped barplot of gender given the citizenship
barplot(t(cond),beside=T,main = "Gender",names.arg = c("Native", "Immigrant"),col=c("lightblue","lightcyan"),ylab="Conditional Frec. (%)",ylim=c(0,100))
legend(4.5,100,legend= c("Male", "Female"),fill=c("lightblue","lightcyan"), bty = "n")
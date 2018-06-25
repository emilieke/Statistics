# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Statistics")

# Install packages
install.packages("foreign")
install.packages("raster")
install.packages("MASS")
install.packages("moments")
install.packages("flexsurv")

# Include packages
library(foreign)
library(raster)
library(MASS) 
library(moments)
library(flexsurv)

# Read data file
piaac.df = read.dta("PIAACdk.dta")

head(piaac.df)
dim(piaac.df)
attach(piaac.df)

#---------------------------------------------------------------------------------------------#
# 5. INFERENCE
#---------------------------------------------------------------------------------------------#

# THE PROPORTION BETWEEN MALE AND FEMALE IN WORKING SECTOR
table(public_sector_dummy,female_dummy)
prop.test(table(public_sector_dummy,female_dummy),correct=TRUE)

# Difference between proportion private sector workers in male and female population
prop.test(table(female_dummy,public_sector_dummy),correct=FALSE)

# Alternative

# Absolute frequencies
n_males<-sum(male_dummy==1);n_males
n_females<-sum(female_dummy==1);n_females
n_private_males<-sum(private_sector_dummy[male_dummy==1]);n_private_males
n_private_females<-sum(private_sector_dummy[female_dummy==1]);n_private_females
n<-sum(n_males,n_females);n
n_difference<-n_males-n_females;n_difference

# Relative frequencies
fj_males<-n_private_males/n_males;fj_males
fj_females<-n_private_females/n_females;fj_females
fj_difference <- max(fj_males,fj_females) - min(fj_males,fj_females);fj_difference

# standard error 
SE_male <- sqrt(fj_males*(1-fj_males)/n_males); SE_male
SE_female <- sqrt(fj_females*(1-fj_females)/n_females); SE_female
SE_difference <- sqrt(SE_male^2 + SE_female^2);SE_difference

# margin of error 
E_male <- qnorm(.975)*SE_male + (0.5/n_males); E_male
conf_male <- fj_males + c(-E_male, E_male);conf_male

E_female <- qnorm(.975)*SE_female + (0.5/n_females); E_female
conf_female <- fj_females + c(-E_female, E_female);conf_female

E_difference <- qnorm(.975)*SE_difference; E_difference
conf_difference <- fj_difference + c(-E_difference, E_difference);conf_difference

# Difference between proportion of male in private and public sector
prop.test(table(public_sector_dummy,female_dummy),correct=FALSE, alternative  = "greater")

# Absolute frequencies
n_private<-sum(private_sector_dummy==1);n_private
n_public<-sum(public_sector_dummy==1);n_public
n_males_private<-sum(male_dummy[private_sector_dummy==1]);n_males_private
n_males_public<-sum(male_dummy[public_sector_dummy==1]);n_males_public
n<-sum(n_private,n_public);n
n_difference<-n_private-n_public;n_difference

# Relative frequencies
fj_private<-n_males_private/n_private;fj_private
fj_public<-n_males_public/n_public;fj_public
fj_difference <- max(fj_private,fj_public) - min(fj_private,fj_public);fj_difference

# standard error 
SE_private <- sqrt(fj_private*(1-fj_private)/n_private); SE_private
SE_public <- sqrt(fj_public*(1-fj_public)/n_public); SE_public
SE_difference <- sqrt(SE_private^2 + SE_public^2);SE_difference

# margin of error 
E_private <- qnorm(.975)*SE_private + (0.5/n_private); E_private
conf_private <- fj_private + c(-E_private, E_private);conf_private

E_public <- qnorm(.975)*SE_public + (0.5/n_public); E_public
conf_public <- fj_public + c(-E_public, E_public);conf_public

E_difference <- qnorm(.975)*SE_difference; E_difference
conf_difference <- fj_difference + c(-E_difference, E_difference);conf_difference


# Confidence interval for a difference of two means

# MEAN EARNINGS BETWEEN MALE AND FEMALE
female = piaac.df$female_dummy == 1;female
female_private = female[private_sector_dummy==1];female_private

male = piaac.df$male_dummy == 1;male
male_private = male[private_sector_dummy==1];male_private

earnings.female = piaac.df[female,]$hourly_earnings;earnings.female
earnings.male = piaac.df[!female,]$hourly_earnings;earnings.male

res.ftest <- var.test(earnings.male, earnings.female,alternative = "two.sided");res.ftest

t.test(earnings.male)
t.test(earnings.female)
t.test(earnings.male,earnings.female)

earnings.female_private = piaac.df[female_private,]$hourly_earnings;earnings.female_private
earnings.male_private = piaac.df[male_private,]$hourly_earnings;earnings.male_private

res.ftest_priate <- var.test(earnings.male_private, earnings.female_private, alternative = "two.sided");res.ftest_priate

t.test(earnings.male_private)
t.test(earnings.female_private)
t.test(earnings.male_private,earnings.female_private, alternative = "two.sided")

# MEAN EARNINGS BETWEEN NATIVE AND IMMIGRANTS
immigrant = piaac.df$immigrant_dummy == 1;immigrant

earnings.immigrant = piaac.df[immigrant,]$hourly_earnings;earnings.immigrant
earnings.native = piaac.df[!immigrant,]$hourly_earnings;earnings.native

t.test(earnings.native)
t.test(earnings.immigrant)
t.test(earnings.native,earnings.immigrant)

# MEAN EARNINGS BETWEEN PRIVATE AND PUBLIC
public = piaac.df$public_sector_dummy == 1;public

earnings.public = piaac.df[public,]$hourly_earnings;earnings.public
earnings.private = piaac.df[!public,]$hourly_earnings;earnings.private

t.test(earnings.private)
t.test(earnings.public)
t.test(earnings.private,earnings.public)


# Set working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Statistics")

# Install packages
install.packages("foreign")
install.packages("raster")
install.packages("MASS")
install.packages("moments")
install.packages("flexsurv")
install.packages("data.table")

# Include packages
library(foreign)
library(raster)
library(MASS) 
library(moments)
library(flexsurv)
library(data.table)

# Read data file
piaac.df = read.dta("PIAACdk.dta")

head(piaac.df)
dim(piaac.df)
attach(piaac.df)

#---------------------------------------------------------------------------------------------#
# 6. SAMPLING
#---------------------------------------------------------------------------------------------#

# 6.1 

# Relative frequency tables (cross-classified)
gender_sector_table<-prop.table(table(female_dummy,public_sector_dummy))*100;gender_sector_table


piaac_table <-data.table(piaac.df)

male_private<-piaac_table[male_dummy==1 & private_sector_dummy==1 & complete.cases(piaac_table)];male_private
sample_male_private<- male_private[sample(nrow(male_private), 130, prob = NULL),];sample_male_private

female_private<-piaac_table[female_dummy==1 & private_sector_dummy==1 & complete.cases(piaac_table)];female_private
sample_female_private<- female_private[sample(nrow(female_private), 130, prob = NULL),];sample_female_private

male_public<-piaac_table[male_dummy==1 & public_sector_dummy==1 & complete.cases(piaac_table)];male_public
sample_male_public<- male_public[sample(nrow(male_public), 130, prob = NULL),];sample_male_public

female_public<-piaac_table[female_dummy==1 & public_sector_dummy==1 & complete.cases(piaac_table)];female_public
sample_female_public<- female_public[sample(nrow(female_public), 130, prob = NULL),];sample_female_public

total_true_mean<-mean(piaac_table$hourly_earnings,na.rm=TRUE);total_true_mean
mean_male_private<-mean(male_private$hourly_earnings);mean_male_private
mean_sample_male_private<-mean(sample_male_private$hourly_earnings);mean_sample_male_private
diff_male_private<-100*(mean_male_private - mean_sample_male_private)/mean_male_private;diff_male_private

mean_female_private<-mean(female_private$hourly_earnings);mean_female_private
mean_sample_female_private<-mean(sample_female_private$hourly_earnings);mean_sample_female_private
diff_female_private<-100*(mean_female_private - mean_sample_female_private)/mean_female_private;diff_female_private

mean_male_public<-mean(male_public$hourly_earnings);mean_male_public
mean_sample_male_public<-mean(sample_male_public$hourly_earnings);mean_sample_male_public
diff_male_public<-100*(mean_male_public - mean_sample_male_public)/mean_male_public;diff_male_public

mean_female_public<-mean(female_public$hourly_earnings);mean_female_public
mean_sample_female_public<-mean(sample_female_public$hourly_earnings);mean_sample_female_public
diff_female_public<-100*(mean_female_public - mean_sample_female_public)/mean_female_public;diff_female_public

# Total population
n<-sum(public_sector_dummy,private_sector_dummy);n

# Relative frequencies
n_male_private<-sum(male_dummy[private_sector_dummy==1]);n_male_private
n_female_private<-sum(female_dummy[private_sector_dummy==1]);n_female_private
n_male_public<-sum(male_dummy[public_sector_dummy==1]);n_male_public
n_female_public<-sum(female_dummy[public_sector_dummy==1]);n_female_public

fj_male_private<-(n_male_private/n);fj_male_private
fj_female_private<-(n_female_private/n);fj_female_private
fj_male_public<-(n_male_public/n);fj_male_public
fj_female_public<-(n_female_public/n);fj_female_public

total_sample_mean<-(fj_male_private*mean_sample_male_private+fj_female_private*mean_sample_female_private+fj_male_public*mean_sample_male_public+fj_female_public*mean_sample_female_public);total_sample_mean

# Proportions of native and immigrants

# Relative frequencies natives
n_native_male_private<-sum(sample_male_private$native_dummy);n_native_male_private
n_native_female_private<-sum(sample_female_private$native_dummy);n_native_female_private
n_native_male_public<-sum(sample_male_public$native_dummy);n_native_male_public
n_native_female_public<-sum(sample_female_public$native_dummy);n_native_female_public

n_total_native<-(n_native_male_private+n_native_female_private+n_native_male_public+n_native_female_public);n_total_native

fj_native<-(n_total_native/520);fj_native

# Relative frequencies immigrants
n_immigrant_male_private<-sum(sample_male_private$immigrant_dummy);n_immigrant_male_private
n_immigrant_female_private<-sum(sample_female_private$immigrant_dummy);n_immigrant_female_private
n_immigrant_male_public<-sum(sample_male_public$immigrant_dummy);n_immigrant_male_public
n_immigrant_female_public<-sum(sample_female_public$immigrant_dummy);n_immigrant_female_public

n_total_immigrant<-(n_immigrant_male_private+n_immigrant_female_private+n_immigrant_male_public+n_immigrant_female_public);n_total_immigrant
total<-(n_total_native+n_total_immigrant);total

fj_native<-(n_total_immigrant/520);fj_native

# Relative frequencies for each stratum

fj_native_male_private=100*(n_native_male_private/(n_immigrant_male_private+n_native_male_private));fj_native_male_private
fj_immigrant_male_private=(100-fj_native_male_private);fj_immigrant_male_private

fj_native_female_private<-100*n_native_female_private/(n_native_female_private+n_immigrant_female_private);fj_native_female_private
fj_immigrant_female_private<-(100-fj_native_female_private);fj_immigrant_female_private

fj_native_male_public<-100*n_native_male_public/(n_native_male_public+n_immigrant_male_public);fj_native_male_public
fj_immigrant_male_public<-(100-fj_native_male_public);fj_immigrant_male_public

fj_native_female_public<-100*n_native_female_public/(n_native_female_public+n_immigrant_female_public);fj_native_female_public
fj_immigrant_female_public<-(100-fj_native_female_public);fj_immigrant_female_public

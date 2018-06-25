setwd("C:/Users/pt1a511/Downloads")

# converting dta file to txt, this is working
install.packages("rio")
library("rio")
convert("PIAACdk.dta", "PIAAC.txt")

# reading dta file
library(foreign)
piaac.df = read.dta("C://Users//pt1a511//Downloads//PIAACdk.dta")

# creating a summary
summary(piaac.df)


# dont think these are working
# datafile<-tempfile()
# codefile<-tempfile()
# write.foreign(piaac.df,datafile,codefile,package="SPSS")
# file.show(datafile)
# file.show(codefile)
# unlink(datafile)
# unlink(codefile)

# write.foreign(piaac.df, "C://Users//pt1a511//Downloads//PIAACdk.txt")

# install.packages("xlsx", dependencies = TRUE)
# library(xlsx)
# data(piaac.df)
# write.xlsx(piaac.df, "piaac.xlsx")
# write.xlsx(piaac.df, "C:/Users/pt1a511/Downloads/PIAACdk.xlsx")


gc()
options(scipen = 999)
options(na.action = "na.exclude")

#use packages
install.packages('lfe')
install.packages('sandwich')
install.packages('lmtest')
install.packages('ggplot2')
install.packages("plm")
install.packages("car")
library(lfe)
library(sandwich)
library(lmtest)
library(ggplot2)
library(plm)
library(car)

rm(list=ls())
data<-read.csv("DACAdata.csv")

#add groups cols

#q3
eligible<-subset(data, elig==1)
famsize_avg<-mean(eligible$famsize)
inpov_avg<-mean(eligible$inpov)
any_col_avg<-mean(eligible$any_col)
nonfluent_avg<-mean(eligible$nonfluent)

not_eligible <- subset(data, noncit==0)
famsize_avg<-mean(not_eligible$famsize)
inpov_avg<-mean(not_eligible$inpov)
any_col_avg<-mean(not_eligible$any_col)
nonfluent_avg<-mean(not_eligible$nonfluent)


#q4
data_before_DACA<-subset(data, post==0)
lm<-lm(hs ~ nsibs + inpov + any_col + nonfluent + ageimmig + mompresent + dadpresent + noncit, data=data_before_DACA)
summary(lm)

#q6
q6_data <- subset(data, ageimmig <=10, yrimmig <= 2007, age >=14, age <=18)
averages<-aggregate(inschool ~ elig+year, data=g6_data, mean)
ggplot(data=averages, mapping = aes(x=year, y=inschool, color=elig))+ geom_line()

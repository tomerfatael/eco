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
q6_data <- subset(data, ageimmig<=10 & yrimmig<=2007 & age<=18 & age>=14)
q6_data$tested_or_comparison[q6_data$elig == 1 & q6_data$noncit == 1]<-"tested"
q6_data$tested_or_comparison[q6_data$elig == 0 & q6_data$noncit == 0]<-"comparison"
q6_graph<-aggregate(inschool ~ tested_or_comparison+year, data=q6_data, mean)
ggplot(data=q6_graph, mapping = aes(x=year, y=inschool, color=tested_or_comparison))+geom_line()

#q7
q7_data <- subset(data, ageimmig<=10 & yrimmig<=2007 & age<=22 & age>=19)
q7_data$tested_or_comparison[q7_data$elig == 1 & q7_data$noncit == 1]<-"tested"
q7_data$tested_or_comparison[q7_data$elig == 0 & q7_data$noncit == 0]<-"comparison"
q7_graph<-aggregate(hs ~ tested_or_comparison+year, data=q7_data, mean)
ggplot(data=q7_graph, mapping = aes(x=year, y=hs, color=tested_or_comparison))+geom_line()


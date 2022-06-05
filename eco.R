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

#initialize data for part B
part_b_data<-subset(data, ageimmig<=10 & yrimmig<=2007)
part_b_data$treated_or_comparison[part_b_data$elig == 1 & part_b_data$noncit == 1]<-"treated"
part_b_data$treated_or_comparison[part_b_data$elig == 0 & part_b_data$noncit == 0]<-"comparison"

#q3
q3_data<-subset(part_b_data, post==0) # post==0 filtering the years before DACA started
q3_data$treated[q3_data$treated_or_comparison == "treated"]<-1
q3_data$treated[q3_data$treated_or_comparison == "comparison"]<-0
q3_data$incwage_pop[is.na(q3_data$incwage_pop)] = 0

# treated group
model_nsibs<-lm(nsibs ~ treated, data=q3_data)
summary(model_nsibs)

model_inpov<-lm(inpov ~ treated, data=q3_data)
summary(model_inpov)

model_any_col<-lm(any_col ~ treated, data=q3_data)
summary(model_any_col)

model_ageimmig<-lm(ageimmig ~ treated, data=q3_data)
summary(model_ageimmig)

model_nonfluent<-lm(nonfluent ~ treated, data=q3_data)
summary(model_nonfluent)

model_singlemom<-lm(singlemom ~ treated, data=q3_data)
summary(model_singlemom)

model_fem<-lm(fem ~ treated, data=q3_data)
summary(model_fem)

# counting num of observations
treated<-subset(q3_data, treated_or_comparison=="treated")
comparison<-subset(q3_data, treated_or_comparison=="comparison")

#q4
q4_data<-subset(part_b_data, post==0)
lm<-lm(hs ~ nsibs + inpov + any_col + ageimmig + nonfluent + singlemom  + fem + noncit, data=q4_data)
summary(lm)


#q6
q6_data<-subset(part_b_data, age<=18 & age>=14)
q6_graph<-aggregate(inschool ~ treated_or_comparison+year, data=q6_data, mean)
ggplot(data=q6_graph, mapping = aes(x=year, y=inschool, color=treated_or_comparison))+geom_line()


#q7
q7_data<-subset(part_b_data, age<=22 & age>=19)
q7_graph<-aggregate(hs ~ treated_or_comparison+year, data=q7_data, mean)
ggplot(data=q7_graph, mapping = aes(x=year, y=hs, color=treated_or_comparison))+geom_line()



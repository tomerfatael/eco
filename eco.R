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

#q11
treated_and_before<-mean(part_b_data$hs[part_b_data$treated_or_comparison == "treated" & part_b_data$post == 0 & part_b_data$age <= 22 & part_b_data$age >= 19])
treated_and_after<-mean(part_b_data$hs[part_b_data$treated_or_comparison == "treated" & part_b_data$post == 1 & part_b_data$age <= 22 & part_b_data$age >= 19])
comparison_and_before<-mean(part_b_data$hs[part_b_data$treated_or_comparison == "comparison" & part_b_data$post == 0 & part_b_data$age <= 22 & part_b_data$age >= 19])
comparison_and_after<-mean(part_b_data$hs[part_b_data$treated_or_comparison == "comparison" & part_b_data$post == 1 & part_b_data$age <= 22 & part_b_data$age >= 19])

#q12
q12_data<-subset(part_b_data, age<=22 & age>=19)
q12_model<-lm(hs ~ elig + post + elig*post, data=q12_data)
summary(q12_model)

#q14
q12_data$residuals<-q12_model$residuals
q12_data$residuals_squared<-q12_data$residuals^2
q14_model<-lm(residuals_squared ~ elig + post + elig*post, data=q12_data)
summary(q14_model)
coeftest(q14_model, vcov = vcovHC(q14_model, type="HC1"))

#q15
q15_model = lm(hs ~ elig + post + nsibs + any_col + ageimmig + singlemom + fem + elig*post + , data=q12_data)
summary(q15_model)
q15_model_effects = plm(hs ~ elig + post + nsibs + any_col + ageimmig + singlemom + fem + elig*post + factor(statefib), index="state", model="within", data=q12_data)
summary(q15_model_effects)
q12_data$residuals_squared_full = q15_model$residuals^2
coeftest(q15_model, vcov = vcovHC(q15_model, type="HC1"))


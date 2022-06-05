rm(list=ls())
data<-read.csv("DACAdata.csv")

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

aviIsTheBest <- 5



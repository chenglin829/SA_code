library(tidyverse)
library(ggplot2)
library(survey)
library(data.table)
library(nlme)
library(Matrix)
setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/context process/1981/")
source("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/blackwell.R")
load("widedta_1981s.Rdata")
names(wide_data)
#gender
table(wide_data$gender)/dim(wide_data)[1]
#migration background
table(wide_data$migration)/dim(wide_data)[1]
sum(wide_data$treatment1_2015+wide_data$treatment1_2016+wide_data$treatment1_2017>0)/dim(wide_data)[1]
#martial status 2001
table(wide_data$Civil_2001)/dim(wide_data)[1]
#martial status 2008
table(wide_data$Civil_2008)/dim(wide_data)[1]
#martial status 2014
table(wide_data$Civil_2014)/dim(wide_data)[1]
#education 2001
table(wide_data$education_2001)/dim(wide_data)[1]
#education 2008
table(wide_data$education_2008)/dim(wide_data)[1]
#education 2014
table(wide_data$education_2014)/dim(wide_data)[1]
library(psych)
names(wide_data)
#Aged 20
describe(wide_data[,c(2,19,36,53,70,87,104,121,138,155,172,189,206,223,240,257,274)])
#Aged 27
describe(wide_data[,c(9,26,43,60,77,94,111,128,145,162,179,196,213,230,247,264,281)])
#Aged 33
describe(wide_data[,c(15,32,49,66,83,100,117,134,151,168,185,202,219,236,253,270,287)])
###cumulative treatment and cumulative exposure
wide_data$cumulative_treatment=wide_data$treatment1_2001+wide_data$treatment1_2002+
  wide_data$treatment1_2003+wide_data$treatment1_2004+wide_data$treatment1_2005+
  wide_data$treatment1_2006+wide_data$treatment1_2007+wide_data$treatment1_2008+
  wide_data$treatment1_2009+wide_data$treatment1_2010+wide_data$treatment1_2011+
  wide_data$treatment1_2012+wide_data$treatment1_2013+wide_data$treatment1_2014

wide_data$cumulative_exposure=(wide_data$nh2_2001+wide_data$nh2_2002+wide_data$nh2_2003+
  wide_data$nh2_2004+wide_data$nh2_2005+wide_data$nh2_2006+wide_data$nh2_2007+
  wide_data$nh2_2008+wide_data$nh2_2009+wide_data$nh2_2010+wide_data$nh2_2011+
  wide_data$nh2_2012+wide_data$nh2_2013+wide_data$nh2_2014)/14
names(wide_data)
# figure 2a
f2a=data.frame(T=20:33,
               mean=rep(0,14))
for (i in 0:13) {
  f2a$mean[1+i]=mean(wide_data[,87+i])
}

ggplot(data = f2a,mapping = aes(x=T,y=mean))+geom_line()+geom_point()+
  labs(x="Age",y="% of SA")+theme_bw()+
  theme(panel.grid = element_blank(),panel.border = element_blank())
#figure 2b
f2b=data.frame(T=0:14,
               mean=rep(0,15))
for (i in 0:14) {
  f2b$mean[1+i]=table(wide_data$cumulative_treatment)[1+i]/dim(wide_data)[1]
}
# never receive SA
sum(table(wide_data$cumulative_treatment)[1])/dim(wide_data)[1]
# receive one-year SA
sum(table(wide_data$cumulative_treatment)[2])/dim(wide_data)[1]
# 5 or more percentage
sum(table(wide_data$cumulative_treatment)[6:15])/dim(wide_data)[1]
ggplot(data = f2b,mapping = aes(x=T,y=mean))+geom_line()+geom_point()+
  labs(x="Cumulative Social Assistance Use (Years)",y="Percentage")+theme_bw()+
  theme(panel.grid = element_blank(),panel.border = element_blank())

#f3c
f3c=aggregate(wide_data$cumulative_exposure,list(wide_data$cumulative_treatment),FUN=mean)
ggplot(data = f3c,mapping = aes(x=Group.1,y=x))+geom_line()+
  labs(x="Cumulative Social Assistance Use (Years)",y="Average DN scores")+
  theme_bw()+theme(panel.grid = element_blank(),panel.border = element_blank())
#f3d
wide_data$final_outcome=wide_data$treatment1_2015+wide_data$treatment1_2016+
  wide_data$treatment1_2017
wide_data$final_outcome=ifelse(wide_data$final_outcome>0,1,0)
f3d=aggregate(wide_data$final_outcome,list(wide_data$cumulative_treatment),FUN=mean)
ggplot(data = f3d,mapping = aes(x=Group.1,y=x))+geom_line()+
  labs(x="Cumulative Social Assistance Use (Years)",y="Percentage of SA (outcome)")+
  theme_bw()+theme(panel.grid = element_blank(),panel.border = element_blank())

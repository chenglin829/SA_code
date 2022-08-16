setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/context process/1981/50/")
source("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/blackwell.R")
library(ggplot2)
library(survey)
library(data.table)
library(tidyverse)
df = read.csv("final1981.csv")

#read.geo data
year = 2001:2017
geo_name = paste0("final_data",year,".csv")
final_data = data.frame()

for (i in 1:length(2001:2017)) {
  geo_df=read.csv(geo_name[i])
  geo_df = geo_df[,c(3,16:20)]
  df1=merge(df[which(df$year==year[i]),],geo_df,by="PersonLopNr")
  final_data = rbind(final_data,df1)
}

df = final_data
setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/housing")
house_name = paste0("housing",year,".csv")
df$homeown=0

for (i in 1:length(2001:2017)){
  house_df=read.csv(house_name[i])
  df2=merge(df[which(df$year==year[i]),],house_df,by="PersonLopNr")
  names=df2$PersonLopNr
  df[which(df$year==year[i]),][which(df[which(df$year==year[i]),]$PersonLopNr%in%names),]$homeown=1
}

sum(df[which(df$year==2001),]$homeown)
setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/context process/1981/")
#unemployment and treatment
df$unemployment = ifelse(df$ArbLos>0,1,0)
#df[which(is.na(df$SocBidrpersF/df$DispInk)),]$DispInk=0.1
#df$treatment1 = ifelse((df$SocBidrpersF/df$DispInk)>0.1|
#                         (df$SocBidrpersF/df$DispInk)<0,1,0)
df$treatment1 = ifelse(df$SocBidrpersF>0,1,0)
#summary(df$treatment1)
#table(df$treatment1)
#dispink
##Disposable income-cpi adjusted
####CPI index
#cpi_2011<-c(311.43)
#cpi_index<-c(267.1,272.8,278.1,279.2,280.40,
#             284.22,290.51,300.61,299.66,303.46,311.43,
##             314.20,314.06,313.49,
#             313.35,316.43,322.11
#)

#cpi_index = cpi_2011/cpi_index
#### get adjusted-income
#years = c(2001,2002,2003,2004,
#          2005,2006,2007,2008,2009,
#          2010,2011,2012,2013,2014,
#          2015,2016,2017)

#for (i in 1:length(years)) {
#  df[which(df$year== years[i]),]$DispInk = 
#    df[which(df$year== years[i]),]$DispInk*cpi_index[i]
#}

##national median_income in 2011
#median_income<-c(233800)
###poverty line
#poverty_line = median_income*0.6/100
#df$treat_income = ifelse(df$DispInk-df$SocBidrpersF<poverty_line,1,0)
df$DispInk=df$DispInk-df$SocBidrpersF-df$BostBidrPersF-df$ForPeng
df$DispInk=ifelse(df$DispInk<1,1,df$DispInk)
df$DispInk=log(df$DispInk)
##choose non-students in 2001
#df$ns_line=0
#for (i in 2001:2017) {
#  df[which(df$year==i & df$treat_income==1),]$ns_line=1
#}

#df_name=aggregate(ns_line~PersonLopNr,df,sum)
#ns_name = unique(df_name[which(df_name$ns_line>=10),]$PersonLopNr)
#df = df[which(df$PersonLopNr %in% ns_name),]
#length(unique(df$PersonLopNr))*17
df$count=log(df$count)
df$count_company=log(df$count_company)
#nh variables
names(df)[50:54]=c("nh_uem","nh_le","nh_SA","nh_in","nh_sk")
library(FactoMineR)
library(factoextra)
df$nh2=0
res=list()
for (i in 1:length(year)) {
  nh_data = df[which(df$year==year[i]),][,c("nh_uem","nh_le","nh_SA","nh_in","nh_sk")]
  res.pca = PCA(nh_data,graph = F)
  pca_ind = get_pca_var(res.pca)
  weight =pca_ind$contrib[,1]/100
  df[which(df$year==year[i]),]$nh2 = weight[1]*df[which(df$year==year[i]),]$nh_uem+
    df[which(df$year==year[i]),]$nh_le*weight[2]+df[which(df$year==year[i]),]$nh_SA*
    weight[3]+df[which(df$year==year[i]),]$nh_in*
    weight[4]+df[which(df$year==year[i]),]$nh_sk*
    weight[5]
res[i]= get_eigenvalue(res.pca)[6]
}

fviz_eig(res.pca)
mean(df$nh2)

#graph=fviz_eig(res.pca)
#manage job
df$Ssyk4 = as.numeric(df$Ssyk4)
df[which(is.na(df$Ssyk4)),]$Ssyk4 = 0
df$Ssyk4 = ifelse(df$Ssyk4>1000&
                    df$Ssyk4<4000,1,0)
#home ownership
#df$homeown= 1 
#df[which(is.na(df$Taljare)),]$homeown=0

#re-order
library(dplyr)
data_income=arrange(df,PersonLopNr)
unique(data_income$PersonLopNr)
#data_income$Civil = ifelse(data_income$Civil!=1,0,1)
###long to wide
setDT(data_income)
names(df)
wide_data=dcast(data_income,PersonLopNr~year,
                value.var = c( "Civil","Lan",
                               "education","homeown",        
                              "unemployment",
                               "treatment1","nh2","DispInk","ForPeng",
                               "BostBidrPersF","Ssyk4","Barn0_3",
                               "Barn4_6","AntFlyttTot","child_u717",
                              "count","count_company"))
unique(wide_data$PersonLopNr)
wide_data$gender = data_income[which(data_income$year==2015),]$Kon
wide_data$migration = data_income[which(data_income$year==2015),]$LandKod
data_income=wide_data
wide_data = data.frame(wide_data)

wide_data1=wide_data
sum(is.na(wide_data$treatment1_2001))
quantile(wide_data$nh2_2001)
save(wide_data,file = "widedta_1981ss.rdata")


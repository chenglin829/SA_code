library(tidyverse)
library(ggplot2)
setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/")

t<-0
tb<-paste0("income",2001:2017,".csv")
data2<-data.frame()
df<-read.csv("income2001.csv")
df_names<-names(df)

for (i in 1:length(tb)) {
  data1<-read.csv(tb[i])
  names(data1)<-df_names
  data1$year<-2001+t
  t<-t+1
  data2<-rbind(data1,data2)
}

unique(data2[which(data2$FodelseAr==1981),]$PersonLopNr)
df_2<-data2 %>% group_by(PersonLopNr) %>% 
  mutate(mean_year = n_distinct(year),   ## number of different years
         all_years = ifelse(mean_year == 17,1,0)) %>% ## binary all years or not
  ungroup()

df_nonduplicated<- df_2 %>% group_by(year) %>% 
  filter(!duplicated(PersonLopNr))

df_full_trajectory <- df_nonduplicated %>% group_by(PersonLopNr) %>% 
  filter(all_years == 1)

df_full_trajectory<-df_full_trajectory %>% arrange(PersonLopNr,desc(year))


unique(df_full_trajectory[which(df_full_trajectory$FodelseAr==1981),]$PersonLopNr)
#education
df_full_trajectory$education = ifelse(df_full_trajectory$Sun2000niva<=299|
                                          df_full_trajectory$Sun2000niva==999,1,
                                        ifelse(df_full_trajectory$Sun2000niva>299 &
                                                 df_full_trajectory$Sun2000niva<=399,2,
                                               ifelse(df_full_trajectory$Sun2000niva>399 &
                                                        df_full_trajectory$Sun2000niva<=599,3,4)))
table(df_full_trajectory$education)
##Social benefit:1=SA recipient 0= Not SAR
#df_full_trajectory$treatment = ifelse(df_full_trajectory$SocBidrpersF>0,1,0)

##the number of child
df_full_trajectory$child_u717=df_full_trajectory$Barn7_10+
  df_full_trajectory$Barn11_15+df_full_trajectory$Barn16_17
#living in big cities(transition)
df_full_trajectory$Lan = ifelse(df_full_trajectory$Lan == 12| 
                                  df_full_trajectory$Lan==1|
                                  df_full_trajectory$Lan==14,1,0)

#maritial status
df_full_trajectory$Civil=trimws(df_full_trajectory$Civil,which = c("both","left","right"),whitespace = "[ \t\r\n]" )
df_full_trajectory$Civil=ifelse(df_full_trajectory$Civil=="G"|df_full_trajectory$Civil=="RP",1,
                                ifelse(df_full_trajectory$Civil=="S"|df_full_trajectory$Civil=="SP",2,
                                       ifelse(df_full_trajectory$Civil=="Ä"|df_full_trajectory$Civil=="EP",3,0)))

table(df_full_trajectory[which(df_full_trajectory$FodelseAr==1981),]$Civil)
#Migrations
df_full_trajectory$LandKod = 
  ifelse(df_full_trajectory$LandKod==1,0,1)
#labor market
year = c(2001:2017)
final_data = data.frame()
for (i in 1:length(year)) {
  data_lm=paste0("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/workplace/company",
                 year[i],".csv")
  df_lm = read.csv(data_lm)
  data_final = df_full_trajectory[which(df_full_trajectory$year==year[i]),]
  data_final = merge(data_final,df_lm,by="LM")
  final_data = rbind(data_final,final_data)
}

final_data=arrange(final_data,PersonLopNr,year)
write.csv(final_data,"final_data.csv")

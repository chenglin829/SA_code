library(tidyverse)
library(ggplot2)
setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/")
df = read.csv("final_data.csv")
final_data2 = df[which(df$FodelseAr==1983),]
length(unique(final_data2$PersonLopNr))*17
dim(final_data2)
names(final_data2)
table(final_data2$Civil)
full_sample = final_data2[,c(4,5,6,7,8,40)]
setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/context process/1983/")
write.csv(final_data2,"final1983.csv")
write.csv(full_sample,"full_sample_2000.csv")

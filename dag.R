library(bnlearn)
setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/context process/1981/")
load("widedta.Rdata")
names(wide_data)
for (i in 2:3) {
  wide_data[,i]=as.numeric(wide_data[,i])
}
df=wide_data[,c(2:3)]
str(df)
fit1=pc.stable(df,alpha = 0.01)
graphviz.plot(bnlearn_fit,shape="ellipse")

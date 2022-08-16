setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/context process/1981/")
MY=read.csv("MY_sim.CSV")
AY=read.csv("AY_sim.CSV")
AM=read.csv("AM_sim.CSV")

# treatment-outcome
AY_pars=data.frame()
for (i in 2:6) {
  AY_pars[i-1,1]=mean(AY[,i])
  AY_pars[i-1,2]=sd(AY[,i])
}
AY_pars$id=rep(1:5)
#AY_pars$effect="Direct"
#AY_pars$effect[7:12]="Indirect"
library(ggplot2)
p=ggplot(data = AY_pars,aes(x=id,y=V1))+geom_point()+
  geom_line()+
geom_errorbar(aes(ymin=V1-V2,ymax=V1+V2),width=0.1)+
  geom_hline(yintercept = 5.10,linetype="dashed",color="blue")

p+scale_x_continuous(breaks=c(1,2,3,4,5),
                     labels=c("0","0.1","0.3",
                              "0.5","1"))+xlab("Unmeasured Associations (T-Y)")+ylab("Effects")

#treatment-mediator
AM_pars=data.frame()
for (i in 2:5) {
  AM_pars[i-1,1]=mean(AM[,i])
  AM_pars[i-1,2]=sd(AM[,i])
}
AM_pars$id=rep(1:4)
#AM_pars$effect="Direct"
#AM_pars$effect[7:12]="Indirect"
library(ggplot2)
p=ggplot(data = AM_pars,aes(x=id,y=V1))+geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=V1-V2,ymax=V1+V2),width=0.1)+
  geom_hline(yintercept = 5.83,linetype="dashed",color="blue")

p+scale_x_continuous(breaks=c(1,2,3,4,5),
                     labels=c("0","0.1","0.3",
                              "0.5","1"))+xlab("Scenarios (T-M)")+ylab("Effects")

#mediator-outcome
MY_pars=data.frame()
for (i in 2:5) {
  MY_pars[i-1,1]=mean(MY[,i])
  MY_pars[i-1,2]=sd(MY[,i])
}
MY_pars$id=rep(1:4)
#MY_pars$effect="Direct"
#MY_pars$effect[7:12]="Indirect"
library(ggplot2)
p=ggplot(data = MY_pars,aes(x=id,y=V1))+geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=V1-V2,ymax=V1+V2),width=0.1)+
  geom_hline(yintercept = 5.83,linetype="dashed",color="blue")

p+scale_x_continuous(breaks=c(1,2,3,4),
                     labels=c("10","7","3.5",
                              "1"))+xlab("Scenarios (M-Y)")+ylab("Effects")


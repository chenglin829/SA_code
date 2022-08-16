library(tidyverse)
library(ggplot2)
library(survey)
library(data.table)
library(nlme)
library(Matrix)
setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/context process/1981/")
source("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/blackwell.R")
load("widedta_1981ss.Rdata")
#df1=wide_data

#setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/context process/1982/")
#source("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/blackwell.R")
#load("widedta_1982.Rdata")
#df2=wide_data

#setwd("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/context process/1983/")
#source("\\\\micro.intra/projekt/P0515$/P0515_Gem/Cheng/income_data/blackwell.R")
#load("widedta_1983.Rdata")
#df3=wide_data

#wide_data=rbind(df1,df2,df3)

###numerator for treatment in outcome MSM
#model specification-numerator for treatment in outcome model
constant = paste0("+","gender","+","migration")
outcome = c()
variables = c()
num.tformula = c()
t=1
for (i in 2002:2014) {
  if(t<=1)
  {outcome[t] = paste0("treatment1_",i,"~")
  variables[t] = paste0("treatment1_",i-1,"+","nh2_",i-1)
  #num.tformula[t] = paste0(outcome[t],variables[t],constant)
  num.tformula[t] = paste0(outcome[t],variables[t])
  }else{
    outcome[t] = paste0("treatment1_",i,"~")
    variables[t] = paste0(variables[t-1],"+",paste0("treatment1_",i-1,"+","nh2_",i-1))
    #num.tformula[t] = paste0(outcome[t],variables[t],constant)
    num.tformula[t] = paste0(outcome[t],variables[t])
  }
  #num.tformula[t]=paste0("treatment1_",i,"~",1)
  t=t+1
}

#model specification-numerator for mediator in outcome model
outcome = c()
variables = c()
num.mformula = c()
t=1
for (i in 2002:2014) {
  if(t<=1)
  {outcome[t] = paste0("nh2_",i,"~")
  variables[t] = paste0("treatment1_",i,"+","treatment1_",i-1,"+","nh2_",i-1)
  #num.mformula[t] = paste0(outcome[t],variables[t],constant)
   num.mformula[t] = paste0(outcome[t],variables[t])
  }else{
    outcome[t] = paste0("nh2_",i,"~")
    variables[t] = paste0(variables[t-1],"+",paste0("treatment1_",i,"+","treatment1_",i-1,"+","nh2_",i-1))
    #num.mformula[t] = paste0(outcome[t],variables[t],constant)
   num.mformula[t] = paste0(outcome[t],variables[t])
  }
  #num.mformula[t]=paste0("nh2_",i,"~",1)
  t=t+1
}
(num.mformula)
##model specification-denominator for treatment in outcome model

outcome = c()
variables = c()
treatment = c()
den.tformula = c()
t=1
names(wide_data)
names=c("Civil_","Lan_","education_","child_u717_",
        "unemployment_","BostBidrPersF_","ForPeng_",
        "count_","count_company_","Ssyk4_","homeown_","AntFlyttTot_","DispInk_",
        "Barn0_3_","Barn4_6_")
for (i in 2002:2014) {
  if(t<=1)
  {outcome[t] = paste0("treatment1_",i,"~")
  treatment[t] = paste0("treatment1_",i-1,"+","nh2_",i-1)
  name_den = paste0(names,i-1)
  variables[t] =paste0("+",name_den[1],"+",name_den[2],"+",name_den[3],"+",name_den[4],"+",
                       name_den[5],"+",name_den[6],"+",name_den[7],"+",name_den[8],"+",
                       name_den[9],"+",name_den[10],"+",name_den[11],"+",name_den[12],
                       "+",name_den[13],"+",name_den[14],"+",name_den[15])
  den.tformula[t] = paste0(outcome[t],treatment[t],variables[t],constant)
  }else{
    outcome[t] = paste0("treatment1_",i,"~")
    treatment[t] = paste0(treatment[t-1],"+",paste0("treatment1_",i-1,"+","nh2_",i-1))
    name_den = paste0(names,i-1)
    variables[t] = paste0(variables[t-1],paste0("+",name_den[1],"+",name_den[2],"+",name_den[3],"+",name_den[4],"+",
                                                name_den[5],"+",name_den[6],"+",name_den[7],"+",name_den[8],"+",
                                                name_den[9],"+",name_den[10],
                                                "+",name_den[11],"+",name_den[12],
                                                "+",name_den[13],"+",name_den[14],"+",name_den[15]))
    
    den.tformula[t] = paste0(outcome[t],treatment[t],variables[t],constant)  
  }
  
  t=t+1
}

# denominator of mediation in outcome model

outcome = c()
variables = c()
treatment = c()
den.mformula = c()
t=1
for (i in 2002:2014) {
  if(t<=1)
  {outcome[t] = paste0("nh2_",i,"~")
  treatment[t] = paste0("treatment1_",i-1,"+","nh2_",i-1,"+","treatment1_",i)
  name_den = paste0(names,i-1)
  variables[t] =paste0("+",name_den[1],"+",name_den[2],"+",name_den[3],"+",name_den[4],"+",
                       name_den[5],"+",name_den[6],"+",name_den[7],"+",name_den[8],"+",
                       name_den[9],"+",name_den[10],"+",name_den[11],"+",name_den[12],
                       "+",name_den[13],"+",name_den[14],"+",name_den[15])
  den.mformula[t] = paste0(outcome[t],treatment[t],variables[t],constant)
  }else{
    outcome[t] = paste0("nh2_",i,"~")
    treatment[t] = paste0(treatment[t-1],"+",paste0("nh2_",i-1,"+","treatment1_",i))
    name_den = paste0(names,i-1)
    variables[t] = paste0(variables[t-1],paste0("+",name_den[1],"+",name_den[2],"+",name_den[3],"+",name_den[4],"+",
                                                name_den[5],"+",name_den[6],"+",name_den[7],"+",name_den[8],"+",
                                                name_den[9],"+",name_den[10],
                                                "+",name_den[11],"+",name_den[12],
                                                "+",name_den[13],"+",name_den[14],"+",name_den[15]))
    
    den.mformula[t] = paste0(outcome[t],treatment[t],variables[t],constant)  
  }
  
  t=t+1
}

#library(logistf)
#weights for treatment in outcome model
(treatname=names(wide_data)[88:100])
weights.t = list()
for (i in 1:length(treatname)) {
  fit1= glm(den.tformula[i],wide_data,family = binomial())
  p1=fit1$fitted.values
  fit2= glm(num.tformula[i],wide_data,family = binomial())
  p2=fit2$fitted.values
  den = wide_data[,treatname[i]]*p1+(1-wide_data[,treatname[i]])*(1-p1)
  numerator = wide_data[,treatname[i]]*p2+(1-wide_data[,treatname[i]])*(1-p2)
  weights.t[[i]]=(numerator/den)
}

warnings()

#weighs for mediator-numerator outcome model
(medname=names(wide_data)[105:117])
weights.m = list()
for (i in 1:length(medname)) {
  fit1= lm(den.mformula[i],wide_data)
  #p1=fit1$fitted.values
  fit2= lm(num.mformula[i],wide_data)
  #p2=fit2$predict
  numerator=dnorm(wide_data[,medname[i]],predict(fit2),sd(fit2$residuals))
  den = dnorm(wide_data[,medname[i]],predict(fit1),sd(fit1$residuals))
  weights.m[[i]]=(numerator/den)
}

wei.outcome=(weights.m[[1]]*weights.t[[1]]*
               weights.m[[2]]*weights.t[[2]]*
               weights.m[[3]]*weights.t[[3]]*
               weights.m[[4]]*weights.t[[4]]*
               weights.m[[5]]*weights.t[[5]]*
               weights.m[[6]]*weights.t[[6]]*
               weights.m[[7]]*weights.t[[7]]*
               weights.m[[8]]*weights.t[[8]]*
               weights.m[[9]]*weights.t[[9]]*
               weights.m[[10]]*weights.t[[10]]*
               weights.m[[11]]*weights.t[[11]]*
               weights.m[[12]]*weights.t[[12]]*
               weights.m[[13]]*weights.t[[13]])
mean(wei.outcome)
sd(wei.outcome)
#models

#model specification-numerator for treatment in mediation model
outcome = c()
variables = c()
num.form = c()
t=1
for (i in 2002:2014) {
  if(t<=1)
  {outcome[t] = paste0("treatment1_",i,"~")
  variables[t] = paste0("treatment1_",i-1)
  #num.form[t] = paste0(outcome[t],variables[t],constant)
  num.form[t] = paste0(outcome[t],variables[t])
  }else{
    outcome[t] = paste0("treatment1_",i,"~")
    variables[t] = paste0(variables[t-1],"+",paste0("treatment1_",i-1))
    #num.form[t] = paste0(outcome[t],variables[t],constant) 
    num.form[t] = paste0(outcome[t],variables[t])
  }
  #num.form[t]=paste0("treatment1_",i,"~",1)
  t=t+1
}


##model specification-denominator for treatment in mediation model
outcome = c()
variables = c()
treatment = c()
den.form = c()
t=1
for (i in 2002:2014) {
  if(t<=1)
  {outcome[t] = paste0("treatment1_",i,"~")
  treatment[t] = paste0("treatment1_",i-1,"+","nh2_",i-1)
  name_den = paste0(names,i-1)
  variables[t] =paste0("+",name_den[1],"+",name_den[2],"+",name_den[3],"+",name_den[4],"+",
                       name_den[5],"+",name_den[6],"+",name_den[7],"+",name_den[8],"+",
                       name_den[9],"+",name_den[10],"+",name_den[11],"+",name_den[12],
                       "+",name_den[13],"+",name_den[14],"+",name_den[15])
  den.form[t] = paste0(outcome[t],treatment[t],variables[t],constant)
  }else{
    outcome[t] = paste0("treatment1_",i,"~")
    treatment[t] = paste0(treatment[t-1],"+",paste0("treatment1_",i-1,"+","nh2_",i-1))
    name_den = paste0(names,i-1)
    variables[t] = paste0(variables[t-1],paste0("+",name_den[1],"+",name_den[2],"+",name_den[3],"+",name_den[4],"+",
                                                name_den[5],"+",name_den[6],"+",name_den[7],"+",name_den[8],"+",
                                                name_den[9],"+",name_den[10],
                                                "+",name_den[11],"+",name_den[12],
                                                "+",name_den[13],"+",name_den[14],"+",name_den[15]))
    
    den.form[t] = paste0(outcome[t],treatment[t],variables[t],constant)  
  }
  
  t=t+1
}

#weights
treatname=names(wide_data)[88:100]
weights.tm = list()
for (i in 1:length(treatname)) {
  fit1= glm(den.form[i],wide_data,family = binomial())
  p1=fit1$fitted.values
  fit2= glm(num.form[i],wide_data,family = binomial())
  p2=fit2$fitted.values
  den = wide_data[,treatname[i]]*p1+(1-wide_data[,treatname[i]])*(1-p1)
  numerator = wide_data[,treatname[i]]*p2+(1-wide_data[,treatname[i]])*(1-p2)
  weights.tm[[i]]=(numerator/den)
}

weight.mediation=weights.tm[[1]]*
  weights.tm[[2]]*weights.tm[[3]]*weights.tm[[4]]*
  weights.tm[[5]]*weights.tm[[6]]*weights.tm[[7]]*
  weights.tm[[8]]*weights.tm[[9]]*weights.tm[[10]]*
  weights.tm[[11]]*weights.tm[[12]]*weights.tm[[13]]

mean(wei.outcome)
quantile(wei.outcome)
sd(wei.outcome)

mean(weight.mediation)
quantile(weight.mediation)
sd(weight.mediation)

##weightings
wei.outcome = ifelse(wei.outcome<quantile(wei.outcome,0.01),
                     quantile(wei.outcome,0.01),wei.outcome)
wei.outcome = ifelse(wei.outcome>quantile(wei.outcome,0.99),
                     quantile(wei.outcome,0.99),wei.outcome)

weight.mediation = ifelse(weight.mediation<quantile(weight.mediation,0.01),
                          quantile(weight.mediation,0.01),weight.mediation)
weight.mediation = ifelse(weight.mediation>quantile(weight.mediation,0.99),
                          quantile(weight.mediation,0.99),weight.mediation)

###
final_treatment1 = wide_data$treatment1_2002+wide_data$treatment1_2003+
  wide_data$treatment1_2004+ wide_data$treatment1_2005+ wide_data$treatment1_2006+ wide_data$treatment1_2007+
  wide_data$treatment1_2008+ wide_data$treatment1_2009+ wide_data$treatment1_2010+
  wide_data$treatment1_2011+ wide_data$treatment1_2012+ wide_data$treatment1_2013+
  wide_data$treatment1_2014

final_mediation = wide_data$nh2_2002+wide_data$nh2_2003+
  wide_data$nh2_2004+ wide_data$nh2_2005+ wide_data$nh2_2006+ wide_data$nh2_2007+
  wide_data$nh2_2008+ wide_data$nh2_2009+ wide_data$nh2_2010+
  wide_data$nh2_2011+ wide_data$nh2_2012+ wide_data$nh2_2013+
  wide_data$nh2_2014
final_outcome = (wide_data$treatment1_2016+wide_data$treatment1_2017+ wide_data$treatment1_2015)
final_outcome = ifelse(final_outcome>0,1,0)
table(final_outcome)
#fit1=glm(final_outcome~final_treatment1+final_mediation,weights = wei.outcome,
#         family = binomial(),start = c(log(mean(final_outcome)),rep(0,2)))
#summary(fit1)

fit11=glm(final_outcome~final_treatment1+final_mediation,weights = wei.outcome,
         family = poisson(),wide_data)
summary(fit11)
#robust
library(sandwich)
cov.fit11=vcovHC(fit11,type = "HC0")
std.rr=sqrt(diag(cov.fit11))
(r.est=cbind(Estimate=coef(fit11),"Robust.SE"=std.rr,"Pr(>|z|)"=
              2*pnorm(abs(coef(fit11)/std.rr),lower.tail=F),
            LL=coef(fit11)-1.96*std.rr,
            UL=coef(fit11)+1.96*std.rr))


wide_data$last_mediator = (wide_data$nh2_2016+wide_data$nh2_2017+wide_data$nh2_2015)/3
avr_treatment1=(wide_data$treatment1_2002+wide_data$treatment1_2003+
                  wide_data$treatment1_2004+ wide_data$treatment1_2005+ wide_data$treatment1_2006+ wide_data$treatment1_2007+
                  wide_data$treatment1_2008+ wide_data$treatment1_2009+ wide_data$treatment1_2010+
                  wide_data$treatment1_2011+ wide_data$treatment1_2012+ wide_data$treatment1_2013+
                  wide_data$treatment1_2014)/13
fit2=lm(wide_data$last_mediator~avr_treatment1,wide_data,weights = weight.mediation)
summary(fit2)
(y2=coef(fit2))
(y1=coef(fit11))
(indirect=13*(y2[2]*y1[3]))
#exp(indirect)
(direct=13*(y1[2]))
((direct=13*(y1[2]))+(indirect=13*(y2[2]*y1[3])))
(indirect=13*(y2[2]*y1[3]))/((direct=13*(y1[2]))+(indirect=13*(y2[2]*y1[3])))
#error correlation
re1=residuals(fit11)
re2=residuals(fit2)
cor(re1,re2)



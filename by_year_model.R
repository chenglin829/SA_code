for (i in 1:13) {
  weights.tm[[i]]=ifelse(weights.tm[[i]]<quantile(weights.tm[[i]],0.01),
                         quantile(weights.tm[[i]],0.01),weights.tm[[i]])
  weights.tm[[i]] = ifelse(weights.tm[[i]]>quantile(weights.tm[[i]],0.99),
                           quantile(weights.tm[[i]],0.99),weights.tm[[i]])
}
quantile(weights.tm[[13]])
fit3=glm(final_outcome~final_treatment1+final_mediation,weights = wei.outcome,
         family = poisson())
summary(fit3)
wide_data$avg_2002=(wide_data$treatment1_2002+wide_data$treatment1_2001)/2
wide_data$avg_2003=(wide_data$treatment1_2003+wide_data$treatment1_2002+wide_data$treatment1_2001)/3
wide_data$avg_2004=(wide_data$treatment1_2004+wide_data$treatment1_2003+wide_data$treatment1_2002+wide_data$treatment1_2001)/4
wide_data$avg_2005=(wide_data$treatment1_2005+wide_data$treatment1_2004+wide_data$treatment1_2003+
                      wide_data$treatment1_2002+wide_data$treatment1_2001)/5
wide_data$avg_2006=(wide_data$treatment1_2006+wide_data$treatment1_2005+wide_data$treatment1_2004+wide_data$treatment1_2003+
                      wide_data$treatment1_2002+wide_data$treatment1_2001)/6
wide_data$avg_2007=(wide_data$treatment1_2007+wide_data$treatment1_2006+wide_data$treatment1_2005+wide_data$treatment1_2004+
                      wide_data$treatment1_2003+
                      wide_data$treatment1_2002+wide_data$treatment1_2001)/7
wide_data$avg_2008=(wide_data$treatment1_2008+wide_data$treatment1_2007+wide_data$treatment1_2006+wide_data$treatment1_2005+
                      wide_data$treatment1_2004+wide_data$treatment1_2003+
                      wide_data$treatment1_2002+wide_data$treatment1_2001)/8
wide_data$avg_2009=(wide_data$treatment1_2009+wide_data$treatment1_2008+wide_data$treatment1_2007+wide_data$treatment1_2006+wide_data$treatment1_2005+
                      wide_data$treatment1_2004+wide_data$treatment1_2003+
                      wide_data$treatment1_2002+wide_data$treatment1_2001)/9
wide_data$avg_2010=(wide_data$treatment1_2010+wide_data$treatment1_2009+wide_data$treatment1_2008+wide_data$treatment1_2007+wide_data$treatment1_2006+wide_data$treatment1_2005+
                      wide_data$treatment1_2004+wide_data$treatment1_2003+
                      wide_data$treatment1_2002+wide_data$treatment1_2001)/10
wide_data$avg_2011=(wide_data$treatment1_2011+wide_data$treatment1_2010+wide_data$treatment1_2009+wide_data$treatment1_2008+wide_data$treatment1_2007+wide_data$treatment1_2006+wide_data$treatment1_2005+
                      wide_data$treatment1_2004+wide_data$treatment1_2003+
                      wide_data$treatment1_2002+wide_data$treatment1_2001)/11
wide_data$avg_2012=(wide_data$treatment1_2012+wide_data$treatment1_2011+wide_data$treatment1_2010+wide_data$treatment1_2009+wide_data$treatment1_2008+wide_data$treatment1_2007+wide_data$treatment1_2006+wide_data$treatment1_2005+
                      wide_data$treatment1_2004+wide_data$treatment1_2003+
                      wide_data$treatment1_2002+wide_data$treatment1_2001)/12
wide_data$avg_2013=(wide_data$treatment1_2013+wide_data$treatment1_2012+wide_data$treatment1_2011+wide_data$treatment1_2010+wide_data$treatment1_2009+wide_data$treatment1_2008+wide_data$treatment1_2007+wide_data$treatment1_2006+wide_data$treatment1_2005+
                      wide_data$treatment1_2004+wide_data$treatment1_2003+
                      wide_data$treatment1_2002+wide_data$treatment1_2001)/13
wide_data$avg_2014=(wide_data$treatment1_2014+wide_data$treatment1_2013+wide_data$treatment1_2012+wide_data$treatment1_2011+wide_data$treatment1_2010+wide_data$treatment1_2009+wide_data$treatment1_2008+wide_data$treatment1_2007+wide_data$treatment1_2006+wide_data$treatment1_2005+
                      wide_data$treatment1_2004+wide_data$treatment1_2003+
                      wide_data$treatment1_2002+wide_data$treatment1_2001)/14
outcome = c()
variables = c()
treatment = c()
gmformula = c()
t=1
for (i in 2002:2014) {
  outcome[t] = paste0("nh2_",i,"~")
  treatment[t] = paste0("avg_",i)
  name_den = paste0(names,i-1)
  variables[t] =paste0("+",name_den[1],"+",name_den[2],"+",name_den[3],"+",name_den[4],"+",
                       name_den[5],"+",name_den[6],"+",name_den[7],"+",name_den[8],"+",
                     name_den[9],"+",name_den[10],"+",name_den[11],"+",name_den[12],
                     "+",name_den[13])
#,variables[t],constant
  gmformula[t] = paste0(outcome[t],treatment[t])
  t=t+1
}
  lm(wide_data$nh2_2004~wide_data$treatment1_2004)
mfit1=lm(as.formula(gmformula[1]),data = wide_data,weights = weights.tm[[1]])
mfit2=lm(as.formula(gmformula[2]),data = wide_data,weights = weights.tm[[2]])
mfit3=lm(as.formula(gmformula[3]),data = wide_data,weights = weights.tm[[3]])
mfit4=lm(as.formula(gmformula[4]),data = wide_data,weights = weights.tm[[4]])
mfit5=lm(as.formula(gmformula[5]),data = wide_data,weights = weights.tm[[5]])
mfit6=lm(as.formula(gmformula[6]),data = wide_data,weights = weights.tm[[6]])
mfit7=lm(as.formula(gmformula[7]),data = wide_data,weights = weights.tm[[7]])
mfit8=lm(as.formula(gmformula[8]),data = wide_data,weights = weights.tm[[8]])
mfit9=lm(as.formula(gmformula[9]),data = wide_data,weights = weights.tm[[9]])
mfit10=lm(as.formula(gmformula[10]),data = wide_data,weights = weights.tm[[10]])
mfit11=lm(as.formula(gmformula[11]),data = wide_data,weights = weights.tm[[11]])
mfit12=lm(as.formula(gmformula[12]),data = wide_data,weights = weights.tm[[12]])
mfit13=lm(as.formula(gmformula[13]),data = wide_data,weights = weights.tm[[13]])
#
(indirect2=(coef(mfit1)[2]+coef(mfit2)[2]+coef(mfit3)[2]+coef(mfit4)[2]+coef(mfit5)[2]+coef(mfit6)[2]+
  coef(mfit7)[2]+coef(mfit8)[2]+coef(mfit9)[2]+
    coef(mfit10)[2]+coef(mfit11)[2]+coef(mfit12)[2]+coef(mfit13)[2])*coef(fit3)[3])

(direct2=coef(fit3)[2]*13)
(Overall2=direct2+indirect2)
(indirect2/Overall2)
#
#X1=model.matrix(mfit1)
#X2=model.matrix(mfit2)
#X3=model.matrix(mfit3)
#X#4=model.matrix(mfit4)
#X5=model.matrix(mfit5)
#X6=model.matrix(mfit6)
#X7=model.matrix(mfit7)
#X8=model.matrix(mfit8)
#X9=model.matrix(mfit9)
#X10=model.matrix(mfit10)
#X11=model.matrix(mfit11)
#X12=model.matrix(mfit12)
#X13=model.matrix(mfit13)

#x14=model.matrix(fit2)
#colnames(wide_data)
#gls_y = c(as.matrix(final_mediation))
#gls_x=as.matrix(bdiag(x14))
#gls_weight = rep(weight.mediation)
#gls_id = as.factor(rep(1:dim(wide_data)[1],1))
####
#mod_gls =gls(gls_y~ -1+gls_x,weights = varFixed(~gls_weight),
#             correlation =corSymm(form = ~1|gls_id),control = list(singular.ok=T))
#sigma2 = rep(1,1)%*%(cov2cor(getVarCov(mod_gls))*summary(mod_gls)$sigma^2)%*%rep(1,1)
#summary(fit2)$sigma^2

#(y2=coef(fit2))
#(y1=coef(fit11))
##write.table(sigma2,"sigma2.txt")
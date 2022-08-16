
### simulated sg
S1=c(0,0.1,0.3,0.5,1)
S2=c(0,0.1,0.3,0.5,1)
parameter_sen=data.frame(S1,S2)

MY=data.frame()
t=1
for (m in 1:5) {
  for (i in 1:500) {
    UC=parameter_sen[m,1]*final_mediation+
      parameter_sen[m,2]*final_outcome+
      rnorm(length(wide_data$last_mediator),0,1)
    fit1_MY=lm(wide_data$last_mediator~UC)
    fit2_MY=glm(final_outcome~final_treatment1+final_mediation+UC,
                weights = wei.outcome,family = poisson())
    fit3_MY=lm(wide_data$last_mediator~avr_treatment1,weights = weight.mediation)
    (y2=coef(fit3_MY))
    (y1=coef(fit2_MY))
    (indirect=13*(y2[2]*y1[3]))
    #exp(indirect)
    (direct=13*(y1[2]))
    MY[i,t]=direct
    MY[i,t+1]=indirect
  }
  t=t+2
}

#AY
AY=data.frame()
t=1
for (m in 1:5) {
    for (i in 1:500) {
      UC=parameter_sen[m,1]*final_mediation+
        parameter_sen[m,2]*final_outcome+
        rnorm(length(wide_data$last_mediator),0,1)
    fit1_AY=lm(final_treatment1~UC)
    fit2_AY=glm(final_outcome~final_treatment1+final_mediation+UC,
                weights = wei.outcome,family = poisson())
    fit3_AY=lm(wide_data$last_mediator~avr_treatment1,weights = weight.mediation)
    (y2=coef(fit3_AY))
    (y1=coef(fit2_AY))
    (indirect=13*(y2[2]*y1[3]))
    #exp(indirect)
    (direct=13*(y1[2]))
    AY[i,t]=direct
    AY[i,t+1]=indirect
    }
  t=t+2
}

###############
#AM
AM=data.frame()
t=1
for (m in 1:5) {
    for (i in 1:500) {
      UC=parameter_sen[m,1]*final_mediation+
        parameter_sen[m,2]*final_outcome+
        rnorm(length(wide_data$last_mediator),0,1)
    fit1_AM=lm(final_treatment1~UC)
    fit2_AM=glm(final_outcome~final_treatment1+final_mediation,
                weights = wei.outcome,family = poisson())
    fit3_AM=lm(wide_data$last_mediator~avr_treatment1+UC,weights = weight.mediation)
    (y2=coef(fit3_AM))
    (y1=coef(fit2_AM))
    (indirect=13*(y2[2]*y1[3]))
    #exp(indirect)
    (direct=13*(y1[2]))
    AM[i,t]=direct
    AM[i,t+1]=indirect
  }
  t=t+2
}
write.csv(MY,"MY_sim1.CSV")
write.csv(AY,"AY_sim1.CSV")
write.csv(AM,"AM_sim1.CSV")
#####

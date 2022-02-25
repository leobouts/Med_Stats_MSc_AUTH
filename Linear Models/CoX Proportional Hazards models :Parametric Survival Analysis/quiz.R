library(survival)
library(survminer)

cox_transfusion <- coxph(Surv(Event.w, Status) ~ age, data = transfusion)
summary(cox_transfusion)

#if GLOBAL is non significant, proportionality assumption holds.


#2_Goodness-of-fit (GOF) tests
gof.s.zph <- cox.zph(cox_transfusion)
gof.s.zph


#3_linearity
#If We notice that hazard ratios are decreasing with higher quantiles linearity holds.


q <- quantile(transfusion$age, c(0, .25, .5, .75, 1))
varQ <- cut(transfusion$age, q, include.lowest = T)
km.q.age <- coxph(Surv(Event.w, Status) ~ varQ, data = transfusion)
km.q.age

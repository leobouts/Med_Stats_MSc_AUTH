library(survival)
library(survminer)

library(readxl)
mac <- read_excel("HSDA_mac.xlsx")
str(mac)

# dthstat : Status
# dthtime_1m : time
# age
# cd4 : CD4 cells/mL
# antibiotic: clarithromycin,rifabutin,combination
# sex
# antiret : Antiretroviral experience = Never or unknown=0, Current/previous use=1

mac$antibiotic<-factor(mac$antibiotic,levels = c(1,2,3),labels=c("rifabutin","clarithromycin","combination"))
mac$antiret <- factor(mac$antiret, levels=c(0,1), labels=c("No", "Yes"))

###############univariate cox regression models###############
coxmac <- coxph(Surv(dthtime_1m, dthstat) ~ cd4, data = mac)
summary(coxmac)

coxmac <- coxph(Surv(dthtime_1m, dthstat) ~ sex, data = mac)
summary(coxmac)

mac$antibiotic<-relevel(mac$antibiotic,ref="combination")

coxmac <- coxph(Surv(dthtime_1m, dthstat) ~ antibiotic, data = mac)
summary(coxmac)

coxmac <- coxph(Surv(dthtime_1m, dthstat) ~ age, data = mac)
summary(coxmac)

coxmac <- coxph(Surv(dthtime_1m, dthstat) ~ antiret, data = mac)
summary(coxmac)

# Perform a log-rank test to test for significant difference
survdiff(Surv(dthtime_1m, dthstat) ~ sex, data = mac)
survdiff(Surv(dthtime_1m, dthstat) ~ antibiotic, data = mac)


###############assumptions###############
#1_Proportionality_KM
# KM for gender
sex.km <- survfit(Surv(dthtime_1m, dthstat) ~ sex, data = mac)

ggsurvplot(sex.km, legend.labs=c("Male", "Female"))

# KM curve plotted
plot(sex.km, 
     col = c("darkgreen", "cadetblue"), 
     xlab = "Time to death (months)",
     ylab = "Cum Survival")
# Adding the Number at risk in each time interval
mtext(text = sex.km$n.risk[1:30], 
      side = 3, at = sex.km$time[1:30], 
      cex = .5, col = "darkgreen")
mtext(text = sex.km$n.risk[31:57], 
      line = 1, at = sex.km$time[31:57], 
      cex = .5, col = "cadetblue")
# and a simple explanatory legend
legend(x = 2.5, y = 0.5, legend = c("Male", "Female"), 
       lty = 1, col = c("darkgreen", "cadetblue"), 
       bty = "n", cex = .75)




#KM for antibiotic treatment
anti.km <- survfit(Surv(dthtime_1m, dthstat) ~ antibiotic, data = mac)

ggsurvplot(anti.km, legend.labs=c("Rifabutin", "Clarithromycin", "Combination"))


#KM curve plotted
plot(anti.km, 
     col = c("darkgreen", "cadetblue", "slateblue"), 
     xlab = "Time to death (months)", ylab = "Cum Survival")
legend(x = 2.5, y = 0.5, 
       legend = c("Rifabutin", "Clarithromycin", "Combination"), 
       lty = 1, col = c("darkgreen", "cadetblue", "slateblue"), 
       bty = "n", cex = .75)
mtext(text = anti.km$n.risk[1:30], 
      side = 3, at = anti.km$time[1:30], 
      cex = .5, col = "darkgreen")
mtext(text = anti.km$n.risk[31:59], 
      line = .5, at = anti.km$time[31:59],
      cex = .5, col = "cadetblue")
mtext(text = anti.km$n.risk[60:87], 
      line = 1, at = anti.km$time[60:87], 
      cex = .5, col = "slateblue")



#1_Proportionality_observed_vs_predicted
coxmac.s <- coxph(Surv(dthtime_1m, dthstat) ~ sex, data = mac)
plot(survfit(coxph(Surv(dthtime_1m, dthstat) ~ 1,
                   data = mac, 
                   subset = sex == 0)), 
     conf.int = F, col = "red", lwd = 2)
par(new = T)
plot(survfit(coxph(Surv(dthtime_1m, dthstat) ~ 1, 
                   data = mac, 
                   subset = sex == 1)), 
     conf.int = F, col = "black", lwd = 2)
par(new = T)
plot(survfit(coxmac.s, newdata = data.frame(sex = 0)), 
     col = "red", conf.int = F)
par(new = T)
plot(survfit(coxmac.s, newdata = data.frame(sex = 1)), 
     col = "black", conf.int = F)
legend(1, .5, 
       legend = c("Observed Male", "Observed Female", 
                  "Predicted Male", "Predicted Female"), 
       lwd = c(2, 2, 1, 1), 
       col = c("red", "black", "red", "black"),
       cex = 0.75, bty = "n")

#1_Proportionality_log-log survival curves against logtime
antib <- Surv(mac$dthtime_1m, mac$dthstat)
antib
plot(survfit(antib ~ mac$antibiotic), 
     lty = c(2:4), col = c(8:10), 
     fun = "cloglog", 
     ylab = "log-log survival", 
     xlab = "logtime")
legend(1, 0, 
       legend = c("Rifabutin", "Clarithromycin", "Combination"), 
       lty = c(2:4), col = c(8:10), 
       bty = "n", cex = .75)


#3_linearity
q <- quantile(mac$cd4, c(0, .25, .5, .75, 1))
varQ <- cut(mac$cd4, q, include.lowest = T)
km.q.cd4 <- coxph(Surv(dthtime_1m, dthstat) ~ varQ, data = mac)
km.q.cd4


###############multivariable cox regression models###############
multicox <- coxph(Surv(dthtime_1m, dthstat) ~ age + sex + cd4 + 
                  antiret, data = mac)
summary(multicox)

#2_Goodness-of-fit (GOF) tests
gof.s <- coxph(Surv(dthtime_1m, dthstat) ~ age + sex + cd4 + antiret, data = mac)
gof.s.zph <- cox.zph(gof.s)
gof.s.zph

ggforest(multicox, data=mac, fontsize=1.2)

########################################################################################################
### Parametric Survival

fitExp <- survreg(Surv(dthtime_1m, dthstat) ~ cd4, data = mac, dist = "exponential")
summary(fitExp)
exp(coef(fitExp))
exp(confint(fitExp))

fitExp <- survreg(Surv(dthtime_1m, dthstat) ~ sex, data = mac, dist = "exponential")
summary(fitExp)
exp(coef(fitExp))
exp(confint(fitExp))

coxmac <- coxph(Surv(dthtime_1m, dthstat) ~ sex, data = mac)
summary(coxmac)

# Get the KM estimates
fitKM = survfit( Surv(dthtime_1m, dthstat) ~ sex,data = mac)

plot(fitKM,mark.time = F,main = "Predicted survival for Exponential model VS KM",
     xlab = "Time",ylab = "Survival probability",lty = 1:2,
     col = c("blue","red"))
# Add the curves fitted by the exponential model
pct = seq(0,1,by = 0.001)
lines(predict(fitExp,newdata = data.frame(sex = 0),type = "quantile",
              p = pct),1-pct,lty = 3,col = "green")
lines(predict(fitExp,newdata = data.frame(sex = 1),type = "quantile",
              p = pct),1-pct,lty = 4,col = "orange")
legend("topright",bty = "n",lty = 1:4,col = c("blue","red","green","orange"),
       legend = c("KM: Males","KM: Females","EXP: Males","EXP: Females"),ncol = 2)


# now fit a corresponding Weibull model

fitWei = survreg( Surv(dthtime_1m, dthstat) ~ sex,data = mac,dist = "weibull")
summary(fitWei)
exp(coef(fitWei))
exp(confint(fitWei))

#produce a graph that compares the survival curves predicted by the
#Weibull model with the raw KM survival estimates

plot(fitKM,mark.time = F,main = "Predicted survival for Weibull model VS KM",
     xlab = "Time",ylab = "Survival probability",lty = 1:2,
     col = c("blue","red"))
# Add curves fitted by the Weibull model
pct = seq(0,1,by = 0.001)
lines(predict(fitWei,newdata = data.frame(sex = 0),type = "quantile",
              p = pct),1-pct,lty = 3,col = "green")
lines(predict(fitWei,newdata = data.frame(sex = 1),type = "quantile",
              p = pct),1-pct,lty = 4,col = "orange")
legend("topright",bty = "n",lty = 1:4,col = c("blue","red","green","orange"),
       legend = c("KM: Males","KM: Females","WEI: Males","WEI: Females"),ncol = 2)

mac$antibiotic<-relevel(mac$antibiotic,ref="combination")

fitExp <- survreg(Surv(dthtime_1m, dthstat) ~ antibiotic, data = mac, dist = "exponential")
summary(fitExp)
exp(coef(fitExp))
exp(confint(fitExp))

coxmac <- coxph(Surv(dthtime_1m, dthstat) ~ antibiotic, data = mac)
summary(coxmac)

# Get the KM estimates
fitKM = survfit( Surv(dthtime_1m, dthstat) ~ as.factor(antibiotic),data = mac)

plot(fitKM,mark.time = F,main = "Predicted survival for Exponential model VS KM",
     xlab = "Time",ylab = "Survival probability",lty = c(1,2,3),
     col = c("violet","red", "blue"))
# Add the curves fitted by the exponential model
pct = seq(0,1,by = 0.001)

lines(predict(fitExp,newdata = data.frame(antibiotic = "combination"),type = "quantile", p = pct),1-pct,lty = 4,col = "brown")
lines(predict(fitExp,newdata = data.frame(antibiotic = "rifabutin"),type = "quantile",
              p = pct),1-pct,lty = 5,col = "orange")
lines(predict(fitExp,newdata = data.frame(antibiotic = "clarithromycin"),type = "quantile",
              p = pct),1-pct,lty = 6,col = "green")
legend("topright",bty = "n",lty = 1:4,col = c("violet","red", "blue","brown", "orange","green"),
       legend = c("KM: combination","KM: rifabutin", "KM: clarithromycin","EXP: combination","EXP: rifabutin", "EXP: clarithromycin"),ncol = 2)


fitWei <- survreg(Surv(dthtime_1m, dthstat) ~ antibiotic, data = mac, dist = "weibull")
summary(fitWei)
exp(coef(fitWei))
exp(confint(fitWei))

coxmac <- coxph(Surv(dthtime_1m, dthstat) ~ antibiotic, data = mac)
summary(coxmac)

# Get the KM estimates
fitKM = survfit( Surv(dthtime_1m, dthstat) ~ as.factor(antibiotic),data = mac)

plot(fitKM,mark.time = F,main = "Predicted survival for Weibull model VS KM",
     xlab = "Time",ylab = "Survival probability",lty = c(1,2,3),
     col = c("violet","red", "blue"))
# Add the curves fitted by the weibull model
pct = seq(0,1,by = 0.001)

lines(predict(fitWei,newdata = data.frame(antibiotic = "combination"),type = "quantile", p = pct),1-pct,lty = 4,col = "brown")
lines(predict(fitWei,newdata = data.frame(antibiotic = "rifabutin"),type = "quantile",
              p = pct),1-pct,lty = 5,col = "orange")
lines(predict(fitWei,newdata = data.frame(antibiotic = "clarithromycin"),type = "quantile",
              p = pct),1-pct,lty = 6,col = "green")
legend("topright",bty = "n",lty = 1:4,col = c("violet","red", "blue","brown", "orange","green"),
       legend = c("KM: combination","KM: rifabutin", "KM: clarithromycin","WEI: combination","WEI: rifabutin", "WEI: clarithromycin"),ncol = 2)

#########################################
# multi weibull model

multifitWei <- survreg(Surv(dthtime_1m, dthstat) ~ cd4+sex+antibiotic, data = mac, dist = "weibull")
summary(multifitWei)
exp(coef(multifitWei))
exp(confint(multifitWei))


#packages needed
library(ggplot2)
library(survival)
library(survminer)

# Take a look at the data set
library(readxl)
transfusion <- read_excel("transfusion.xlsx")
str(transfusion)


# Construct a survival object
trsf.s <- Surv(transfusion$Event.w, transfusion$Status)
trsf.s

# Run the survfit for the above survival object 
km <- survfit(Surv(Event.w, Status) ~ 1, data = transfusion)
km
# Obtain a summary for the survival object above
summary(km)

#plot the results - Kaplan - Meier Curve for the overall survival (mark.time=T is used to indicate the censored data on the graph)
# using base R
plot(km, xlab = "Time", ylab = "Survival probability", mark.time=T)
# using ggsurvplot
ggsurvplot(km)




###KM curves and log-rank test for Transfusion group
#plot by Transfusion group
km.tr <- survfit(Surv(Event.w, Status) ~ Transfusion, data = transfusion)
# to find which month corresponds to median survival run:
km.tr
# using base R
plot(km.tr, lty = 2:3, xlab = "Days", mark.time=T)
legend(x = "bottomleft", legend = c("Not transfused", "Transfused"), lty = 2:3, cex = .75)
# using ggsurvplot
ggsurvplot(km.tr)


#Run log-Rank test to compare the curves for the Transfusion groups
survdiff(Surv(Event.w, Status) ~ Transfusion, data = transfusion, rho = 0)

#Plot by Transfusion group including the log-rank test result  and diplaying a risk table at the end 
ggsurvplot(km.tr, data = transfusion, pval = T, risk.table = "nrisk_cumevents")
#Plot by Transfusion group including the log-rank test result  and diplaying a risk table at the end and including the median survival 
ggsurvplot(km.tr, data = transfusion, pval = T, risk.table = "nrisk_cumevents", surv.median.line="h")

#To check the difference of the curves between the Transfusion groups only 
#for those with Blood type "B"(note the argument "subset"): 
survdiff(Surv(Event.w, Status) ~ Transfusion, data = transfusion, subset = Blood_type == "B", rho = 0)



###KM curves and log-rank test for Blood type group
#plot by Blood Type group
km.bt <- survfit(Surv(Event.w, Status) ~ Blood_type, data = transfusion)
# to find which month corresponds to median survival run:
km.bt 
# Plotting in this case requires an amount of formatting to be able to distringuish the lines
# using base R
plot(km.bt, col = c("brown1", "gold", "dodgerblue3", "black"), xlab = "Weeks", ylab = "Survival probability", lwd = c(3, 3, 2, 2), mark.time=TRUE)
legend(x = "bottomleft", legend = c("A", "AB", "B", "O"), col = c("brown1", "gold", "dodgerblue3", "black"), lty = 1, lwd = c(2, 3, 2, 2), cex = 0.75)
# using ggsurvplot
ggsurvplot(km.bt)

#Run log-Rank test to compare the curves for Blood type groups
survdiff(Surv(Event.w, Status) ~ Blood_type, data = transfusion, rho = 0)
#Plot by Blood type group including the log-rank test result  and diplaying a risk table at the end 
ggsurvplot(km.bt, data = transfusion, pval = T, risk.table = "nrisk_cumevents")
#Plot by Blood type group including the log-rank test result  and diplaying a risk table at the end and including the median survival 
ggsurvplot(km.bt, data = transfusion, pval = T, risk.table = "nrisk_cumevents", surv.median.line="h")






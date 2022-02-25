################################    Missing Data   #################################
# ____________________________________________________________________________________
# 1.Complete Cases analysis
# ____________________________________________________________________________________ 
# install required packages
install.packages("mice")
library(mice)

# Import the data
# dataset: birthwt

str(birthwt)

###Format each variable appropriately###
birthwt$race <- factor(birthwt$race, levels=c(1,2,3),labels = c("white", "black", "other"))
birthwt$smoke <- factor(birthwt$smoke, levels=c(0,1),labels = c ("no","yes"))

summary(birthwt)
 
###examine the pattern of missingness in the data###
md.pattern(birthwt,plot = FALSE)

m1.cc <- lm(bwt ~ age + smoke, data = birthwt)
summary(m1.cc)

# ______________________________________________________________________________________ 
# 2.Multiple Imputation
# 2.1 Linear regression                          
# ______________________________________________________________________________________ 

#impute the missing values
small <- birthwt[, c("bwt", "age", "smoke", "race", "ftv")]

imp <- mice(small, m = 5, print = FALSE, seed = 12345)
imp

#check imputed values
imp$imp$bwt
imp$imp$age
imp$imp$smoke
imp$imp$race
imp$imp$ftv

#get complete data (1st out of 5)
completeData1 <- complete(imp,1)
View(completeData1)

#Visualizing missing values for continuous variables
stripplot(imp, col=c("grey", "blue"), pch = c(1, 20))

#building the model
m1.mi <- with(imp, lm(bwt ~ age + smoke))
t(sapply(m1.mi$analyses, coef))

summary(pool(m1.mi), conf.int = TRUE)


# ______________________________________________________________________________________ 
# 2.Multiple Imputation
# 2.2 Logistic regression                          
# ______________________________________________________________________________________
birthwt$bwt_cat <- cut(birthwt$bwt, 
                       breaks=c(-Inf, 2500, Inf),
                       levels=c(1,2),
                       labels=c("2500 gr or less","more than 2500 gr"))
View(birthwt)

#impute the missing values
small_2 <- birthwt[, c("bwt_cat", "age", "smoke", "race", "ftv")]

imp_2 <- mice(small_2, m = 5, print = FALSE, seed = 12345)
imp_2

#building the model
m2.mi <- with(imp_2, glm(bwt_cat ~ age + smoke,family = binomial()))
summary(pool(m2.mi), exponentiate = TRUE, conf.int = TRUE)

# ______________________________________________________________________________________ 
# 3.Cox proportional hazards regression
#                                      
# ______________________________________________________________________________________

# install required packages
install.packages("survival")
library(survival)

###examine the pattern of missingness in the data###
View(stanford2)
summary(stanford2)
md.pattern(stanford2, plot = TRUE)

stanford2$nelsonaalen <- nelsonaalen(stanford2, time, status)

imp.surv <- mice(stanford2[,c("time","status","age","t5","nelsonaalen")], m = 20, print = FALSE)
m2.mi <- with(imp.surv, coxph(Surv(time, status) ~ t5 + age))
summary(pool(m2.mi), conf.int = TRUE, exponentiate = TRUE)

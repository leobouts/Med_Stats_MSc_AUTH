library(dplyr)

data_with_factors <- q1 %>%
  mutate(Gender = factor(Gender, labels = c("M", "F"))) %>%
  mutate(Race = factor(Race, labels = c("Other", "Black", "White")))



data_with_factors %>%
  rstatix::shapiro_test(Systolic)

data_with_factors %>%
  rstatix::shapiro_test(Diastolic)

data_with_factors %>%
  rstatix::shapiro_test(Weight)


data_with_factors %>%
  group_by(Gender) %>%
  rstatix::shapiro_test(Systolic)

data_with_factors %>%
  group_by(Race) %>%
  rstatix::shapiro_test(Systolic)

data_with_factors %>%
  group_by(Gender) %>%
  rstatix::shapiro_test(Diastolic)

data_with_factors %>%
  group_by(Race) %>%
  rstatix::shapiro_test(Diastolic)

data_with_factors %>%
  group_by(Gender) %>%
  rstatix::shapiro_test(Weight)

data_with_factors %>%
  group_by(Race) %>%
  rstatix::shapiro_test(Weight)

#### Summary measures for quantitative data 

mean(data_with_factors$Systolic, na.rm = T)

median(data_with_factors$Systolic)

sd(data_with_factors$Systolic)

var(data_with_factors$Systolic)

IQR(data_with_factors$Systolic)

quantile(data_with_factors$Systolic)

range(data_with_factors$Systolic)

# Multiple summary measures, by groups
data_with_factors %>%
  group_by(Gender) %>%
  summarise(mean(Systolic), sd(Systolic), median(Systolic), IQR(Systolic))

prop.table(table(data_with_factors$Gender))

mean(c(15,15,15))
sd(c(15,15,15))

mean(c(0,15,30))
sd(c(0,15,30))

mean(c(0,0,0))
sd(c(0,0,0))

mean(c(10,20,15))
sd(c(10,20,15))

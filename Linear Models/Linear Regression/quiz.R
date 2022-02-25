library(ggplot2)
library(GGally)
library(dplyr)
library(rstatix)
library(moderndive)

LungCapData %>%
  select(LungCap, Age) %>%
  ggscatmat(corMethod = "pearson")


LungCapData %>%
  select(LungCap, Age) %>%
  cor_test(method="pearson")

model <- lm(LungCap ~ Age, data = LungCapData)
summary(model)

confint(model, level=0.95)

get_regression_table(model)


anova(model)


########## MULTIPLE


mul_model <- lm(LungCap ~ Age + Height + Gender, data = LungCapData)
summary(mul_model)



library(tidyverse)
library(lubridate)
library(here)

arrhythmia

#allows you to see the classes of the variables (all numeric)
str(arrhythmia)

arrhythmia$sex <- as.factor(arrhythmia$sex)

data_with_bmi <- arrhythmia %>%
  filter(age>=18) %>%
  mutate(BMI = weight/((height/100)^2))

groupped <- data_with_bmi %>%
  mutate(Category=cut(data_with_bmi$BMI,
                breaks = c(-Inf, 18.5, 24.9, 30.0, +Inf),
                labels=c("underweight","normal","overweight","obese")))

groupped

(obese <- filter(groupped, Category=='obese'))

(obese_w_heart_rate_high <- filter(groupped, Category=='obese', heart_rate>85))

obese_w_heart_rate_high %>% dplyr::summarise(mean_confirmed = mean(obese_w_heart_rate_high$QRS, na.rm = TRUE),
                 sd_confirmed = sd(QRS, na.rm = TRUE))

groupped %>% group_by(Category) %>% dplyr::summarise(mean_confirmed = mean(obese_w_heart_rate_high$QRS, na.rm = TRUE),
                                                     sd_confirmed = sd(QRS, na.rm = TRUE))


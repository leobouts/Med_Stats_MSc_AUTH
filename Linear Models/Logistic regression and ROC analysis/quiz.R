library(ggplot2)
library(GGally)
library(dplyr)
library(rstatix)
library(moderndive)


# Explore data and convert data types
# load dataset
glimpse(icu)

# create a correlation plot
corrplot::corrplot(cor(icu))

# converting into factors
icuf <- icu %>%
  mutate(STA = factor(STA, levels = c(0,1), labels = c("Lived", "Died"))) %>%
  mutate(INF = factor(INF, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(SEX = factor(SEX, levels = c(0,1), labels = c("Male", "Female")))

glimpse(icuf)

# Define model
mod_age <- glm(STA ~ INF, data = icuf, family = binomial)

summary(mod_age)

### draw a sigmoid curve for the model
# probability is modeled as a fraction that ranges from 0 to 1; 
# subtract 1 from the converted values as follows 
ggplot(icuf, aes(x = INF, y = as.numeric(STA) - 1)) + 
  geom_point(alpha = .5) +
  stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
              method.args = list(family = binomial)) +
  ylab("Survival") + xlab("Infection") #+ xlim(-100, 100)




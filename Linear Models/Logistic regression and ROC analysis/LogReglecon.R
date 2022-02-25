library(ggplot2)
library(dplyr);library(tidyr);library(magrittr)
library(epiDisplay)
library(gtsummary);library(gt)

# Explore data and convert data types
# load dataset
glimpse(MASS::birthwt)
# create a correlation plot: note that low and bwt are essentially the same variable
corrplot::corrplot(cor(MASS::birthwt))
# converting into factors
lbw <- MASS::birthwt %>%
  mutate(low.b = factor(low, levels = c(0,1), labels = c("Normal", "Underweight"))) %>%
  mutate(race = factor(race, levels = c(1,2,3), labels = c("White", "Black", "Other"))) %>%
  mutate(smoke = factor(smoke, levels = c(0,1), labels = c("No", "Yes"))) %>%
  mutate(ht = factor(ht, levels = c(0,1), labels = c("Normal", "Hypertension"))) %>%
  mutate(ui = factor(ui, levels = c(0,1), labels = c("Normal", "Urinary irritability")))
glimpse(lbw)

--------------------------------------------------------------------------------
# Define model
mod_age <- glm(low.b ~ age, data = lbw, family = binomial)

  ### draw a sigmoid curve for the model
    # probability is modeled as a fraction that ranges from 0 to 1; 
    # subtract 1 from the converted values as follows 
ggplot(lbw, aes(x = age, y = as.numeric(low.b) - 1)) + 
  geom_point(alpha = .5) +
  stat_smooth(method = "glm", se = FALSE, fullrange = TRUE, 
              method.args = list(family = binomial)) + 
  ylab("Low birth weight") + xlab("Mother's age") #+ xlim(-100, 100)

--------------------------------------------------------------------------------
# Assess model assumptions

## Linearity (continuous predictors)

### Visual inspection
  # Calculate predicted values for the model
prob <- predict(mod_age, type = "response")
  # isolate continuous predictors
dt_cont <- lbw %>%
  select(age)
  # bind logit and probabilities
dt_cont %<>%
  mutate(logit = log(prob/(1-prob))) # log odds
  # scatter plots
ggplot(dt_cont, aes(logit, age)) +
  geom_point(size = 0.5, alpha = 0.5) + 
  geom_smooth(method = "loess")

### The Box-Tidwell test
  # use the integer version of the response variable
car::boxTidwell(low ~ age, data = lbw, family = binomial)

--------------------------------------------------------------------------------
## Identify influential values

### Cook's distance
  ### Visualize Cook's distance from base R
plot(mod_age, which = 4)
  # Calculate Cook's distance
mod <- broom::augment(mod_age) %>%
  mutate(id = 1:n()) # useful for the plot of standardized residuals
  # Cook’s Distance > 3*mean(Cook's Di)
which(mod$.cooksd > 3*mean(mod$.cooksd))
  ### Cook’s Distance > 4/N(observations)
which(mod$.cooksd > 4/nrow(mod))
  ### F-distribution's 50th percentile function for this dataset
    # qf(.5, ncol(lbw), nrow(lbw)-ncol(lbw))
which(mod$.cooksd > qf(.5, 12, 177))
  # Rule of thumb
which(mod$.cooksd > 1)
which(mod$.cooksd > .5)

### Standardized residuals
  ### Calculate the standardized residuals (>3 implies an outlier)
  # plot standardized residuals
#plot(mod$.std.resid)
ggplot(mod, aes(id, .std.resid)) +
  geom_point(aes(color = low.b), alpha = 0.5)
  # filter data points with absolute standardized residuals > 3
mod %>%
  filter(abs(.std.resid) > 3)

--------------------------------------------------------------------------------
# Assess model fit

## Compare deviance
mod_age$null.deviance
mod_age$deviance
# the smaller the deviance, the better the fit
mod_age$null.deviance - mod_age$deviance
mod_age$df.null - mod_age$df.residual
1 - pchisq(2.76, 1)
lrtest(mod_age, glm(low.b ~ 1, lbw, family = binomial))

## Nagelkerke's R-squared
#The variability explained by the model, in this example 2.0%
rms::lrm(low.b ~ age, lbw)


# Retrieve model summaries

  ## base R
summary(mod_age)
cbind(exp(coef(mod_age)), exp(confint(mod_age)))

  ## epiDisplay
logistic.display(mod_age)

  ## gtsummary
tbl_regression(mod_age, exponentiate = T, 
               label = age ~ "Mother's age")

--------------------------------------------------------------------------------
# Multiple logistic regression

multilog <- glm(low.b ~ lwt + age + smoke + race + ptl + ht + ui, 
                data = lbw, 
                family = binomial)
summary(multilog)
tbl_regression(multilog, exponentiate = T, label = list(age ~ "Mother's age",
                                                        lwt ~ "Mother's weight",
                                                        race  ~ "Mother's race",
                                                        smoke ~ "Smoking status",
                                                        ptl ~ "Previous preterm labours",
                                                        ht ~ "Hypertension",
                                                        ui ~ "Uterine irritability"))

## Multicollinearity assumption diagnostics

# VIF: variance inflation factor
#equal to the ratio of the overall model variance to 
# the variance of a model that includes only that single independent variable
car::vif(glm(low.b ~ lwt + age + smoke + race + ptl + ht + ui, 
        data = lbw, 
        family = binomial))


# Comparing models
## Analysis of deviance table
    # For illustrative purposes, fit a second, reduced model
multilog.red <- glm(low.b ~ lwt + smoke + race + ht, 
                    data = lbw, 
                    family = binomial)

anova(multilog, multilog.red, test = "Chisq")

## AIC
AIC(multilog)
AIC(multilog.red)

--------------------------------------------------------------------------------
# Stepwise model selection

## Backward elimination
step(multilog, direction = "backward")

## Forward selection
lbwn <- na.omit(lbw)
nullmod <- glm(low.b ~ 1, data = lbwn, family = binomial)
step(nullmod, scope = list(lower = formula(nullmod), upper = formula(multilog)),
     direction = "forward", na.rm = T)

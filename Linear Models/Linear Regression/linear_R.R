##########################  Linear regression   ############################


# download and load the following packages ---------------------------------------------

library(GGally)
library(jtools)
library(interactions)
library(pmsampsize)
library(summarytools)
library(performance)
library(ggstatsplot)

library(here)
library(tidyverse)

# import the data ---------------------------------------------------------

# if you use "Projects" (otherwise use the "Import Dataset" from the environment pane)
library(readxl)
BirthWeight <- read_excel(here("data", "BirthWeight.xlsx"))


# inspect the data using `dfSummary` from `summarytools` package 
summarytools::view(dfSummary(BirthWeight))




# transformations to the variables  ---------------------------------------

BirthWeight <- BirthWeight %>% 
  select(-id) %>%                 # remove id variable
  mutate(weight = weight*1000,    # multiply weight by 1000
         gender = factor(gender),
         education = fct_relevel(education, "tertiary", after = Inf),   # tertiary to the end
         parity = factor(parity, levels = c("Singleton", "One sibling",
                                            "2 or more siblings"))      # singleton first
  )


summarytools::view(dfSummary(BirthWeight))





#####################  simple linear regression  #########################


# continuous explanatory variables -----------------------------------------

## correlation matrix
BirthWeight %>% 
  select(weight, height, headc) %>% 
  ggscatmat()



## fit regression: weight ~ height

model_height <- lm(weight ~ height, data = BirthWeight)
summ(model_height, confint = TRUE, digits = 3)



## fit regression: weight ~ headc

model_headc <- lm(weight ~ headc, data = BirthWeight)
summ(model_headc, confint = TRUE, digits = 3)





# categorical explanatory variables -----------------------------------------


## fit regression: weight ~ gender

model_gender <- lm(weight ~ gender, data = BirthWeight)
summ(model_gender, confint = TRUE, digits = 3)




## fit regression: weight ~ parity

model_parity <- lm(weight ~ parity, data = BirthWeight)
summ(model_parity, confint = TRUE, digits = 3)



## fit regression: weight ~ education

model_education <- lm(weight ~ education, data = BirthWeight)
summ(model_education, confint = TRUE, digits = 3)







####################  multiple linear regression  #########################


# sample size calculation -----------------------------------------

# Initally we had 5 candidate variables (height, headc, gender, parity and education). 
# Parity and education have dummy variables.
# so we need to calculate the sample size with a total of 7 parameters.


pmsampsize(type = "c",         # continuous outcome
           rsquared = 0.4,     # variance in outcome explained by the model
           parameters = 7,     # the number of candidate predictor parameter
           shrinkage = 0.9,    # measure of overfitting, and can range from 0 to 1
           intercept = 4000,   # the average outcome value in the population of interest 
           sd = 700,           # the standard deviation of outcome in the population
           mmoe = 1.1)         # multiplicative margin of error for intercept (10%)


# i) small overfitting defined by an expected shrinkage of predictor effects by 10% or less,
#ii) small absolute difference of 0.05 in the model’s apparent and adjusted R-squared value,
#iii) precise estimation of the residual standard deviation, and
#iv) precise estimation of the average outcome value.





# final_model -----------------------------------------

# The multivariable model will include the following explanatory variables:
# height, headc, gender, and parity (p <0.2 in univariable analysis).
# We excluded education that has p >0.2 in univariable analysis.


final_model <- lm(weight ~ height + headc + gender + parity, data = BirthWeight)
summ(final_model, confint = TRUE, vifs = TRUE, digits = 3)

# Note: A VIF less than 5 indicates a no-multicollinearity





# plot final_model -----------------------------------------
plot_summs(final_model, scale = F) + 
  theme_minimal(base_size = 14)



# or using the ggcoefstats() from package {ggstatsplot}
ggstatsplot::ggcoefstats(
  x = stats::lm(weight ~ height + headc + gender + parity, data = BirthWeight),
  exclude.intercept = T,
  sort = "ascending", # sorting the terms of the model based on estimate values
  ggtheme = ggplot2::theme_gray(), # changing the default theme
  stats.label.color = c("#CC79A7", "darkgreen", "#0072B2", "darkred", "black")
)  






# Diagnostics for the final_model -----------------------------------------

# Linearity of relationship between variables
# Equality of variance of the residuals (homogeneity of variance)
# Collinearity
# Influential observations
# Normality of the residuals

check_model(final_model, check = "all")





# Automated models (AIC selection) -----------------------------------------
#NOTE: Lower AIC, the better model


# Backward elimination
model_back <- step(lm(weight ~ ., data = BirthWeight),
                   direction = "backward")



# Forward selection
model_forward <- step(lm(weight ~ 1, data = BirthWeight),
                      direction = "forward",
                      scope = ~ height + headc + gender + parity + education)






# Interaction between continuous and dichotomous variables -----------------------------------------

# model with interaction
interact_model <- lm(weight ~ height + gender + height:gender, data=BirthWeight)

# Get regression table
summ(interact_model, confint = TRUE, digits = 3)


# Graphical investigation: interaction between height and gender (non-parallel lines)
interact_plot(interact_model, plot.points = T, pred = height, modx = gender) +
  theme_bw()


# Conclusion: The slopes of the regression lines are not parallel 
# which indicates that there is interaction between height and gender.





# Interaction between two numeric variables ------------------------------

# model with interaction
interact_model2 <- lm(weight ~ height + headc + height:headc, data=BirthWeight)

# Get regression table
summ(interact_model2, confint = TRUE, digits = 3)

# plot the regression line between weight and height for 1 sd above and below the mean
interact_plot(interact_model2, plot.points = T, pred = height, modx = headc) +
  theme_bw()


# Conclusion: The slopes of the regression lines are parallel 
# which indicates that there is not interaction between height and headc


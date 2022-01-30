######################## Solutions: categorical variables #########################

library(rstatix)
library(finalfit)
library(here)
library(tidyverse)


########################### Activity 1: dataInflu   ###############################

library(readxl) # Read Excel Files
dataInflu <- read_excel(here("data", "dataInflu.xlsx"), col_names=TRUE)
dataInflu

# prepare the data 
dataInflu <- dataInflu %>% 
  mutate(exposure = factor(exposure, levels=c(0,1), labels=c("placebo","vaccine")),
         influenza = factor(influenza))

dataInflu


# create the 2x2 table
influ <- table(dataInflu$exposure, dataInflu$influenza)
influ

# obtain a more informative table
row_tb <- dataInflu %>%
  summary_factorlist(dependent = "influenza", add_dependent_label = T,
                     explanatory = "exposure", 
                     column = FALSE, total_col = TRUE)

row_tb


# calculate the expected frequencies
epitools::expected(influ)

# perform the chi-squared test
chisq_test(influ)




########################### Activity 2   ###############################

# create the table
cgh <- c(123, 77, 27, 73)

cgh <- matrix(cgh, nrow = 2, ncol = 2, byrow = T, 
              dimnames = list(c("Cough before", "No cough before"), 
                              c("Cough after", "No cough after")))
cgh


(123 + 77) * 100/300 # cough before
(123 + 27) * 100/300 # cough after


# perform the mcnemar test
mcnemar_test(cgh)




########################### Activity 3   ###############################

# create the table
dat <- c(7, 17, 2, 42)

poly <- matrix(dat, nrow = 2, dimnames = list(c("replacement", "synonymous"), c("fixed", "polymorphic ")))

poly



# calculate the expected frequencies
epitools::expected(poly)

# perform the fisher exact test
fisher_test(poly)


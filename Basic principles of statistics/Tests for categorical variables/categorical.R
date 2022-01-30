#################### Tests for categorical variables ######################



# load the packages ----------------------------------------------------------
library(rstatix) # Pipe-Friendly Framework for Basic Statistical Tests
library(epitools)
library(ggsci)
library(finalfit)
library(janitor)
library(modelsummary)

library(here) # A Simpler Way to Find Your Files
library(patchwork)
library(tidyverse) # Easily Load the 'Tidyverse'



############# Pearson’s Chi-squared test of independence ##################

# Import and prepare the data ----------------------------------------------

# import the data
library(readxl)
meldata <- read_excel(here("data", "meldata.xlsx"), col_names=TRUE)

glimpse(meldata)


# convert status and ulcer to factors

meldata <- meldata %>%
  convert_as_factor(status, ulcer)

glimpse(meldata)




# Plots -------------------------------------------------------------

# counts plot
p1 <- meldata %>%
  ggplot(aes(x = ulcer, fill = status)) +
  geom_bar(width = 0.7) +
  scale_fill_jco() +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")

# percentage plot
p2 <- meldata %>%
  ggplot(aes(x = ulcer, fill = status)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jco() +
  ylab("Percentage") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")


# combine the two plots
p1 + p2 +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')



# tables ------------------------------------------------------------------

# create a basic table
tb1 <- table(meldata$ulcer, meldata$status)
tb1


# row percentages table
datasummary_crosstab(ulcer ~ status, data = meldata)



# calculate the expected frequencies for each cell
epitools::expected(tb1)





# perform a Chi-squared test of independence -----------------------------

chisq.test(tb1)




# Ratios with the 95% CI using R ------------------------------------------

# The risk ratio with the 95% CI using R
riskratio(tb1)$measure


# The odds ratio with the 95% CI using R:
oddsratio(tb1, method = "wald")$measure





######################### Fisher’s exact test ##############################

# Import and prepare the data ----------------------------------------------

# import the data
library(readxl)
hemophilia <- read_excel(here("data", "hemophilia.xlsx"), col_names=TRUE)
glimpse(hemophilia)


# convert treatment and bleeding to factors
hemophilia <- hemophilia %>%
  convert_as_factor(treatment, bleeding)

glimpse(hemophilia)


# Plots -------------------------------------------------------------

# counts plot
p3 <- hemophilia %>%
  ggplot(aes(x = treatment, fill = bleeding)) +
  geom_bar(width = 0.7) +
  scale_fill_jama() +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")


# percentage plot
p4 <- hemophilia %>%
  ggplot(aes(x = treatment, fill = bleeding)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jama() +
  ylab("Percentage") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")


# combine the two plots
p3 + p4 +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')



# tables ------------------------------------------------------------------

# create a basic table
tb2 <- table(hemophilia$treatment, hemophilia$bleeding)
tb2


# row percentages table
datasummary_crosstab(treatment ~ bleeding, data = hemophilia)




# calculate the expected frequencies for each cell
epitools::expected(tb2)


# perform a Fisher’s exact test -------------------------------------------
fisher.test(tb2)






############################# McNemar test ##############################

# Import and prepare the data ----------------------------------------------

# import the data
library(readxl)
asthma <- read_excel(here("data", "asthma.xlsx"), col_names=TRUE)
glimpse(asthma)



# convert treatment and bleeding to factors
asthma <- asthma %>%
  convert_as_factor(know_begin, know_end)

glimpse(asthma)




# tables ------------------------------------------------------------------

# create a basic table
tb3 <- table(know_begin = asthma$know_begin, know_end = asthma$know_end)
tb3

# a more informative table
datasummary_crosstab(know_begin ~ know_end, 
                     statistic = 1 ~ 1 + N + Percent(), data = asthma)



# perform a McNemar test (discordant cells >25) ----------------------------

mcnemar.test(tb3)


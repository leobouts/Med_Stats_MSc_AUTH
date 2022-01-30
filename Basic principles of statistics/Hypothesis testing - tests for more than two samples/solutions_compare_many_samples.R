###################### Solutions:  compare more than 2 samples ####################


######################### Activity 1: data_headache   ############################
library(rstatix) # Pipe-Friendly Framework for Basic Statistical Tests
library(here) # A Simpler Way to Find Your Files
library(tidyverse) # Easily Install and Load the 'Tidyverse'


# Prepare the data ------------------------------------------------------------

# Import the data
library(readxl)
data_headache <- read_excel(here("data", "data_headache.xlsx"))
data_headache


# convert 'group' to factor
data_headache <- data_headache %>% 
  mutate(group = factor(group, levels=c(1, 2, 3), 
                        labels=c("with feedback", "without feedback", "not treated")))


# inspect the data type again
glimpse(data_headache)


table(data_headache$group)


# Assumptions ------------------------------------------------------------

# descriptive statistics by group
data_headache %>%
  group_by(group) %>%
  dlookr::describe(reduction) %>%
  select(variable, group, n, mean, sd, p25, p50, p75, skewness, kurtosis) %>% 
  ungroup()


# dot plots
data_headache %>%
  ggplot(aes(x = reduction, fill = group)) +
  geom_dotplot(fill = "black", dotsize = 0.8) +
  facet_wrap(~ group, ncol=1)


# normality test
data_headache %>%
  group_by(group) %>%
  shapiro_test(reduction) %>%
  ungroup()



# Run the test ------------------------------------------------------------

# Kruskal-Wallis test

data_headache %>%
  kruskal_test(reduction ~ group)


# Post-hoc analysis (Dunn approach)

pwc_Dunn <- data_headache %>%
  dunn_test(reduction ~ group, p.adjust.method = "bonferroni")

pwc_Dunn






#################### Activity 2: data_temperature   ####################


# Prepare the data ------------------------------------------------------------

# Import the data
library(readxl)
data_temperature <- read_excel(here("data", "data_temperature.xlsx"))
data_temperature


# convert 'group' to factor
data_temperature <- data_temperature %>% 
  mutate(group = factor(group, levels=c(1, 2, 3, 4), 
                        labels=c("dose1", "dose2", "dose3", "dose4")))


# inspect the data type again
glimpse(data_temperature)

table(data_temperature$group)



# Assumptions ------------------------------------------------------------

# descriptive statistics by group
data_temperature %>%
  group_by(group) %>%
  dlookr::describe(temp.change) %>%
  select(variable, group, n, mean, sd, p25, p50, p75, skewness, kurtosis) %>% 
  ungroup()


# density plots
data_temperature %>%
  ggplot(aes(x = temp.change, fill = group)) +
  geom_density(binwidth =0.4, alpha=0.7) +
  facet_wrap(~ group, ncol=1)


# normality test
data_temperature %>%
  group_by(group) %>%
  shapiro_test(temp.change) %>%
  ungroup()



# Levene's test for equality of variances
data_temperature %>% 
  levene_test(temp.change ~ group)



# Run the test ------------------------------------------------------------

# ANOVA test

data_temperature %>%
  anova_test(temp.change ~ group, detailed = T)



# Post-hoc analysis (Tukey test)

pwc_Tukey <- data_temperature %>%
  tukey_hsd(temp.change ~ group)

pwc_Tukey

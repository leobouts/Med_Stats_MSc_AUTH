# load the packages ----------------------------------------------------------
library(rstatix) # Pipe-Friendly Framework for Basic Statistical Tests
library(here) # A Simpler Way to Find Your Files
library(tidyverse) # Easily Install and Load the 'Tidyverse'


######################## ANOVA ###########################

# Import and prepare the data ----------------------------------------------

# import the data
library(readxl)
folate <- read_excel("/Users/leonidas/Desktop/Auth Medical Statistics Msc Material/Basic principles of statistics/Hypothesis testing - tests for more than two samples/data_folate.xlsx", col_names=TRUE)
View(folate)

# convert 'group' to factor

folate <- folate %>% 
  mutate(group = factor(group, levels=c("1","2","3"), labels=c("group1", "group2", "group3")))

View(folate)

table(folate$group)

# Check the assumptions ---------------------------------------------------

# normality graphically
ggplot(folate, aes(x = group, y = folate, fill = group)) + 
  geom_boxplot(width=0.3) +
  theme_classic(base_size = 18) +
  theme(legend.position="none",
        axis.text = element_text(size = 14))

# summary statistics with dlookr (includes statistical moments)
folate %>%
  group_by(group) %>%
  dlookr::describe(folate) %>%
  select(variable, group, n, mean, sd, p25, p50, p75, skewness, kurtosis) %>% 
  ungroup()

# Shapiro-Wilk test for normality
folate %>%
  group_by(group) %>%
  shapiro_test(folate) %>%
  ungroup()

# Levene's test for homoscedasticity
folate %>%
  levene_test(folate ~ group)


# Run the test ------------------------------------------------------------

# ANOVA

folate %>%
  anova_test(folate ~ group, detailed = T)



# Post-hoc analysis (Tukey test)

folate %>%
  tukey_hsd(folate ~ group)


#or pairwise comparisons using t-tests with Bonferroni correction
folate %>%
  pairwise_t_test(folate ~ group, pool.sd = TRUE, p.adjust.method = "bonferroni")

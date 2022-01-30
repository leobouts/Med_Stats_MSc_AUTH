# load the packages ----------------------------------------------------------
library(rstatix) # Pipe-Friendly Framework for Basic Statistical Tests
library(here) # A Simpler Way to Find Your Files
library(tidyverse) # Easily Install and Load the 'Tidyverse'


######################## ANOVA ###########################

# Import and prepare the data ----------------------------------------------

# import the data
library(readxl)
head <- read_excel("/Users/leonidas/Desktop/Auth Medical Statistics Msc Material/Basic principles of statistics/Hypothesis testing - tests for more than two samples/data_headache.xlsx", col_names=TRUE)
View(head)

# convert 'group' to factor

head <- head %>% 
        mutate(group = factor(group, levels=c("1","2","3"), labels=c("relaxation_without_bio", "relaxation_with_bio", "untreated")))

# inspect the data type again
glimpse(head)

table(head$group)


# Check the assumptions ---------------------------------------------------

# normality graphically
ggplot(head, aes(x = group, y = reduction, fill = group)) + 
  geom_boxplot(width=0.3) +
  theme_classic(base_size = 18) +
  theme(legend.position="none",
        axis.text = element_text(size = 14))

# summary statistics with dlookr (includes statistical moments)
head %>%
  group_by(group) %>%
  dlookr::describe(reduction) %>%
  select(variable, group, n, mean, sd, p25, p50, p75, skewness, kurtosis) %>% 
  ungroup()

# Shapiro-Wilk test for normality
head %>%
  group_by(group) %>%
  shapiro_test(reduction) %>%
  ungroup()

# Levene's test for homoscedasticity
head %>%
  levene_test(reduction ~ group)

# Run the test ------------------------------------------------------------

# ANOVA

head %>%
  anova_test(reduction ~ group, detailed = T)



# Post-hoc analysis (Tukey test)

head %>%
  tukey_hsd(reduction ~ group)


#or pairwise comparisons using t-tests with Bonferroni correction
head %>%
  pairwise_t_test(reduction ~ group, pool.sd = TRUE, p.adjust.method = "bonferroni")



#not assuming equal variance --------------------------------------------------

# Welch one-way ANOVA test (not assuming equal variance)

head %>%
  welch_anova_test(reduction ~ group)


# Pairwise comparisons (Games-Howell)
head %>%
  games_howell_test(reduction ~ group)

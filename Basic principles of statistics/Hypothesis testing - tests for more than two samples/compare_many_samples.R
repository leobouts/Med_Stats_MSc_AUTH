# load the packages ----------------------------------------------------------
library(rstatix) # Pipe-Friendly Framework for Basic Statistical Tests
library(here) # A Simpler Way to Find Your Files
library(tidyverse) # Easily Install and Load the 'Tidyverse'


######################## ANOVA ###########################

# Import and prepare the data ----------------------------------------------

# import the data
library(readxl)
dataDWL <- read_excel(here("data", "dataDWL.xlsx"), col_names=TRUE)
dataDWL

# inspect the data
glimpse(dataDWL)

table(dataDWL$Diet)

# convert 'Diet' to factor
dataDWL <- dataDWL %>%
  mutate(Diet = factor(Diet))

#NOTE: if we wnat to recode-rename the categories
#dataDWL <- dataDWL %>% 
  #mutate(Diet = factor(Diet, levels=c("A","B","C","D"), labels=c("group1", "group2", "group3", "group4")))



# inspect the data type again
glimpse(dataDWL)


# Check the assumptions ---------------------------------------------------

# normality graphically
ggplot(dataDWL, aes(x = Diet, y = WeightLoss, fill = Diet)) + 
  geom_boxplot(width=0.3) +
  theme_classic(base_size = 18) +
  theme(legend.position="none",
        axis.text = element_text(size = 14))



# summary statistics with dlookr (includes statistical moments)
dataDWL %>%
  group_by(Diet) %>%
  dlookr::describe(WeightLoss) %>%
  select(variable, Diet, n, mean, sd, p25, p50, p75, skewness, kurtosis) %>% 
  ungroup()


# Shapiro-Wilk test for normality
dataDWL %>%
  group_by(Diet) %>%
  shapiro_test(WeightLoss) %>%
  ungroup()



# Levene's test for homoscedasticity
dataDWL %>%
  levene_test(WeightLoss ~ Diet)





# Run the test ------------------------------------------------------------

# ANOVA

dataDWL %>%
  anova_test(WeightLoss ~ Diet, detailed = T)



# Post-hoc analysis (Tukey test)

dataDWL %>%
  tukey_hsd(WeightLoss ~ Diet)


#or pairwise comparisons using t-tests with Bonferroni correction
dataDWL %>%
  pairwise_t_test(WeightLoss ~ Diet, pool.sd = TRUE, p.adjust.method = "bonferroni")



#not assuming equal variance --------------------------------------------------

# Welch one-way ANOVA test (not assuming equal variance)

dataDWL %>%
  welch_anova_test(WeightLoss ~ Diet)


# Pairwise comparisons (Games-Howell)
dataDWL %>%
  games_howell_test(WeightLoss ~ Diet)






######################## Kruskal-Wallis ###########################

# Import and prepare the data ----------------------------------------------

library(readxl)
dataVO2 <- read_excel(here("data", "dataVO2.xlsx"), col_names=TRUE)

dataVO2


# inspect the data
glimpse(dataVO2)

table(dataVO2$sport)

# convert 'Diet' to factor
dataVO2 <- dataVO2 %>%
  mutate(sport = factor(sport))


# inspect the data type again
glimpse(dataVO2)





# Check the assumptions ---------------------------------------------------

# normality graphically
ggplot(dataVO2, aes(x = sport, y = VO2max, fill = sport)) + 
  geom_boxplot(width=0.3) +
  theme_classic(base_size = 18) +
  theme(legend.position="none",
        axis.text = element_text(size = 14))



# summary statistics with dlookr (includes statistical moments)
dataVO2 %>%
  group_by(sport) %>%
  dlookr::describe(VO2max) %>%
  select(variable, sport, n, mean, sd, p25, p50, p75, skewness, kurtosis) %>% 
  ungroup()




# Shapiro-Wilk test for normality
dataVO2 %>%
  group_by(sport) %>%
  shapiro_test(VO2max) %>%
  ungroup()




# Run the test ------------------------------------------------------------

# Kruskal-Wallis

dataVO2 %>%
  kruskal_test(VO2max ~ sport)



# Post-hoc analysis (Dunn's test)

dataVO2 %>%
  dunn_test(VO2max ~ sport, p.adjust.method = "bonferroni")



#or Pairwise comparisons using WMW tests with Bonferroni correction

dataVO2 %>%
  pairwise_wilcox_test(VO2max ~ sport, p.adjust.method = "bonferroni")

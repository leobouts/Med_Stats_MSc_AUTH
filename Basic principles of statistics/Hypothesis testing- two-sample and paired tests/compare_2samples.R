# we must download the following new packages ------------------------------------
# install.packages(c("rstatix", "dlookr", "skimr", "infer", "PupillometryR"))

# load the packages ----------------------------------------------------------
library(rstatix) # Pipe-Friendly Framework for Basic Statistical Tests
library(skimr) # Provides Summary statistics

library(here) # A Simpler Way to Find Your Files
library(tidyverse) # Easily Load the 'Tidyverse'
library(PupillometryR) # includes the function geom_flat_violin


######################## 2 independent samples ###########################


# Import and prepare the data ----------------------------------------------

# import the data
library(readxl)
depression <- read_excel(here("data", "depression.xlsx"), col_names=TRUE)
depression


# convert 'group' to factor
depression <- depression %>%
  mutate(group = factor(group))

# inspect the data type again
glimpse(depression)   

skim(depression) %>% 
  summary()



# Check the assumptions ---------------------------------------------------

# normality graphically
ggplot(depression, aes(x=group, y=HDRS)) + 
  geom_flat_violin(aes(fill = group), scale = "count") +
  geom_boxplot(width = 0.11, outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitter(width = 0.05), 
             size = 1.2, alpha = 0.6) +
  scale_fill_brewer(palette = "Spectral") +
  theme_classic(base_size = 14) +
  theme(legend.position="none",
        axis.text = element_text(size = 14))


# summary statistics with skim (includes histograms)
depression %>% 
  group_by(group) %>% 
  skim() 

# or summary statistics with dlookr (includes statistical moments)
depression %>%
  group_by(group) %>%
  dlookr::describe(HDRS) %>%
  select(variable, group, n, mean, sd, p25, p50, p75, skewness, kurtosis) %>% 
  ungroup()





# Shapiro-Wilk test for normality

depression %>%
  group_by(group) %>%
  shapiro_test(HDRS) %>%
  ungroup()



# Levene's test for equality of variances

depression %>%
  levene_test(HDRS ~ group)



# Run the test ------------------------------------------------------------

# t_test

depression %>% 
  t_test(HDRS ~ group, var.equal = T, detailed = T)


# Graphical Explanation -------------------------------------------------- 

# generate the null distribution with the theoretical t
null_distribution_2_sample_theoretical <- depression %>%
  infer::specify(HDRS ~ group) %>%
  infer::hypothesize(null = "independence") %>%
  infer::calculate(stat = "t", order = c("paroxetine", "placebo"))

# visualize the theoretical null distribution and test statistic!
null_distribution_2_sample_theoretical %>%
  infer::visualize(method = "theoretical") + 
  infer::shade_p_value(qt(0.025, 74), direction = "two-sided", color= "transparent", fill = "red") +
  infer::shade_p_value(-1.42, direction = "left", color = "black")

# find the shaded area
shade_area <- pt(-1.42, df = 74)
shade_area

# find the p-value for a two-tailed test
p <- 2*shade_area
p




# Alternative: Run the Wilcoxon-Mann-Whitney (WMW) test------------
depression %>% 
  wilcox_test(HDRS ~ group)







######################## 2 dependent samples ###########################


# Import and prepare the data ----------------------------------------------

# import the data
library(readxl)
weight <- read_excel(here("data", "weight.xlsx"), col_names=TRUE)
weight



# calculate the differences

weight <- weight %>%
  mutate(dif_weight = birth_weight - discharge_weight)


# check the data again
glimpse(weight)




# Check the assumptions ---------------------------------------------------


# check the distribution of the differences

weight %>%
  ggplot(aes(x = dif_weight)) +
  geom_density(fill = "#76B7B2", color="black", alpha = 0.2) +
  labs(x = "Weight difference",
       title = "Density plot of the weight differences (n = 25)") +
  theme_minimal()



# summary statistics for the differences

weight %>%
  dlookr::describe(dif_weight, birth_weight, discharge_weight) %>%
  select(variable, n, mean, sd, p25, p50, p75, skewness, kurtosis)



# Shapiro-Wilk test for normality

weight %>%
  shapiro_test(dif_weight)




# Run the test ------------------------------------------------------------

# t_test

weight %>% 
  t_test(dif_weight ~ 1, detailed = T)


# or t.test from base R
t.test(weight$birth_weight, weight$discharge_weight, paired=T)



# Alternative: Wilcoxon Signed-Rank test
weight %>% 
  wilcox_test(dif_weight ~ 1, detailed = T)


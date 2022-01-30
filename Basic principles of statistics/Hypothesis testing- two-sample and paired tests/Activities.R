# load the packages ----------------------------------------------------------
library(rstatix) # Pipe-Friendly Framework for Basic Statistical Tests
library(skimr) # Provides Summary statistics

library(here) # A Simpler Way to Find Your Files
library(tidyverse) # Easily Load the 'Tidyverse'
library(PupillometryR)

# import the data
library(readxl)
clinic_program <- read_excel("Desktop/Auth Medical Statistics Msc Material/Basic principles of statistics/Hypothesis testing- two-sample and paired tests/clinic_program.xlsx")
View(clinic_program)

clinic_program <- clinic_program %>% 
                  mutate(difference_weight = weight_after - weight_before)

clinic_program %>%
  ggplot(aes(x = difference_weight)) +
  geom_density(fill = "#76B7B2", color="black", alpha = 0.2) +
  geom_vline(aes(xintercept=mean(difference_weight)),color="blue", linetype="dashed", size=1.4) +
  geom_vline(aes(xintercept=median(difference_weight)),
             color="red", linetype="dashed", size=1.2) +
  labs(x = "Weight difference",
       title = "Density plot of the weight differences (n = 30)") +
  theme_minimal() +
  theme(plot.title.position = "plot")

clinic_program %>%
  dlookr::describe(difference_weight, weight_before, weight_after) %>%
  select(variable,  n, mean, sd, p25, p50, p75, skewness, kurtosis)

clinic_program %>%
  shapiro_test(difference_weight)

clinic_program %>%
  t_test(difference_weight ~ 1, detailed = T)


############## activity 2 #################################

lungs <- read_excel("Desktop/Auth Medical Statistics Msc Material/Basic principles of statistics/Hypothesis testing- two-sample and paired tests/dataLC.xlsx")
View(lungs)

lungs <- lungs %>% 
  filter(Age>17)
  
lungs <- lungs %>% mutate(Gender=factor(Gender))
  
ggplot(lungs, aes(x=Gender, y=LungCap)) +
  geom_flat_violin(aes(fill = Gender), scale = "count") +
  geom_boxplot(width = 0.11, outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitter(width = 0.05),
             size = 1.2, alpha = 0.6) +
  scale_fill_brewer(palette = "Spectral") +
  theme_classic(base_size = 14) +
  theme(legend.position="none",
        axis.text = element_text(size = 14))

shapiroLungs <- lungs %>% 
         filter(Age>17) %>%
         group_by(Gender) %>% 
         rstatix::shapiro_test(LungCap) %>%
         ungroup()

shapiroLungs


lungs_summary <- lungs %>%
  group_by(Gender) %>%
  dplyr::summarise(
    n = n(),
    min = min(LungCap, na.rm = TRUE),
    q1 = quantile(LungCap, 0.25, na.rm = TRUE),
    median = quantile(LungCap, 0.5, na.rm = TRUE),
    q3 = quantile(LungCap, 0.75, na.rm = TRUE),
    max = max(LungCap, na.rm = TRUE),
    mean = mean(LungCap, na.rm = TRUE),
    sd = sd(LungCap, na.rm = TRUE),
    skewness = EnvStats::skewness(LungCap, na.rm = TRUE),
    kurtosis= EnvStats::kurtosis(LungCap, na.rm = TRUE)
  ) %>%
  ungroup()

lungs_summary

lungs %>%
  levene_test(LungCap ~ Gender)

lungs %>%
  t_test(LungCap ~ Gender, var.equal = T, detailed = T)


############## quiz #################################

diverticulosis <- read_excel("Desktop/Auth Medical Statistics Msc Material/Basic principles of statistics/Hypothesis testing- two-sample and paired tests/diverticulosis.xlsx")
View(diverticulosis)

diverticulosis <- diverticulosis %>% mutate(treatment=factor(treatment))
diverticulosis

ggplot(diverticulosis, aes(x=treatment, y=transit.times)) +
  geom_flat_violin(aes(fill = treatment), scale = "count") +
  geom_boxplot(width = 0.11, outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitter(width = 0.05),
             size = 1.2, alpha = 0.6) +
  scale_fill_brewer(palette = "Spectral") +
  theme_classic(base_size = 14) +
  theme(legend.position="none",
        axis.text = element_text(size = 14))

diverticulosisShapiro <- diverticulosis %>% 
  group_by(treatment) %>% 
  rstatix::shapiro_test(transit.times) %>%
  ungroup()

diverticulosisShapiro

diverticulosis %>%
  group_by(treatment) %>%
  dlookr::describe(transit.times) %>%
  select(variable,  n, mean, sd, p25, p50, p75, skewness, kurtosis)

diverticulosis %>%
  t_test(transit.times ~ treatment, detailed = T)

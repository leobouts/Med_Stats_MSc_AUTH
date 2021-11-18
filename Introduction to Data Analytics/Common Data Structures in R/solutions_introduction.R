### data_structures activities  ####


# Activity 1

x <- c(5.4, 6.2, 7.1, 7.5, 4.8)

## solution
x[2:4]
x[-c(1, 5)]
x[c(-1, -5)]
x[c(FALSE, TRUE, TRUE,TRUE,FALSE)]




# Activity 2

stokes_position <- c("U", "D", "D", "D", "U", "U", "U", "U", "D", "U")

stokes_position <- factor(stokes_position, 
                          levels = c("D", "U"),
                          labels = c("down", "up"),
                          ordered = TRUE)
stokes_position




# Activity 3

# Create a matrix 3x5 containing all consecutive integer numbers between 16
# and 30 by columns.

matrix(16:30, nrow = 3)






# Activity 4 

# find the variance using matrix notation
y <- c(1, 5, 7, 4, 3, 8, 2, 1, 4, 7)


# solution

# find the mean using matrix notation
n <- length(y)
U <- matrix(1, n, 1)
U
V <- matrix(y, n, 1)
V
mean_y <- t(U) %*% V/n
mean_y

# which is equal to 
mean(y)


# find the sampling variance
r <- y - mean_y[1, 1]  # use [] to select the value
R <- matrix(r, n, 1)

t(R) %*% R/(n-1)


# which is equal to 
var(y)





# Activity 5 

library(lobstr)

a <- runif(1e6)
obj_size(a)



b <- list(a, a)
obj_size(b)


obj_size(a, b)

ref(a, b)




b[[1]][[1]] <- 10
obj_size(b)


obj_size(a, b)


b[[2]][[1]] <- 10

obj_size(b)
obj_size(a, b)
ref(a, b)




### data_explore activities  ####

# Activity 1

library(tidyverse)
library(here)

arrhythmia <- read_csv(here("data", "arrhythmia.csv"))

skimr::skim(arrhythmia)


# Convert the variable sex into a factor variable
arrhythmia <- arrhythmia %>%
  mutate(sex = recode_factor(sex, `0` = "male", `1` = "female"))

# or

#arrhythmia <- arrhythmia %>%
 # mutate(sex = factor(sex, levels = c(0, 1), labels = c("male", "female")))

glimpse(arrhythmia)



# Calculate the Body mass index (BMI)
arrhythmia <- arrhythmia %>%
  mutate(bmi = round(weight / (height / 100)^ 2, digits=1))

arrhythmia



# Create BMI categories only for the adults (age >=18)
arrhythmia_adults <- arrhythmia %>%
  filter(age >= 18) %>%
  mutate(bmi_cat=cut(bmi, breaks=c(-Inf, 18.5, 25.0, 30.0, Inf),
                     labels=c("underweight","normal","overweight", "obese"))
  )

arrhythmia_adults




# Activity 2

overweight_obese <- arrhythmia_adults %>%
  filter(bmi_cat == "overweight" | bmi_cat == "obese")

overweight_obese

# or
#overweight_obese2 <- arrythmia_adults %>%
  #filter(bmi_cat %in% c("overweight", "obese"))


# overweight or obese people who have heart rate greater than or equal to 85.
overweight_obese3 <- overweight_obese %>%
  filter(heart_rate >= 85)

overweight_obese3



# calculate the mean and standard deviation of the QRS variable

summary_QRS <- arrhythmia_adults %>%
  summarize(mean_QRS = mean(QRS, na.rm = TRUE),
            sd_QRS = sd(QRS, na.rm = TRUE))
summary_QRS



# calculate the mean and sd of QRS for the participants in each BMI category

summary_QRS_bmi <- arrhythmia_adults %>%
  group_by(bmi_cat) %>%
  summarize(mean_QRS = mean(QRS, na.rm = TRUE),
            sd_QRS = sd(QRS, na.rm = TRUE)) %>%
  ungroup() # ungrouping variable is a good habit to prevent errors

summary_QRS_bmi





### data_visual activities  ####

# Activity 1
library(tidyverse)
library(here)

library(ggforce)

covid_data <- read_csv(here("data", "covid_data.csv"))


dat <- covid_data %>%
  filter(date == "2021-06-12", population > 1000000) %>%
  mutate(cases_per_100k = confirmed / population * 100000,
         tests_per_capita = total_tests / population)


dat %>%
  filter(country %in% c("United Kingdom", "France", "Germany")) %>%
  ggplot(aes(x = country, y = life_expectancy)) +
  geom_col() +
  facet_zoom(ylim = c(80, 83))




# Activity 2

dat %>%
  filter(region == "Europe & Central Asia") %>%
  ggplot(aes(y = fct_reorder(country, life_expectancy),
             x = life_expectancy,
             color = pop_density)) +
  geom_point(shape = 15, size = 3.5) +
  scale_color_distiller(palette = "Greens", direction = 1) +
  labs(x="Life Expectancy", 
       y = "Country",
       title="Life Expectancy in Europe & Central Asia Countries") +
  theme_bw()





# Activity 3

library(ggpubr)
#devtools::install_github("rensa/ggflags")
library(ggflags)
library(countrycode)


dat20 <- dat %>% 
  mutate(vac_doses_per_capita = round(total_vaccinations/population, 
                                      digits = 2),
         code = countrycode(iso3c, "iso3c", "iso2c"), # we need iso2c for geom_flag()
         code = tolower(code)) %>% 
  arrange(desc(vac_doses_per_capita)) %>% 
  top_n(20, vac_doses_per_capita)



ggplot(dat20, aes(x = vac_doses_per_capita, y = reorder(country, vac_doses_per_capita))) +
  geom_col(fill = "#A9B0DE", width = 0.70) +
  geom_flag(aes(x = 0, country = code), size = 11.0) +
  geom_text(aes(label = vac_doses_per_capita), size= 6, hjust =  -0.2) +
  geom_text(aes(x = 0.02, label = country), size= 6, vjust = +0.25, hjust = -0.15) + 
  scale_x_continuous(expand = c(0.008, 0), limits=c(0,1.45), breaks = seq(0, 1.45, by = 0.1)) +
  labs(x="Vaccination doses per capita", y=" Country") +
  theme_pubclean(base_size = 26) +
  theme(panel.grid.major.x = element_line(linetype = "dotted", size = 1, color = "grey"),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(hjust = 1, face = "bold", margin = margin(t = 10, r = 0, b = 10, l = 0), size = 24),
        axis.title.y = element_text(face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0), size = 24),
        axis.text = element_text(color = "#464a62"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


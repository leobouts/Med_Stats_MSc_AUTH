#--------------------------------------------------------------------------------#
# DATA EXPLORATION AND WRANGLING
#--------------------------------------------------------------------------------#
# Author: Konstantinos I. Bougioukas
# Date: 2021-10-07





# Data  ----------------------------------------------------------------------

library(tidyverse)
library(lubridate)

#remotes::install_github("joachim-gassen/tidycovid19")
# https://github.com/joachim-gassen/tidycovid19
library(tidycovid19)


# We will work on Covid-19 data. The download_merged_data() from {tidycovid19}
# downloads all data sources and creates a merged country-day panel 



# get the data
covid_data <- download_merged_data(cached = TRUE)     # accumulated data



# We can see the definitions for each variable included in the covid_data in the link:
# https://github.com/joachim-gassen/tidycovid19


# inspect the data
glimpse(covid_data)



# Let's have a look at the types of variables and an overview of the data frame
skimr::skim(covid_data)


# There are 32 numeric variables, 6 variables of character type, 
# and two variables with dates (one of Date type and the other of POSIXct type).






# Subseting observations (rows) using filter() -------------------------------


# select all the data in which the date is "2021-06-11"

filter(covid_data, date == "2021-06-11")



# save the result using the assignment operator <-

june11 <- filter(covid_data, date == "2021-06-11")




# select all the data in which the date is "2021-06-11" 
# and the confirmed cases are larger than 5,000,000.

june11b <- filter(covid_data, date == "2021-06-11", confirmed > 5000000)
june11b




# select all the data in which the date is "2021-06-11" or "2021-06-12"

june11_12 <- filter(covid_data, date == "2021-06-11" | date == "2021-06-12")

# alternatively
june11_12b <- filter(covid_data, date %in% ymd("2021-06-11", "2021-06-12"))

# note: covid_data$date %in% ymd("2021-06-11", "2021-06-12") is a logical vector




# Reorder rows using arrange() ----------------------------------------------


# arrange the rows of the june11b table by the number of confirmed cases 

june11b_arrange <- arrange(june11b, confirmed) #  ascending order default

june11b_arrange


june11b_arrange_desc <- arrange(june11b, desc(confirmed))

june11b_arrange_desc




# Subseting variables (columns) using select() ------------------------------



# select only the `country`, `confirmed`, `deaths`, `recovered` variables

select(covid_data, country, region, date, confirmed, deaths, recovered)



# select variables by index (it is not suggested)

select(covid_data, 2, 32, 3:6)


# exclude the first variable `iso3c`

select(covid_data, -iso3c)


# select all character columns by using select_if()

select_if(covid_data, is.character)
  




# Subsetting columns and rows using pipe operator `%>%` and `dplyr` -------- 


covid_data2 <- covid_data %>% 
  filter(date == "2021-06-12") %>% 
  select(country, region, date, confirmed, deaths, recovered)

covid_data2





# Summaries of variables using summarise() and across() --------------------


# calculate the summary stats for the confirmed cases until `2021-06-12`

summary_confirmed <- covid_data %>% 
  filter(date == "2021-06-12") %>%
  dplyr::summarise(mean_confirmed = mean(confirmed, na.rm = TRUE),
                   sd_confirmed = sd(confirmed, na.rm = TRUE))
summary_confirmed





# utilize the across() function in the summarise() to apply stats 
# to multiple columns (confirmed` and deaths variables)


summary_2variables <- covid_data %>% 
  filter(date == "2021-06-12") %>%
  dplyr::summarise(across(
    .cols = c(confirmed, deaths), 
    .fns = list(
      N = ~n(),
      Min = min,
      Q1 = ~quantile(., 0.25, na.rm = TRUE),
      median = median,
      Q3 = ~quantile(., 0.75, na.rm = TRUE),
      Max = max,
      Mean = mean,
      Sd = sd,
      Skewness = EnvStats::skewness,
      Kurtosis= EnvStats::kurtosis),
    na.rm = TRUE,
    .names = "{col}_{fn}")
  )

summary_2variables






# Grouped summaries with group_by() and summarise() ---------------------

# we are interested in the number of confirmed cases per 100,000 inhabitants
# up to `2021-06-12` and the number of countries for each geographic region.


cases_by_region <- covid_data %>%
  filter(date == "2021-06-12") %>%
  group_by(region) %>%
  summarise(
    cases_per_100k = sum(confirmed, na.rm = TRUE) / sum(population, na.rm = TRUE)*100000,
    countries = n()
  ) %>%
  filter(region!= 'NA') %>%
  ungroup() # ungrouping variable is a good habit to prevent errors

cases_by_region






# Add new variables with mutate() ------------------------------------------


#calculate the cases per 100,000 inhabitants and tests per capita 
#up to `2021-06-12` for countries with more than 1,000,000 inhabitants:


dat <- covid_data %>%
  filter(date == "2021-06-12", population > 1000000) %>%
  mutate(cases_per_100k = confirmed / population * 100000,
         tests_per_capita = total_tests / population)
dat







# Count the unique values with count() --------------------------------------

#Let’s say that we also want to categorize the numeric variable `life_expectancy` 
#to countries with life expectancy 65 years or less and countries with more than 65 years. 
#We can use the `cut()` function inside the `mutate()`:

dat2 <- dat %>%
  mutate(life_expectancy_cat=cut(life_expectancy, 
                                 breaks=c(-Inf, 65, Inf),
                                 labels=c("65 yrs or less","more than 65 yrs")))

dat2



# Using `count()` is a convenient way to get a sense of the distribution 
# of values of one or more categorical variables in a dataset. 


count_dat <- dat2 %>%
  count(life_expectancy_cat)

count_dat

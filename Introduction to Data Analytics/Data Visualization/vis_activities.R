#Caution in filter function, dplyr filter is what we need
library(dplyr)
library(ggplot2)


dat <- dat %>%
  filter(date == "2021-06-12", population > 1000000) %>%
  mutate(cases_per_100k = confirmed / population * 100000,
         tests_per_capita = total_tests / population)


bar_plot <- filter(dat, country == "France" |
                     country == "Germany" | 
                     country == "United Kingdom") %>% 
                     ggplot(aes(x=country,y=life_expectancy, fill=country)) +
                     geom_bar(stat = "summary", width=0.4) +
                     theme(axis.text=element_text(size = 10),
                           axis.title = element_blank()) +
                     labs(title = "Life expectancy") +
                     coord_cartesian(ylim=c(80, 83))
bar_plot


europe_asia_vis <- filter(dat, region=="Europe & Central Asia" & life_expectancy != "NA") %>%
                   ggplot(aes(x=life_expectancy, y=reorder(country,life_expectancy))) +
                   geom_point(aes(color=pop_density), shape=15) +
                   xlim(70,86) + 
                   scale_colour_gradient(low = "yellow", high = "red", na.value = NA)+
                   labs(x = "Life expectancy (years)",
                        y = "Country")
europe_asia_vis


library(here)
library(devtools)
devtools:: install_github("jimjam-slam/ggflags", force = TRUE)
library(ggflags)
library(countrycode)
library(scales)

abbriviate_millions <- Vectorize(function(x) {
  paste(round((as.numeric(as.character(x))/1000000)), "M")
})

higher_prop_countries <- dat %>% 
                        mutate(proportion=round(total_vaccinations/population, digits = 2),
                               code = countrycode(iso3c, "iso3c", "iso2c"),
                               code = tolower(code),
                               population_factor = as.factor(population),
                               total_vaccinations_factor = as.factor(total_vaccinations),
                               x_labels = paste(round(population/1000000), "M"),
                               y_labels = paste(round(total_vaccinations/1000000), "M")) %>%
                        arrange(desc(proportion)) %>% 
                        slice(1:20)

higher_prop_countries_vis <- ggplot(higher_prop_countries, aes(x=total_vaccinations_factor, y=population_factor, country=code)) +
                             scale_x_discrete(labels = abbriviate_millions) +
                             scale_y_discrete(labels = abbriviate_millions) +
                             geom_flag() + 
                             scale_country() +
                             labs(x = "Total Vaccinations",
                                  y = "Population")
higher_prop_countries_vis

test1 <- higher_prop_countries %>% mutate(millios_vac = paste(round(higher_prop_countries$total_vaccinations/1000000, digits=0), "M"))

test <- (as.numeric(as.character(higher_prop_countries$population_factor)))
round(test[9]/1000000)


#-----------------------------------------------------------------------------#
# DATA VISUALIZATION
#-----------------------------------------------------------------------------#
# Author: Konstantinos I. Bougioukas
# Date: 2021-10-07




# Data  ----------------------------------------------------------------------

library(tidyverse)

# remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)

library(ggrepel)
library(ggsci)
library(gghighlight)
library(ggfx)
library(ggtext)
library(ggforce)
library(plotly)


# We will work on Covid-19 data. The download_merged_data() from {tidycovid19}
# downloads all data sources and creates a merged country-day panel 

# get the data

covid_data <- download_merged_data(cached = TRUE)


# We can see the definitions for each variable included in the covid_data in the link:
# https://github.com/joachim-gassen/tidycovid19


# inspect the data
glimpse(covid_data)



# Let's have a look at the types of variables and an overview of the data frame
skimr::skim(covid_data)


# There are 32 numeric variables, 6 variables of character type, 
# and two variables with dates (one of Date type and the other of POSIXct type).



# We will investigate graphically the association between a country’s wealth 
# and COVID-19 cases




# Data preparation for the plots ----------------------------------------------

dat <- covid_data %>%
  filter(date == "2021-06-12", population > 1000000) %>%
  mutate(cases_per_100k = confirmed / population * 100000,
         tests_per_capita = total_tests / population)





# A first basic plot ---------------------------------------------------------  

# Start with a default blank ggplot 
# x: Country gross domestic product per capita,in 2010 US-$
# y: cases_per_100K inhabitants

ggplot(data = dat, mapping = aes(x = gdp_capita, y = cases_per_100k))




# Add a point geometry
ggplot(data = dat, mapping = aes(x = gdp_capita, y = cases_per_100k)) + 
  geom_point()





# Aesthetics: add properties to geometries of the plot ------------------------


# add color aesthtics to point geometry with aes()

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) + 
  geom_point(aes(color = region))



# alternatively choose a point shape aesthetics 

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) + 
  geom_point(aes(shape = region))



# we can add a third numeric variable using the size aesthetics

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(aes(size = tests_per_capita))




# Add more geoms: text geom with geom_text_repel from {ggrepel} ----------------

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point() +
  geom_text_repel(aes(label = country)) 
  


  
  
  
# Scales: Change the default properties of plot ------------------------------


# Change the scale of the y-axis

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(aes(size = tests_per_capita, color = region)) +
  geom_text_repel(aes(label = country), 
                  min.segment.length = 0, 
                  seed = 42, 
                  box.padding = 0.1) +
  scale_y_continuous(trans = "log10") # or scale_y_log10()  




# Change the default colors
# here we use the palette from Nature Publishing Group in {ggsci}

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(aes(size = tests_per_capita, color = region)) +
  geom_text_repel(aes(label = country), 
                  min.segment.length = 0, seed = 42, 
                  box.padding = 0.1) +
  scale_y_continuous(trans = "log10") +
  scale_color_npg()  #palette from Nature Publishing Group




# Change the default shape points

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(aes(size = tests_per_capita, shape= region)) +
  geom_text_repel(aes(label = country), 
                  min.segment.length = 0, seed = 42, 
                  box.padding = 0.1) +
  scale_y_continuous(trans = "log10") +
  scale_shape_manual(values = c(4, 16, 2, 1, 0, 19, 8))






# Modify axis, legend, and plot labels adding labs() --------------------------

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(aes(size = tests_per_capita, color = region)) +
  geom_text_repel(aes(label = country), 
                  min.segment.length = 0, seed = 42, 
                  box.padding = 0.1) +
  scale_y_continuous(trans = "log10") +
  scale_color_npg() +
  labs(x = "GDP per capita ($)",
       y = "Cases per 100,000 inhabitants",
       color = "Region",
       size = "Proportion tested",
       title = "Confirmed cases per 100,000 inhabitants, GDP per capita, and COVID-19 testing rate by country", 
       subtitle = "June 12, 2021", 
       caption = "Source Data: Covid-19 related data from {tidycovid19} package",
       tag = 'A') 


# If we want to retain the order of legends
# See: https://github.com/tidyverse/ggplot2/issues/1353
# and https://www.edureka.co/community/52697/change-the-order-of-multiple-legends-in-ggplot
# guides(size = guide_legend(order = 1),color = guide_legend(order = 2))





# Modify theme components using theme() ----------------------------------------

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(aes(size = tests_per_capita, color = region)) +
  geom_text_repel(aes(label = country), 
                  min.segment.length = 0, seed = 42, 
                  box.padding = 0.1, color = "white", size = 5) +
  scale_y_continuous(trans = "log10") +
  scale_color_npg() +
  labs(x = "GDP per capita ($)",
       y = "Cases per 100,000 inhabitants",
       color = "Region",
       size = "Proportion tested",
       title = "Confirmed cases per 100,000 inhabitants, GDP per capita, and COVID-19 \ntesting rate by country", 
       subtitle = "June 12, 2021", 
       caption = "Source Data: Covid-19 related data from {tidycovid19} package") +
  theme(
    # background, panel and grid lines
    plot.background = element_rect(fill = "green"),
    panel.background = element_rect(fill = "grey30"),
    panel.grid.major = element_line(size = 0.1, color = "grey50"),
    panel.grid.minor = element_blank(),
    # title, subtitle and caption
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 13),
    # axis
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    # legend
    legend.background = element_blank(),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15),
    legend.key = element_rect(color = "white"),
    legend.position= "bottom",
    legend.box = 'vertical'
  )






# Add a minimal in-build theme: theme_minimal()

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(aes(size = tests_per_capita, color = region)) +
  geom_text_repel(aes(label = country), 
                  min.segment.length = 0, seed = 42, 
                  box.padding = 0.1, color = "black", size = 5) +
  scale_y_continuous(trans = "log10") +
  scale_color_npg() +
  labs(x = "GDP per capita ($)",
       y = "Cases per 100,000 inhabitants",
       color = "Region",
       size = "Proportion tested",
       title = "Confirmed cases per 100,000 inhabitants, GDP per capita, and COVID-19 testing rate by country", 
       subtitle = "June 12, 2021", 
       caption = "Source Data: Covid-19 related data from {tidycovid19} package") +
  theme_minimal()  # in-build theme


# NOTE: if we want to use both theme_minimal() and remove the gridlines, we
# need to make sure any theme adjustments come after theme_minimal()
# theme(panel.grid = element_blank())





# In-build theme and specificication of the theme elements ---------------------

# Suppose we want to change the angle of axis title to 45 degrees

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(aes(size = tests_per_capita, color = region)) +
  geom_text_repel(aes(label = country), 
                  min.segment.length = 0, seed = 42, 
                  box.padding = 0.1, color = "black", size = 5) +
  scale_y_continuous(trans = "log10") +
  scale_color_npg() +
  theme_minimal() +
  theme(axis.title = element_text(angle = 45))



# Why the change takes effect only in one axis?
# the `theme_minimal` sets the axis.title.y to 90 degrees

theme_minimal()


ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(aes(size = tests_per_capita, color = region)) +
  geom_text_repel(aes(label = country), 
                  min.segment.length = 0, seed = 42, 
                  box.padding = 0.1, color = "black", size = 5) +
  scale_y_continuous(trans = "log10") +
  scale_color_npg() +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.title = element_text(angle = 45),
        axis.title.y = element_text(angle = 45, vjust = 0.5))


# NOTE: when we are customizing theme elements the most specific wins





# Focus the attention on specific data or plot area -----------------------------



# Blur points

# Let's say we want to focus on countries from East Asia & Pacific region
# use the with_blur() function, which blurs the layers to which it is applied.
# additional styling can be applied via inline CSS (Cascading Style Sheets)



ggplot(dat, aes(x = gdp_capita, y = cases_per_100k, size = tests_per_capita, color = region)) +
  with_blur(
    geom_point(data = dat %>% filter(region != "East Asia & Pacific")),
    sigma = unit(0.95, 'mm') # you can choose the amount of blur 
  ) +
  geom_point(data=dat %>% filter(region == "East Asia & Pacific")) +
  geom_text_repel(data=dat %>% filter(region == "East Asia & Pacific"), aes(label = country), min.segment.length = 0, seed = 42, 
                  box.padding = 0.1, 
                  color = "black", size = 5) +
  scale_y_continuous(trans = "log10") +
  scale_color_npg() +
  labs(x = "GDP per capita ($)",
       y = "Cases per 100,000 inhabitants",
       color = "Region",
       size = "Proportion tested",
       title = "Confirmed cases per 100,000 inhabitants, GDP per capita, and <span style='font-size:18pt'>COVID-19</span> testing rate by country with emphasis in  <br> <span style='color:#E64B35FF'>East Asia & Pacific</span> countries", 
       subtitle = "May 20, 2021", 
       caption = "Source Data: Covid-19 related data from {tidycovid19} package") +
  theme_minimal() +
  theme(plot.title = element_markdown())






# Highlight data points

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k, 
                color = region)) +
  geom_point(aes(size = tests_per_capita)) +
  scale_y_continuous(trans = "log10") +
  scale_color_npg() +
  gghighlight(region == "East Asia & Pacific", keep_scales = TRUE, 
              use_direct_label = FALSE) +
  geom_text_repel(aes(label = country),
                  min.segment.length = 0, seed = 42, 
                  box.padding = 0.3, 
                  color = "black", size = 4) +
  labs(x = "GDP per capita ($)",
       y = "Cases per 100,000 inhabitants",
       color = "Region",
       size = "Proportion tested",
       title = "Confirmed cases per 100,000 inhabitants, GDP per capita, and <span style='font-size:18pt'>COVID-19</span> testing rate by country with emphasis in  <br> <span style='color:#E64B35FF'>East Asia & Pacific</span> countries", 
       subtitle = "May 20, 2021", 
       caption = "Source Data: Covid-19 related data from {tidycovid19} package") +
  theme_minimal() +
  theme(plot.title = element_markdown())





# Limit Axis Range (Zoom)

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(aes(size = tests_per_capita, color = region)) +
  geom_text(aes(label = country), size = 5) +
  scale_color_npg() +
  labs(x = "GDP per capita ($)",
       y = "Cases per 100,000 inhabitants",
       color = "Region",
       size = "Proportion tested") +
  coord_cartesian(ylim=c(3.8*10^3, 1.6*10^4)) +
  theme_minimal() +
  theme(legend.position= "bottom",
        legend.box = 'vertical')




# Highlight a certain area of the figure: facet zoom

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(aes(size = tests_per_capita, color = region)) +
  geom_text(aes(label = country), size = 5) +
  scale_y_log10() +
  scale_color_npg() +
  labs(x = "GDP per capita ($)",
       y = "Cases per 100,000 inhabitants",
       color = "Region",
       size = "Proportion tested"
  ) +
  facet_zoom(xlim = c(50000, 70000))






# Facet_wrap multiple-panel plots based on one variable ------------------------

#Facets can be placed next to each other, 
#wrapping with a certain number of columns or rows.

ggplot(dat, aes(x = gdp_capita, y = cases_per_100k)) +
  geom_point(size = 2.0) +
  geom_text_repel(aes(label = country),
                  min.segment.length = 0, seed = 42,
                  box.padding = 0.1, color = "black", size = 4) +
  scale_y_continuous(trans = "log10") +
  facet_wrap(~region, ncol=2)










# Integration with ggplot2: ggplotly() and plotly ------------------------------


# A ggplot object can be transformed into an interactive plot by calling 
# the function ggplotly().


# Example: gdp_capita~life_expectancy, region, country

g_plot <- ggplot(dat, aes(x = gdp_capita, y = life_expectancy)) +
  geom_point(aes(color = region)) +
  geom_text(aes(label = country)) +
  labs(title = "Life expectancy vs. GDP per capita",
       xaxis = list(
         title = "GDP per capita ($)"),
       yaxis = list(
         title = "Life expectancy (years)")
  )

ggplotly(g_plot)





# with plot_ly()

plotly3 <- dat %>%
  plot_ly(x = ~gdp_capita, y = ~life_expectancy, 
          color=~region,
          text = ~country) %>% 
  add_markers(symbol = I("circle-open"), colors = "Set1") %>% 
  layout(title = "Life expectancy vs. GDP per capita",
         xaxis = list(
           title = "GDP per capita ($)"),
         yaxis = list(
           title = "Life expectancy (years)")
  )

plotly3




# We can also create 3D interactive plots using the scatter3d type

plotly4 <- dat %>%
  plot_ly(x = ~gdp_capita, y = ~life_expectancy, 
          z= ~pub_health, color=~region, 
          text = ~ country, size = I(200)) %>%
  add_trace(type = "scatter3d", mode = "markers", 
            symbol = I("circle-open"), colors = "Set1") %>%
  layout(scene = list(
    xaxis = list(
      title = "x:GDP per capita ($)"),
    yaxis = list(
      title = "y:Life expectancy (years)"),
    zaxis = list(
      title = "z:No. health measures")
  ))

plotly4





### Graphs
library(ggplot2)
library(dplyr)
library(readxl)
transfusion <- read_excel("transfusion.xlsx", na = "NA")


trsf <- transfusion %>%
  mutate(bltp = factor(Blood_type, labels = c("A","AB","B", "O"))) %>%
  mutate(Transfusion = factor(Transfusion, labels = c("N", "Y"))) %>%
  mutate(gender = factor(sex, labels = c("M", "F")))


# Create a histogram and add a stat to calculate density
trsf %>%
  ggplot(aes(HB, stat(density))) +
  geom_histogram(bins = 15, colour = "#8833FF", fill = "orange") +
  geom_density(colour = "#8833FF")

# Group the histogram by gender
trsf %>%
  ggplot(aes(x = HB, stat(density), fill = gender)) + 
  geom_histogram(bins = 15, fill = "#56038a") + 
  geom_density(alpha = .35, colour = "#f5bd05")

# Create a barplot for one categorical variable
trsf %>%
  ggplot(aes(bltp)) +
  geom_bar()

# Create a barplot for two categorical variables
trsf %>%
  ggplot(aes(bltp)) +
  geom_bar(aes(fill = gender), position = "dodge")

# Create a boxplot for weight. To invert it, change the variable's position from x to y
trsf %>%
  ggplot(aes(weight)) +
  geom_boxplot(colour = "#bd3026", fill = "cadet blue")

# Create a scatterplot with two variables
trsf %>%
  ggplot(aes(Ht_b, Ht_a)) +
  geom_point()

# Create a scatterplot with four variables: two of them are plotted and the other two are weighted
trsf %>%
  ggplot(aes(Ht_b, Ht_a)) +
  geom_point(aes(alpha = HB, colour = Transfusion))

# Boxplots with formating (grouping by blood type and gender):
#   change the outlier colour and shape
#   change the colour palette using one of the available palettes in ggsci
#   use a dark background
trsf %>%
  ggplot(aes(x = bltp, y = HB, fill = gender)) +
  geom_boxplot(outlier.color = "#f5bd05", outlier.shape = 18) +
  ggsci::scale_fill_simpsons() +
  theme_dark()


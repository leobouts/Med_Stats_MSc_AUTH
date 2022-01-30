############################ Solutions: correlation ###############################
library(GGally) # Extension to 'ggplot2'
library(rstatix) # Pipe-Friendly Framework for Basic Statistical Tests
library(here) # A Simpler Way to Find Your Files
library(tidyverse) # Easily Install and Load the 'Tidyverse'

########################### Activity: dataLungCap   ###############################



library(readxl) # Read Excel Files
dataLungCap <- read_excel(here("data", "dataLungCap.xlsx"), col_names=TRUE)
dataLungCap


dataLungCap12 <- dataLungCap %>% 
  filter(Age > 12, Gender == "male")

dataLungCap12 %>% 
  ggscatmat(columns = c(1,3), corMethod = "pearson")  # alternative: "spearman", "kendall"


dataLungCap12 %>% 
  cor_test(vars = c(1,3), method="pearson")   # alternative: "spearman", "kendall"



#or

dataLungCap12 %>% 
  select(LungCap, Height) %>% 
  ggscatmat( corMethod = "pearson")



dataLungCap12 %>% 
  select(LungCap, Height) %>% 
  cor_test(method="pearson")  


library(GGally) # Extension to 'ggplot2'
library(rstatix) # Pipe-Friendly Framework for Basic Statistical Tests
library(here) # A Simpler Way to Find Your Files
library(tidyverse) # Easily Install and Load the 'Tidyverse'



# Import the data ----------------------------------------------

library(readxl)
BirthWeight <- read_excel(here("data", "BirthWeight.xlsx"), col_names=TRUE)

BirthWeight





# 2 continuous variables ----------------------------------------------


# correlation graph weight Vs height

BirthWeight %>%
  select(weight, height) %>%
  ggscatmat(corMethod = "pearson") # alternative: "spearman", "kendall"



# correlation test weight Vs height

BirthWeight %>%
  select(weight, height) %>%
  cor_test(method="pearson") # alternative: "spearman", "kendall"






# More than 2 continuous variables ----------------------------------------------

# Plot many pairs of continuous variables

BirthWeight %>%
  select(weight, height, headc) %>%
  ggscatmat(corMethod = "pearson")      # alternative: "spearman", "kendall"


# Plot a heatmap

BirthWeight %>%
  select(weight, height, headc) %>%
  ggcorr(method = c("pairwise", "pearson"), label = TRUE, label_round = 2)



# correlation tests for many pairs of variables (also by group)

BirthWeight %>%
  #group_by(gender) %>% 
  cor_test(vars =c("weight", "headc"),
           vars2 = c("height", "headc"), method = "pearson") %>%
  filter(var1 != var2) 

# %>% adjust_pvalue(method = "bonferroni")




# Plot many pairs of continuous variables by group

ggscatmat(BirthWeight, columns = 2:4, color = "gender", corMethod = "pearson")


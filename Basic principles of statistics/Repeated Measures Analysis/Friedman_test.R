#####loading the following R packages#####
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
library(tidyverse)
library(ggpubr)
library(rstatix)

#Data preparation
data <- data%>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(Subject, time)
head(data, 3)

#Summary statistics
data %>%
  group_by(time) %>%
  get_summary_stats(score, type = "common")


#Visualization 
ggboxplot(data, x = "time", y = "score", add = "jitter")


#Computation
res.fried <- data %>% friedman_test(score ~ time |Subject)
res.fried

#pairwise comparisons
pwc <- data %>%
  wilcox_test(score ~ time, paired = TRUE, p.adjust.method = "bonferroni")
pwc
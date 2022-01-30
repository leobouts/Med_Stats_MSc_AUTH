#####loading the following R packages#####
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
library(tidyverse)
library(ggpubr)
library(rstatix)

#import your database
str(data)

# Data preparation
# Wide format
# Gather columns t0, t30, t60 and t120 into long format
# Convert Subjects and time into factor variables

data <- data %>%
     gather(key = "time", value = "heart_rate", t0, t30, t60, t120) %>%
    convert_as_factor(Subjects, time)
 head(data)
 
#Summary statistics
 data %>%
   group_by(time) %>%
   get_summary_stats(heart_rate, type = "mean_sd")
 
 #Visualization 

 data$time <- factor(data$time , levels=c("t0", "t30", "t60", "t120"))
 bxp <- ggboxplot(data, x = "time", y = "heart_rate", add = "point")
 bxp
 
 
 #Check assumptions
 #Outliers         
 
 data %>%
   group_by(time) %>%
   identify_outliers(heart_rate)
 
 #Normality assumption
 data %>%
   group_by(time) %>%
   shapiro_test(heart_rate)
 
 
 #Computation
 res.aov <- anova_test(data = data, dv = heart_rate, wid = Subjects, within = time)
res.aov
get_anova_table(res.aov)

# pairwise comparisons
pwc <- data %>%
  pairwise_t_test(
    heart_rate ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

 
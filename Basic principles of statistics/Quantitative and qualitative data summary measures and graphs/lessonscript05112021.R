


library(dplyr)
library(readxl)
transfusion <- read_excel("transfusion.xlsx", na = "NA")
irs <- read_excel("intersalt.xlsx", na = "NA")


trsf <- transfusion %>%
  mutate(bltp = factor(Blood_type, labels = c("A","AB","B", "O"))) %>%
  mutate(Transfusion = factor(Transfusion, labels = c("N", "Y"))) %>%
  mutate(gender = factor(sex, labels = c("M", "F")))



#### Summary measures for qualitative data

table(trsf$gender)
prop.table(table(x = trsf$gender))
round(prop.table(table(x = trsf$gender)), digit=2)


#two way table
s.b <- table(trsf$gender, trsf$bltp)
s.b

prop.table(x = s.b, margin = NULL) #total sum as denominator
prop.table(x = s.b, margin = 1) #the sum of each row as denominator
prop.table(x = s.b, margin = 2) #the sum of each column as denominator

addmargins(s.b)

#k-way table
s.b.t <- table(trsf$gender, trsf$bltp, trsf$Transfusion)
ftable(s.b.t)




#### Summary measures for quantitative data 

mean(trsf$age, na.rm = T)

median(trsf$age)

sd(trsf$age)

var(trsf$age)

IQR(trsf$age)

quantile(trsf$age)

range(trsf$age)


# Multiple summary measures
trsf %>%
  summarise(mean(age), sd(age), median(age), IQR(age))


# Multiple summary measures, by groups
trsf %>%
  group_by(bltp) %>%
  summarise(mean(age), sd(age), median(age), IQR(age))


# Summary statistics for all numeric variables
trsf.dscr <- select(trsf, -ID, -sex, -Blood_type, -Status)
glimpse(trsf.dscr)

library(psych)

trsf.dscr %>%
  psych::describe(na.rm = T, ranges = T, IQR = T, quant = c(.25, .75), check = T)


# Summary statistics for all numeric variables, by blood type
trsf.dscr %>%
  psych::describeBy(group = .$bltp, na.rm = T, ranges = T, IQR = T, quant = c(.25, .75), check = T)


#Table for Transfusion
library(gtsummary)
library(gt)
# Summary statistics, by numeric variables and for all categorical variables 
trsf %>%
  select(age, HB, Ht_b, Ht_a, Transfusion, gender, bltp, INR) %>%
  tbl_summary(by = gender,
              statistic = list(INR ~ "{median} ({IQR})",
                               age ~ "{mean} ({sd})",
                               HB ~ "{mean} ({sd})",
                               Ht_a ~ "{mean} ({sd})",
                               Ht_b ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)")) %>%
  # Add a label indicating the statistic
  add_stat_label() %>%
   #add_p(test = list(contains("response") ~ "fisher.test",
   #                  all_continuous() ~ "t.test")) %>%
  
  # Make a gt table
  as_gt() %>%
  # Add title and subtitle
  tab_header(title = "Patient characteristics", subtitle = "Transfusion dataset") %>%
  # Change the background colour
  tab_options(table.background.color = "lightyellow")



#formating a table with gt
library(gt)
# Create a gt table and indicate the grouping variable
irs %>% 
  gt(groupname_col = "country") %>%
  # Exclude the ID
  cols_hide(columns = vars(ID)) %>%
  # Add a table tile and subtitle 
  tab_header(title = "Individual patient data", subtitle = "Salt excretion") %>%
  # Add a footer and indicate the columns that carry that footer
  tab_footnote(footnote = "All units are in SI", 
               locations = cells_column_labels(columns = vars(systolic.pressure, diastolic.pressure, sodium.excretion))) %>%
  # Create a group of variables with a title
  tab_spanner(label = "Arterial pressure",
              columns = vars(systolic.pressure, diastolic.pressure)) %>%
  # Change the background colour
  tab_options(table.background.color = "lightyellow")




#Normality test
trsf %>%
  rstatix::shapiro_test(age)

trsf %>%
  rstatix::shapiro_test(PT)

trsf %>%
  rstatix::shapiro_test(HB)

trsf %>%
  group_by(gender) %>%
  rstatix::shapiro_test(age)

trsf %>%
  group_by(bltp) %>%
  rstatix::shapiro_test(PT)


# Apply the shapiro.test function to every variable in the new dataset
trsf.num <- select(trsf, -ID, -sex, -Blood_type, -Status) %>% select_if(is.numeric)
lshap <- lapply(trsf.num, shapiro.test)
sapply(lshap, `[`, c("statistic","p.value"))




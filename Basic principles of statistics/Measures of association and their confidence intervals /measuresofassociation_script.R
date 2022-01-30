

# Import the data 
library(readxl)
transfusion <- read_excel("transfusion.xlsx")
str(transfusion)


# Re-code sex and Transfusion with appropriate labels
transfusion$Transfusion = factor(transfusion$Transfusion, labels = c("N", "Y"))
transfusion$sex = factor(transfusion$sex, labels = c("M", "F"))


addmargins(table(transfusion$sex, transfusion$Transfusion))


#transfusion$sex <- relevel(transfusion$sex, ref = 2)


#calculate the Risk Ratio
epitools::riskratio(transfusion$sex, transfusion$Transfusion)

#calculate the Odds Ratio
epitools::oddsratio(transfusion$sex, transfusion$Transfusion)



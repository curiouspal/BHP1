Meanwhile, it would be interesting qs to explore possible relationships between self assessment of health status (poor-excellent) 
and health insurance coverage  as well as  some barrier questions that reference health.  Similarly, possible relationships between 
not having (enough) child care and employment status.  And between expectation of future time in BHP housing and level of education, 
vocational credentials, primary language at home, and possibly other variables.


Health issues as barrier to SS in Housing SS  Q3_4
Health problems as barrier to SS in Employment Q8_1_2
Very hard to pay for health care for myself and family Q10_3
Do you receive Colorado Health Plan Plus support? Q25_3
Health insurance Q28
How would you rate your current health?  Q29
Where do you go for primary health care? Q30
Self assessment of health SS Q36_9

########################################################################3
setwd("~/Documents/BoulderHousingPartnersData")
all <- read.csv("Merged.csv") 

# Crosstabs of actual counts: Health insurance Q28 vs. How would you rate your current health? Q29
xtabs(formula = ~Q28+Q29, data=all)

# Crosstabs of percentage frequencies: Health insurance Q28 vs. How would you rate your current health? Q29
# Each row of numbers add up to 100 percent.
round(prop.table(xtabs(formula = ~Q28+Q29, data=all), 1)*100, 0)

# Crosstabs of percentage frequencies: Health insurance Q28 vs. How would you rate your current health? Q29
# Each column of numbers add up to 100 percent.
round(prop.table(xtabs(formula = ~Q28+Q29, data=all), 2)*100, 0)

#######################################################################
# Crosstabs of actual counts: Self assessment of health SS Q36_9 vs. How would you rate your current health? Q29
xtabs(formula = ~Q36_9+Q29, data=all)

# Crosstabs of percentage frequencies: Self assessment of health SS Q36_9 vs. How would you rate your current health? Q29
# Each row of numbers add up to 100 percent.
round(prop.table(xtabs(formula = ~Q36_9+Q29, data=all), 1)*100, 0)

# Crosstabs of percentage frequencies: Self assessment of health SS Q36_9 vs. How would you rate your current health? Q29
# Each column of numbers add up to 100 percent.
round(prop.table(xtabs(formula = ~Q36_9+Q29, data=all), 2)*100, 0)



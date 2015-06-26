library("ggplot2")
library("pander")
panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', Inf)
panderOptions('table.alignment.rownames', 'left')
panderOptions('table.style', 'multiline')
panderOptions('table.emphasize.rownames', FALSE)


########################################################################################################

summary(all)

summary(all$Mailing.Zip)

ggplot(all, aes(Age, fill=PH_or_S8)) + geom_bar(position='dodge', binwidth=2) +
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=18, angle = 0, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) 
x <- subset(all, Total.Annual.Income>0, select=c(Total.Annual.Income, PH_or_S8))
ggplot(x, aes(Total.Annual.Income, fill=PH_or_S8)) + geom_bar(binwidth=2000, position='dodge') +
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=18, angle = 0, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) 

all$Household.Member

summary(all$Status)
summary(all$Household.Member)


#######################################################################
# Crosstabs of actual counts: Status (Current, Notice, Past) vs. Future housing assistance expectation Q2?
pander(xtabs(formula = ~Q2+Status, data=all))

# Crosstabs of percentage frequencies: Status (Current, Notice, Past) vs. Future housing assistance expectation Q2?
# Each row of numbers add up to 100 percent.
pander(round(prop.table(xtabs(formula = ~Q2+Status, data=all), 1)*100, 0))

# Crosstabs of percentage frequencies: Status (Current, Notice, Past) vs. Future housing assistance expectation Q2?
# Each column of numbers add up to 100 percent.
pander(round(prop.table(xtabs(formula = ~Q2+Status, data=all), 2)*100, 0))

#######################################################################
ggplot(ph, aes(Age, fill=Surveyed)) + geom_bar(position='dodge', binwidth=2) +
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=18, angle = 0, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) 
x <- subset(ph, Total.Annual.Income>0, select=c(Total.Annual.Income, Surveyed))
ggplot(x, aes(Total.Annual.Income, fill=Surveyed)) + geom_bar(binwidth=2000, position='dodge') +
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=18, angle = 0, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) 
##############################################################################

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



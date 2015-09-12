## Code to change the names of variables to conform to the variable names in the previous dataset from 2014.

## The variable Program has to be changed to PH_or_S8.

path <- "/media/anirban/Ubuntu/Downloads/BHP Survey 2015 - production (Responses) - Form Responses 1.csv"
ph2015 <- read.csv(path, na.strings = "")

for(i in 1:length(ph2015))
  if(names(ph2015)[i] == "Program") names(ph2015)[i] <- "PH_or_S8"

## Change the names of the variables that start with "Q" and remove the part of the variable name that starts with "." so that they are similar to 2014 data.

for(i in 1:length(ph2015))
  if(substr(names(ph2015)[i], 1, 1) == 'Q') {
    b <-as.vector(strsplit(names(ph2015)[i], ".", fixed=TRUE))
    names(ph2015)[i]<-b[[1]][1]
  }

  
### Now check if all the TCodes were correctly entered.
for(i in 1:length(ph2015$TCode))
  if(ph2015$TCode[i] == ph2015$TCode.1[i]) ph2015$TCodeCorrect[i] <- 1 else ph2015$TCodeCorrect[i] <- 0
ph2015$TCodeCorrect <- as.factor(ph2015$TCodeCorrect)
summary(ph2015$TCodeCorrect)  

### Missing values need to be assigned correctly.
for(i in 1:length(ph2015))
  for(j in 1:length(ph2015$TCode))
    if(is.na(ph2015[j, i]) | (ph2015[j, i])=='No response') ph2015[j, i]<-NA

## Check for duplicate TCodes.
summary(ph2015$TCode)

## Remove "No response" as one of the factors in all responses. ### Need to correct this code.

#for(i in 1:length(names(ph2015)))
#  if(is.factor(ph2015[i]) & length(unique(as.character(ph2015[i])))<7)
#    ph2015[i] = as.ordered(ph2015[i], levels=sort(unique(as.character(ph2015[i])))[unique(as.character(ph2015[i])) != "No response"], labels = sort(unique(as.character(ph2015[i])))[unique(as.character(ph2015[i]))!="No response"])

d <- ph2015

#### PLOT TO SHOW WHICH QUESTIONS HAVE THE MOST MISSING DATA


library("ggplot2")

missing <- data.frame(ques=names(d), missingvalues=0)

for(i in 1:length(d)) {
  for(j in 1:length(d$TCode)) {
    if(is.na(d[j, i])) missing[i, 2] <- missing[i, 2] + 1
  }
}

ggplot(subset(missing, missingvalues/length(d$TCode)>0.50), aes(reorder(ques, missingvalues), missingvalues)) + geom_bar(stat="identity") + coord_flip()

ggplot(subset(missing, missingvalues/length(d$TCode)<0.1), aes(reorder(ques, missingvalues), missingvalues)) + geom_bar(stat="identity") + coord_flip()



### Add "-15" at the end of each variable name to denote that the data is from 2015 round of data collection.
for(i in 3:length(ph2015))
  names(ph2015)[i]<-paste(names(ph2015)[i], "-15")



## Now we can merge this file with the previous file "all" with TCode as the pivot.
ph-all <- merge(all, ph2015, by = "TCode", all=TRUE)


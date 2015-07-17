## Code to change the names of variables to conform to the variable names in the previous dataset from 2014.

## The variable Program has to be changed to PH_or_S8.

ph2015 <- read.csv("/media/anirban/Ubuntu/Downloads/BHP-trial.csv")

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
  if(ph2015$TCode[i] == ph2015$TCode_verify[i]) ph2015$TCodeCorrect[i] <- 1 else ph2015$TCodeCorrect[i] <- 0
summary(ph2015$TCodeCorrect)  

### Add "-15" at the end of each variable name to denote that the data is from 2015 round of data collection.
for(i in 3:length(ph2015))
  names(ph2015)[i]<-paste(names(ph2015)[i], "-15")

## Now we can merge this file with the previous file "all" with TCode as the pivot.
ph-all <- merge(all, ph2015, by = "TCode", all=TRUE)

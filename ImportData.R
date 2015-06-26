setwd("/media/anirban/a84ef5e0-59cf-454d-aeae-e112c9915900/home/anirban/Documents/BoulderHousingPartnersData")
library("foreign")

#### LOAD DATA ####

ph <- read.spss('PH.sav', to.data.frame=TRUE) ## Import PH data from SPSS files.
s8 <- read.spss('S8_NA_999.sav', to.data.frame=TRUE) ## Import S8 data from SPSS files.

# Now load the additional data that BHP provided from their administrative database.
phadditional <- read.csv("ph.csv")
s8additional <- read.csv("s8.csv")  

#### CLEANING THE DATA FILES ####
names(phadditional)[1] <- "location"
names(s8additional)[1] <- "location"
names(phadditional)[2] <- "TCode"
names(s8additional)[2] <- "TCode"
s8$TCode<-tolower(s8$TCode)
ph$TCode<-tolower(ph$TCode)


### Some of the TCodes from "s8" and "ph" don't match exactly with those from "s8addition" and "phadditional" respectively. They are as follows: 
# t0000151 - This exists in "ph" but not in "phadditional". But "phadditional" has a0000151. We therefore change the TCode in ph to one starting with "a".
# a0000696 - This exists in "s8" but not in "s8additional". But "s8additional" has this TCode but starting with "t" instead of "a". We therefore change the TCode in s8 to one starting with "t".
# a0001948 - This exists in "s8" but not in "s8additional". But "s8additional" has this TCode but starting with "t" instead of "a". We therefore change the TCode in s8 to one starting with "t".
# t0001972 - This exists in "s8" but not in "s8additional". Also "s8additional" does not contain the TCode "a0001972". So this is the most problematic. We therefore leave this as it is.

subset(s8, TCode=="a0000696")
s8[243, 1] <- "t0000696"

subset(ph, TCode=="t0000151")
subset(phadditional, TCode=="a0000151")
ph[38, 1] <- "a0000151"

subset(s8, TCode=="a0001948")
subset(s8additional, TCode=="t0001948")
s8[144, 1]<-"t0001948"

subset(s8, TCode=="t0001972")
subset(s8additional, TCode=="a0001972")
s8[145, 1]

# ph_loc$location <- as.factor(as.integer(ph_loc$location)) # Make "location" a categorical variable.


ph$Q1 <- factor(ph$Q1,
                levels = c("I owned my home", "I lived in private market rental housing (including student housing)", "I lived in subsidized housing (for example, Section 8, Public Housing)", "I lived with family and friends", "I was homeless"),
                labels = c("Owned my home", "Market rental", "Subsidized housing", "With family/friends", "Was homeless"))

s8$Q1 <- factor(s8$Q1,
                levels = c("I owned my home", "I lived in private market rental housing (including student housing)", "I lived in subsidized housing (for example, Section 8, Public Housing)", "I lived with family and friends", "I was homeless"),
                labels = c("Owned my home", "Market rental", "Subsidized housing", "With family/friends", "Was homeless"))

ph <- ph[1:85, ]      ## Rows beyond row 85 do not have any data. So we remove those rows.
s8 <- s8[-35, ]       ## Row 35 does not have any data. So we remove that row.


# Next we create a new variable PH_or_S8 for both the files. This will be helpful in identifying which household is S8 and which is PH after we have merged the two files. 
ph$PH_or_S8 <- "PH"
ph$PH_or_S8 <- as.factor(ph$PH_or_S8)

s8$PH_or_S8 <- "S8"
s8$PH_or_S8 <- as.factor(s8$PH_or_S8)

## There are duplicate TCode entries in the s8 data frame. We need to get rid of them.
s8<-s8[-198, ]  
s8<-s8[-197, ]
s8<-s8[-196, ]
s8<-s8[-195, ]
s8<-s8[-194, ]
s8<-s8[-193, ]
s8<-s8[-192, ]
s8<-s8[-191, ]
s8<-s8[-190, ]
s8<-s8[-189, ]
s8<-s8[-188, ]

# Now lets try to merge the two files. But merging does not work because the two sets of variables in the two files do not match. PH has variables "Q3_10" and "BarrierHousing" that S8 does not. S8 has variables "Q30_Other" and "SSLanguage" that PH does not have. 

summary(ph$Q3_10) # All 85 obs have the value NA.
summary(s8$Q3_10) # NULL

summary(ph$BarrierHousing) # All 85 observations have NA values.
summary(s8$BarrierHousing) # NULL

summary(s8$Q30_Other) # 255 obs have the value NA. The others have a value of 0.
summary(ph$Q30_Other) # NULL

summary(s8$SSLanguage) # This has useful data. It would be costly to delete this variable
summary(ph$SSLanguage) # NULL

# Lets create variables Q3_10 and BarrierHousing for s8 and variables Q30_Other and SSLanguage for ph and assigned a value of NA to these variables. That made the two files have the same set of variables.

s8$Q3_10 <- NA
ph$Q30_Other <- NA
s8$BarrierHousing <- NA
ph$SSLanguage <- NA


# Notice that the file "s8" has Q11, Q17, Q19, Q20 coded differently than file "ph". So lets fix those.
summary(s8$Q11)
s8$Q11 <- as.factor(as.integer(s8$Q11))
s8$Q11 <- factor(s8$Q11,
                 levels = c(1, 2, 3, 4, 5, 999),
                 labels = c("No, never", "Rarely", "Sometimes", "Most of the time", "Yes, all of the time", NA))
ph$Q11 <- ordered(ph$Q11,
                  levels = c("No, never", "Rarely", "Sometimes", "Most of the time", "Yes, all of the time"),
                  labels = c("No, never", "Rarely", "Sometimes", "Most of the time", "Yes, all of the time"))
s8$Q11 <- ordered(s8$Q11,
                  levels = c("No, never", "Rarely", "Sometimes", "Most of the time", "Yes, all of the time"),
                  labels = c("No, never", "Rarely", "Sometimes", "Most of the time", "Yes, all of the time"))

summary(s8$Q17)
s8$Q17 <- as.factor(as.integer(s8$Q17))
s8$Q17 <- factor(s8$Q17,
                 levels = c(1, 2, 3, 4, 5, 999),
                 labels = c("No, never", "Yes, 1 time", "Yes, 2 times", "Yes, 3 times", "Yes, 4 times or more", NA))
summary(s8$Q19)
s8$Q19 <- as.factor(as.integer(s8$Q19))
s8$Q19 <- factor(s8$Q19,
                 levels = c(1, 2, 999),
                 labels = c("Yes", "No", NA))
summary(s8$Q20)
s8$Q20 <- as.factor(as.integer(s8$Q20))
s8$Q20 <- factor(s8$Q20,
                 levels = c(1, 2, 3, 4, 999),
                 labels = c("Yes, I currently have sufficient child care", "No, I currently have no child care, but need it", "No, I currently have some child care, but need more", "N/A, I currently do not need child care", NA))

## In order to check if we have included all the observations from the Excel file in our analysis, lets try to see if there are observations in the Excel file that do not exist in the SPSS file for S8.
s8TCodesFromExcel <- read.csv("s8-listOfTCodes-fromExcel.csv")
summary(s8TCodesFromExcel)

s8_merged <- merge(s8, s8TCodesFromExcel, by = intersect(names(s8), names(s8TCodesFromExcel)), all=TRUE)
s8_merged$TCode <- as.factor(s8_merged$TCode)
summary(s8_merged$TCode)

# Check to see if the variables in S8 and PH are the same

setdiff(names(s8), intersect(names(ph), names(s8)))
setdiff(names(ph), intersect(names(ph), names(s8)))

# Now lets create the merged file. PH survey sent = 131 (received 85); S8 surveys sent = 273 (received 257).
all <- rbind(ph, s8)
all$PH_or_S8<-as.factor(all$PH_or_S8)

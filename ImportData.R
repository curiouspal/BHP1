
library("foreign")

#### LOAD DATA ####

ph <- read.spss('Data/PH.sav', to.data.frame=TRUE) ## Import PH data from SPSS files.
s8 <- read.spss('Data/S8_NA_999.sav', to.data.frame=TRUE) ## Import S8 data from SPSS files.

# Now load the additional data that BHP provided from their administrative database.
phadditional <- read.csv("Data/BHP-Admin_records/ph.csv")
s8additional <- read.csv("Data/BHP-Admin_records/s8.csv")  

#### CLEANING THE DATA FILES ####
s8$Surveyed <- "Yes"
ph$Surveyed <- "Yes"

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

# Now lets add the location codes to "ph" file from "ph_loc" file.
ph <- merge(phadditional, ph, by = "TCode", all=TRUE)
ph$location <- as.factor(ph$location)


s8 <- merge(s8additional, s8, by = "TCode", all=TRUE)
s8$location <- NA

# Next we create a new variable PH_or_S8 for both the files. This will be helpful in identifying which household is S8 and which is PH after we have merged the two files. 
ph$PH_or_S8 <- "PH"
s8$PH_or_S8 <- "S8"


# I notice that the file "s8" has Q11, Q17, Q19, Q20 coded differently than file "ph". So lets fix those.
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
s8TCodesFromExcel <- read.csv("Data/s8-listOfTCodes-fromExcel.csv")
summary(s8TCodesFromExcel)

s8_merged <- merge(s8, s8TCodesFromExcel, by = intersect(names(s8), names(s8TCodesFromExcel)), all=TRUE)
s8_merged$TCode <- as.factor(s8_merged$TCode)
summary(s8_merged$TCode)

# Check to see if the variables in S8 and PH are the same

setdiff(names(s8), intersect(names(ph), names(s8)))
setdiff(names(ph), intersect(names(ph), names(s8)))

# Now lets create the merged file. PH survey sent = 131 (received 85); S8 surveys sent = 273 (received 257).

ph$Q11 <- ordered(ph$Q11,
                  levels = c("No, never", "Rarely", "Sometimes", "Most of the time", "Yes, all of the time"),
                  labels = c("No, never", "Rarely", "Sometimes", "Most of the time", "Yes, all of the time"))
s8$Q11 <- ordered(s8$Q11,
                  levels = c("No, never", "Rarely", "Sometimes", "Most of the time", "Yes, all of the time"),
                  labels = c("No, never", "Rarely", "Sometimes", "Most of the time", "Yes, all of the time"))

# Check to see if the variables in S8 and PH are the same

setdiff(names(s8), intersect(names(ph), names(s8)))
setdiff(names(ph), intersect(names(ph), names(s8)))

names(ph)[names(ph)=="Rec.d"]<-"Recd."
names(ph)[names(ph)=="Scanned"]<-"Scanned."
names(ph)[names(ph)=="Sent.to.CU"]<-"Sent.to.CU."
names(ph)[names(ph)=="Non.Students.Age.18."]<-"Non.Students.Ages.18."

# Now lets create the merged file. PH survey sent = 131 (received 85); S8 surveys sent = 273 (received 257).
all <- rbind(ph, s8)
all$PH_or_S8<-as.factor(all$PH_or_S8)

all$Mailing.Zip<-as.factor(all$Mailing.Zip)
all$Member.Age...as.of.date<-as.numeric(all$Member.Age...as.of.date)
names(all)[names(ph)=="Member.Age...as.of.date"]<-"Age"
all$Total.Annual.Income<-as.numeric(all$Total.Annual.Income)
all$Member.Ethnicity<-as.factor(all$Member.Ethnicity)
all$Bedrooms<-as.factor(all$Bedrooms)
all$Family.Members<-as.factor(all$Family.Members)
all$Eligible..Members<-as.factor(all$Eligible..Members)
all$Dependants<-as.factor(all$Dependants)
all$HH.Members.under.age.13<-as.factor(all$HH.Members.under.age.13)
all$Student.Houshold.Members<-as.factor(all$Student.Houshold.Members)
all$Non.Students.Ages.18.<-as.factor(all$Non.Students.Ages.18.)
for(i in 1:length(all$Surveyed)) if(is.na(all$Surveyed[i])) all$Surveyed[i]<-"No"
all$Surveyed<-as.factor(all$Surveyed)
ph<-all[all$PH_or_S8=="PH", ]
s8<-all[all$PH_or_S8=="S8", ]
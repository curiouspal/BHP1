setwd("/home/anirban/Documents/BoulderHousingPartnersData")
load("all.RData")
ph <- subset(all, PH_or_S8=="PH")
s8 <- subset(all, PH_or_S8=="S8")

phQ8_1_8 <- subset(ph, Single.Parent.HH=="Yes" & Q8_1_8=="Yes")
phQ8_2_8 <- subset(ph, Single.Parent.HH=="Yes" & Q8_2_8=="Yes")
phQ10_5 <- subset(ph, Single.Parent.HH=="Yes" & (Q10_5=="Very Difficult" | Q10_5=="Somewhat Difficult"))
phQ20 <- subset(ph, Single.Parent.HH=="Yes" & (Q20=="No, I currently have no child care, but need it" | Q20=="No, I currently have some child care, but need more"))
phQ21_1 <- subset(ph, Single.Parent.HH=="Yes" & (Q21_1=="Very poor" | Q21_1=="Poor"))
phQ21_2 <- subset(ph, Single.Parent.HH=="Yes" & (Q21_2=="Very poor" | Q21_2=="Poor"))
phQ21_3 <- subset(ph, Single.Parent.HH=="Yes" & (Q21_3=="Very poor" | Q21_3=="Poor"))

phQ22_1 <- subset(ph, Single.Parent.HH=="Yes" & (Q22_1=="Definitely yes" | Q22_1=="Probably yes"))
phQ22_2 <- subset(ph, Single.Parent.HH=="Yes" & (Q22_2=="Definitely yes" | Q22_2=="Probably yes"))
phQ22_3 <- subset(ph, Single.Parent.HH=="Yes" & (Q22_3=="Definitely yes" | Q22_3=="Probably yes"))

phQ35_3 <- subset(ph, Single.Parent.HH=="Yes" & Q35_3=="Yes")

phQ36_6 <- subset(ph, Single.Parent.HH=="Yes" & (Q36_6=="Urgent situation, currently in crisis" | Q36_6=="Vulnerable, need support to move forward"))


ph_childcarebarrier <- merge(phQ8_1_8, phQ8_2_8, by = intersect(names(phQ8_1_8), names(phQ8_2_8)), all=TRUE)
ph_childcarebarrier <- merge(ph_childcarebarrier, phQ10_5, by = intersect(names(ph_childcarebarrier), names(phQ10_5)), all=TRUE)
ph_childcarebarrier <- merge(ph_childcarebarrier, phQ20, by = intersect(names(ph_childcarebarrier), names(phQ20)), all=TRUE)
ph_childcarebarrier <- merge(ph_childcarebarrier, phQ21_1, by = intersect(names(ph_childcarebarrier), names(phQ21_1)), all=TRUE)
ph_childcarebarrier <- merge(ph_childcarebarrier, phQ21_2, by = intersect(names(ph_childcarebarrier), names(phQ21_2)), all=TRUE)
ph_childcarebarrier <- merge(ph_childcarebarrier, phQ21_3, by = intersect(names(ph_childcarebarrier), names(phQ21_3)), all=TRUE)
ph_childcarebarrier <- merge(ph_childcarebarrier, phQ22_1, by = intersect(names(ph_childcarebarrier), names(phQ22_1)), all=TRUE)
ph_childcarebarrier <- merge(ph_childcarebarrier, phQ22_2, by = intersect(names(ph_childcarebarrier), names(phQ22_2)), all=TRUE)
ph_childcarebarrier <- merge(ph_childcarebarrier, phQ22_3, by = intersect(names(ph_childcarebarrier), names(phQ22_3)), all=TRUE)
ph_childcarebarrier <- merge(ph_childcarebarrier, phQ35_3, by = intersect(names(ph_childcarebarrier), names(phQ35_3)), all=TRUE)
ph_childcarebarrier <- merge(ph_childcarebarrier, phQ36_6, by = intersect(names(ph_childcarebarrier), names(phQ36_6)), all=TRUE)

ph_childcarebarrier <- data.frame(ph_childcarebarrier$TCode, ph_childcarebarrier$Single.Parent.HH, ph_childcarebarrier$location, ph_childcarebarrier$Q8_1_8, ph_childcarebarrier$Q8_2_8, ph_childcarebarrier$Q10_5, ph_childcarebarrier$Q20, ph_childcarebarrier$Q21_1, ph_childcarebarrier$Q21_2, ph_childcarebarrier$Q21_3, ph_childcarebarrier$Q22_1, ph_childcarebarrier$Q22_2, ph_childcarebarrier$Q22_3, ph_childcarebarrier$Q35_3, ph_childcarebarrier$Q36_6)


write.csv(ph_childcarebarrier, "/home/anirban/Documents/BoulderHousingPartnersData/ph_childcarebarrier.csv")

ph_childcarebarrier$score <- 0

for(i in length(ph_childcarebarrier$TCode)) {
  for(j in 3:14) {
    ph_childcarebarrier[i, 15] <- 0
    if(!is.na(ph_childcarebarrier[i, j])) {
      if(ph_childcarebarrier[i, j]=="Yes" | ph_childcarebarrier[i, j]=="Somewhat Difficult" | ph_childcarebarrier[i, j]=="Very Difficult" | ph_childcarebarrier[i, j]=="No, I currently have no child care, but need it" | ph_childcarebarrier[i, j]=="No, I currently have some child care, but need more" | ph_childcarebarrier[i, j]=="Poor" | ph_childcarebarrier[i, j]=="Very poor" | ph_childcarebarrier[i, j]=="Definitely yes" | ph_childcarebarrier[i, j]=="Probably yes" | ph_childcarebarrier[i, j]=="Vulnerable, need support to move forward") {
        ph_childcarebarrier$score[i] <- ph_childcarebarrier$score[i] + 1
        }
      }  
    }
  }


x <- c(t0000991, t0004383, t0006541, t0006159, t0008459, t0000874, t0000915, t0003764, t0004123, t0005295, t0000944, t0000963, t0000871, t0000943, t0000966, t0000987, t0004128, t0004214)

library(tidyverse)

d =read.csv("Data/Subjectbook/FK_NSF-Math-Exam-SB_to_S002.csv")
dx =read.csv("Data/Subjectbook/NSF-Math-Pilot.csv")
dd =read.csv("Data/Subjectbook/AffectiveMath.csv")


d$Day = "MathExam"

d1 = d

hridata = which(!is.na(d$HR.I))


for (i in hridata) {
  print(i)
  if (i != 1) {
    if (is.na(d$HR.I[i-1])) {
    d$HR.I[i-1] = d$HR.I[i]
    }
  }
  
  if (i != nrow(d)) {
    if (is.na(d$HR.I[i+1])) {
      d$HR.I[i+1] = d$HR.I[i]
    }
  }
}

plot(d1$HR.I, type = "l")
plot(d$HR.I, type = "l")

write.csv(d,"Data/Subjectbook/AffectiveMath.csv", row.names = F)

class(dd$VideoTime)
class(dd$Time)
dd$Time = as.integer(dd$Time)
colnames(dd)[3] = "TreatmentTime"
write.csv(dd,"Data/Subjectbook/AffectiveMath.csv", row.names = F)



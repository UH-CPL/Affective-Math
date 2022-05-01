library(tidyverse)

d =read.csv("Data/Subjectbook/AffectiveMathV2.csv")

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
 
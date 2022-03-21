library(tidyverse)
library(lubridate)

source("Scripts/@getQtype_order.R")

# No. of Participants
p = list.files("Pilot_CSV_WorkingCopy/")
px = p[4:8]

d = read_csv("Pilot_CSV_WorkingCopy/Physiology_v2.csv", col_types = list("HR" = col_double(),"EDA" = col_double()))
d[c("SAI.Score", "Question.Number", "Question.Name", "Question.Type", "Question.Order", "Total.Attempts", "Attempt", "Attempt.StartTime", "Attempt.StopTime", "Attempt.Start", "Attempt.Stop", "Beliefs/Perception", "Final.Thoughts", "Answer", "Attempt.Score", "Calculator")] = NA

colnames(d)[1] = "Participant.ID"
colnames(d)[3] = "Exam.Version"

d$Gender[which(d$Gender == "Female")] = "F"
d$Gender[which(d$Gender == "Male")] = "M"
d$Session[which(d$Session == "Instructions")] = "I"

dt = NULL
disp = 1

#p1 = "P006"
disparity = c(0, 11, 12, 12, 0, 16)
disparity = disparity[-1]
for (p1 in px) {
  print(p1)
  dispar = disparity[disp]
  f = list.files(paste0("Pilot_CSV_WorkingCopy/",p1,"/App"), 
                 pattern = "Exam.csv")
  ex = read.csv(paste0("Pilot_CSV_WorkingCopy/",p1,"/App/",f), header = F)
  colnames(ex) = c("Problem","Start","End","Answer","Check","Calculator",c(1:15))
  ex = add_row(ex, Problem = "99")
  ex = add_row(ex, Problem = "99")
  
  ans = which(ex$Answer == "Answer" )
  #ans = ans[3:length(ans)]

  dx = filter(d, Participant.ID == p1)
  sdate = as.Date(dx$Timestamp[1])
  
  flag = 1
  for (i in ans) {
    print(ex[i,])
    
    atm = 1
    s1 = ex$Start[i+1]
    s1 = paste0(sdate, " ", s1)
    s1 = parse_date_time(s1, "%Y-%m-%d  %H:%M:%S %p")
    s1 = s1 + dispar
    ss = s1
    
    e1 = ex$End[i+1]
    e1 = paste0(sdate, " ", e1)
    e1 = parse_date_time(e1, "%Y-%m-%d  %H:%M:%S %p")
    e1 = e1 + dispar
    ee = e1
    
    a1 = ex$Answer[i+1]
    ac1 = ex$Check[i+1]
    calc1 = ex$Calculator[i+1]
    qname = ex$Problem[i-2]

    
    if (ex$Problem[i+2] == 2) {
      try(silent = T)
      atm = 2
      s2 = ex$Start[i+2]
      s2 = paste0(sdate, " ", s2)
      s2 = parse_date_time(s2, "%Y-%m-%d  %H:%M:%S %p")
      s2 = s2 + dispar
      
      e2 = ex$End[i+2]
      e2 = paste0(sdate, " ", e2)
      e2 = parse_date_time(e2, "%Y-%m-%d  %H:%M:%S %p")
      e2 = e2 + dispar
      ee = e2
      
      a2 = ex$Answer[i+2]
      ac2 = ex$Check[i+2]
      calc2 = ex$Calculator[i+2]
    }
    if (ex$Problem[i+3] == 3) {
      try(silent = T)
      atm = 3
      s3 = ex$Start[i+3]
      s3 = paste0(sdate, " ", s3)
      s3 = parse_date_time(s3, "%Y-%m-%d  %H:%M:%S %p")
      s3 = s3 + dispar
      
      e3 = ex$End[i+3]
      e3 = paste0(sdate, " ", e3)
      e3 = parse_date_time(e3, "%Y-%m-%d  %H:%M:%S %p")
      e3 = e3 + dispar
      ee = e3
      
      a3 = ex$Answer[i+3]
      ac3 = ex$Check[i+3]
      calc3 = ex$Calculator[i+3]
    }

    bp = ex$Start[i-1]
    fti = which(ex$Problem[i+1:nrow(ex)] == "Final thoughts text")[1]
    ft = ex$Start[(i+fti)]
    qtime = which(dx$Timestamp >= ss & dx$Timestamp <= ee)
    
############################################
    if (flag == 1) {
      dx$Question.Number[qtime] = "Ex1"
      dx$Question.Name[qtime] = "Ex1"
      dx$Question.Order[qtime] = 1
      dx$Question.Type[qtime] = "Example"
      dx$`Beliefs/Perception`[qtime] = NA
      dx$Final.Thoughts[qtime] = NA
      dx$Total.Attempts[qtime] = atm
    }
    
    if (flag == 2) {
      dx$Question.Number[qtime] = "Ex2"
      dx$Question.Name[qtime] = "Ex2"
      dx$Question.Order[qtime] = 2
      dx$Question.Type[qtime] = "Example"
      dx$`Beliefs/Perception`[qtime] = NA
      dx$Final.Thoughts[qtime] = NA
      dx$Total.Attempts[qtime] = atm
    }
################################################################

    if (flag > 2) {
      dx$Question.Number[qtime] = flag -2
      dx$Question.Name[qtime] = qname
      dx$Question.Type[qtime] = getQ_type_order(dx$Exam.Version[1], 
                                                flag-2)[[1]][1]
      dx$Question.Order[qtime] = getQ_type_order(dx$Exam.Version[1], 
                                                 flag-2)[[2]][1]
      dx$`Beliefs/Perception`[qtime] = bp
      dx$Final.Thoughts[qtime] = ft
      dx$Total.Attempts[qtime] = atm
    }


    
      
    if (atm == 1) {
      qt1 = which(dx$Timestamp >= s1 & dx$Timestamp <= e1)
      dx$Attempt[qt1] = 1
      dx$Answer[qt1] = a1
      dx$Attempt.Score[qt1] = ac1
      dx$Calculator[qt1] = calc1
      dx$Attempt.StartTime[qt1] = (strsplit(as.character(s1), 
                                            split = " ")[[1]][2])
      dx$Attempt.StopTime[qt1] = (strsplit(as.character(e1), 
                                            split = " ")[[1]][2])
      dx$Attempt.Start[qt1] = head(dx$Time[qt1],1)
      dx$Attempt.Stop[qt1] = tail(dx$Time[qt1],1)
    }
    if (atm == 2) {
      qt1 = which(dx$Timestamp >= s1 & dx$Timestamp <= e1)
      dx$Attempt[qt1] = 1
      dx$Answer[qt1] = a1
      dx$Attempt.Score[qt1] = ac1
      dx$Calculator[qt1] = calc1
      dx$Attempt.StartTime[qt1] = (strsplit(as.character(s1), 
                                            split = " ")[[1]][2])
      dx$Attempt.StopTime[qt1] = (strsplit(as.character(e1), 
                                               split = " ")[[1]][2])
      dx$Attempt.Start[qt1] = head(dx$Time[qt1],1)
      dx$Attempt.Stop[qt1] = tail(dx$Time[qt1],1)
      
      qt2 = which(dx$Timestamp >= s2 & dx$Timestamp <= e2)
      dx$Attempt[qt2] = 2
      dx$Answer[qt2] = a2
      dx$Attempt.Score[qt2] = ac2
      dx$Calculator[qt2] = calc2
      dx$Attempt.StartTime[qt2] = (strsplit(as.character(s2), 
                                            split = " ")[[1]][2])
      dx$Attempt.StopTime[qt2] = (strsplit(as.character(e2), 
                                               split = " ")[[1]][2])
      dx$Attempt.Start[qt2] = head(dx$Time[qt2],1)
      dx$Attempt.Stop[qt2] = tail(dx$Time[qt2],1)
    }
    if (atm == 3) {
      qt1 = which(dx$Timestamp >= s1 & dx$Timestamp <= e1)
      dx$Attempt[qt1] = 1
      dx$Answer[qt1] = a1
      dx$Attempt.Score[qt1] = ac1
      dx$Calculator[qt1] = calc1
      dx$Attempt.StartTime[qt1] = (strsplit(as.character(s1), 
                                            split = " ")[[1]][2])
      dx$Attempt.StopTime[qt1] = (strsplit(as.character(e1), 
                                               split = " ")[[1]][2])
      dx$Attempt.Start[qt1] = head(dx$Time[qt1],1)
      dx$Attempt.Stop[qt1] = tail(dx$Time[qt1],1)
      
      qt2 = which(dx$Timestamp >= s2 & dx$Timestamp <= e2)
      dx$Attempt[qt2] = 2
      dx$Answer[qt2] = a2
      dx$Attempt.Score[qt2] = ac2
      dx$Calculator[qt2] = calc2
      dx$Attempt.StartTime[qt2] = (strsplit(as.character(s2), 
                                            split = " ")[[1]][2])
      dx$Attempt.StopTime[qt2] = (strsplit(as.character(e2), 
                                               split = " ")[[1]][2])
      dx$Attempt.Start[qt2] = head(dx$Time[qt2],1)
      dx$Attempt.Stop[qt2] = tail(dx$Time[qt2],1)
      
      qt3 = which(dx$Timestamp >= s3 & dx$Timestamp <= e3)
      dx$Attempt[qt3] = 3
      dx$Answer[qt3] = a3
      dx$Attempt.Score[qt3] = ac3
      dx$Calculator[qt3] = calc3
      dx$Attempt.StartTime[qt3] = (strsplit(as.character(s3), 
                                            split = " ")[[1]][2])
      dx$Attempt.StopTime[qt3] = (strsplit(as.character(e3), 
                                               split = " ")[[1]][2])
      dx$Attempt.Start[qt3] = head(dx$Time[qt3],1)
      dx$Attempt.Stop[qt3] = tail(dx$Time[qt3],1)
    }
    
    flag = flag+1
    
  }
  
  dt = bind_rows(dt, dx)
  disp = disp+1
}

dt$Attempt.Score[which(dt$Attempt.Score == "Yes")] = 1
dt$Attempt.Score[which(dt$Attempt.Score == "No")] = 0

write_csv(dt, file = "Pilot_CSV_WorkingCopy/Physiology+Exam_Synced_v2.csv")

library(tidyverse)
library(lubridate)

source("Scripts/@DownSampleTimeStamp.R")
source("Scripts/@RemoveNoise.R")

# No. of Participants
p = list.files("Data/RAW_noDAT/")



# Baseline
for (p1 in p) {
  print(p1)
  nfiles = length(list.files(paste0("Data/RAW_noDAT/",p1)))
  if (nfiles == 5) {
    dir.create(paste0("Pilot_CSV_WorkingCopy/",p1,"/Curation"))
  }
  #PP
  f = list.files(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Baseline"), 
                 pattern = "pp.csv")
  bpp_raw = read.csv(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Baseline/",f))
  bpp = bpp_raw
  bpp$Perspiration <- remove_noise(bpp$Perspiration)
  bpp = downsample_using_mean(bpp, "Perspiration")
  bpp$Perspiration = as.numeric(bpp$Perspiration)
  
  ggplot(bpp_raw, aes(x = Time, y = Perspiration)) + 
    geom_line() +
    geom_line(data = bpp, aes(x = Time, y = Perspiration), 
              size = 1.5, color = "green")+ 
    ggtitle(paste0(p1," Baseline Perspiration")) +
    xlab("Time [s]") + 
    ylab(expression(paste(Delta,degree," C"^2,)))
  ggsave(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation/",p1,"-PP-BL_v2.pdf"),
        width = 8, height = 4)
  

  
  #BR
  f = list.files(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Baseline"), 
                 pattern = "breath.csv")
  bbr_raw = read.csv(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Baseline/",f))
  bbr = downsample_using_mean(bbr_raw, "Breathing")
  bbr$Breathing = as.numeric(bbr$Breathing)
  
  bpp <-  bpp %>% 
    mutate(Breathing = bbr$Breathing, Session = "BL", Subject = p1,
           Gender = "Female", ExamVersion = "VAW") %>% 
    select("Subject", "Gender", "ExamVersion", "Timestamp", everything())
  
  ggplot(bpp, aes(x = Time, y = Breathing)) + geom_line() +
    ggtitle(paste0(p1," Baseline Breathing")) +
    xlab("Time [s]") + ylab("BPM")
  ggsave(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation/",p1,"-BR-BL_v2.pdf"),
         width = 8, height = 4)
  
  write_csv(bpp, paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation/",
                        p1,"-BL_v2.csv"))
}




# Exam
for (p1 in px) {
  print(p1)
  nfiles = length(list.files(paste0("Pilot_CSV_WorkingCopy/Exp/",p1)))
  if (nfiles == 5) {
    dir.create(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation"))
  }
  #PP
  f = list.files(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Exam"), 
                 pattern = "pp.csv")
  bpp_raw = read.csv(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Exam/",f))
  bpp = bpp_raw
  bpp$Perspiration <- remove_noise(bpp$Perspiration)
  bpp = downsample_using_mean(bpp, "Perspiration")
  bpp$Perspiration = as.numeric(bpp$Perspiration)
  
  ggplot(bpp_raw, aes(x = Time, y = Perspiration)) + 
    geom_line() +
    geom_line(data = bpp, aes(x = Time, y = Perspiration), 
              size = 1.5, color = "green")+ 
    ggtitle(paste0(p1," Exam Perspiration")) +
    xlab("Time [s]") + 
    ylab(expression(paste(Delta,degree," C"^2,)))
  ggsave(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation/",p1,"-PP-Exam_v2.pdf"),
         width = 8, height = 4)

  
  #BR
  f = list.files(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Exam"), 
                 pattern = "breath.csv")
  bbr_raw = read.csv(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Exam/",f))
  bbr = downsample_using_mean(bbr_raw, "Breathing")
  bbr$Breathing = as.numeric(bbr$Breathing)
  
  bpp <-  bpp %>% 
    mutate(Breathing = bbr$Breathing, Session = "Exam", Subject = p1,
           Gender = "Female", ExamVersion = "VAW") %>% 
    select("Subject", "Gender", "ExamVersion", "Session" , "Timestamp", everything())
  
  ggplot(bpp, aes(x = Time, y = Breathing)) + geom_line() +
    ggtitle(paste0(p1," Exam Breathing")) +
    xlab("Time [s]") + ylab("BPM")
  ggsave(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation/",p1,"-BR-Exam_v2.pdf"),
        width = 8, height = 4)

  write_csv(bpp, paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation/",p1,"-Exam_v2.csv"))
  
}


# One Timeline
glbltemp = as.tibble(as.data.frame(matrix(nrow = 0, ncol = 8)))
for (p1 in px) {
  print(p1)

  #BL
  f <- list.files(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation"), 
                  pattern = "BL_v2.csv")
  bl = read_csv(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation/",f))
  
  #Exam
  f <- list.files(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation"), 
                  pattern = "Exam_v2.csv")
  ex = read_csv(paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation/",f))
  
  
  be = tail(bl$Timestamp,1)
  es = head(ex$Timestamp,1)
  tdif = seq(be+1, es-1, by = 1)
  tdn = length(tdif)

  temp = as.data.frame(matrix(nrow = tdn, ncol = dim(bl)[2]))
  temp = as_tibble(temp)
  colnames(temp) = colnames(bl)
  temp$Timestamp = tdif
  temp$Subject = bl$Subject[1]
  temp$Gender = bl$Gender[1]
  temp$ExamVersion = bl$ExamVersion[1]
  temp$Time = seq(0, tdn-1, by = 1)
  temp$Session = "Instructions"
  
  pcsv = rbind(bl,temp,ex)
  
  #write_csv(pcsv, paste0("Pilot_CSV_WorkingCopy/Exp/",p1,"/Curation/",p1,"  -Physiology2.csv"))
  
  glbltemp = rbind(glbltemp, pcsv)
  
}
glbltemp <-  glbltemp[,c(1:3,8,4:7)]





#E4 HR
glbltemp["HR"] = NA
py = p[4:7]
for (p1 in py) {
  print(p1)
  
  #BL E4 HR
  f <- list.files(paste0("Pilot_CSV_WorkingCopy/",p1,"/Sensors/E4"), 
                  pattern = "HR.csv")
  bl = read_csv(paste0("Pilot_CSV_WorkingCopy/",p1,"/Sensors/E4/",f))
  s = as.numeric(colnames(bl))
  s = as.POSIXct(s, origin = "1970-01-01", tz = "UTC")
  bl <- bl[c(2:nrow(bl)),]
  colnames(bl) = "HR"
  s = seq(s, s+nrow(bl)-1, by = 1) - 3600*5
  #s = as.POSIXct(s, tx = "UTC")
  bl["Timestamp"] = s
  bl = bl[,c(2,1)]
  
  tempx = filter(glbltemp, Subject == p1)
  s = head(tempx$Timestamp, 1)
  e = tail(tempx$Timestamp, 1)
  temphr = bl$HR[which(bl$Timestamp >= s & bl$Timestamp <= e)]
  
  glbltemp$HR[which(glbltemp$Subject == p1)] = temphr
}





#E4 EDA
glbltemp["EDA"] = NA
py = p[4:7]
for (p1 in py) {
  print(p1)
  
  f <- list.files(paste0("Pilot_CSV_WorkingCopy/",p1,"/Sensors/E4"), 
                  pattern = "EDA.csv")
  bl = read_csv(paste0("Pilot_CSV_WorkingCopy/",p1,"/Sensors/E4/",f))
  s = as.numeric(colnames(bl))
  s = as.POSIXct(s, origin = "1970-01-01", tz = "UTC")
  bl <- bl[c(2:nrow(bl)),]
  colnames(bl) = "EDA"
  bl["calc"] = seq(0, nrow(bl)/4 - 0.25, by = 0.25)
  bl$calc = floor(bl$calc)
  bl <- aggregate(EDA~calc, FUN = mean, data = bl)
  s = seq(s, s+nrow(bl)-1, by = 1) - 3600*5
  bl["Timestamp"] = s
  bl = bl[,c(3,2)]
  
  tempx = filter(glbltemp, Subject == p1)
  s = head(tempx$Timestamp, 1)
  e = tail(tempx$Timestamp, 1)
  tempeda = bl$EDA[which(bl$Timestamp >= s & bl$Timestamp <= e)]
  
  glbltemp$EDA[which(glbltemp$Subject == p1)] = tempeda
}




#Iwatch HR
glbltemp["HR.I"] = NA
py = p[3:7]
for (p1 in py) {
  print(p1)
  
  #BL E4 HR
  f <- list.files(paste0("Pilot_CSV_WorkingCopy/",p1,"/Sensors/"), 
                  pattern = "HRI.csv")
  bl = read_csv(paste0("Pilot_CSV_WorkingCopy/",p1,"/Sensors/",f))
  colnames(bl) = c("Timestamp","HRI")
  bl$Timestamp = bl$Timestamp - 3600*5
  bls = head(bl$Timestamp,1)
  ble = tail(bl$Timestamp,1)
  
  s = seq(bls, ble, by = 1)
  
  hri = as.data.frame(matrix(nrow = length(s), ncol = 2))
  colnames(hri) = c("Timestamp","HR.I")
  hri$Timestamp = s
  hri = merge(hri, bl, by = "Timestamp", all.x = T)
  hri = hri[,c(1,3)]
  
  
  
  tempx = filter(glbltemp, Subject == p1)
  s = head(tempx$Timestamp, 1)
  e = tail(tempx$Timestamp, 1)
  temphri = hri$HRI[which(hri$Timestamp >= s & hri$Timestamp <= e)]
  
  glbltemp$HR.I[which(glbltemp$Subject == p1)] = temphri
}

glbltemp$Gender[which(glbltemp$Subject == "P003")] = "Male"
glbltemp$ExamVersion[which(glbltemp$Subject == "P002")] = "WVA"
glbltemp$ExamVersion[which(glbltemp$Subject == "P003")] = "VAW"
glbltemp$ExamVersion[which(glbltemp$Subject == "P004")] = "AWV"
glbltemp$ExamVersion[which(glbltemp$Subject == "P005")] = "WVA"



write_csv(glbltemp, file = paste0("Pilot_CSV_WorkingCopy/Physiology_v2.csv"))

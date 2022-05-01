library(tidyverse)

# Read first E4 HR file
hr1 = read.csv("C:/Users/cpladmin/Downloads/S003_E4_1/HR1.csv")
# Read Second E4 HR file
hr2 = read.csv("C:/Users/cpladmin/Downloads/S003_E4_2/HR2.csv")

#Read first E4 HR file
eda1 = read.csv("C:/Users/cpladmin/Downloads/S003_E4_1/EDA1.csv")
#Read first E4 HR file
eda2 = read.csv("C:/Users/cpladmin/Downloads/S003_E4_2/EDA2.csv")

#Get starting points for both HR 1 and 2
hr1s = as.numeric(strsplit(colnames(hr1), split = "X")[[1]][2])
hr2s = as.numeric(strsplit(colnames(hr2), split = "X")[[1]][2])

# Calculate second difference between two HR files
diff = hr2s - hr1s
# Calculate how many 0's or NA's to insert
tdiff = hr2s - (hr1s + nrow(hr1) - 1)

hr1d = hr1

hrna = data.frame(x = rep(NA, tdiff))
colnames(hrna) = colnames(hr1)

hr2d = data.frame(hr2[-1,])
colnames(hr2d) = colnames(hr1)


hr = rbind(hr1d, hrna, hr2d)  





#Get starting points for both EDA 1 and 2
eda1s = as.numeric(strsplit(colnames(eda1), split = "X")[[1]][2])
eda2s = as.numeric(strsplit(colnames(eda2), split = "X")[[1]][2])

# Calculate second difference between two HR files
diff = eda2s - eda1s
# Calculate how many 0's or NA's to insert
tdiff = eda2s - (eda1s + (nrow(eda1)-1)/4)

eda1d = eda1

edana = data.frame(x = rep(NA, tdiff*4))
colnames(edana) = colnames(eda1)

eda2d = data.frame(eda2[-1,])
colnames(eda2d) = colnames(eda1)


eda = rbind(eda1d, edana, eda2d)  
  
  
write.csv(hr, "Data/Curated/Meta/HR.csv", row.names = F)
write.csv(eda, "Data/Curated/Meta/EDA.csv", row.names = F)

library(tidyverse)

saiscore <- function(fileAddress){
  x = read.csv(fileAddress, header = F, nrows = 1)[-1]
  return(x)
}
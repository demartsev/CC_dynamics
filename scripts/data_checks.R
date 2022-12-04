library(seewave)
library(lubridate)
library(clock)

setwd("//10.126.19.90/EAS_shared/meerkat/working/processed/acoustic/total_synched_call_tables/") # folder where the processed data are located. Used to load the csv files containing the labels

to_calls <- read.csv("//10.126.19.90/EAS_shared/meerkat/working/processed/acoustic/DO_NOT_OVERWRITE/to_call.csv", header = F)
to_nocalls <- read.csv("//10.126.19.90/EAS_shared/meerkat/working/processed/acoustic/DO_NOT_OVERWRITE/to_nocall.csv", header = F)


files <- list.files()
for (i in 1:length(files)) {
  
  data <- read.csv(files[i])
  
  data[which(data$entryName %in% to_calls$V1) , "isCall"] <- 1
  data[which(data$entryName %in% to_nocalls$V1) , "isCall"] <- 0
  
  write.csv(data, files[i])}




setwd("V:/meerkat/working/processed/acoustic/extract_calls")
#op <- options(digits.secs=6)
call_data <- read.csv("labelfile.csv", sep = "\t")

call_data_1 <- call_data[which(call_data$isCall == 1) ,]


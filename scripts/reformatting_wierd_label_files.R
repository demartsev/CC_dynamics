#test change of script for GIT
setwd("V:/meerkat/working/processed/acoustic/HM2019/2_labels_verified/20190623")

file_name <- "HM_VHMF010_SOUNDFOC_20190623_LL_BA"
Labels <- read.csv(paste(file_name, ".csv", sep = ""), sep="\t", header = T, quote = "") #loading the file
Labels[, 1] <- gsub('"', '', Labels[, 1]) #getting rid of quotes generated if csv file are opened in excell
Labels[, 2] <- gsub('"', '', Labels[, 2]) #getting rid of quotes generated if csv file are opened in excell
Labels[, 3] <- gsub('"', '', Labels[, 3]) #getting rid of quotes generated if csv file are opened in excell
Labels[, 4] <- gsub('"', '', Labels[, 4]) #getting rid of quotes generated if csv file are opened in excell
Labels[, 5] <- gsub('"', '', Labels[, 5]) #getting rid of quotes generated if csv file are opened in excell
Labels <- Labels[ , 1:6]                  #leaving only the columns that we want
colnames(Labels) <- c("Name", "Start", "Duration", "TimeFormat", "Type", "Description")
Labels[,1] <- trimws(Labels[,1],"both")

write.table(Labels, paste(file_name, "_1.csv", sep = ""), row.names = F, sep = "\t", quote=FALSE)
library(gtools)
library(stringr)

####### Meerkat call-type recoding and re-arranging data #######

#uncomment to use on Vlad's machine
datadir <- "C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics"


### as it is now it takes the data from conflicts resolved file and recodes them first into 
### standardized form and later also adds a call_categories column
### all the original columns are keept intact

setwd(datadir)


  # load call data
  calls.all <- read.delim("labelfile_conflicts_resolved_2022-11-04.csv")
  
  # re code call type to lowercase
  calls.all$entryName <- tolower(calls.all$entryName)
  

  calls.all$callType <-  calls.all$entryName ##### at this point we have all  entries together and the call_type column is duplicated


### re code some of the calls due to between years inconsistency ###
#If we encounter more naming issues which are not resolved, the following lines could be modified

#here set the vectors for unifying modifier and call names
hybrid <- c("hybrid|hyb")
sequence <- c("seq|sq")
move <- c("move|mov")
agression <- c("aggress|agress|chat|growl|grunt|bark")
alarm <- c("alarm|alrm|alert|ala")
lost <- c("lost|loc|lc")


#versions of skip-on/skip-of labels
skip_s <- c("oor|skipon|skip on|outrange|pause|recoff|bir|skipof|skipoff|skip off|skip of|resume|recon" )
 

calls.all$skip_mark <- NA
calls.all[ which(grepl(skip_s, calls.all$entryName)), "skip_mark"] <- 1
 
# remove some miss labeled  non calls
calls.all[ which(calls.all$entryName == "x"),"isCall"] <- 0
calls.all[ which(calls.all$entryName == "eating"),"isCall"] <- 0
calls.all[ which(calls.all$entryName == "chew"),"isCall"] <- 0
calls.all[ which(calls.all$entryName == "digging"),"isCall"] <- 0
calls.all[ which(calls.all$entryName == "?"),"isCall"] <- 0
calls.all[ which(calls.all$entryName == "na"),"isCall"] <- 0
calls.all[ which(calls.all$entryName == "//"),"isCall"] <- 0
calls.all[ which(calls.all$callType == "skip"),"isCall"] <- 0
calls.all[ which(calls.all$callType == "syn"),"isCall"] <- 0
calls.all[ which(grepl("Mar", calls.all$entryName)), "isCall"] <- 0
calls.all[ which(grepl("bark", calls.all$entryName)), "isCall"] <- 0
calls.all[ which(grepl("bird", calls.all$entryName)), "isCall"] <- 0
calls.all[ which(grepl("#", calls.all$entryName)), "isCall"] <- 0
calls.all[ which(grepl("be", calls.all$entryName)), "isCall"] <- 0

#removing all modifiers from callType column
calls.all$callType <- gsub("[][!#$%?x()*,.;<=>@^_`|~.{}]", "", calls.all$callType)

#removing nf modifier
calls.all$callType <- gsub("nf","", calls.all$callType )

#removing spaces at the end or start of the string
calls.all$callType <- str_trim(calls.all$callType, side = "both")



#getting rid of all "not a call" entries exept from skip markers
calls.all <- calls.all[which(calls.all$isCall == 1 | calls.all$skip_mark == 1) , ]

#getting rid of all non_focal enteries
calls.all <- calls.all[which(calls.all$pred_focalType != "NF") , ]

calls.all$callType <- str_replace_all(calls.all$callType, hybrid, "hyb")
calls.all$callType <- str_replace_all(calls.all$callType, sequence, "sq")
calls.all$callType <- str_replace_all(calls.all$callType, move, "mo")
calls.all$callType <- str_replace_all(calls.all$callType, c("lead"), "ld")
calls.all$callType <- str_replace_all(calls.all$callType, c("social"), "soc")
calls.all$callType <- str_replace_all(calls.all$callType, c("ukn"), "unk")
calls.all$callType <- str_replace_all(calls.all$callType, agression, "agg")
calls.all$callType <- str_replace_all(calls.all$callType, alarm, "al")
calls.all$callType <- str_replace_all(calls.all$callType, lost, "lc")
calls.all[which(calls.all$callType == "s"), "callType"] <- "sn"  
calls.all[which(calls.all$callType == "c"), "callType"] <- "cc"  


calls.all$callType <- str_replace(calls.all$callType, fixed("s+"), "sn ") #renaming s as the first element
calls.all$callType <- str_replace(calls.all$callType, "\\+s$", " sn ")   #renaming s as the second element

### get hyb calls ###

calls.all$hyb <- NA
calls.all[grepl("fu|hyb", calls.all$callType), "hyb"] <- "fu"
calls.all[grepl("sq", calls.all$callType), "hyb"] <- "sq"


### get call type elements ####
call_types <- c("agg|al|cc|ld|mo|sn|soc|lc|unk") #core call types only
calls.all <- cbind(calls.all , str_extract_all(calls.all$callType, call_types, simplify = TRUE)) #getting the call type elements 
calls.all$`1` <- as.character(calls.all$`1`) #removing factors
calls.all$`2` <- as.character(calls.all$`2`) #removing factors

#collecting the call type elements in an alphabetic order
calls.all$final <- ifelse(calls.all$`1` < calls.all$`2`, paste(calls.all$`1`, calls.all$`2`), paste(calls.all$`2`, calls.all$`1`)) 
# keeping the original order for sequential calls
calls.all[which(calls.all$hyb == "sq") , "final"] <- paste(calls.all[which(calls.all$hyb == "sq") , "1"], calls.all[which(calls.all$hyb == "sq") , "2"])


#looking at the frequencies of the main call types as a self check
call_types <- c("cc", "soc", "al", "agg", "sn", "ld", "mo", "lc", "unk")


freq <- data.frame()
for (i in 1:length (call_types))
{
  freq[i,1] <- sum(str_count(rbind(calls.all$`1`, calls.all$`2`) , pattern = call_types[i]), na.rm = T)
  freq[i,2] <- call_types[i]
}
freq<- freq[order(freq$V1, decreasing = F),] ### if we decide to recode by the rarest category decreasing is to be set to TRUE


#get sample size plot for sanity check
par(mfrow=c(1,2))
bp <- barplot(freq$V1, names.arg = freq$V2, main = "Sample sizes per call type") 
text(bp, 0, freq$V1 ,cex=1,pos=3) 
#recode the call-types into main call categories. The order of recoding is frequency based 
# hybrid call_types are collapsed to the more frequent type. 

calls.all$type_group <- NA  

for ( i in 1:nrow(freq))
{ type <- freq$V2[i]

calls.all[which(grepl(type, calls.all$final)), "type_group"] <- type 
}


#sample sizes per call category

freq <- data.frame()
for (i in 1:length (call_types))
{
  freq[i,1] <- sum(str_count(calls.all$type_group , pattern = call_types[i]), na.rm = T)
  freq[i,2] <- call_types[i]
}
freq<- freq[order(freq$V1, decreasing = F),] 

bp <- barplot(freq$V1, names.arg = freq$V2, main = "Sample sizes per call category") #get sample size plot for sanity check
text(bp, 0, freq$V1 ,cex=1,pos=3) 

names(calls.all)[names(calls.all) == "final"] <- "stn_call_type"


## the columns containing call type info:
## hyb - indicator of fused or sequential calls
## stn_call_type - details the call type elements in each call
## type_group - only main calls, hybrids were collapsed into the parent type acording to overal call frequency 

#check sample sizes
table(calls.all$stn_call_type)
table(calls.all$type_group)

write.csv(calls.all, "foc_calls_resolved.csv")

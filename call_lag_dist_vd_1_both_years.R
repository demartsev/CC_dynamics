library(lubridate)
library(dplyr)
library(splitstackshape) 
library(varhandle)
library(ggplot2)
library(gridExtra)
library(markovchain) 
library(asnipe)
library(igraph)
library(CINNA)
library(reshape2)
library(gtools)
library(ggpubr)
library(stringr)

# Analyses of Meerkat call pair dynamics


### this script arranges all recorded meerkat calls into a reasonable filtered state.
### the call names are recoded into a standard format and collapsed into main categories
### the skip times are deleted across the whole day and non-focal and no-call enteries are filtered out
### call positions are updated from the movement files and scan positions are added with buffer times before and after
### call pair distances and time lags are calculated
### sequence initiations and sequence continuation cutoffs are set
### self replies and caller exchange replies are marked
### the data re-arrangement section end in L360
### after the data is fully arranged a series of plots are generated

# set working directory
setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics")

#directory with scans GPS data 
scans_dir <- ("V:/meerkat/working/processed/scans")
scan_buffer <- 15 #before and after buffer time
scan_files <- list.files(scans_dir, pattern = ".csv")

#making object to control for scans based GPS point addition
scan_addition <- data.frame(matrix(ncol = 8, nrow = 0), stringsAsFactors = F)
colnames(scan_addition) <- c("action", "scan_ID", "aidio_id", "Scan_t", "audio_t", "diff", "scan_date", "audio_date")


# parameters

#select deployment
HM2 <- c(20190712: 20190719)

#setup of analysis variables
dist_thresh <- 10 #distance to neighbor

sequence_lenght <- 3 #length of call sequence
seq_start = 10 #silence time before initiating a sequence
seq.reply = 5 #time frame for a response call

pair_sample_size <- 300 #minimal sample size for call pair analyses


#versions of skip-on/skip-of labels
skip_s <- c("oor|skipon|skip on|outrange|pause|recoff" )
skip_e <- c("bir|skipof|skipoff|skip off|skip of|resume|recon")



# load call data
calls.all <- read.csv("V:/meerkat/working/processed/acoustic/resolve_conflicts/2021-05-17/2021-05-17_ALL_conflicts_resolved.csv", sep = "\t")
# recode call type to lowercase for consistency between years
calls.all$callType <- tolower(calls.all$callType)

# load movement data
load("V:/meerkat/working/processed/movement/HM2019_COORDINATES_all_sessions_with_scans.RData")
load("V:/meerkat/working/processed/movement/L2019_COORDINATES_all_sessions_with_scans.RData")
load("V:/meerkat/working/processed/movement/HM2017_COORDINATES_all_sessions.RData")



individuals <- unique(calls.all$ind)

date_list <- as.character(unique(calls.all$date))

all_calls_seq <- data.frame() 


for (date in date_list) {
  
  #get current data
  calls <- calls.all[which(calls.all$date == date),]
  #select only the middle hour
  if (date %in% HM2)
  {calls <- calls[which(substr(calls$t0GPS_UTC,12,13)==13) , ]
  }
  #else{
  # calls <- calls[which(substr(calls$t0GPS_UTC,12,13)==7) , ]}
  # change times to numeric
  options(digits.secs=3)
  calls$t0.numeric <- as.numeric(as.POSIXlt(calls$t0GPS_UTC,tz="UTC", format = "%Y-%m-%d %H:%M:%OS"))  #NOTE: Looking at these times, they seem rounded to the second - this would be a big problem! LOOK INTO THIS -- FIX
  calls$tf.numeric <- as.numeric(as.POSIXlt(calls$tEndGPS_UTC,tz="UTC", format  = "%Y-%m-%d %H:%M:%OS")) 
  
  # remove some miss labeled  non calls
  calls[ which(calls$entryName == "x"),"isCall"] <- 0
  calls[ which(calls$entryName == "eating"),"isCall"] <- 0
  calls[ which(calls$entryName == "chew"),"isCall"] <- 0
  calls[ which(calls$entryName == "digging"),"isCall"] <- 0
  calls[ which(calls$entryName == "?"),"isCall"] <- 0
  calls[ which(calls$entryName == "na"),"isCall"] <- 0
  calls[ which(calls$entryName == "//"),"isCall"] <- 0
  calls[ which(calls$callType == "skip"),"isCall"] <- 0
  calls[ which(calls$callType == "syn"),"isCall"] <- 0
  calls[ which(calls$callType == "sync"),"isCall"] <- 0
  
  calls[ which(grepl("Mar", calls$entryName)), "isCall"] <- 0
  calls[ which(grepl("bark", calls$entryName)), "isCall"] <- 0
  calls[ which(grepl("bird", calls$entryName)), "isCall"] <- 0
  calls[ which(grepl("#", calls$entryName)), "isCall"] <- 0
  calls[ which(grepl("be", calls$entryName)), "isCall"] <- 0
  
  calls[ which(grepl("1:", calls$entryName)), "isCall"] <- 0
  
  calls$entryName <- droplevels(calls$entryName)
  
  calls <- calls[order(calls$t0.numeric ),]
  
  ##### removing skipped segments across recorders ######
  #find the times that need to be skipped across recorders
  skip_st <- which(grepl(skip_s,  calls$entryName) == T)
  skip_et <- which(grepl(skip_e,  calls$entryName) == T)
  
  #mark enteries to skip
  if (length(skip_st) < 1 || length(skip_et) < 1) {
    calls$skip <- NA
  }else{
    for (skip_time in skip_et) {
      skipof <- calls[skip_time, "t0.numeric"]
      if (any(skip_st < skip_time)){
        skipon <- calls[max(skip_st[which(skip_st < skip_time)]) , "t0.numeric"]
        calls[which(calls$t0.numeric >= skipon & calls$t0.numeric <= skipof), "skip"] <- "skip"
      }else{next}
    }
  }
  
  
  # remove nonfocal calls and non calls and skipped segments
  calls <- calls[which(calls$pred_focalType == "F" &  calls$isCall == 1 &  is.na(calls$skip)), ]
  calls <- calls[order(calls$t0.numeric ),]
  
  #### get updated position data ####
  #### this bit is getting the GPS coordinates from the main RDATA files
  
  for (c in 1:nrow(calls)) {
    
    
    year <- substr(calls$date[c], 1, 4)  #get year
    group <-
      ifelse(substr(calls$wavFileName[c] , 1, 1) == "L", "L", "HM")  #get group
    
    row <-
      match(as.character(calls$ind[c]), as.character(get(paste(
        group, year, "_indInfo", sep = ""
      ))[, 4]))
    
    column <-
      match(as.character(round(as.POSIXct(
        calls$t0GPS_UTC[c]
      ))), as.character(get(as.character(
        paste(group, year, "_timeLine", sep = "")
      ))))  
    
    calls$x_emitted[c] <- get(paste(group, year, "_allX", sep = ""))[row, column]
    calls$y_emitted[c] <- get(paste(group, year, "_allY", sep = ""))[row, column]
  } 
  
  
  ##### this get the positions from scans data , giving buffer time for each data point###
  
  #look for scan file on this date
  if (paste(group, "_SCAN_WITH_GPS_", calls$date[1], ".csv", sep = "") %in% scan_files){
    daily_scan <- NULL
    scanned_callers <- NULL
    daily_scan <- read.csv(paste(scans_dir,"/", group, "_SCAN_WITH_GPS_", calls$date[1], ".csv", sep = "")) #load scan file
    
    #find individuals that were scanned and recorded
    scanned_callers <- which(calls$ind %in% unique(daily_scan$MeerkatID) & is.na(calls$x_emitted))
    
    for (one_call in scanned_callers) {
      ind_select <- NULL
      point_select<- NULL
      #for each call select the caller scans
      ind_select <- daily_scan[which(as.character(daily_scan$MeerkatID) == as.character(calls$ind[one_call])) ,]
      
      #select the scan time that match the time of the call + buffer
      point_select <- ind_select[which(as.character(round(as.POSIXct(ind_select$GPS_time_UTC))) %in% 
                                         as.character(seq(round(as.POSIXct(calls$tMidGPS_UTC[one_call]))-scan_buffer,round(as.POSIXct(calls$tMidGPS_UTC[one_call]))+scan_buffer, by = 1))), ] 
      
      
      if(nrow(point_select) > 0) {
        
        
        scan_addition[nrow(scan_addition)+1,] <- c("GPS Point added", as.character(point_select$MeerkatID[1]), 
                                                   as.character(calls$ind[one_call]),  as.character(as.POSIXct(point_select$GPS_time_UTC[1])), 
                                                   as.character(as.POSIXct(calls$tMidGPS_UTC[one_call])), as.POSIXct(point_select$GPS_time_UTC[1]) - as.POSIXct(calls$tMidGPS_UTC[one_call]),
                                                   as.character(point_select$Time[1]) , calls$date[one_call])
        calls$x_emitted[one_call] <- point_select$x_UTM
        calls$y_emitted[one_call] <- point_select$y_UTM}
    }
  }
  
  
  
  #get caller distance
  calls$c_dist <- NA
  for(i in 2:nrow(calls)){
    dist <- sqrt((calls$x_emitted[i]-calls$x_emitted[i-1])^2 + (calls$y_emitted[i]-calls$y_emitted[i-1])^2 ) 
    if (!is.na(dist)) {calls$c_dist[i] <- dist}
  }
  
  #duplicate all calls data frame
  all_calls <- calls
  all_calls$lag <- NA
  for(i in 2:nrow(all_calls)){
    all_calls$lag[i] <- abs(all_calls$t0.numeric[i-1] - all_calls$t0.numeric[i])}
  
  
  #this bit is identifying call sequences needs to be revised 
  #all_calls$seq <- NA
  #seq_start_points <- which(all_calls$lag >= seq_start)+1
  #
  #tseq <- 1
  #for (r in seq_start_points){
  #  l <- r+1
  #  if (l >= nrow(all_calls)-1) {next}
  #  all_calls$seq[l-1] = tseq
  #  while(all_calls$lag[l-1] < 5) {
  #    all_calls$seq[l] <- tseq
  #    l <- l+1}
  #  
  #  tseq <- tseq + 1
  #}
  
  
  
  all_calls[which(all_calls$lag >= seq_start) , "trigger"] <- "trigger" #mark trigger calls after 5 sec of group silence
  
  
  responce_idx <- which(all_calls$trigger == "trigger")+1 #getting the index of potential response calls
  
  #print sample size for call sequences
  print(length(responce_idx))
  
  #mark all calls given as a response to trigger. within the designated time frame
  for (i in responce_idx){
    if (all_calls [i , "lag"] <= seq.reply & i < nrow(all_calls)) {all_calls[i , "trigger"] <- "responce"}  #getting response calls
  }
  
  
  #####recode some calls due to between year mismatches######
  
  all_calls$entryName<-  droplevels(all_calls$entryName)
  all_calls <- all_calls[c(names(all_calls)[-1], "entryName")]
  
  
  all_calls[which(all_calls$callType == "s"), "callType"] <- "sn"  
  all_calls[which(all_calls$callType == "social"), "callType"] <- "soc"  
  all_calls[which(all_calls$callType == "c"), "callType"] <- "cc"  
  all_calls[which(all_calls$callType == "aggress"), "callType"] <- "agg"  
  all_calls[which(grepl("chat",all_calls$callType)), "callType"] <- "agg" 
  all_calls[which(grepl("ala",all_calls$callType)), "callType"] <- "al" 
  
  all_calls$ini <- NA
  all_calls$self <- NA
  for (i in 2:nrow(all_calls))
  {all_calls$ini[i] <- as.character(all_calls$callType[i-1])
  all_calls$self[i] <- as.character(all_calls$ind [i-1])}
  
  print(paste("end", date))
  
  all_calls_seq <- rbind(all_calls_seq, all_calls)
  
}


both_years <- all_calls_seq
both_years$callType <- both_years$entryName ##### at this point we have all focal entries together 


### recode some of the calls due to between years inconsistency ###
### this is the part that does the actual name recoding ###
#If we encounter more naming issues which are not resolved, the following lines could be modified

#here set the vectors for unifying modifier and call names
hybrid <- c("hybrid|hyb")
sequence <- c("seq|sq")
move <- c("move|mov")
agression <- c("aggress|agress|chat|growl|grunt|bark")
alarm <- c("alarm|alrm|alert|ala")
lost <- c("lost|loc|lc")

both_years$callType <- str_replace_all(both_years$callType, hybrid, "hyb")
both_years$callType <- str_replace_all(both_years$callType, sequence, "sq")
both_years$callType <- str_replace_all(both_years$callType, move, "mo")
both_years$callType <- str_replace_all(both_years$callType, c("lead"), "ld")
both_years$callType <- str_replace_all(both_years$callType, c("social"), "soc")
both_years$callType <- str_replace_all(both_years$callType, c("ukn"), "unk")
both_years$callType <- str_replace_all(both_years$callType, agression, "agg")
both_years$callType <- str_replace_all(both_years$callType, alarm, "al")
both_years$callType <- str_replace_all(both_years$callType, lost, "lc")
both_years[which(both_years$callType == "s"), "callType"] <- "sn"  
both_years[which(both_years$callType == "c"), "callType"] <- "cc"  


both_years$callType <- str_replace(both_years$callType, fixed("s+"), "sn ") #renaming s as the first element
both_years$callType <- str_replace(both_years$callType, "\\+s$", " sn ")   #renaming s as the second element

### get hyb calls ###

both_years$hyb <- NA
both_years[grepl("fu|hyb", both_years$callType), "hyb"] <- "fu"
both_years[grepl("sq", both_years$callType), "hyb"] <- "sq"



### get call type elements ####
call_types <- c("agg|al|cc|ld|mo|sn|soc|lc|unk") #call types that we need
both_years <- cbind(both_years , str_extract_all(both_years$callType, call_types, simplify = TRUE)) #getting the call type elements 
both_years$`1` <- as.character(both_years$`1`) #removing factors
both_years$`2` <- as.character(both_years$`2`) #removing factors

#collecting the call type elements in an alphabetic order
both_years$final <- ifelse(both_years$`1` < both_years$`2`, paste(both_years$`1`, both_years$`2`), paste(both_years$`2`, both_years$`1`)) 
# keeping the original order for sequential calls
both_years[which(both_years$hyb == "sq") , "final"] <- paste(both_years[which(both_years$hyb == "sq") , "1"], both_years[which(both_years$hyb == "sq") , "2"])


#looking at the frequencies of the main call types as a self check
call_types <- c("cc", "soc", "al", "agg", "sn", "ld", "mo", "lc", "unk")


freq <- data.frame()
for (i in 1:length (call_types))
{
  freq[i,1] <- sum(str_count(rbind(both_years$`1`, both_years$`2`) , pattern = call_types[i]), na.rm = T)
  freq[i,2] <- call_types[i]
}
freq<- freq[order(freq$V1, decreasing = F),] ### if we decide to recode by the rarest category decreasing is to be set to TRUE


#get sample size plot for sanity check
par(mfrow=c(1,2))
bp <- barplot(freq$V1, names.arg = freq$V2, main = "Sample sizes per call type") 
text(bp, 0, freq$V1 ,cex=1,pos=3) 
#recode the call-types into main call categories. The order of recoding is frequency based 
# hybrid call_types are collapsed to the more frequent type. 

both_years$type_group <- NA  

for ( i in 1:nrow(freq))
{ type <- freq$V2[i]

both_years[which(grepl(type, both_years$final)), "type_group"] <- type 
}


#sample sizes per call category

freq <- data.frame()
for (i in 1:length (call_types))
{
  freq[i,1] <- sum(str_count(both_years$type_group , pattern = call_types[i]), na.rm = T)
  freq[i,2] <- call_types[i]
}
freq<- freq[order(freq$V1, decreasing = F),] 

bp <- barplot(freq$V1, names.arg = freq$V2, main = "Sample sizes per call category") #get sample size plot for sanity check
text(bp, 0, freq$V1 ,cex=1,pos=3) 


#cleaning
both_years <- subset(both_years, select=-c(`1`, `2`, `3`))
names(both_years)[names(both_years) == "final"] <- "stn_call_type"


#get the pairs for all calls
all_calls_seq <- both_years
all_calls_seq$pair <- NA
all_calls_seq$pair <- paste(lag(all_calls_seq$type_group, n=1), all_calls_seq$type_group) 
for (i in 2:nrow(all_calls_seq)) {
  all_calls_seq[i, "ini"] <- all_calls_seq[i-1, "type_group"]}


#remove first call of each day
all_calls_seq[which(is.na(all_calls_seq$lag)), "ini"] <- NA

######end data arranging bit############
#_______________________________________________________________________________________#


###make heat maps for call type transitions###

#duplicate data for easier handling
all_pairs <- all_calls_seq


#remove very fast self replies to account for SN sequences
all_pairs <-  all_calls_seq[-(which( all_calls_seq$lag < 0.1 &  all_calls_seq$ind ==all_calls_seq$self)) , ]  
quantile(all_pairs$lag, na.rm = T, probs = c(0.25 ,0.5, 0.75, 0.90))    # get response lag IQR and 90th percentiles


all_pairs <- all_pairs [which(all_pairs$c_dist < dist_thresh), ] #filter by distance here


nrow( all_pairs) #get sample sizes
table(all_pairs$type_group)

all_pairs <- all_pairs[which(all_pairs$lag <= seq.reply), ] #remove slow replies


all_pairs <- all_pairs[which(!is.na(all_pairs$ini) & !is.na(all_pairs$type_group)), ] #remove  NAs
#all_pairs <- all_pairs[which(all_pairs$trigger == "responce") ,]    #uncomment this line for sequence start cutoff

#allcalls <- c(as.character(all_pairs$ini ), as.character(all_pairs$type_group ))
all_pairs$lag <- as.numeric(as.character(all_pairs$lag))

all_pairs$null <- sample(all_pairs$type_group, size = nrow(all_pairs), replace = F)
all_pairs$rand_lag <- sample(all_pairs$lag, size = nrow(all_pairs), replace = F)

#only focal-focal pairs
all_focal_pairs <- all_pairs[which(all_pairs$ind == all_pairs$self), ]
#all call pairs
all_foc_NF_pairs <-  all_pairs
#only focal-non_focal pairs
all_NF_pairs <- all_pairs[which(all_pairs$ind != all_pairs$self), ]
#all call pairs no same pairs
inter_type <- all_pairs[which(all_pairs$type_group != all_pairs$ini) ,]


call_pairs_list <- list(all_NF_pairs, all_focal_pairs, all_foc_NF_pairs , inter_type)
names(call_pairs_list) <- c("caller exchange", "self reply", "all replies", "inter_type")

#make heat maps with real probability values and color code corresponding to null subtraction
p <- list()
for (x in 1:length(call_pairs_list))
{
  #make empty matrix with call types
  gbiMat <- matrix(0,nrow=8,ncol=8)
  row.names(gbiMat) <- c("sn",  "soc",  "cc",  "mo", "ld", "agg", "al", "unk")
  colnames(gbiMat) <- c("sn",  "soc",  "cc",  "mo", "ld", "agg", "al", "unk")
  
  #make empty matrix with call types for null
  gbiMat_null <- gbiMat
  
  all_pairs <- call_pairs_list[[x]]
  
  #transform the raw assosiation into  matrix
  for (j in 1:nrow(all_pairs))
  {
    gbiMat[which(row.names(gbiMat) == all_pairs$ini[j]), which(colnames(gbiMat)== all_pairs$type_group[j])] <- gbiMat[which(row.names(gbiMat) == all_pairs$ini[j]), which(colnames(gbiMat)== all_pairs$type_group[j])]+1
  }
  
  prob_matrix <- gbiMat/rowSums(gbiMat)
  if (x == 4) {prob_matrix <- sna::diag.remove(prob_matrix, remove.val=NA)} #for the intercall only remove diagonal
  melted_prob_matrix <- melt(prob_matrix) #melt table into long format
  
  
  colnames(melted_prob_matrix) <- c("initiation", "reply", "value")
  
  
  #transform the null call association into  matrix
  for (j in 1:nrow(all_pairs))
  {
    gbiMat_null[which(row.names(gbiMat_null) == all_pairs$ini[j]), which(colnames(gbiMat_null)== all_pairs$null[j])] <- gbiMat_null[which(row.names(gbiMat_null) == all_pairs$ini[j]), which(colnames(gbiMat_null)== all_pairs$null[j])]+1
  }
  
  
  null_prob_matrix <- gbiMat_null/rowSums(gbiMat_null)
  melted_net_null <- melt(null_prob_matrix) #melt table into long format
  colnames(melted_net_null) <- c("initiation", "reply", "value")
  
  
  #substracting data from null
  delta_probs <- prob_matrix - null_prob_matrix 
  if (x == 4) {delta_probs <- sna::diag.remove(delta_probs, remove.val=NA)} #for the intercall only remove diagonal
  
  melted_delta_probs <- melt(delta_probs) #melt table into long format
  colnames(melted_delta_probs) <- c("initiation", "reply", "value")
  
  p[[x]] <- ggplot(data = melted_delta_probs, aes(y=initiation, x=reply, fill=value)) + 
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", name="Null substracion" ) +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed() + ggtitle(names(call_pairs_list[x])) + geom_text(data = melted_prob_matrix ,aes(label = round(value, 2))) #get the values from the real prob matrix
  
  
}

grid.arrange(p[[1]], p[[2]],p[[3]], p[[4]] ,  nrow = 2, top = paste("Delta call_type transition matrix (20m cutoff)"))

#_________________________________________________________________________________________________________________________

#### calculate self reply time vs caller exchange reply time ####



###calculate proportions of self replies vs caller exchange per call type

tmp_pairs <- all_calls_seq[which(!is.na(all_calls_seq$ini) & !is.na(all_calls_seq$type_group)) ,]
tmp_pairs$caller_match <-ifelse( tmp_pairs$ind == tmp_pairs$self, T, F) 


tmp_pairs <- tmp_pairs[-(which(tmp_pairs$lag < 0.1 & tmp_pairs$caller_match ==T)) , ] #remove quick self replies
#tmp_pairs <- tmp_pairs[-(which(tmp_pairs$lag < 0.1)) , ] #remove all quick replies

tmp_pairs <- tmp_pairs[which(tmp_pairs$lag <= seq.reply), ] #remove slow replies
tmp_pairs <- tmp_pairs [which(tmp_pairs$c_dist < dist_thresh), ] #filter by distance here
#tmp_pairs$rand_lag <- sample(tmp_pairs$lag, nrow(tmp_pairs), replace = F)


#get sample sizes
sample_sizes <- data.frame(table(tmp_pairs$pair))

#get call pairs of reasonable sample size
call_pairs <- sample_sizes[which(sample_sizes$Freq > pair_sample_size), 1]
tmp_pairs <- tmp_pairs[which(tmp_pairs$pair %in% call_pairs), ] #select  call type pairs of interest


# general proportion plot self vs non self reply
p1 <- ggplot(tmp_pairs, aes(pair , fill=caller_match)) +
  geom_bar(position="fill", width=0.7) + xlab("call_pair")+ ylab("Proportion") +
  ggtitle("Self reply / caller exchange proportion") + labs(fill = "Self reply") 

p1
#Plot all together
#grid.arrange(p1, p2, nrow = 2)


### box plots for self vs non self response times with tests 
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}
ggplot(data = tmp_pairs, aes(x = pair, y = lag, fill = caller_match)) + geom_boxplot(notch = T) + 
  theme_minimal() + scale_fill_grey(start = 0.4,end = 0.6, na.value = "red", aesthetics = "fill") +
  stat_summary(fun.data = give.n, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  stat_compare_means(aes(group = caller_match, label = paste0(..method.., "\n", "p =", ..p.format..)))


#####################################################################

test <- all_calls_seq[which(!is.na(all_calls_seq$c_dist)), ] #only get pairs with distance
test <- test[which(test$self != test$ind), ] #only get caller exchange
test <- test[which(test$c_dist < dist_thresh), ] #limit reply distance 

test <- test[which(test$lag < seq.reply), ] #limit time lag 



#test <- test[which(test$pair %in% call_pairs), ] #select  call type pairs
sample_sizes <- data.frame(table(test$pair))

#run plots and models for call types with reasonable sample sizes only
call_pairs <- sample_sizes[which(sample_sizes$Freq > pair_sample_size), 1]
tmp_pairs <- test[which(test$pair %in% call_pairs), ] #select  call type pairs of interest


par(mfrow=c(1,1))


colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#009999")

plot(1, type="n", xlab=bquote(bold("Distance(m)")), ylab=bquote(bold("Response time (sec)")), xlim=c(0, dist_thresh), ylim=c(0, 2.5), main = "Response time by distance", bty = "l")
legend("topleft", 
       legend = call_pairs, 
       col = adjustcolor(colors, alpha.f=0.4), 
       pch = 20, 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.01))

for (call in call_pairs)
{
  clr <- colors[which(call_pairs == call)]
  test_plot <-tmp_pairs[which(tmp_pairs$pair == call), ]
  
  library(lme4)
  
  model<- lm(lag~c_dist, data = test_plot)
  
  summary(model)
  
  
  
  pred.data=data.frame(c_dist= seq(from=min(test_plot$c_dist),
                                   to=max(test_plot$c_dist),
                                   length.out=100))
  ci.plot=predict.lm(object=model, newdata=pred.data,
                     interval="confidence") 
  
  #plot(1, type="n", xlab=bquote(bold("Distance(m)")), ylab=bquote(bold("Response time (sec)")), xlim=c(min(test_plot$c_dist), max(test_plot$c_dist)), ylim=c(0, 3.5), main = call)
  
  polygon(
    x=c(pred.data$c_dist, rev(pred.data$c_dist)),
    y=c(ci.plot[, "lwr"], rev(ci.plot[, "upr"])),
    border=NA, col = adjustcolor(clr, alpha.f=0.2))
  lines(x=pred.data$c_dist, y=ci.plot[, "fit"], lwd=2, lty=2,col=clr )
  
}

##plotting response distance per call pair

par(mfrow=c(2,2))

for (one_pair in call_pairs) {
  hist(tmp_pairs[which(tmp_pairs$pair == one_pair) ,"c_dist"], col = adjustcolor("#000000", alpha.f=0.4), main = paste(one_pair, "response distance", sep = ""), xlab = "meters")
}


#____________________________________________________________________________________
#plotting models for CC and SN

call_pairs <- c("cc cc", "sn sn")

tmp_pairs <- tmp_pairs[which(tmp_pairs$pair %in% call_pairs), ] #select  only CC and SN


library(sjPlot)
library(sjmisc)
library(lme4)
library(jtools)
library(ggstance)

m3.lmer <- lmer(lag ~ c_dist + pair + c_dist*pair + (1|ind), data = tmp_pairs)
summary(m3.lmer) 

sjPlot::plot_model(m3.lmer, type = "int")


summ(m3.lmer)
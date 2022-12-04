
date <- "20190623"
print(date)

#creation of the empty data table for a given day
dayTable <- data.frame()

#reading all files available for a given day
files <- list.files(paste(folderCsv,date,sep="/"),recursive=T)

if(onlyFocals)files <- files[grep("SOUNDFOC",files)]

#loop for each file of that day
for (file in files){
  
  print("")
  print(paste0("*****",file,"*****"))
  
  #getting info about that file (mainly individual ID):
  
  info <- strsplit(file,"_")[[1]]
  
  group <- info[1]
  
  nbrFile <- labeller <- verifier <- NA
  
  if(info[[3]]=="SOUNDFOC"){ #if it is a sound focal (file names differ)
    indCode <- info[2]
    if(!is.na(as.numeric(info[5]))){
      nbrFile <- as.numeric(info[[5]])
      if(session != "HM2017")labeller <- substr(info[6],1,2)
    }else{
      if(session != "HM2017")labeller <- substr(info[5],1,2)
    }
    if(is.na(nbrFile)){
      wavFileName <- allWavFileName[which(grepl(indCode,allWavFileName) &
                                            grepl(date,allWavFileName))]
    }else{
      wavFileName <- allWavFileName[which(grepl(indCode,allWavFileName) &
                                            grepl(paste(date,nbrFile,sep="_"),allWavFileName))]
    }
  }else{
    if(info[2] %in% indInfo$code){
      indCode <- info[2]
    }else if(info[2] %in% indInfo$dye){
      indCode <- as.character(indInfo$code[which(indInfo$dye==info[2])])
    }else if(info[2]=="PET"){
      indCode <- as.character(indInfo$code[which(indInfo$name=="Pet")])
    }else{
      print("error name")
      indCode <- NA
    }
    
    if(session != "HM2017"){
      labeller <- info[14]
      verifier <- substr(info[15],1,2)
    }
    wavFileName <- allWavFileName[which(grepl(indCode,allWavFileName) &
                                          grepl(paste0(substr(date,1,4),"_",substr(date,5,6),"_",substr(date,7,8)),allWavFileName))]
    if(length(wavFileName)==0){
      wavFileName <- allWavFileName[which(grepl("VHM008",allWavFileName) &
                                            grepl(paste0(substr(date,1,4),"_",substr(date,5,6),"_",substr(date,7,8)),allWavFileName))]
    }
  }
  
  status <- indInfo$status[which(indInfo$code==indCode)]
  
  if(length(wavFileName)>1)print("WARNING: error wavFileName")
  
  
  Labels <- read.csv(paste(folderCsv,date,file,sep="/"), sep="\t", header = T, quote = "") #loading the file
  Labels[, 1] <- gsub('"', '', Labels[, 1]) #getting rid of quotes generated if csv file are opened in excell
  Labels[, 2] <- gsub('"', '', Labels[, 2]) #getting rid of quotes generated if csv file are opened in excell
  Labels[, 3] <- gsub('"', '', Labels[, 3]) #getting rid of quotes generated if csv file are opened in excell
  Labels[, 4] <- gsub('"', '', Labels[, 4]) #getting rid of quotes generated if csv file are opened in excell
  Labels[, 5] <- gsub('"', '', Labels[, 5]) #getting rid of quotes generated if csv file are opened in excell
  Labels <- Labels[ , 1:6]                  #leaving only the columns that we want
  colnames(Labels) <- c("Name", "Start", "Duration", "TimeFormat", "Type", "Description")
  Labels[,1] <- trimws(Labels[,1],"both")
  
  #putting the times in the right format
  Labels$Start <- as.character(Labels$Start)
  Labels$Start[which(substr(Labels$Start,5,5)==".")] <- paste("0",Labels$Start[which(substr(Labels$Start,5,5)==".")],sep="")
  Labels$Start[which(substr(Labels$Start,5,5)==":")] <- paste("0",Labels$Start[which(substr(Labels$Start,5,5)==":")],sep="")
  Labels$Start[which(substr(Labels$Start,6,6)!=":")] <- paste("00:",Labels$Start[which(substr(Labels$Start,6,6)!=":")],sep="")
  
  #formatting the labels (mainly homogeanizing between 2017 and 2019 notation)
  Labels[,1] <- tolower(Labels[,1])
  Labels[,1] <- sub("lead","ld",Labels[,1])
  Labels[,1] <- sub("end","stop",Labels[,1])
  Labels[,1] <- gsub(" \\+ ","+",Labels[,1])
  if(session == "HM2017"){
    Labels[,1] <- sub("nonfoc","nf",Labels[,1])
    Labels[,1] <- sub("x"," x",Labels[,1])
    Labels[,1] <- sub("\\*"," \\*",Labels[,1])
    Labels[,1] <- sub("\\["," \\[",Labels[,1])
    Labels[,1] <- sub("\\("," \\(",Labels[,1])
    Labels[,1] <- sub("\\?"," \\?",Labels[,1])
    Labels[,1] <- sub("\\!"," \\!",Labels[,1])
    Labels[,1] <- sub("\\#"," \\#",Labels[,1])
    Labels[,1] <- sub("\\%"," \\%",Labels[,1])
    Labels[,1] <- sub("alarm","al",Labels[,1])
    #calls that were labelled "marker" to save time but were actually cc
    Labels[,1] <- sub("marker","cc",Labels[,1])
  }else{
    Labels[,1] <- sub("s$","sn",Labels[,1])
    Labels[,1] <- sub("s ","sn ",Labels[,1])
    Labels[,1] <- sub("s\\+","sn\\+",Labels[,1])
  }
  if(any(nchar(Labels[,1])==0)){
    Labels[,1][which(nchar(Labels[,1])==0)] <- NA
  }
  
  #this is because there seems to be some weird stuff with this particular time
  if(length(which(Labels[,1]=="synch 01:49:30"))!=0){
    Labels <- Labels[-which(Labels[,1]=="synch 01:49:30"),]
  }
  
  startLine <- grep("start",Labels[,1],ignore.case = T)[1]
  stopLine <- tail(grep("stop",Labels[,1 ],ignore.case = T),1)
  
  #location of synch calls in the label table
  synchIdx <- which(grepl(synchType,Labels[,1],ignore.case=T) & grepl("\\:",Labels[,1],ignore.case=T))
  
  #synchronizing:
  
  #computing the GPS time at which the talking clock was started----
  
  #first we need to find in the synchInfo table the GPS time at which the talking clock was started
  #for certain days there are several entries in the synchInfo table for various reasons, so we need to select the appropriate one:
  if( (date == "20190712" & indCode %in% c("VCVM001","VHMM007","VHMM008")) |  # day when there was a group split and the two sub-groups were followed with different talking clocks
      (date=="20170809" & (grepl("clockGap",file) | nbrFile>3)) |  # day when the synch calls were restarted mid-session
      (date == "20170825" & grepl("clockGap",file))){ # day when the talking clock was accidentally paused
    #in such cases we take the second line
    synchStart <- as.POSIXct(synchInfo$GPS.Time.UTC[match(date,synchInfo$Date)+1],tz="UTC") - as.difftime (substr(synchInfo$Speaker.Time[match(date,synchInfo$Date)+1],12,19))
  }else{
    #otherwise it is the first line
    synchStart <- as.POSIXct(synchInfo$GPS.Time.UTC[match(date,synchInfo$Date)],tz="UTC") - as.difftime (substr(synchInfo$Speaker.Time[match(date,synchInfo$Date)],12,19))
  }
  
  #location of the synch calls in the label table
  synchIdx <- which(grepl(synchType,Labels[,1],ignore.case=T) & grepl("\\:",Labels[,1],ignore.case=T))
  synchIdx <- synchIdx[!grepl("x",Labels[synchIdx,1])]
  
  if(length(synchIdx)==nrow(Labels)){
    print("WARNING: csv file contain only synch calls")
  }
  
  if(length(synchIdx) == 0){
    print("WARNING: No Synch calls in csv file, skipping")
    next()
  }
  
  synchFileTime <- Labels$Start[synchIdx] #time at which the synch calls occured in the wav file
  synchClockTime <- sapply(strsplit(Labels[synchIdx,1]," "),function(f)grep("\\:",f,value=T)) #time told by the talking clock for each synch call
  
  if(any(string2sec(synchClockTime) %% 90 != 0,na.rm=T)){
    print("---WARNING: following synch calls not multiples of 90s:")
    print(paste(which(string2sec(synchClockTime) %% 90 != 0),as.character(Labels[synchIdx,1])[string2sec(synchClockTime) %% 90 != 0]))
  }
  
  #getting the GPS time of each synch call, using the time at which the talking clock was started:
  synchDiff <- as.difftime(synchClockTime,format="%T")
  synchGPSTime <- synchStart + synchDiff
  
  synchGPS <- as.numeric(sapply(substr(synchGPSTime,12,19),string2sec)) #GPS time as a number of seconds since start of the day 
  synchFile <- as.numeric(sapply(synchFileTime,string2sec)) #wav file time as a number of seconds since start of the file
  
  if(length(synchGPS)==1){ #if there is only one synch call, "simulating" a second one with no drift, because cannot fit a line on a single point
    synchGPS <- c(synchGPS,synchGPS+180)
    synchFile <- c(synchFile,synchFile+180)
  }
  
  #finding the parameters of the line of best fit
  linearModel <- lm(synchGPS~synchFile)
  polyModel <- lm(synchGPS~poly(synchFile,2,raw=T))
  
  polyPredictionSynch <-predict(polyModel,data.frame(synchFile=synchFile))
  polyPredictionDiff <- synchGPS-polyPredictionSynch
  linearPredictionSynch <-predict(linearModel,data.frame(synchFile=synchFile))
  linearPredictionDiff <- synchGPS-linearPredictionSynch
  
  thresh <- 0.2
  
  #plotting the gps times against the difference between actual gps time and predicted
  plot(synchFile,polyPredictionDiff,col=2,pch=16,xlab="Position of synch calls in the wav file (seconds)")
  points(synchFile,linearPredictionDiff,col=4,pch=16)
  legend("topleft",legend=paste(indCode,date,nbrFile),bty="n")
  legend("topright",legend=c("linear","polynomial"),col=c(4,2),pch=16,bty="n")
  abline(h=thresh,lty=2)
  abline(h=-thresh,lty=2)
  abline(h=0)
  
 
  test <- data.frame(cbind(synchFile,polyPredictionDiff, linearPredictionDiff))
  print(plot_ly(data = test, x = ~synchFile)  %>% 
    
    add_trace(y = ~polyPredictionDiff, name = 'polyPredictionDiff', type = "scatter", mode = 'markers') %>%
      add_trace(y = ~linearPredictionDiff, name = 'linearPredictionDiff', type = "scatter", mode = 'markers') %>%
    layout(title = paste(indCode,date,nbrFile), xaxis = list(title = 'Position of synch calls in the wav file (seconds)') ))
    
  
  weird <- which(abs(as.numeric(polyPredictionDiff)) > thresh)
  if(length(weird)>(length(synchFile)/2)){
    print("---WARNING: many synch calls are off, most likely because some are mislabeled: see graph")
  }else if(length(weird)>0){
    print("---WARNING: following synch calls seem to be off:")
    print(paste0(weird," ",as.character(Labels[synchIdx,1])[weird]," (",round(polyPredictionDiff[weird],2),"s)"))
  }
  
  #calculating all the gps times in seconds from the model
  t0GpsSec <- predict.lm(polyModel,data.frame(synchFile=string2sec(as.character(Labels$Start))))
  t0gps <- as.POSIXct(paste(date,sec2string(t0GpsSec)),format="%Y%m%d %H:%M:%OS",tz="UTC")
  
  Labels$Start_UTC <- t0gps
  
  beepIdx <-grep("beep",Labels[,1],ignore.case = T)
  synchWithBeep <- beepIdx+1
  f <- which(is.na(match(synchWithBeep,synchIdx)))
  if(length(f)!=0){
    beepIdx <- beepIdx[-f]
    synchWithBeep <- synchWithBeep[-f]
  }
  
  #putting all the infos in a table, with each line = one label
  fileTable <- data.frame("callID"=NA,"wavFileName"=wavFileName,"csvFileName"=file,"entryName"=Labels[,1],
                          "date"=date,"ind"=indCode,"status"=status,"labeller"=labeller,"verifier"=verifier,
                          "t0File"=Labels$Start,"duration"=Labels$Duration,"t0GPS_UTC"=as.character(Labels$Start_UTC),
                          "tMidGPS_UTC" = as.character(Labels$Start_UTC + as.difftime(as.character(Labels$Duration),format="%M:%OS")/2),
                          "tendGPS_UTC" = as.character(Labels$Start_UTC + as.difftime(as.character(Labels$Duration),format="%M:%OS")),
                          "callType"=NA,"isCall"=0,"focalType"="F","hybrid"=0,"noisy"=0,"unsureType"=0)
  
  if(is.na(nbrFile)){
    fileTable$callID <- paste(date,indCode,Labels$Start,as.character(Labels$Duration),Labels[,1],sep="_")
  }else{
    fileTable$callID <- paste(date,nbrFile,indCode,Labels$Start,as.character(Labels$Duration),Labels[,1],sep="_")
  }
  
  callsInfo <- sapply(Labels[,1],strsplit," ")
  
  fileTable$hybrid <- as.numeric(sapply(fileTable$entryName,function(f)grepl("fu|sq|hyb",f)))
  
  hyb <- as.logical(fileTable$hybrid)
  fileTable$callType[hyb] <- sapply(callsInfo[hyb],function(f)grep("\\+",f,value=T))
  fileTable$callType[!hyb] <- sapply(callsInfo[!hyb],function(f)f[[1]])
  fileTable$isCall <- as.numeric(sapply(fileTable$entryName,function(f)grepl(paste(callTypes,collapse="|"),f)))
  fileTable$focalType[grep("\\*",fileTable$entryName)] <- "U"
  fileTable$focalType[grep("nf",fileTable$entryName,ignore.case = T)] <- "NF"
  fileTable$noisy[grep("x",fileTable$entryName,ignore.case = T)] <- 1
  fileTable$unsureType[grep("\\?",fileTable$entryName)] <- 1
  
  dayTable <- rbind(dayTable,fileTable)
}

#dev.off()
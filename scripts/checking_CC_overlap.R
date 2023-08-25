###looking at overlapping CC calls###
#This script loads meerkat call data after synchronization and focal non focal resolver (Maras notebookd)
#it finds focal CC calls occurring at the same time, loads their spectrograms and allows secondary manual confirmation 
#of the calls being produced by the same or not the same meerkat

setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics")

library(soundgen)
library(seewave)
library(warbleR)
library(tidyverse)
library(ggpubr)
library(gridGraphics)
library(utils)


#load focal channel table
meerkat_channels <- read.csv("soundfoc_channels.csv")

#load the most recent call data after filtering the non-focal calls out
call_data <- read.csv("all_calls_sync_resolved_2023-03-23.csv")

#subset CC calls only
cc_call_data <- call_data[which(call_data$type_group == "cc"), ]

#set threshold for calls to overlap
overlap_tres <- 0.1


  for (date in unique(cc_call_data$date)[21:length(unique(cc_call_data$date))]) {
  #make empty table for collecting focal/non focal reports
  report <- data.frame()
  
  #date = 20170823
  
  #get one day of data
  date_select <- cc_call_data[which(cc_call_data$date == date), ]
  #calculate the time lag between consecutive CCs
  date_select$lag <- date_select$t0.numeric -  lag(date_select$tf.numeric)
  
  ##change the address for reading the raw wav files. 
   date_select$wav_loc <- str_replace_all(date_select$wav_loc, 
                                         "//home//jupyter-vdemartsev//eas_shared/meerkat/archive/rawdata/MEERKAT_RAW_DATA", 
                                        "D://Meerkat_Audio/") #adapt the path to the EAS server or local folder with the raw audio
  
  #find the calls that overlap in time
  overlaps <- which(date_select$lag <= overlap_tres)
  #overlaps <- c(885, 933)
  
  ## this loop reads in the overlapping calls in to R, prints their spectrograms 
  #and can play the calls but it is not recommended
  for (call in overlaps) {
  
  #for (call in overlaps[185:204]) {
  
  #delete previous meerkat channel index  
  meerkat_channel <- NA
  
  #read call 1
  wave1 <- readWave(date_select$wav_loc[call - 1], date_select$start_s[call-1] - 0.05, 
                    date_select$start_s[call - 1]+date_select$duration_s[call -1] + 0.05, 
                    units = "seconds")
 
  #check if channels need to be flipped
  if (tolower(date_select$wavFileName[call-1]) %in% tolower(meerkat_channels$wavFile)) {
    #get the meerkat channel from the soudfoc_channel.csv
    meerkat_channel <- meerkat_channels[which(tolower(meerkat_channels$wavFile) == tolower(date_select$wavFileName[call - 1])), "meerkatChannel"]
    if (meerkat_channel == 0){wave1 <- channel(wave1, which = "mirror")} #flip the channel if not "left"
  } 
  
  #downsample the audio to make collar and soundfoc audio comparable
  wave1<- downsample(wave1, 8000)
  
  #filter low frequency noise
   wave1 <- ffilter(wave1, f= 8000, channel = 1, from = 100, to = 4000, bandpass = TRUE,
           custom = NULL, wl = 512, ovlp = 75, wn = "blackman", fftw = FALSE,
           rescale=FALSE, listen=F, output="Wave")
   
  #play(wave1) #this works on a PC but slows down the process
  
  
  #read call 2
  wave2 <- readWave(date_select$wav_loc[call], date_select$start_s[call] - 0.05, 
                      date_select$start_s[call ]+date_select$duration_s[call] + 0.05, 
                      units = "seconds")
  #check if channels need to be flipped
  if (date_select$wavFileName[call] %in% meerkat_channels$wavFile) {
    #get the meerkat channel
    meerkat_channel <- meerkat_channels[which(meerkat_channels$wavFile == date_select$wavFileName[call]), "meerkatChannel"]
    if (meerkat_channel == 0){wave2 <- channel(wave2, which = "mirror")}
  } 
  #downsample the audio
  wave2 <- downsample(wave2, 8000)
  
  wave2 <- ffilter(wave2, f= 8000, channel = 1, from = 100, to = 4000, bandpass = TRUE,
                   custom = NULL, wl = 512, ovlp = 75, wn = "blackman", fftw = FALSE,
                   rescale=FALSE, listen=F, output="Wave")
  #play(wave2)
  
  
  #define the parameters of paneled plot
  par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))
  
  #plot 4 spectrograms. Diferent settings to acout for fainter calls. the left and rigth columns are the same calls
  #just the gain settings are diferetn
  
  spectro(wave1, wl = 256, wn = "hanning", noisereduction = NULL,
          norm = T, dB = "max0", flog = F, grid = FALSE, scale = F, axisX = T, collevels = seq(-40,0,3),
          palette = reverse.gray.colors.1, axisY = T, ovlp = 90)
  spectro(wave2, wl = 256,  wn = "hanning", noisereduction = NULL,
          norm = T, dB = "max0", flog = F, grid = FALSE, scale = F, axisX = T, collevels = seq(-40,0,3),
          palette = reverse.gray.colors.1, axisY = T, ovlp = 90)
  spectro(wave1, wl = 256, wn = "hanning", noisereduction = NULL,
          norm = T, dB = "max0", flog = F, grid = FALSE, scale = F, axisX = T, collevels = seq(-60,0,3),
          palette = reverse.gray.colors.2, axisY = T, ovlp = 90)
  spectro(wave2, wl = 256,  wn = "hanning", noisereduction = NULL,
          norm = T, dB = "max0", flog = F, grid = FALSE, scale = F, axisX = T, collevels = seq(-60,0,3),
          palette = reverse.gray.colors.2, axisY = T, ovlp = 90)
  
###############################################################################################
  ### Alternative option for plotting spectrograms ####
  
# #combine the two calls into 1 Wave object
# wave_comb <- pastew(wave1, wave2, f = 8000, channel = c(1,1), at = "end",
#        join = FALSE, tjunction = 0,
#        choose = FALSE, plot = F,
#        marks = TRUE, output = "Wave")
# 
# #play the calls one after another
#
# 
# #filter low frequency noise
# wave_comb<- ffilter(wave_comb, f= 8000, channel = 1, from = 300, to = 4000, bandpass = TRUE,
#         custom = NULL, wl = 1024, ovlp = 75, wn = "blackman", fftw = FALSE,
#         rescale=FALSE, listen=F, output="Wave")
# 
#    #play(wave_comb)
#    
# #draw a spectrogram
# spectrogram(wave_comb, samplingRate = 8000, dynamicRange = 80, overlap = 85, 
#             wn = "blackman", windowLength = 20, normalize = T, 
#             plot= T, output = "processed", colorTheme = "seewave")
##########################################################################################
  
  # this is the interactice part. requesting input if the calls are similar yes (1) or diferent (2)
  # if calls are similar which one is the focal? left (1) or right (2)
  
  {
    n1<-readline(prompt="Are these calls similar (1) or diferent(2): " )
    ifelse (n1 == 2, n2 <- NA, n2<-readline(prompt="Which one us the focal: Left (1) or Right (2) " ))
  }
  report <- rbind(report, c(date_select$callID[call-1], date_select$callID[call], n1, n2))
  }
   colnames(report) <- c("call1", "call2", "is_same", "focal") 
   write.csv(report, paste(date, "_foc.csv", sep = "")) #write a report for each of the processed dates
}



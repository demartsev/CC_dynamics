#This is a script for looking at meerkat call rate after arriving at a new foraging patch
#the script generates several time windows after arrival to the patch and calculates call rate in each one of the windows.
#It uses Baptiste's track discretization data and cross references it with the audio
library(ggplot2)
library(lubridate)
library(plotly)
library(viridis)
library(ggExtra)
library(cowplot)
library(ggpubr)
library(akima)
library(tidyverse)
library(grid)
library(gridExtra)
library(lemon)
setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics")

#set the displacement distance
distance <- 10

#set time window
tw <- 10

#load movement data object
load(paste(
  "discreet_tracks_data/spatialMetrics_",
  distance,
  "m.RData",
  sep = ""
))


#load audio data
call_data <- read.csv("foc_calls_resolved.csv")

#get all dates
dates <- unique(spatialMetrics$date)
#remove 2021 data
dates <- dates[-grep(21, dates)]


all_arrival_points <- data.frame()

for (date in 1:length (dates)) {
  #date <- 1
  #get one day of data
  date_select <-
    spatialMetrics[which(spatialMetrics$date == dates[date]), ]
  #get all animals observed on that day
  individuals <- unique(date_select$indUniqID)
  for (individual in individuals) {
    
    #individual <- individuals[1]
    
    #get one animal movement data
    individual_select <-
      date_select[which(date_select$indUniqID == individual) ,]
    #get the CODE ID for the selected animals
    ind_code <-
      allIndInfo[which(allIndInfo$uniqueID == individual) , "code"]
    #get the date in a format compatible with the audio data
    day_form <- gsub("[[:punct:]]", "", dates[date])
    #select the calls of the meerkat of interest on the day of interest
    calls_select <-
      call_data[which(as.character(call_data$date) == day_form &
                        call_data$ind == ind_code) ,]
    
    #get the time when labeling started
    labels_start <-
      calls_select[which(calls_select$entryName == "start") , "t0GPS_UTC"][1]
    #get the time hen labeling ended
    labels_stop  <-
      calls_select[which(calls_select$entryName == "stop") , "t0GPS_UTC"][length(calls_select[which(calls_select$entryName == "stop") , "t0GPS_UTC"])]
    
    
    #filter positions for which we have labeled calls
    individual_select <-
      individual_select[which(
        individual_select$t > as.POSIXct (labels_start, tz = "UTC") &
          individual_select$t <  as.POSIXct (labels_stop, tz = "UTC")
      ),]
    #if there is no call data skip to the next individual
    if (nrow(individual_select) == 0) {
      next
    }
    
    
    
    #select positions with future step
    individual_select <-
      individual_select[which(!is.na(individual_select$futurStepDuration)) ,]
    
    
    #if there is no call data skip to the next individual
    if (nrow(individual_select) == 0) {
      next
    }
    
    
     ### getting individual baseline call rates
      #get the base call rate for the time window of interest
      base_call_rate <- length(which(calls_select$isCall == 1)) / 
        as.numeric(difftime(labels_stop ,  labels_start, units="secs"))
      
      #get the CC call rate for the time window of interest
      base_cc_rate <- length(which(calls_select$type_group == "cc" )) / 
        as.numeric(difftime(labels_stop ,  labels_start, units="secs"))
      
      #get the SN call rate for the time window of interest
      base_sn_rate <- length(which(calls_select$type_group == "sn" )) / 
        as.numeric(difftime(labels_stop ,  labels_start, units="secs"))
    
    
    i <- 1
    while (i < nrow(individual_select)) {
      #get call rate for each position
      t0 <-
        individual_select$t[i] + individual_select$futurStepDuration[i]
      if (t0 > individual_select$t[nrow(individual_select)]) {break}
      
      arrival_row <- which(individual_select$t == t0)
      if (length(arrival_row) == 0) {
        i <- which(individual_select$t > t0 )[1]
      }else{
        fut_step_dur <- individual_select$futurStepDuration[arrival_row]
        
        if (fut_step_dur < 50) {
          i <- i + 1
        }else{
          
          nwind <- floor(fut_step_dur/10)-1
          
          for (win in 0:nwind) {
            
            calls_in_window <-
              calls_select[which(
                as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t0+tw*win &
                  as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t0+tw*(win+1)
              ) , ]
            calls_in_window <-
              calls_in_window[which(calls_in_window$isCall == 1) , ]
            
            
            
            cc_calls_in_window <-
              calls_select[which(
                as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t0+tw*win &
                  as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t0+tw*(win+1) &
                  calls_select$type_group == "cc"
              ) , ]
            
            sn_calls_in_window <-
              calls_select[which(
                as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t0+tw*win &
                  as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t0+tw*(win+1) &
                  calls_select$type_group == "sn"
              ) , ]
    
            
     all_arrival_points <- bind_rows(all_arrival_points,               
          bind_cols(
            individual_select[arrival_row, ],
            (nrow(calls_in_window)/tw)-base_call_rate,
            "call_rate",
            tw*win
          ))
     all_arrival_points <- bind_rows(all_arrival_points,               
                                     bind_cols(
                                       individual_select[arrival_row, ],
                                       (nrow(cc_calls_in_window)/tw)-base_cc_rate,
                                       "cc_call_rate",
                                       tw*win
                                     ))
     all_arrival_points <- bind_rows(all_arrival_points,               
                                     bind_cols(
                                       individual_select[arrival_row, ],
                                       (nrow(sn_calls_in_window)/tw)-base_sn_rate,
                                       "sn_call_rate",
                                       tw*win
                                     ))
     
    
          }
          
        }
        i <-  arrival_row 
      }
      
    }
       
  }
}

colnames(all_arrival_points)[42:44] <- c("rate", "type", "time_window")
#colnames(all_arrival_points)[40:42] <- c("a", "b", "C")
all_arrival_points$rate <- as.numeric(all_arrival_points$rate)
all_arrival_points$time_window <- as.factor(all_arrival_points$time_window)
all_arrival_points$time_window <- as.numeric(all_arrival_points$time_window)
write.csv(all_arrival_points, "time_spent_in_patch_long.csv")

#all_arrival_points <- read.csv("time_spent_in_patch.csv")

head(all_arrival_points)

table(all_arrival_points$time_window)
as.numeric(quantile (all_arrival_points$time_window, probs = seq(0, 1, 0.1), na.rm= T)[10])
all_arrival_points <- all_arrival_points[which(all_arrival_points$time_window <= 
                                                 as.numeric(quantile (all_arrival_points$time_window, probs = seq(0, 1, 0.1), na.rm= T)[10])),]

ggplot(data = all_arrival_points, aes(x = time_window, y = rate))  + geom_smooth(method = "lm")+
  facet_wrap(~ type)

arrival_times <- seq(0, 100, 10)
for (t in arrival_times) {
print(ggplot(data = all_arrival_points[which(all_arrival_points$pastStepDuration > t & all_arrival_points$pastStepDuration < t+10 ),], aes(x = time_window, y = rate)) + 
  geom_smooth(method = "glm", alpha=0.2, fill = "blue")+ ggtitle(paste(t,"-", t+10))+facet_wrap(~ type) )}


quantile (all_arrival_points$indSpeedPast, probs = seq(0, 1, 0.1), na.rm= T)
quantile (all_arrival_points$pastStepDuration, probs = seq(0, 1, 0.1), na.rm= T)

ggplot(data = all_arrival_points[which(all_arrival_points$indSpeedPas < 0.053418050),], aes(x = time_window, y = rate)) + geom_hline(yintercept=0, linetype="dashed", color = "black", size= 1) + 
        geom_smooth(method = "gam", alpha=0.2, color = "blue", fill = "blue")+ xlab("sec*10") +
        geom_smooth(data = all_arrival_points[which(all_arrival_points$indSpeedPas > 0.221951660),], aes(x = time_window, y = rate), method = "gam", color = "red", fill= "red", alpha=0.2) +
        geom_smooth(data = all_arrival_points[which(all_arrival_points$indSpeedPas > 0.083393879 & all_arrival_points$indSpeedPas < 0.126636001),], aes(x = time_window, y = rate), method = "gam", color = "darkgreen", fill= "darkgreen", alpha=0.2) +
        facet_wrap(~ type) + scale_x_continuous(sec.axis = sec_axis(~ . , name = "Call_rate vs time spent in the patch (red = fast)", breaks = NULL, labels = NULL)) +
        theme_minimal() 


ggplot(data = all_arrival_points[which(all_arrival_points$indSpeedPas < 0.080830568),], aes(x = time_window, y = rate)) + geom_hline(yintercept=0, linetype="dashed", color = "black", size= 1) + 
  geom_smooth(method = "gam", alpha=0.2, fill = "blue")+
  geom_smooth(data = all_arrival_points[which(all_arrival_points$indSpeedPas > 0.123309581),], aes(x = time_window, y = rate), method = "gam", color = "red", fill= "red", alpha=0.2) +
  facet_wrap(~ type) + scale_x_continuous(sec.axis = sec_axis(~ . , name = "Call_rate vs time spent in the patch (red = fast)", breaks = NULL, labels = NULL)) +
  theme_minimal() 



ggplot(data = all_arrival_points, aes(x = time_window, y = rate)) + geom_smooth(method = "gam")+
  facet_wrap(~ type)



all_arrival_points <- all_arrival_points[-which(is.na(all_arrival_points$pastStepDuration)),]
all_arrival_points <- all_arrival_points[which(all_arrival_points$pastStepDuration < 200),]


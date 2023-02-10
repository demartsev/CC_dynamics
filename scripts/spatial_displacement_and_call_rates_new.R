#This is a script for looking at meerkat call rate as a function of spatial displacement.
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

setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics")

#set the displacement distance
distance <- 5

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
      } else{
        fut_step_dur <- individual_select$futurStepDuration[arrival_row]
        
        if (fut_step_dur < 50) {
          i <- i + 1
        } else{
          t1 <- t0 + tw
          t2 <- t1 + tw
          t3 <- t2 + tw
          t4 <- t3 + tw
          
          calls_in_5_window <-
            calls_select[which(
              as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t0 &
                as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t1
            ) , ]
          calls_in_5_window <-
            calls_in_5_window[which(calls_in_5_window$isCall == 1) , ]
          
          calls_in_10_window <-
            calls_select[which(
              as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t1 &
                as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t2
            ) , ]
          calls_in_10_window <-
            calls_in_10_window[which(calls_in_10_window$isCall == 1) , ]
          
          calls_in_15_window <-
            calls_select[which(
              as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t2 &
                as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t3
            ) , ]
          calls_in_15_window <-
            calls_in_15_window[which(calls_in_15_window$isCall == 1) , ]
          
          calls_in_20_window <-
            calls_select[which(
              as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t3 &
                as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t4
            ) , ]
          calls_in_20_window <-
            calls_in_20_window[which(calls_in_20_window$isCall == 1) , ]
          
          
          #number of calls in 1 sec windows
          #individual_select$call_rate[i] <- nrow(calls_in_window)/call_rate_all
          
          #number of CC calls in 1 sec windows
          #individual_select$cc_call_rate[i] <- length(which(calls_in_window$type_group  == "cc")) / call_rate_all
          
          #number of sn calls in 1 sec windows
          #individual_select$sn_call_rate[i] <- length(which(calls_in_window$type_group  == "sn")) / call_rate_sn
          
          summ_point <-
            bind_rows(
              bind_cols(
                individual_select[arrival_row, ],
                (nrow(calls_in_5_window)/tw)-base_call_rate,
                "call_rate",
                tw
              ),
              bind_cols(
                individual_select[arrival_row, ],
                (nrow(calls_in_10_window)/tw) - base_call_rate,
                "call_rate",
                tw*2
              ),
              bind_cols(
                individual_select[arrival_row, ],
                (nrow(calls_in_15_window)/tw) - base_call_rate,
                "call_rate",
                tw*3
              ),
              bind_cols(
                individual_select[arrival_row, ],
                (nrow(calls_in_20_window)/tw) - base_call_rate,
                "call_rate",
                tw*4
              ),
              bind_cols(
                individual_select[arrival_row, ],
                length(which(
                  calls_in_5_window$type_group  == "cc"
                )) / tw - base_cc_rate,
                "cc_call_rate",
                tw
              ),
              bind_cols(
                individual_select[arrival_row, ],
                length(which(
                  calls_in_10_window$type_group  == "cc"
                )) / tw- base_cc_rate,
                "cc_call_rate",
                tw*2
              ),
              bind_cols(
                individual_select[arrival_row, ],
                length(which(
                  calls_in_15_window$type_group  == "cc"
                )) / tw- base_cc_rate,
                "cc_call_rate",
                tw*3
              ),
              bind_cols(
                individual_select[arrival_row, ],
                length(which(
                  calls_in_20_window$type_group  == "cc"
                )) / tw - base_cc_rate,
                "cc_call_rate",
                tw*4
              ),
              bind_cols(
                individual_select[arrival_row, ],
                length(which(
                  calls_in_5_window$type_group  == "sn"
                )) / tw - base_sn_rate,
                "sn_call_rate",
                tw
              ),
              bind_cols(
                individual_select[arrival_row, ],
                length(which(
                  calls_in_10_window$type_group  == "sn"
                )) / tw - base_sn_rate,
                "sn_call_rate",
                tw*2
              ),
              bind_cols(
                individual_select[arrival_row, ],
                length(which(
                  calls_in_15_window$type_group  == "sn"
                )) / tw - base_sn_rate,
                "sn_call_rate",
                tw*3
              ),
              bind_cols(
                individual_select[arrival_row, ],
                length(which(
                  calls_in_20_window$type_group  == "sn"
                )) / tw - base_sn_rate,
                "sn_call_rate",
                tw*4
              )
            )
          i <-  arrival_row
          all_arrival_points <-
            bind_rows(all_arrival_points, summ_point)
          
         
        }
      }
      
    }
    
  }
  
}

colnames(all_arrival_points)[39:41] <- c("rate", "type", "time_window")
all_arrival_points$rate <- as.numeric(all_arrival_points$rate)
all_arrival_points$time_window <- as.factor(all_arrival_points$time_window)

ggplot(data = all_arrival_points, aes(x = time_window, y = rate)) + geom_jitter(alpha = 0.2) + geom_boxplot()+
  facet_wrap(~ type)
ggplot(data = all_arrival_points[which(all_arrival_points$pastStepDuration < 10),], aes(x = time_window, y = rate)) + geom_jitter(alpha = 0.2) + geom_boxplot()+
  facet_wrap(~ type)



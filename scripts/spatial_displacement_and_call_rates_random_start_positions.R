#This is a script for looking at meerkat call rate after arriving at a new foraging patch
#the script generates several time windows after arrival to the patch and calculates call rate in each one of the windows.
#It uses already discretisized movement data data and cross references it with the audio recording
#all analysis is done on individual level

# the script uses two data objects:
  ###### Descretizised meerkat movement data. 
 # pastStepDuration = time since last patch
 # futurStepDuration = time until next patch
 # indSpeedFutur = speed until next patch
 # indHeadPast = speed until arriving to current patch (in previous patch)
 # t = GPS time


  ###### Meerkat call data 
 #ind = meerkat ID 
 #isCall = index for call and non call labels. 
 #type_group = stable call types
 #date = day of recording
 #t0GPS_UTC = GPS synced time stamp




#load libraries (some are just leftovers from the larger script
#but I lost track which ones are actually needed for this bit)

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


#### SETTINGS AND DATA LOADING###
#set time window in which call rate will be calculated
tw <- 10

#load movement data object
load("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics/discreet_tracks_data/spatialMetrics_10m.RData")
#load audio data object
load("call_data.RData")


##########################################################################################################

#get all dates for which spatial data is available
dates <- unique(spatialMetrics$date)
#remove 2021 data since we are not using it in the analysis
dates <- dates[-grep(21, dates)]

#start an empty data frame to collect data summary at the end
all_arrival_points <- data.frame()

  ####This is a loop for going through single days in the data### 
for (date in 1:length (dates)) {
  #date <- 1
  #get one day of data
  date_select <-
    spatialMetrics[which(spatialMetrics$date == dates[date]), ]
  #get the IDs of all animals observed on that day
  individuals <- unique(date_select$indUniqID)
  
    ### This is a loop for ging through each individual animal on a selected day
  for (individual in individuals) {
    
    #individual <- individuals[1]
    
    #get the movement data for selected individual
    individual_select <-
      date_select[which(date_select$indUniqID == individual) ,]
    
    #get the CODE ID for the selected individual
    ind_code <-
      allIndInfo[which(allIndInfo$uniqueID == individual) , "code"]
    
    #get the date of interest in a format compatible with the date format in audio data
    day_form <- gsub("[[:punct:]]", "", dates[date])
    
    #select all calls of the individual of interest on the day of interest
    calls_select <-
      call_data[which(as.character(call_data$date) == day_form &
                        call_data$ind == ind_code) ,]
    
    #get the time stamp when labeling of calls  started
    labels_start <-
      calls_select[which(calls_select$entryName == "start") , "t0GPS_UTC"][1]
    
    #get the time hen labeling of calls ended
    labels_stop  <-
      calls_select[which(calls_select$entryName == "stop") , "t0GPS_UTC"][length(calls_select[which(calls_select$entryName == "stop") , "t0GPS_UTC"])]
    
    
    #filter the spatial data to keep the time period for which we have labeled audio data
    #between labels_start and labels_stop
    individual_select <-
      individual_select[which(
        individual_select$t > as.POSIXct (labels_start, tz = "UTC") &
          individual_select$t <  as.POSIXct (labels_stop, tz = "UTC")
      ),]
    
    #if there is no call data available for this particular individual skip to the next one
    if (nrow(individual_select) == 0) {
      next
    }
    
    
    
    #select data points for which future step is available
    individual_select <-
      individual_select[which(!is.na(individual_select$futurStepDuration)) ,]
    
    
    #if there are no positions with future step, skip to the next individual
    if (nrow(individual_select) == 0) {
      next
    }
    
    
     ### getting individual baseline call rates for the individual on the day of interest
     
     # #get the general base call rate -  calls per sec
     # #divide the total number of calls by the time between label_stop and label_start 
     #  base_call_rate <- length(which(calls_select$isCall == 1)) / 
     #    as.numeric(difftime(labels_stop ,  labels_start, units="secs"))
     #  
     #  #get the Close calls rate  - cc calls per sec
     #  base_cc_rate <- length(which(calls_select$type_group == "cc" )) / 
     #    as.numeric(difftime(labels_stop ,  labels_start, units="secs"))
     #  
     #  #get the Short Note call rate - sn calls per sec
     #  base_sn_rate <- length(which(calls_select$type_group == "sn" )) / 
     #    as.numeric(difftime(labels_stop ,  labels_start, units="secs"))
     
   
      row_sample <- sample(1:nrow(individual_select), nrow(individual_select)/100, replace = F)
      for (i in row_sample) {
       
       #set the start time for calculating call rates
      #find the time point when the individual arrived to a new patch
      #take the time at row i  - individual_select$t[i], 
      #get the time it spent in the current patch  -  individual_select$futurStepDuration[i]
      #set t0 as the time the meerkat arrived to a new patch
       t0 <-
        individual_select$t[i] + individual_select$futurStepDuration[i] 
      
       if (t0 > individual_select$t[nrow(individual_select)]) {next} #if t0 goes beyond the relevant data window break the loop
      
       
       #get the row number of to
      arrival_row <- which(individual_select$t == t0)
      
       #if there is no GPS data at t0 get the next available time point 
      if (length(arrival_row) == 0) {next}
       
        #get the the duration of time individual spent in this patch
        fut_step_dur <- individual_select$futurStepDuration[arrival_row]
        
        #avoid very quick passes through the patch by setting min time in patch to 30 sec
        if (fut_step_dur < 30) {next}
             
          #calculate number of time windows in the current patch
          #the time in patch is different so the number of window is also different
             
             #tw is set to 10 sec at the beginning of the script
          nwind <- floor(fut_step_dur/tw)-1
          
          #loop through all 10 second windows in the patch and calculate call rates (calls/sec)
          for (win in 0:nwind) {
            
            # number of calls in window
            calls_in_window <-
              calls_select[which(
                as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t0+tw*win &
                  as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t0+tw*(win+1)
              ) , ]
            #here we just double check that we only count calls and not some other labels (calls_in_window$isCall == 1)
            calls_in_window <-
              calls_in_window[which(calls_in_window$isCall == 1) , ]
            
            
            # number of Close-calls (cc) in the window
            cc_calls_in_window <-
              calls_select[which(
                as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t0+tw*win &
                  as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t0+tw*(win+1) &
                  calls_select$type_group == "cc"
              ) , ]
            
            # number of Short Note calls (sn) in the window
            sn_calls_in_window <-
              calls_select[which(
                as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t0+tw*win &
                  as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t0+tw*(win+1) &
                  calls_select$type_group == "sn"
              ) , ]
    
       #####combine all call rate data together ####
            
    call_rates <- data.frame( t(c(individual_select[1, ], nrow(calls_in_window)/tw, "call_rate", tw*win, recursive = TRUE)))
    cc_call_rates <-  data.frame( t(c(individual_select[1, ], nrow(cc_calls_in_window)/tw, "cc_call_rate", tw*win, recursive = TRUE)))
    sn_call_rates <-  data.frame( t(c(individual_select[1, ], nrow(sn_calls_in_window)/tw, "sn_call_rate", tw*win, recursive = TRUE))) 
    all_arrival_points <- bind_rows(all_arrival_points, call_rates, cc_call_rates, sn_call_rates)
            
      }
      
    }
       
  }
}

#rename columns 
colnames(all_arrival_points)[42:44] <- c("rate", "type", "time_window")

#change data format for plotting
all_arrival_points$rate <- as.numeric(all_arrival_points$rate)
#all_arrival_points$time_window <- as.factor(all_arrival_points$time_window)
all_arrival_points$time_window <- as.numeric(all_arrival_points$time_window)

   ##write data into CSV file
   write.csv(all_arrival_points, "time_spent_in_patch_30sec_pass.csv")

   ##load data from previously saved CSV
      all_arrival_points <- read.csv("time_spent_in_patch_30sec_pass.csv")

#remove overall call rate as it is irrelevant
all_arrival_points <- all_arrival_points[which(all_arrival_points$type !="call_rate"),]   

#length (table(all_arrival_points$indUniqID, all_arrival_points$t))

#df <- all_arrival_points %>% group_by(indUniqID) %>% group_by(t) %>% tally()
#check data structure
#head(all_arrival_points)

# standardize (Z score) call rates per individual per day
all_arrival_points <- all_arrival_points %>% group_by(date, indUniqID, type) %>% mutate(rate_stn = scale(rate)) %>% ungroup()


###PLOTS####

#removing very long stays (last decile)
all_arrival_points <- all_arrival_points[which(all_arrival_points$time_window <= 
                                                 as.numeric(quantile (all_arrival_points$time_window, probs = seq(0, 1, 0.1), na.rm= T)[10])),]

#get basic linear regression for overall effects of time in spent in patch (x) and call rate (y)
ggplot(data = all_arrival_points, aes(x = time_window, y = rate_stn))  + geom_smooth(method = "gam")+
  facet_wrap(~ type)

 
#calculate speed of arrival to the currents patch. How fast the individual approached the current patch?
#taking the 30% ranges for slow, medium and fast arrivals
slow <- as.numeric(quantile (as.numeric(all_arrival_points$indSpeedPast), probs = seq(0, 1, 0.1), na.rm= T)[2])
fast <- as.numeric(quantile (as.numeric(all_arrival_points$indSpeedPast), probs = seq(0, 1, 0.1), na.rm= T)[10])
med_low <-  as.numeric(quantile (as.numeric(all_arrival_points$indSpeedPast), probs = seq(0, 1, 0.1), na.rm= T)[4])
med_high <- as.numeric(quantile (as.numeric(all_arrival_points$indSpeedPast), probs = seq(0, 1, 0.1), na.rm= T)[8])
 


#calculate speed of arrival to the currents patch. How fast the individual approached the current patch?
#taking the 30% ranges for slow, medium and fast arrivals
#slow <- 0.05
#fast <- 0.3
#med_low <-  0.1
#med_high <- 0.

 #adding category for Speed of Arrival
all_arrival_points[which(all_arrival_points$indSpeedPast < slow),"SOA"] <- "slow"
all_arrival_points[which(all_arrival_points$indSpeedPast > fast), "SOA"] <- "fast"
all_arrival_points[which(all_arrival_points$indSpeedPast > med_low & all_arrival_points$indSpeedPast < med_high), "SOA"] <- "medium"

  #removing points with NA - speed of arrival in the intermediates
all_arrival_points <- all_arrival_points[which(!is.na(all_arrival_points$SOA)), ]

 


#this is the main plot showing differences in call rate (y) as a function of time spent in patch (x) 
facet_names <- list(
  'cc_call_rate'="Close calls",
  'sn_call_rate'="Short note calls",
  'call_rate' = "All calls")
facet_labeller <- function(variable,value){
  return(facet_names[value])}


ggplot(data = all_arrival_points, aes(x = time_window, y = rate, linetype = SOA)) + #geom_hline(yintercept=0, linetype="dashed", color = "black", size= 1) + 
  geom_smooth(color = "black", method = "gam", alpha=0.2, level=0.95)+ xlab("sec") + ylab("call rate (calls/sec)")  + facet_wrap(~ type, labeller=facet_labeller) + 
   theme_minimal(base_size = 20) + theme( legend.position="top", legend.title = element_blank())

ggplot(data = all_arrival_points, aes(x = time_window, y = rate, linetype = type)) + #geom_hline(yintercept=0, linetype="dashed", color = "black", size= 1) + 
  geom_smooth(color = "black", method = "gam", alpha=0.2, level=0.95)+ xlab("sec") + ylab("stn call rate (calls/sec)")  + facet_wrap(~ SOA) + 
  theme_minimal(base_size = 20) + theme( legend.position="top", legend.title = element_blank())



ggplot(data = all_arrival_points, aes(x = time_window, y = rate_stn, linetype = SOA)) + #geom_hline(yintercept=0, linetype="dashed", color = "black", size= 1) + 
  geom_smooth(color = "black", method = "gam", alpha=0.2, level=0.95)+ xlab("sec") + ylab("call rate (calls/sec)")  + facet_wrap(~ type, labeller=facet_labeller) + 
  theme_minimal(base_size = 20) + theme( legend.position="top", legend.title = element_blank())

ggplot(data = all_arrival_points, aes(x = time_window, y = rate_stn, linetype = type)) + #geom_hline(yintercept=0, linetype="dashed", color = "black", size= 1) + 
  geom_smooth(color = "black", method = "gam", alpha=0.2, level=0.95)+ xlab("sec") + ylab("stn call rate (calls/sec)")  + facet_wrap(~ SOA) + 
  theme_minimal(base_size = 20) + theme( legend.position="top", legend.title = element_blank())



all_arrival_points_cc <- all_arrival_points[which(all_arrival_points$type == "cc_call_rate"),]
all_arrival_points_cc[ ,46] <-pull(all_arrival_points_cc[ ,46])
all_arrival_points_sn <- all_arrival_points[which(all_arrival_points$type == "sn_call_rate"),]
all_arrival_points_sn[ ,46] <-pull(all_arrival_points_sn[ ,46])

#### STATS ####
library(glmmTMB)
library(sjPlot)
library(modelsummary)
model_CC <- glmmTMB(data = all_arrival_points_cc , rate_stn ~ time_window*SOA + (1|indUniqID))
summary(model_CC)

model_sn <- glmmTMB(data = all_arrival_points_sn , rate_stn ~ time_window*SOA + (1|indUniqID))
summary(model_sn)

plot_model(model_CC, type = "eff")
tab_model(model_CC, model_sn, dv.labels = c("Cloce calls", "Short note calls"))

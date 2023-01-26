#This is a script for looking at meerkat call rate as a function of spatial displacement.
#It uses Baptiste's track discretization data and cross references it with the audio

setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics/")

#set the displacement distance 
distance <- 5

#load movement data object
load(paste("discreet_tracks_data.spatialMetrics_", distance, "m.RData", sep = ""))

#load audio data
call_data <- read.csv("foc_calls_resolved.csv")

#get all dates
dates <- unique(spatialMetrics$date)
 
for(date in 1:length (dates)) { 
  date_select <- spatialMetrics[which(spatialMetrics$date == dates[date]), ]
  individuals <- unique(date_select$indIdx)
  for (individual in individuals) { 
    individual_select <- date_select[which(date_select$indIdx == individual) ,]
    ind_code <- allIndInfo[which(allIndInfo$idx == individual) , "code"]
    calls_select <- call_data[which(call_data$date == dates[date] & call_data$ind == individual) ,]
    
  }
  
}


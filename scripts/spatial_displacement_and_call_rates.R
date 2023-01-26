#This is a script for looking at meerkat call rate as a function of spatial displacement.
#It uses Baptisite's track discritisation data and cross references it with the audio

setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics/discreet_tracks_data")

#set the displacement distance 
distance <- 5

#load respective data object
load(paste("spatialMetrics_", distance, "m.RData", sep = ""))
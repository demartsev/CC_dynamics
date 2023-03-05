#This is a script for looking at meerkat call rate as a function of spatial displacement.
#It uses Baptiste's track discretization data and cross references it with the audio
#the call rate is calculated from the time or arrival to a patch and along few equal size windows 
#while the individual is in the patch
library(ggplot2)
library(lubridate)
library(plotly)
library(viridis)
library(ggExtra)
library(cowplot)
library(ggpubr)
library(akima)
library(latticeExtra) 
library(mgcv)


setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics")
all_points <- read.csv("call_rates_5m_displacement_5sec_tw.csv")
all_points <- all_points[which(all_points$pastStepDuration < 200), ]
all_points <- all_points[which(all_points$futurStepDuration < 200), ]

#all_points <- all_points[sample(nrow(all_points), 50000), ]
#all_points_red <- all_points[sample(nrow(all_points), 250), ]

all_points <- all_points[-which(is.na(all_points$cc_call_rate)) ,]

s = interp(y = all_points[ , "pastStepDuration" ], 
           x = all_points[ , "futurStepDuration" ],
           z = all_points[ , "cc_call_rate" ], duplicate = "mean")


 #fit GAM to steptime data.
mod <- gam(cc_call_rate ~ te(futurStepDuration) + te(pastStepDuration) + ti(futurStepDuration, pastStepDuration), data=all_points)
 # make a sequence of values for each variable that goes from their minima to their maxima
futurStepDuration.seq <- seq(min(all_points$futurStepDuration, na.rm=TRUE), max(all_points$futurStepDuration, na.rm=TRUE), length=500)
pastStepDuration.seq <- seq(min(all_points$pastStepDuration, na.rm=TRUE), max(all_points$pastStepDuration, na.rm=TRUE), length=500)
 # a function that will generate predictions
predfun <- function(x,y){
  newdat <- data.frame(futurStepDuration = x, pastStepDuration=y)
  predict(mod, newdata=newdat)
}
 #apply the prediction function to the sequences of data
fit <- outer(futurStepDuration.seq, pastStepDuration.seq, Vectorize(predfun))

plot_ly() %>% 
   add_surface(x = ~futurStepDuration.seq, y = ~pastStepDuration.seq, z = t(fit)) 

#%>% 
#  add_markers(x = ~all_points_red$futurStepDuration, y=all_points_red$pastStepDuration, z=all_points_red$cc_call_rate)

#fit GAM to stepspeed data.
mod <- gam(cc_call_rate ~ te(indSpeedFutur) + te(indSpeedPast) + ti(indSpeedFutur, indSpeedPast), data=all_points)
# make a sequence of values for each variable that goes from their minima to their maxima
indSpeedFutur.seq <- seq(min(all_points$indSpeedFutur, na.rm=TRUE), max(all_points$indSpeedFutur, na.rm=TRUE), length=500)
indSpeedPast.seq <- seq(min(all_points$indSpeedPast, na.rm=TRUE), max(all_points$indSpeedPast, na.rm=TRUE), length=500)
# a function that will generate predictions
predfun <- function(x,y){
  newdat <- data.frame(indSpeedFutur = x, indSpeedPast=y)
  predict(mod, newdata=newdat)
}
#apply the prediction function to the sequences of data
fit <- outer(indSpeedFutur.seq, indSpeedPast.seq, Vectorize(predfun))

plot_ly() %>% 
  add_surface(x = ~indSpeedFutur.seq, y = ~indSpeedPast.seq, z = t(fit)) 
#%>% 
#  add_markers(x = ~all_points_red$indSpeedFutur, y=all_points_red$indSpeedPast, z=all_points_red$cc_call_rate)

####################   Do #the same but for SN

#fit GAM to steptime data.
mod <- gam(sn_call_rate ~ te(futurStepDuration) + te(pastStepDuration) + ti(futurStepDuration, pastStepDuration), data=all_points)
# make a sequence of values for each variable that goes from their minima to their maxima
futurStepDuration.seq <- seq(min(all_points$futurStepDuration, na.rm=TRUE), max(all_points$futurStepDuration, na.rm=TRUE), length=500)
pastStepDuration.seq <- seq(min(all_points$pastStepDuration, na.rm=TRUE), max(all_points$pastStepDuration, na.rm=TRUE), length=500)
# a function that will generate predictions
predfun <- function(x,y){
  newdat <- data.frame(futurStepDuration = x, pastStepDuration=y)
  predict(mod, newdata=newdat)
}
#apply the prediction function to the sequences of data
fit <- outer(futurStepDuration.seq, pastStepDuration.seq, Vectorize(predfun))

plot_ly() %>% 
  add_surface(x = ~futurStepDuration.seq, y = ~pastStepDuration.seq, z = t(fit)) 

mod <- gam(sn_call_rate ~ te(indSpeedFutur) + te(indSpeedPast) + ti(indSpeedFutur, indSpeedPast), data=all_points)
# make a sequence of values for each variable that goes from their minima to their maxima
indSpeedFutur.seq <- seq(min(all_points$indSpeedFutur, na.rm=TRUE), max(all_points$indSpeedFutur, na.rm=TRUE), length=500)
indSpeedPast.seq <- seq(min(all_points$indSpeedPast, na.rm=TRUE), max(all_points$indSpeedPast, na.rm=TRUE), length=500)
# a function that will generate predictions
predfun <- function(x,y){
  newdat <- data.frame(indSpeedFutur = x, indSpeedPast=y)
  predict(mod, newdata=newdat)
}
#apply the prediction function to the sequences of data
fit <- outer(indSpeedFutur.seq, indSpeedPast.seq, Vectorize(predfun))

plot_ly() %>% 
  add_surface(x = ~indSpeedFutur.seq, y = ~indSpeedPast.seq, z = t(fit))






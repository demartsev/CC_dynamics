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

setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics")

#set the displacement distance 
distance <- 10

#set time window 
tw <- 5

#load movement data object
load(paste("discreet_tracks_data/spatialMetrics_", distance, "m.RData", sep = ""))


#load audio data
call_data <- read.csv("foc_calls_resolved.csv")

#get all dates
dates <- unique(spatialMetrics$date)
#remove 2021 data
dates <- dates[-grep(21, dates)]
 

all_points <- data.frame()

for(date in 1:length (dates)) { 
  #date <- 1
   #get one day of data
  date_select <- spatialMetrics[which(spatialMetrics$date == dates[date]), ]
  #get all animals observed on that day
  individuals <- unique(date_select$indUniqID)
  for (individual in individuals) { 
    #individual <- individuals[1]
    #get one animal movement data
    individual_select <- date_select[which(date_select$indUniqID == individual) ,]
    #get the CODE ID for the selected animals
    ind_code <- allIndInfo[which(allIndInfo$uniqueID == individual) , "code"]
    #get the date in a format compatible with the audio data
    day_form <- gsub("[[:punct:]]", "", dates[date])
    #select the calls of the meerkat of interest on the day of interest
    calls_select <- call_data[which(as.character(call_data$date) == day_form & call_data$ind == ind_code) ,]
   
     #get the time when labeling started
    labels_start <- calls_select[which(calls_select$entryName == "start") , "t0GPS_UTC"][1]
     #get the time hen labeling ended
    labels_stop  <- calls_select[which(calls_select$entryName == "stop") , "t0GPS_UTC"][length(calls_select[which(calls_select$entryName == "stop") , "t0GPS_UTC"])]
    
    
    #filter positions for which we have labeled calls
    individual_select <- individual_select[which(individual_select$t > as.POSIXct (labels_start, tz = "UTC") & 
                                                 individual_select$t <  as.POSIXct (labels_stop, tz = "UTC")),]
     #if there is no call data skip to the next individual
     if (nrow(individual_select) == 0) {next}
    
    #select positions with past step
    individual_select <- individual_select[which(!is.na(individual_select$pastStepDuration)) ,]
    
    #select positions with future step
    individual_select <- individual_select[which(!is.na(individual_select$futurStepDuration)) ,]
    
    
    #if there is no call data skip to the next individual
    if (nrow(individual_select) == 0) {next}
    
    #calculate past/future
    #hist(individual_select$pastStepDuration)
    #hist(individual_select$futurStepDuration)
    
    #individual_select$stability <- individual_select$pastStepDuration / individual_select$futurStepDuration
    
    
 #  #getting individual baseline call rates
 #  #get the base call rate for the time window of interest
 #  base_call_rate <- length(which(calls_select$isCall == 1)) / 
 #    as.numeric(difftime(labels_stop ,  labels_start, units="secs"))
 #  
 #  #get the CC call rate for the time window of interest
 #  base_cc_rate <- length(which(calls_select$type_group == "cc" )) / 
 #    as.numeric(difftime(labels_stop ,  labels_start, units="secs"))
 #  
 #  #get the SN call rate for the time window of interest
 #  base_sn_rate <- length(which(calls_select$type_group == "sn" )) / 
 #    as.numeric(difftime(labels_stop ,  labels_start, units="secs"))
 #  
 
   ################################################################################
    #getting random intervals of duration tw 
    
    #generating random time points within the recorded interval
    rand_times <- as.POSIXct(labels_start, tz = "UTC") + runif(n=5000, min=0, 
                                              max =   as.numeric(difftime(labels_stop ,  labels_start, units="secs"))-tw)
    all_rand_widows <- c()
    all_cc_calls_in_rand_window <- c()
    all_sn_calls_in_rand_window  <- c()
    
    for (j in 1:length(rand_times)) { 
     
      #get call rate for each position
      t0r <- rand_times[j]
      t1r <- rand_times[j] + tw
      
      calls_in_rand_window <- calls_select[which(as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t0r & 
                                              as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t1r) , ]
      calls_in_rand_window <- calls_in_rand_window[which(calls_in_rand_window$isCall == 1) , ]
      
      
      all_cc_calls_in_rand_window <- c(all_cc_calls_in_rand_window, 
                                      length(which(calls_in_rand_window$type_group == "cc")))
      all_sn_calls_in_rand_window <- c(all_sn_calls_in_rand_window, 
                                       length(which(calls_in_rand_window$type_group == "sn")))
      
      all_rand_widows <- c(all_rand_widows, nrow(calls_in_rand_window))
      }
    
    call_rate_all <- mean(all_rand_widows)
    call_rate_all <- mean(all_cc_calls_in_rand_window)
    call_rate_sn <- mean(all_sn_calls_in_rand_window)
 ################################################################################################################
    
    for (i in 1:nrow(individual_select)) { 
      
      #get call rate for each position
      t0 <- individual_select$t[i] 
      t1 <- individual_select$t[i] + tw
      
    calls_in_window <- calls_select[which(as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") > t0 & 
                                          as.POSIXct(calls_select$t0GPS_UTC,  tz = "UTC") < t1) , ]
    calls_in_window <- calls_in_window[which(calls_in_window$isCall == 1) , ]
    
    
    #number of calls in 1 sec windows
    individual_select$call_rate[i] <- nrow(calls_in_window)/call_rate_all
    
    #number of CC calls in 1 sec windows
    individual_select$cc_call_rate[i] <- length(which(calls_in_window$type_group  == "cc")) / call_rate_all
    
    #number of sn calls in 1 sec windows
    individual_select$sn_call_rate[i] <- length(which(calls_in_window$type_group  == "sn")) / call_rate_sn
    
   
    
     
    }
    all_points <- rbind(all_points, individual_select) 
    

  }
  
}
all_points_1 <- all_points
#write.csv(all_points, "call_rates_5m_displacement_5sec_tw.csv")
all_points <- read.csv("call_rates_5m_displacement_5sec_tw.csv")
all_points <- all_points[which(all_points$pastStepDuration < 300), ]
all_points <- all_points[which(all_points$futurStepDuration < 300), ]


 #plot speed vs past step duration
 ggplot(data = all_points, aes(x = pastStepDuration, y = indSpeedPast)) + geom_smooth(method = "lm")
 past_step_cutoff <- quantile(all_points$pastStepDuration, probs = seq(0, 1, 0.1))[2]

all_points <- all_points[-which(is.na(all_points$cc_call_rate)) ,]


### calculating call rates by time of arrival ####
####################################################

#getting 10% of the futurStepDuration
quantile(all_points$futurStepDuration, probs = seq(0, 1, 0.1))[2]

quantile(all_points$futurStepDuration, probs = seq(0, 1, 0.1))

#getting 90% of the futurStepDuration
quantile(all_points[which(all_points$futurStepDuration<22), "pastStepDuration"], probs = seq(0, 1, 0.1))[10]

#leaving fast < 22 sec 
p <- ggplot(data = all_points[which(all_points$futurStepDuration < 22), ] , aes(x = pastStepDuration, y = cc_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw() + ggtitle("CC rate when leaving fast < 22 sec") + 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  xlim(0, quantile(all_points[which(all_points$futurStepDuration < 22), "pastStepDuration"], probs = seq(0, 1, 0.1))[10])
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = all_points[which(all_points$futurStepDuration < 22), ], 
               aes(x = pastStepDuration))
p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top") 
ggdraw(p1)


#leaving slow > 20 sec
b <- ggplot(data = all_points[which(all_points$futurStepDuration > 22), ] , aes(x = pastStepDuration, y = cc_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw() + ggtitle("CC rate when leaving slow > 22 sec") + 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  xlim(0, quantile(all_points[which(all_points$futurStepDuration > 22), "pastStepDuration"], probs = seq(0, 1, 0.1))[10])
xdens <- axis_canvas(b, axis = "x") +
  geom_density(data = all_points[which(all_points$futurStepDuration > 22), ], 
               aes(x = pastStepDuration))
b1 <- insert_xaxis_grob(b, xdens, grid::unit(.2, "null"), position = "top")
ggdraw(b1)


##########################################################################################
#SN plots
#########################################################################################


#arriving and staying for at least 30 sec (no fast departures)
p <- ggplot(data = all_points[which(all_points$futurStepDuration > 22), ] , aes(x = pastStepDuration, y = sn_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw() + ggtitle("sn rate when leaving slow > 22 sec") + 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  xlim(0, quantile(all_points[which(all_points$futurStepDuration > 22), "pastStepDuration"], probs = seq(0, 1, 0.1))[10])
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = all_points[which(all_points$futurStepDuration > 22), ], 
               aes(x = pastStepDuration))
p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
ggdraw(p1)


#arriving and leaving within 20 sec (just passing by)
b <- ggplot(data = all_points[which(all_points$futurStepDuration < 22), ] , aes(x = pastStepDuration, y = sn_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw() + ggtitle("sn rate when leaving fast < 22 sec") + 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  xlim(0, quantile(all_points[which(all_points$futurStepDuration < 22), "pastStepDuration"], probs = seq(0, 1, 0.1))[10])
xdens <- axis_canvas(b, axis = "x") +
  geom_density(data = all_points[which(all_points$futurStepDuration < 22), ], 
               aes(x = pastStepDuration))
b1 <- insert_xaxis_grob(b, xdens, grid::unit(.2, "null"), position = "top")
ggdraw(b1)

gridExtra::grid.arrange(b1, p1)


####################################################
### calculating call rates by speed of arrival ####
####################################################


all_points <- read.csv("call_rates_5m_displacement.csv")
all_points <- all_points[which(all_points$pastStepDuration < 300), ]
all_points <- all_points[which(all_points$futurStepDuration < 300), ]

#calculate passing time between each point
all_points$pass_speed <- apply(all_points[ , c("indSpeedPast", "indSpeedFutur")] , MARGIN = 1 , FUN = mean )


#getting speed threshold for detecting fast movement (current y set at 9th decile)
speed_tresh <- quantile(all_points$pass_speed, probs = seq(0, 1, 0.1))[10]

 #### PLOTTING CC CALL RATES ####
#slow passing between segments
p <- ggplot(data = all_points[which(all_points$indSpeedPast < speed_tresh & all_points$indSpeedFutur < speed_tresh), ] , aes(x = pastStepDuration, y = cc_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw() + 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  ggtitle("CC rate when arriving slow and leaving slow")
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = all_points[which(all_points$indSpeedPast < speed_tresh & all_points$indSpeedFutur < speed_tresh), ], 
               aes(x = pastStepDuration))
a1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top") 
#ggdraw(a1)


#fast passing between segments
p <- ggplot(data = all_points[which(all_points$indSpeedPast > speed_tresh & all_points$indSpeedFutur > speed_tresh), ] , aes(x = pastStepDuration, y = cc_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw()+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  ggtitle("CC rate when arriving fast and leaving fast")
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = all_points[which(all_points$indSpeedPast > speed_tresh & all_points$indSpeedFutur> speed_tresh), ], 
               aes(x = pastStepDuration))
b1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top") 
#ggdraw(b1)


#arriving slow but leaving fast
p <- ggplot(data = all_points[which(all_points$indSpeedPast < speed_tresh & all_points$indSpeedFutur > speed_tresh), ] , aes(x = pastStepDuration, y = cc_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + xlim(0, 300)  + theme_bw()+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  ggtitle("CC rate when arriving slow and leaving fast")
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = all_points[which(all_points$indSpeedPast < speed_tresh & all_points$indSpeedFutur > speed_tresh), ], 
               aes(x = pastStepDuration))
c1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top") 
#ggdraw(c1)


#arriving fast but leaving slow
p <- ggplot(data = all_points[which(all_points$indSpeedPast > speed_tresh & all_points$indSpeedFutur < speed_tresh), ] , aes(x = pastStepDuration, y = cc_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw() + 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  ggtitle("CC rate when arriving fast and leaving slow")
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = all_points[which(all_points$indSpeedPast > speed_tresh & all_points$indSpeedFutur < speed_tresh), ], 
               aes(x = pastStepDuration))
d1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top") 
#ggdraw(d1)


#### PLOTTING SN CALL RATES ####
#slow passing between segments
p <- ggplot(data = all_points[which(all_points$indSpeedPast < speed_tresh & all_points$indSpeedFutur< speed_tresh), ] , aes(x = pastStepDuration, y = sn_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw()+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  ggtitle("SN rate when arriving slow and leaving slow")
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = all_points[which(all_points$indSpeedPast < speed_tresh & all_points$indSpeedFutur < speed_tresh), ], 
               aes(x = pastStepDuration))
e1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top") 
#ggdraw(e1)


#fast passing between segments
p <- ggplot(data = all_points[which(all_points$indSpeedPast > speed_tresh & all_points$indSpeedFutur > speed_tresh), ] , aes(x = pastStepDuration, y = sn_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw()+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  ggtitle("SN rate when arriving fast and leaving fast")
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = all_points[which(all_points$indSpeedPast > speed_tresh & all_points$indSpeedFutur > speed_tresh), ], 
               aes(x = pastStepDuration))
f1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top") 
#ggdraw(f1)


#arriving slow but leaving fast
p <- ggplot(data = all_points[which(all_points$indSpeedPast < speed_tresh & all_points$indSpeedFutur> speed_tresh), ] , aes(x = pastStepDuration, y = sn_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw()+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  ggtitle("SN rate when arriving slow and leaving fast")
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = all_points[which(all_points$indSpeedPast < speed_tresh & all_points$indSpeedFutur > speed_tresh), ], 
               aes(x = pastStepDuration))
g1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top") 
#ggdraw(g1)


#arriving fast but leaving slow
p <- ggplot(data = all_points[which(all_points$indSpeedPast > speed_tresh & all_points$indSpeedFutur < speed_tresh), ] , aes(x = pastStepDuration, y = sn_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw()+ 
  geom_hline(yintercept=1, linetype="dashed", color = "red", size=1) + 
  ggtitle("SN rate when arriving fast and leaving slow")
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = all_points[which(all_points$indSpeedPast > speed_tresh & all_points$indSpeedFutur < speed_tresh), ], 
               aes(x = pastStepDuration))
h1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top") 
#ggdraw(h1)

#build one plot with all call rates
ggarrange(a1, e1, b1, f1, c1, g1, d1, h1, 
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
          ncol = 2, nrow = 4)

all_points$mean_ind_speed <- rowMeans(all_points[ , c("indSpeedPast", "indSpeedFutur")], na.rm = T)

ggplot(data = all_points , aes(x = indSpeedFutur, y = sn_call_rate)) + 
  geom_smooth(method = "gam", colour="black") + theme_bw() 

ggplot(data = all_points , aes(x = indSpeedFutur, y = cc_call_rate)) + #xlim(0, 2) +
  geom_smooth(method = "lm", colour="black") + theme_bw() 


ggplot(data = all_points , aes(x = indSpeedPast, y = sn_call_rate)) + 
  geom_smooth(method = "lm", colour="black") + theme_bw() 

ggplot(data = all_points , aes(x = indSpeedPast, y = cc_call_rate)) + #xlim(0, 2) +
  geom_smooth(method = "gam", colour="black") + theme_bw() 


#plot combining CC and Sn call rates as a function of future speed
ggplot(data = all_points , aes(x = indSpeedFutur, y = sn_call_rate)) + 
  geom_smooth(method = "gam", colour="#C4961A") +  
  geom_smooth(aes(x = indSpeedFutur, y = cc_call_rate*8), method = "gam", colour="#00AFBB") +
  # Custom the Y scales:
  scale_y_continuous(
    
    # Features of the first axis
    name = "SN Call Rate",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./8, name="CC Call Rate")
  ) +
  xlim(0, 3) + geom_hline(yintercept=c(1, 8), linetype="dotted",size = 2,  color = c("#C4961A", "#00AFBB")) + 
  xlab("Movement speed (m/sec)") + theme_bw() + theme(text = element_text(size = 20))   



#plot combining CC and Sn call rates as a function of mean speed
ggplot(data = all_points , aes(x = mean_ind_speed, y = sn_call_rate)) + 
  geom_smooth(method = "gam", colour="#C4961A") +  
  geom_smooth(aes(x = mean_ind_speed, y = cc_call_rate*8), method = "gam", colour="#00AFBB") +
  # Custom the Y scales:
  scale_y_continuous(
    
    # Features of the first axis
    name = "SN Call Rate",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./8, name="CC Call Rate")
  ) +
  xlim(0, 3) + geom_segment(aes(x=0,xend=2,y=1,yend=1), linetype="dotted",size = 2,  color = "#C4961A") + 
               geom_segment(aes(x=1,xend=3,y=8,yend=8), linetype="dotted",size = 2,  color = "#00AFBB") +
               #geom_line(data=data.frame(x=0:2,y=1), linetype="dotted",size = 2,  color = "#C4961A")+
               #geom_line(data=data.frame(x=0:2,y=8), linetype="dotted",size = 2,  color = "#00AFBB")+
  xlab("Movement speed (m/sec)") + theme_bw() + theme(text = element_text(size = 20))   

 


# Bin size control + color palette
ggplot(data = all_points , aes(x = pastStepDuration, y = futurStepDuration)) + 
  stat_density_2d(
  geom = "raster",
  aes(fill = cc_call_rate),
  contour = FALSE
) + scale_fill_viridis_c()


# library
library(latticeExtra) 


# showing data points on the same color scale 
levelplot(cc_call_rate ~ pastStepDuration * futurStepDuration, all_points[sample(nrow(all_points), 10000), ],
          region = TRUE) + 
  layer_(panel.2dsmoother(..., n = 5, method = "lm"))




all_points %>% group_by(pastStepDuration = cut(pastStepDuration, 10),
                        futurStepDuration = cut(futurStepDuration, 10)) %>%
  summarise(y=mean(cc_call_rate)) %>%
  ggplot(aes(futurStepDuration, pastStepDuration, fill=y)) +
  geom_tile() + 
  theme_classic()


library(ggplot2)

# create example data
set.seed(123)
n <- 1000
x <- rnorm(n)
y <- rnorm(n)
z <- rnorm(n)

# combine the data into a data frame
df <- data.frame(x, y, z)

# create the heat map with kernel density of Z
ggplot(df, aes(x = x, y = y, fill = stat(density))) +
  geom_raster() +
  scale_fill_gradientn(colors = c("white", "red"), na.value = "transparent") +
  stat_density2d(geom = "polygon", aes(alpha = ..level..), fill = NA, color = "black") +
  guides(alpha = FALSE) +
  theme_bw()









ggplot(data = all_points[which(all_points$pastStepDuration < 30 & all_points$futurStepDuration < 30), ] , aes(x = pastStepDuration, y = cc_call_rate)) + 
  geom_smooth(method = "gam")+ geom_point() 

ggplot(data = all_points[which(all_points$pastStepDuration < 30 & all_points$futurStepDuration < 30), ] , aes(x = pastStepDuration, y = cc_call_rate)) + 
  geom_smooth(method = "gam") 

ggplot(data = all_points[which(all_points$pastStepDuration < 300 & all_points$futurStepDuration < 300), ] , aes(x = pastStepDuration, y = cc_call_rate)) + 
  geom_smooth(method = "gam") 




all_points <- all_points[-which(is.na(all_points$sn_call_rate)) ,]
all_points <- all_points[-which(is.na(all_points$cc_call_rate)) ,]
all_points <- all_points[-which(is.na(all_points$call_rate)) ,]
all_points <- all_points[-which(all_points$call_rate == "Inf") ,]


ggplot(data = all_points[which(all_points$pastStepDuration < 300), ] , aes(x = pastStepDuration, y = call_rate)) + geom_smooth(method = "gam") + 
  geom_point()



ggplot(data = all_points[which(all_points$pastStepDuration < 300), ] , aes(x = pastStepDuration, y = cc_call_rate)) + geom_smooth(method = "gam") + 
  geom_point(alpha = 0.05, shape = ".")
ggplot(data = all_points[which(all_points$pastStepDuration < 300), ] , aes(x = pastStepDuration, y = sn_call_rate)) + geom_smooth(method = "gam") + 
  geom_point()


all_points <- all_points[-which(is.na(all_points$cc_call_rate)) ,]

s = interp(y = all_points[ , "pastStepDuration" ], 
           x = all_points[ , "futurStepDuration" ],
           z = all_points[ , "cc_call_rate" ], duplicate = "mean")

p <- plot_ly(x = s$x, y = s$y, z = s$z) %>% add_surface()
p

# showing data points on the same color scale 
levelplot(cc_call_rate ~ pastStepDuration * futurStepDuration, all_points[sample(nrow(all_points), 10000), ],
          region = TRUE) + 
  layer_(panel.2dsmoother(..., n = 5, method = "lm"))

fig <- plot_ly(x = all_points[ , "futurStepDuration" ],
               y = all_points[ , "indSpeedFutur" ],
               z = all_points[ , "cc_call_rate" ],
               type = "heatmap" )
fig

y <- all_points[which(all_points$pastStepDuration < 300 & 
                                      all_points$futurStepDuration < 300), "futurStepDuration" ]
x <- all_points[which(all_points$pastStepDuration < 300 & 
                                      all_points$futurStepDuration < 300), "futurStepDuration" ]
z <- all_points[which(all_points$pastStepDuration < 300 & 
                                      all_points$futurStepDuration < 300), "cc_call_rate" ]


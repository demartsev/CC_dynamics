# Load libraries here
library(dplyr)
library(tidyr)
library(crayon)
library(sp)
library(rgdal)
library(zoo)
library(ggplot2)
library(hms)
#install.packages('hms')
#Set the working directory (the folder with your files)

setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics")

meerkatdata <- read.delim("MeerkatGPSdataAll.csv",  sep = ",") %>% as_tibble()


#convert to UTM
cord.dec <- SpatialPoints(cbind(meerkatdata$location.lon, meerkatdata$location.lat), proj4string = CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=34 +south +datum=WGS84"))
#add as new columns to original dataframe
cord.UTM <- as_tibble(cord.UTM)
meerkatdata <- meerkatdata %>% mutate(x=cord.UTM$coords.x1, y=cord.UTM$coords.x2)
#drop unused columns
meerkatdata <- dplyr::select(meerkatdata, Group, ID, Date, dttm, x, y)
meerkatdata
rm(cord.dec, cord.UTM)

#specify factors
meerkatdata$Group <- factor(meerkatdata$Group)
meerkatdata$ID <- factor(meerkatdata$ID)
meerkatdata$dttm <- as.POSIXct(meerkatdata$dttm, format="%Y-%m-%d %H:%M:%S")
meerkatdata$Date <- as.Date(meerkatdata$Date, format="%d-%m-%y")


#example of how to visualize the spatial data
meerkatdata %>% filter(ID=="VNQM028" & Date=="2022-06-13") %>% ggplot(aes(x=x, y=y)) + geom_point()


#calculate the heading for each second: angle = atan2(y2-y1, x2-x1)
#angle is in radians
meerkatdata <- meerkatdata %>% group_by(Group, ID, Date) %>% mutate(angle = atan2(y-lag(y), x-lag(x)))
#calculate the distance for each second: distance = sqrt((x2-x1)^2+(y2-y1)^2)
meerkatdata <- meerkatdata %>% group_by(Group, ID, Date) %>% mutate(distance = sqrt((x-lag(x))^2 + (y-lag(y))^2))
#check that everything looks OK
#the min and max angle should be -3.14 to +3.14 and this corresponds to north, south, east, and west
##the min distance should be 0 for when teh meerkats did not move and the max should not be higher than 10 meters or so because the group cannot move much faster than a few meters per second 
summary(meerkatdata)
#we can see that the distance traveled for some meerkats is 1000m in 1 second which is impossible
meerkatdata %>% filter(distance > 100) %>% arrange(desc(distance)) #222 rows with crazy high distances per 1 second 

#remove all enteries of more than 10M distance per sec
meerkatdata[which(meerkatdata$distance > 10), "distance"] <- NA
summary(meerkatdata)


meerkatdata <- meerkatdata %>% group_by(ID, Date) %>% mutate(distance_10=zoo::rollmean(distance, k = 9, fill = NA, align = "right"))
meerkatdata <- meerkatdata %>% group_by(ID, Date) %>% mutate(t = rep(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,1), length.out = n()))
#hist(meerkatdata$distance)

meerkatdata %>% arrange(desc(distance))








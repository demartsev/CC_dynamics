#script for calculating informational update rate
#distance that individual passed between calls of diferent types

setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics")

# Define the function to calculate the Euclidean distance
euclidean_distance <- function(x1, y1, x2, y2) {
  return (sqrt((x2 - x1)^2 + (y2 - y1)^2))
}


data <- read.csv("all_calls_sync_resolved_2022-11-18.csv")
dates <- unique(data$date)

all_distances <- data.frame()

for (day in dates) {
  day_select <- data[which(data$date == day),]
  individuals <- unique(day_select$ind)
  for (ind in individuals){
    ind_select <- day_select[which(day_select$ind == ind),]
    
    # Loop through the data and calculate the distance between each subsequent row
    for (i in 1:(nrow(ind_select) - 1)) {
     ind_select$dist[i] <- euclidean_distance(ind_select$x_emitted[i], ind_select$y_emitted[i], ind_select$x_emitted[i + 1], ind_select$y_emitted[i + 1])
    ind_select$call_pair[i] <- paste(ind_select$type_group[i],  ind_select$type_group[i+1])
    ind_select$lag[i] <- ind_select$tf.numeric[i+1] - ind_select$t0.numeric[i]
    
      }
    all_distances <- rbind(all_distances, ind_select)
                       }
}

#remove long responce pairs
all_distances <- all_distances[all_distances$lag < 300, ]
all_distances <- all_distances[all_distances$dist > 0, ]
all_distances <- all_distances[all_distances$dist < 10, ]
library(ggplot2)
library(plyr)
mu <- ddply(all_distances[which(all_distances$call_pair %in% c("cc cc", "sn sn")),], "call_pair", summarise, grp.mean=mean(dist))
head(mu)

# Calculate the x-coordinate of the maximum density
density_cc <- density(all_distances[which(all_distances$call_pair == "cc cc"), "dist"])
density_sn <- density(all_distances[which(all_distances$call_pair == "sn sn"), "dist"])

# Find the maximum density point for each variable
max_cc <- which.max(density_cc$y)
max_sn <- which.max(density_cc$y)

# Get the x value of the maximum density point for each variable
cc_max_density <- density_cc$x[max_cc]
sn_max_density <- density_sn$x[max_sn]

ggplot(data = all_distances[which(all_distances$call_pair %in% c("cc cc", "sn sn")),], aes (x = dist, fill =  call_pair)) + 
  geom_density(alpha=0.2) + geom_vline(xintercept=c(cc_max_density, 0.18),
                                       linetype="dashed") 

ggplot(data = all_distances[which(all_distances$call_pair %in% c("mo mo", "ld ld", "soc soc", "cc cc", "sn sn")),], aes (x = dist, fill =  call_pair)) + 
  geom_density(alpha=0.2) 
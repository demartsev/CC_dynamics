setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics")

all_calls_seq <- read.csv("all_calls_sync_resolved_2022-11-08.csv")
#set kernell variables
max.lag <- 5
hop.time <- 0.01
bandwidth <- 0.4
jitter <- F
jitter.time <- 10

###### looking at probability with  kernell function ########
dates <- as.character(unique(all_calls_seq$date)) #get relevant dates
individuals <- unique(all_calls_seq$ind) #get individual meerkats

#empty lists to fill with data
all_ind_self_kernels<-list()
all_ind_non_self_kernels <- list()


#choose individual
for (main in individuals) {
  #empty lists to fill with data
  all_self_kernels<- list()
  all_non_self_kernels <- list()
  
  #choose date  
  for (date in dates) {
    
    print(date)
    print(main)
    
    #get calls from relevant date
    curr <- all_calls_seq[which(all_calls_seq$date == date), ]
    
    #identify self replies and non_self replies
    curr$self_trigger <- NA
    curr$main <- 0
    curr[which(curr$ind == main), "main"] = 1
    
    ###getting trigger tims for self and no self replies
    ##for (row in 2:(nrow(curr)-1)) {
    ##  
    ##   if (curr$type_group[row] == "cc" & curr$lag[row] > 5 & curr$ind[row] == main & curr$ind[row+1] == main & curr$type_group[row+1] == "cc" ) {curr$self_trigger[row] = 1} 
    ##   if (curr$type_group[row] == "cc" & curr$lag[row] > 5 & curr$ind[row] == main & curr$ind[row+1] != main & curr$type_group[row+1] == "cc" & curr$lag[row+1] < 5) {curr$self_trigger[row] = 0} 
    ##}
    
    #getting trigger times for self and no self replies
    for (row in 2:(nrow(curr)-1)) {
      
      if (curr$type_group[row] == "cc" & curr$lag[row] > 5 & curr$ind[row] == main) {curr$self_trigger[row] = 1} 
      if (curr$type_group[row] == "cc" & curr$lag[row] > 5 & curr$ind[row] == main & curr$ind[row+1] != main & curr$type_group[row+1] == "cc" & curr$lag[row+1] < 5) {curr$self_trigger[row] = 0} 
    }
    
    
    
    
    
    #getting main call times and trigger times
    self_reply_trigger <-  curr[which(curr$self_trigger == 1), "tf.numeric"]
    non_self_reply_trigger <-  curr[which(curr$self_trigger == 0), "tf.numeric"]
    
    #get call times and remove trigger calls from it
    call_times <-  curr[which(curr$ind == main & curr$type_group == "cc"), "t0.numeric"] 
    
    if (length(self_reply_trigger) >0)
    {call_times <- call_times[-which(call_times %in% curr[which(curr$self_trigger == 1), "t0.numeric"])]} 
    if (length(non_self_reply_trigger) >0)
    {call_times <- call_times[- which(call_times %in% curr[which(curr$self_trigger == 0), "t0.numeric"])]}
    
    # get latest start time and earliest end time of all individuals in table
    start.times <- tapply(curr$t0.numeric, curr$ind, min, na.rm = T)
    latest.start <- max(start.times, na.rm = T)
    
    # get start and end times
    end.times <- tapply(curr$t0.numeric, curr$ind, max, na.rm = T)
    earliest.finish <- min(end.times, na.rm = T)
    
    #run the kernell function while skipping non existing conditions
    
    if (length(self_reply_trigger >0))
    { kernel_self <- kernel.call.rates(call.times = call_times, 
                                       trigger.times = self_reply_trigger, start = latest.start, 
                                       end = earliest.finish, max.lag = max.lag, hop.time = hop.time, kernel.size = bandwidth, 
                                       jitter = jitter, jitter.time = jitter.time)
    }else{
      kernel_self <- NULL }
    
    
    #run the kernell function for non-self replies
    if (length(non_self_reply_trigger >0))
    {kernel_non_self <- kernel.call.rates(call.times = call_times, 
                                          trigger.times = non_self_reply_trigger, start = latest.start, 
                                          end = earliest.finish, max.lag = max.lag, hop.time = hop.time, kernel.size = bandwidth, 
                                          jitter = jitter, jitter.time = jitter.time)
    
    }else{
      kernel_non_self <- NULL } 
    
    #collect all outputs
    all_self_kernels[[date]] <- kernel_self
    all_non_self_kernels[[date]] <- kernel_non_self
  }
  
  all_ind_self_kernels[[main]] <-  all_self_kernels
  all_ind_non_self_kernels[[main]] <- all_non_self_kernels
}

# a data frame to collect all calls from all days and individuals
all_self_call_rates <- (data.frame(matrix(NA, nrow = 1001, ncol = 0)))
all_nonself_call_rates <- (data.frame(matrix(NA, nrow = 1001, ncol = 0)))

#summarise call probabilities

for (i in 1:length(all_ind_self_kernels)) {
  test <- sapply(all_ind_self_kernels[[i]], colMeans)
  
  group_mean <- rowMeans(test)
  
  
  all_self_call_rates <- cbind(all_self_call_rates, group_mean)
}


for (i in 1:length(all_ind_non_self_kernels)) {
  nonself_test <- sapply(all_ind_non_self_kernels[[i]], colMeans)
  
  
  
  group_nonself_mean <- rowMeans(nonself_test)
  
  
  all_nonself_call_rates <- cbind( all_nonself_call_rates,group_nonself_mean)
}

#plot
par(mfrow=c(1,1))

group_mean <- apply(all_self_call_rates, 1, mean)
group_nonself_mean <- apply(all_nonself_call_rates,1,mean)

plot(seq(-max.lag, max.lag, hop.time), group_mean, type = "l",ylim = c(0,0.15), xlim = c(-0.02, 5), lwd = 2, xlab = bquote(bold("time after call (sec)")), ylab =bquote(bold(" focal call rate")))
lines(x =seq(-max.lag, max.lag, hop.time),  y = group_nonself_mean, col = "blue", lw = 2)
legend("topright", 
       legend = c("No conspesific response", "Conspesific response"), 
       col = c("black", "blue" ), 
       pch = c(20,20), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

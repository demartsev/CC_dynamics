
#Function to compute call rates at specified times after specified 'trigger times' using kernel estimates for a given time series
#INPUTS:
# call.times: vector containing time of call start for each focal call
# start: time of recording start (number)
# end: time of recording end (number)
# trigger.times: times to start the clock (serves as a time lag of t = 0) (vector)
# max.lag = 3 #maximum time lag
# kernel.size = .05 #size of the kernel for the estimates
# hop.time = .001 #increment by which to move forward (controls temporal resolution of the output)
# jitter = boolean, whether to jitter times
# jitter.time = window in which to jitter times (if jitter = T), defaults to 5 sec
#OUTPUT:
# out.mat = matrix of size length(trigger.times) x T, which holds kernel call sequences of focal individual triggered on each trigger time
kernel.call.rates <- function(call.times, trigger.times, start, end, max.lag = 120, kernel.size = .05, hop.time = .01, jitter = F, jitter.time = 5) {
  
  #data frame to hold output
  times <- seq(-max.lag,max.lag, hop.time)
  out.mat <- matrix(nrow = length(trigger.times), ncol = length(times))
  
  #loop over window times, gather up kernel time series
  idx <- 1
  for(i in 1:length(trigger.times)){
    
    t0 <- trigger.times[i] #get beginning time of window
    
    #jitter trigger times randomly in a time window of size max.lag, if desired (as null model)
    if(jitter){
      t0 <- t0 + runif(n=1,min=-jitter.time, max=jitter.time)
    }
    
    dt <- call.times - t0 #get difference between call times and trigger times
    start.dt <- start - t0
    
    #remove dts outside the measurement window
    if(length(which((dt > max.lag) | (dt < -max.lag)))){
      dt <- dt[-which((dt > max.lag) | (dt < -max.lag))]
    }
    
    #create time series of dt at specified resolution
    ts.time <- seq(from = -max.lag, to = max.lag, by = hop.time)
    ts.val <- rep(0,length(ts.time))
    if(length(dt)>0){
      for(j in 1:length(dt)){
        #lower <- min(which(ts.time >= dt[j]))
        #upper <- min(lower + 10, length(ts.time))
        #ts.val[lower:upper] <- 1
        ts.val[min(which(ts.time >= dt[j]))] <- 1 / hop.time #divide by hop time to give a call rate rather than probability per bin
      }
    }
    
    #apply kernel to smooth at a certain kernel size
    ts.smooth <- ksmooth(ts.time, ts.val, kernel = 'normal', bandwidth = kernel.size)
    
    #store aligned times in a row of the output matrix
    out.mat[idx,] <- ts.smooth$y
    idx <- idx + 1
  }
  
  return(out.mat)
  
  
}


###################################################################################################################################################################################

#this script calculates the probability for a self reply of a meerkat when neighbors are calling and not calling (ignored the initial call)
#it uses the Kernell call rate function written by Ari. See the comments at the top of the script


setwd("C:/Users/vdemartsev/My Cloud/Git_projects/CC_dynamics")

#input of meerkat call data after focal/non focal resolver and data cleaning (resolving inconsistencies in call types, getting rid of non-focal and non-call entries)
#skipped time segments are omitted across all the group. These steps are done in call_rename_filt.R script

#load data
all_calls_seq <- read.csv("all_calls_sync_resolved_2022-11-18.csv")
#all_calls_seq <- all_calls_seq[ , -c(29:40)] #get rid of some garbage columns



#set kernell variables
max.lag <- 5
hop.time <- 0.01
bandwidth <- 0.4
jitter <- F
jitter.time <- 10



###### looking at probability with  kernell function ########
dates <- as.character(unique(all_calls_seq$date)) #get relevant dates

#set the call type and age of interest 
calls <- c("sn", "cc")
age_class <- c("Juvenile", "Adult")

plot
par(mfrow=c(2,2))

for (call_type in calls) {
  for (age in age_class) {
    
    
    if (age != "Juvenile") {
      individuals <- unique(all_calls_seq[which(all_calls_seq$status != "Juvenile"), "ind"]) #only adult main
    }else{
      individuals <- unique(all_calls_seq[which(all_calls_seq$status == "Juvenile"), "ind"])} #only juveniles main
    
    
    #empty lists to fill with data
    all_ind_self_kernels<-list()
    all_ind_non_self_kernels <- list()
    
    #here we loop over individuals and dates to identify events of self reply and of caller exchange
    
    #choose main individual
    for (main in individuals) {
      #empty lists to fill with data
      all_self_kernels<- list()
      all_non_self_kernels <- list()
      
      #choose one date  
      for (date in dates) {
        
        print(date)
        print(main)
        
        #get calls from relevant date
        curr <- all_calls_seq[which(all_calls_seq$date == date), ]
        
        #identify self replies and non_self replies
        curr$self_trigger <- NA
        curr$main <- 0 #fill the column with 0s. This will be edited later
        
        curr[which(curr$ind == main), "main"] = 1 #when individual caller is "main" set as 1
        
        
        #getting trigger times for self and no self replies
        for (row in 2:(nrow(curr)-1)) {
          
          if (curr$type_group[row] == call_type & curr$lag[row] > 5 & curr$ind[row] == main) {curr$self_trigger[row] = 1} 
          if (curr$type_group[row] == call_type & curr$lag[row] > 5 & curr$ind[row] == main & curr$ind[row+1] != main & curr$type_group[row+1] == call_type & curr$lag[row+1] < 5) {curr$self_trigger[row] = 0} 
          
          #  if (curr$lag[row] > 5 #if call lag is > 5 sec
          #      & curr$ind[row] == main) #and the caller is the "main" caller
          #    {curr$self_trigger[row] = 1} #assign the call as self trigger call (the first call in a self reply event)
          #  
          #  if (curr$lag[row] > 5 #if call lag is > 5 sec
          #      & curr$ind[row] == main #and the caller is the "main" caller
          #      & curr$ind[row+1] != main #and the next caller is not the  "main" caller
          #      &  curr$lag[row+1] < 5) #and the lag to the next call is < 5 sec
          #    {curr$self_trigger[row] = 0} #assign the call as not self trigger call (the first call in a caller exchange event)
        }
        
        
        
        
        
        #getting main call times and trigger times
        self_reply_trigger <-  curr[which(curr$self_trigger == 1), "tf.numeric"] #get all times in which a self reply event was initiated
        non_self_reply_trigger <-  curr[which(curr$self_trigger == 0), "tf.numeric"] #get all times in which a caller exchange event was initiated
        
        #get call times and remove trigger calls from it
        call_times <-  curr[which(curr$ind == main & curr$type_group == call_type), "t0.numeric"] 
        #call_times <-  curr[which(curr$ind == main), "t0.numeric"] 
        
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
      if (length(all_ind_self_kernels[[i]]) == 0) {next}
      test <- sapply(all_ind_self_kernels[[i]], colMeans)
      
      group_mean <- rowMeans(test)
      
      
      all_self_call_rates <- cbind(all_self_call_rates, group_mean)
    }
    
    
    for (i in 1:length(all_ind_non_self_kernels)) {
      if (length(all_ind_non_self_kernels[[i]]) == 0) {next}
      nonself_test <- sapply(all_ind_non_self_kernels[[i]], colMeans)
      
      
      
      group_nonself_mean <- rowMeans(nonself_test)
      
      
      all_nonself_call_rates <- cbind( all_nonself_call_rates,group_nonself_mean)
    }
    
    
    
    group_mean <- apply(all_self_call_rates, 1, mean)
    group_nonself_mean <- apply(all_nonself_call_rates,1,mean)
    
    plot(seq(-max.lag, max.lag, hop.time), group_mean, type = "l",ylim = c(0,1), xlim = c(0, 5), lwd = 2, 
         xlab = bquote(bold("time after call (sec)")), ylab =bquote(bold("focal call rate")), 
         main = paste(call_type, "call rate\n", age))
    lines(x =seq(-max.lag, max.lag, hop.time),  y = group_nonself_mean, col = "blue", lw = 2)
    ##legend("topright", 
    ##       legend = c("No conspesific response", "Conspesific response"), 
    ##       col = c("black", "blue" ), 
    ##       pch = c(20,20), 
    ##       bty = "n", 
    ##       pt.cex = 2, 
    ##       cex = 1.2, 
    ##       text.col = "black", 
    ##       horiz = F , 
    ##       inset = c(-0.1, -0.05))
  }
}
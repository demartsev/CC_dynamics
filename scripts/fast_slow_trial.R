


##load data from previously saved CSV
all_arrival_points <- read.csv("time_spent_in_patch_30sec_pass.csv")

# standardize (Z score) call rates per individual per day
all_arrival_points <- all_arrival_points %>% group_by(date, indUniqID, type) %>% mutate(rate_stn = scale(rate)) %>% ungroup()



fast <- 0.3
forage_slow <- 0.005
forage_fast <- 0.1

fast_slow <- all_arrival_points[which(all_arrival_points$indSpeedPast >= fast &
                                      all_arrival_points$indSpeedFutur < forage_fast &
                                      all_arrival_points$indSpeedFutur >= forage_slow),]

#get basic linear regression for overall effects of time in spent in patch (x) and call rate (y)

ggplot(data = fast_slow, aes(x = time_window, y = rate_stn))  + geom_smooth(method = "gam")+geom_point()+
  facet_wrap(~ type)

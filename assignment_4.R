library(tidyverse)
# 3
# sampling with replacement, repeated 35 times, 3 red and 7 blue
prob_red_spheres<-function(n_red){
  c<-choose(35, n_red)
  prob<-c*(3/10)^n_red*(7/10)^(35-n_red)
  return(prob)
}

prob_red_spheres(20)

num_reds<-c(1:35)
prob<-map_dbl(n_red_seq, prob_red_spheres)
prob_by_num_red<-data.frame(num_reds,  prob)

prob_by_num_red%>%head(3)

library(ggplot2)
prob_plot<-ggplot(data=prob_by_num_red, aes(x=num_reds, y=prob))
prob_plot + geom_line() + xlab("Number of reds") + ylab("Prob")

# sample(): 
# simulate a random experiment in which we sample with replacement
# from a collection of 10 objects, and repeat this process 35 times
sample(10,35,replace=TRUE)

# the outcome of sample() is different each time
# in order to have a same result, we can set the seed to be a constant
# and each time the outcome will be the same
## Setting the random seed just once
set.seed(0)
for(i in 1:5){
  print(sample(100,5,replace=FALSE))
  3
  # The result may well differ every time
}
## Resetting the random seed every time
for(i in 1:5){
  set.seed(1)
  print(sample(100,5,replace=FALSE))
  # The result should not change
}

count_lessthan3<-function(n){
  count<-0
  for (i in 1:length(n)){
    if (n[i]<=3)
      count = count + 1
  }
  return(count)
}

num_trials<-1000 # set the number of trials
set.seed(0) # set the random seed
sampling_with_replacement_simulation<-data.frame(trial=1:num_trials)%>%
  # simulation
  mutate(sample_balls=map(.x=trial,~sample(10,35,replace = TRUE)))%>%
  # the number of red balls
  mutate(num_reds=map_dbl(sample_balls, count_lessthan3))

num_reds_in_simulation<-sampling_with_replacement_simulation%>%pull(num_reds)
# we extract a vector corresponding to the number of reds in each trial

prob_by_num_red<-prob_by_num_red%>%
  # caulate the number of trails where the red ball equal to num_reds
  mutate(simulation_count=map_dbl(.x=num_reds,~sum(num_reds_in_simulation==.x)))
# add a column which gives the number of trials with a given number of reds

prob_by_num_red<-prob_by_num_red%>%
  mutate(expected_count=num_trials*prob)
# add a column which gives the expected number of reds
# based on the probability formula

prob_by_num_red%>%
  rename(Simulation=simulation_count,Expected=expected_count)%>%
  pivot_longer(cols=c("Simulation","Expected"),
               names_to="Type",values_to="count")%>%
  ggplot(aes(num_reds,count)) +
  geom_line(aes(linetype=Type, color=Type)) +
  scale_linetype_manual(values = c("solid", "dashed"))+
  theme_bw()+
  xlab("Number of reds")+
  ylab("Count")

# 4

# simulation of "a game of marbles"
num_trials<-10000 # set the number of trials
set.seed(0) # set the random seed
sampling_with_replacement_simulation<-data.frame(trial=1:num_trials)%>%
  # simulation
  mutate(sample_balls=map(.x=trial,~sample(2,100,replace = TRUE)))%>%
  # the number of red balls
  mutate(num_reds=map_dbl(sample_balls, count_lessthan3))





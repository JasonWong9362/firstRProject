library(tidyverse)
set.seed(0)
num_trails_per_sample_size<-10
max_sample_size<-10000
q<-0.3



# how sample mean vary while the sample size increase
sim_by_n_df<-crossing(trail=seq(num_trails_per_sample_size),
                      sample_size=seq(to=sqrt(max_sample_size), by=0.1)**2)%>%
  mutate(simulation=pmap(.l=list(trail, sample_size), .f=~rbinom(.y,1,q)))%>% # .y the second col of df?
  mutate(sample_mean=map_dbl(.x=simulation, .f=~mean(.x, na.rm=1)))


head(sim_by_n_df, 100)
# rbinom(n, size, prob)


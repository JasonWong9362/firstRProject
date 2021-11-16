library(Stat2Data)
library(tidyverse)
data("Hawks")


# 1
RedTailedDf <- Hawks%>%
  filter(Species=='RT')%>%
  select(Weight, Tail, Wing)

Tail<-RedTailedDf%>%pull(Tail)
n<-length(Tail)
Tail_mean<-mean(Tail, na.rm=1)
Tail_sd<-sd(Tail, na.rm=1)*sqrt((n-1)/n)
x<-seq(Tail_mean-3*Tail_sd, Tail_mean+3*Tail_sd, Tail_sd*0.01)
Tail_MLE<-dnorm(x, mean=Tail_mean, sd=Tail_sd)
colors<-c("MLE density"="red", "Kernel density"="blue")

ggplot()+geom_line(data=data.frame(x,Tail_MLE),aes(x=x, y=Tail_MLE, color="MLE density"))+
  geom_density(data=data.frame(Tail),aes(x=Tail, color="Kernel density"))+
  xlab("weight")+ylab("Density function")+theme_bw()+scale_color_manual(values=colors)


# 2
set.seed(0)
num_trials_per_sample_size<-100
min_sample_size<-5
max_sample_size<-1000
sample_size_inc<-5
mu_0<-1
sigma_0<-3
simulation_df<-crossing(trial=seq(num_trials_per_sample_size),
                        sample_size=seq(min_sample_size,
                                        max_sample_size,sample_size_inc))%>%
  # create data frame of all pairs of sample_size and trial
  mutate(simulation=pmap(.l=list(trial,sample_size),
                         .f=~rnorm(.y,mean=mu_0,sd=sigma_0)))%>%
  # simulate sequences of Gaussian random variables
  mutate(sample_md=map_dbl(.x=simulation,.f=median))%>%
  # compute the sample medians
  mutate(sample_mean=map_dbl(.x=simulation,.f=~mean(.x, na.rm=1)))%>%
  # compute the sample mean
  group_by(sample_size)%>%
  summarise(msq_error_md=mean((sample_md-mu_0)^2), msq_error_mean=mean((sample_mean-mu_0)^2))

color2<-c("mean"="red", "md"="blue")
ggplot()+geom_smooth(data=simulation_df,aes(x=sample_size, y=msq_error_md, color="md"), span=0.4)+
  geom_smooth(data=simulation_df, aes(x=sample_size, y=msq_error_mean, color="mean"), span=0.4)+
  scale_color_manual(values=color2)

# 3
compute_MLE<-function(seq){
  seq_mean<-mean(seq, na.rm=1)
  return(sqrt((1/length(seq))*sum((seq-seq_mean)^2)))
}

compute_U<-function(seq){
  seq_mean<-mean(seq, na.rm=1)
  return(sqrt((1/(length(seq)-1))*sum((seq-seq_mean)^2)))
}

set.seed(0)
num_trials_per_sample_size<-100
min_sample_size<-5
max_sample_size<-1000
sample_size_inc<-5
mu_0<-1
sigma_0<-3
simulation_df<-crossing(trial=seq(num_trials_per_sample_size),
                        sample_size=seq(min_sample_size,
                                        max_sample_size,sample_size_inc))%>%
  # create data frame of all pairs of sample_size and trial
  mutate(simulation=pmap(.l=list(trial,sample_size),
                         .f=~rnorm(.y,mean=mu_0,sd=sigma_0)))%>%
  # simulate sequences of Gaussian random variables
  mutate(sample_mean=map_dbl(.x=simulation,.f=~mean(.x, na.rm=1)))%>%
  mutate(var_MLE=map_dbl(.x=simulation, .f=~compute_MLE(.x)))%>%
  mutate(var_U=map_dbl(.x=simulation, .f=~compute_U(.x)))%>%
  pivot_longer(cols=c("var_MLE", "var_U"), names_to="var", values_to="value")

ggplot(simulation_df, aes(x=sample_size, y=value, color=var) )+labs(x="sample size", y="var")+theme_bw()+
  geom_point()+geom_hline(aes(yintercept=sigma_0))+facet_wrap(~var)

# 5.1 
set.seed(0)
num_trials_per_sample_size<-100
min_sample_size<-5
max_sample_size<-1000
sample_size_inc<-5
lambda<-3
simulation_df<-crossing(trial=seq(num_trials_per_sample_size),
                        sample_size=seq(min_sample_size,
                                        max_sample_size,sample_size_inc))%>%
  # create data frame of all pairs of sample_size and trial
  mutate(simulation=pmap(.l=list(trial,sample_size),
                         .f=~rpois(.y, lambda = lambda)))%>%
  # simulate sequences of Gaussian random variables
  mutate(sample_mean=map_dbl(.x=simulation,.f=~mean(.x, na.rm=1)))%>%
  # compute the sample mean
  group_by(sample_size)%>%
  summarise(msq_error_mean=mean((sample_mean-lambda)^2))

ggplot()+geom_smooth(data=simulation_df, aes(x=sample_size, y=msq_error_mean, color="mean"), span=0.4)

# 5.2 
folder_path<-"C:/Users/Jason/Desktop/R_project/assignment/assignment_6" 
file_name<-"/VonBortkiewicz.csv" 
file_path<-paste(folder_path,file_name,sep="") 

# create win_tidy
fatality_df<-read.csv(file_path,fill = 1) 
head(fatality_df)
fatalities<-fatality_df%>%
  pull(fatalities)

lambda_0<-mean(fatalities, na.rm=1)
p_0<-dpois(0,lambda = lambda_0)

# 6
file_name<-"/CustomerPurchases.csv" 
file_path<-paste(folder_path,file_name,sep="") 

purchase_df<-read.csv(file_path,fill = 1)
time_diffs<-purchase_df%>%
  mutate(time_diffs=lead(Time)-Time)%>%
  pull(time_diffs)
lambda_MLE<-1/mean(time_diffs, na.rm=1)
1-pexp(60,rate=lambda_MLE)
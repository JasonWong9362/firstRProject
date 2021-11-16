# 1
library(tidyverse)
library(palmerpenguins)
head(penguins)
adelie_flippers<-penguins%>%
  filter(species=="Adelie")%>%
  pull(flipper_length_mm)

alpha<-0.05
sample_size<-length(adelie_flippers)
sample_mean<-mean(adelie_flippers, na.rm=1)
sample_sd<-sd(adelie_flippers, na.rm=1)
t<-qt(1-alpha/2,df=sample_size-1)
confidence_interval_l<-sample_mean-t*sample_sd/sqrt(sample_size)
confidence_interval_u<-sample_mean+t*sample_sd/sqrt(sample_size)
confidence_interval<-c(confidence_interval_l,confidence_interval_u)
confidence_interval

library(Stat2Data)
head(Hawks)
RT_weight<-Hawks%>%
  filter(Species=="RT")%>%
  pull(Weight)
alpha<-0.01
sample_size<-length(RT_weight)
sample_mean<-mean(RT_weight, na.rm=1)
sample_sd<-sd(RT_weight, na.rm=1)
t<-qt(1-alpha/2,df=sample_size-1)
confidence_interval_l<-sample_mean-t*sample_sd/sqrt(sample_size)
confidence_interval_u<-sample_mean+t*sample_sd/sqrt(sample_size)
confidence_interval<-c(confidence_interval_l,confidence_interval_u)
confidence_interval

ggplot(data.frame(RT_weight=RT_weight), aes(x=RT_weight))+geom_density()

ggplot(data.frame(RT_weight=RT_weight), aes(sample=RT_weight))+stat_qq()+stat_qq_line(color="blue")

# 2
bill_adelie<-penguins%>%
  filter(species=="Adelie")%>%
  pull(bill_length_mm)
t.test(bill_adelie, mu=40, conf.level=0.99)


# 3
t_test<-function(x, mu){
  alpha<-0.05
  sample_size<-length(x)
  sample_mean<-mean(x, na.rm=1)
  sample_sd<-sd(x, na.rm=1)
  test_statistic<-(sample_mean-mu)/(sample_sd/sqrt(sample_size))
  p_value<-2*(1-pt(abs(test_statistic), df=sample_size-1))
  return(p_value)
}

t_test(adelie_flippers, 190)
t.test(adelie_flippers, mu = 190)

# 4

# 5
student_t_confidence_interval<-function(sample,confidence_level){
  sample<-sample[!is.na(sample)] # remove any missing values
  n<-length(sample) # compute sample size
  mu_est<-mean(sample) # compute sample mean
  sig_est<-sd(sample) # compute sample sd
  alpha = 1-confidence_level # alpha from gamma
  t<-qt(1-alpha/2,df=n-1) # get student t quantile
  l=mu_est-(t/sqrt(n))*sig_est # lower
  u=mu_est+(t/sqrt(n))*sig_est # upper
  return(c(l,u))
}

num_trials<-10000
sample_size<-30
mu_0<-1
sigma_0<-3
alpha<-0.05
set.seed(0) # set random seed for reproducibility
single_alpha_coverage_simulation_df<-data.frame(trial=seq(num_trials))%>%
  mutate(sample=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_0,sd=sigma_0)))%>%
  # generate random Gaussian samples
  mutate(ci_interval=map(.x=sample,.f=~student_t_confidence_interval(.x,1-alpha)))%>%
  # generate confidence intervals
  mutate(cover=map_lgl(.x=ci_interval,.f=~((min(.x)<=mu_0)&(max(.x)>=mu_0))))%>%
  # check if interval covers mu_0
  mutate(ci_length=map_dbl(.x=ci_interval,.f=~(max(.x)-min(.x)))) 

# compute interval length
single_alpha_coverage_simulation_df%>%
  pull(cover)%>%
  mean() # estimate of coverage probability

gamma<-seq(0,1,0.01)

coverage_probability<-function(gamma){
  num_trials<-1000
  sample_size<-30
  mu_0<-1
  sigma_0<-3
  alpha<-1-gamma
  set.seed(0) # set random seed for reproducibility
  single_alpha_coverage_simulation_df<-data.frame(trial=seq(num_trials))%>%
    mutate(sample=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_0,sd=sigma_0)))%>%
    # generate random Gaussian samples
    mutate(ci_interval=map(.x=sample,.f=~student_t_confidence_interval(.x,1-alpha)))%>%
    # generate confidence intervals
    mutate(cover=map_lgl(.x=ci_interval,.f=~((min(.x)<=mu_0)&(max(.x)>=mu_0))))%>%
    # check if interval covers mu_0
    mutate(ci_length=map_dbl(.x=ci_interval,.f=~(max(.x)-min(.x)))) 
  
  # compute interval length
  coverage_probability<-single_alpha_coverage_simulation_df%>%
    pull(cover)%>%
    mean() # estimate of coverage probability
  
  return(coverage_probability)
  
}

coverage_probability_outcome<-map_dbl(.x=alphas, .f=~coverage_probability(.x))
ggplot(data.frame(x=alphas, y=coverage_probability_outcome), aes(x=x, y=y))+geom_line()+
  labs(x="gamma", y="coverage_probability")


average_length<-function(gamma){
  num_trials<-1000
  sample_size<-30
  mu_0<-1
  sigma_0<-3
  alpha<-1-gamma
  set.seed(0) # set random seed for reproducibility
  single_alpha_coverage_simulation_df<-data.frame(trial=seq(num_trials))%>%
    mutate(sample=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_0,sd=sigma_0)))%>%
    # generate random Gaussian samples
    mutate(ci_interval=map(.x=sample,.f=~student_t_confidence_interval(.x,1-alpha)))%>%
    # generate confidence intervals
    mutate(cover=map_lgl(.x=ci_interval,.f=~((min(.x)<=mu_0)&(max(.x)>=mu_0))))%>%
    # check if interval covers mu_0
    mutate(ci_length=map_dbl(.x=ci_interval,.f=~(max(.x)-min(.x)))) 
  
  # compute interval length
  average_length<-single_alpha_coverage_simulation_df%>%
    pull(ci_length)%>%
    mean() # estimate of coverage probability
  
  return(average_length)
  
}

average_length_outcome<-map_dbl(.x=alphas, .f=~average_length(.x))
ggplot(data.frame(x=alphas, y=average_length_outcome), aes(x=x, y=y))+geom_line()+
  labs(x="gamma", y="average_length")

# 6
library(PropCIs)
driving_test_results<-c(1,0,1,0,0,0,0,0,0,1,0,0,0,1,0,1,0,1,0,1,0,0,1,0)
alpha<-0.01 # failure probability
num_successes<- sum(driving_test_results) # total passes
sample_size<-length(driving_test_results)
scoreci(x=num_successes, n=sample_size, conf.level=1-alpha)
# compute Wilson's confidence intervals



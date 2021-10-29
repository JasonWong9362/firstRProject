library(tidyverse)

# 2
# generate 1000 observations of uniform distribution in [0,1]
set.seed(0)
n<-1000
sample_X<-data.frame(U=runif(n))%>% 
  mutate(X=case_when(
    (0<=U)&(U<0.25)~1,
    (0.25<=U)&(U<0.5)~5,
    (0.5<=U)&(U<=1)~0))%>%
  pull(X)


sample_X_015<-function(a,b,n){
  set.seed(0)
  sample_X<-data.frame(U=runif(n))%>% 
    mutate(X=case_when(
      (0<=U)&(U<a)~1,
      (a<=U)&(U<a+b)~5,
      (a+b<=U)&(U<=1)~0))%>%
    pull(X)
  return(sample_X)
}

sample_1<-sample_X_015(1/2, 1/10, 10000)
# sample average of X1, . . . , Xn
sample_1_mean<-mean(sample_1)
head(sample_1_mean)
# E(X)
E_x<-(1/2)*1+(1/10)*5
head(E_x)
# sample variance
sample_1_var<-var(sample_1)
head(sample_1_var)
# Var(X)
Var_x<-(1/2)*(1-E_x)^2+(1/10)*(5-E_x)^2+(1-1/2-1/10)*(0-E_x)^2
head(Var_x)

a<-1/10
b<-seq(0,9/10,0.01)
set.seed(0)
sample_X_015<-function(b){
  a<-1/10
  n<-100
  sample_X<-data.frame(U=runif(n))%>% 
    mutate(X=case_when(
      (0<=U)&(U<a)~1,
      (a<=U)&(U<a+b)~5,
      (a+b<=U)&(U<=1)~0))%>%
    pull(X)
  return(sample_X)
}

outcome<-c()
for (i in seq(length(b))){
  tem<-sample_X_015(b[i])
  outcome[i] = mean(tem)
}
mean_df<-data.frame(outcome, b)
head(mean_df, 100)
mean_plot<-ggplot(mean_df, aes(x=b, y=outcome)) 
mean_plot + geom_line()

# 3 
# dnorm: return the value of probability function of normal distribution given x, mean and std var
# pnorm: return the value of cumulative distribution function given x, mean and std var
# qnorm: return the value of quantile given probability, mean and std var (inverse pnorm)
# rnorm: generate a vector of normally distributed random numbers
　
# generate probability density function using "dnorm"
x<-seq(-4, 6, 0.1)
head(length(x))

var_1<-dnorm(x,1,1^(1/2))
var_2<-dnorm(x,1,2^(1/2))
var_3<-dnorm(x,1,3^(1/2))
data.frame(x, var_1, var_2, var_3)%>%
  pivot_longer(!x, names_to = "variance", values_to = "y")%>%
  ggplot(aes(x=x, y=y, color=variance, linetype=variance)) + geom_line() + xlab("x") + ylab("Density")

# generate cumulative probability function using "pnorm"
var_1<-pnorm(x,1,1^(1/2))
var_2<-pnorm(x,1,2^(1/2))
var_3<-pnorm(x,1,3^(1/2))
data.frame(x, var_1, var_2, var_3)%>%
  pivot_longer(!x, names_to = "variance", values_to = "y")%>%
  ggplot(aes(x=x, y=y, color=variance, linetype=variance)) + geom_line() + xlab("x") + ylab("Density")

# generate qurntile function using "qnorm"
x<-seq(0, 1, 0.01)
var_1<-qnorm(x,1,1^(1/2))
var_2<-qnorm(x,1,2^(1/2))
var_3<-qnorm(x,1,3^(1/2))
data.frame(x, var_1, var_2, var_3)%>%
  pivot_longer(!x, names_to = "variance", values_to = "y")%>%
  ggplot(aes(x=x, y=y, color=variance, linetype=variance)) + geom_line() + xlab("x") + ylab("Density")

# "W = αZ+β"
n<-100
# Make sure your code is reproducible by using the set.seed() function
set.seed(1)
standardGaussianSample<-rnorm(n,0,1)
mean1Var3GaussianSampleA<-(3^(1/2))*standardGaussianSample+1
set.seed(1)
mean1Var3GaussianSampleB<-rnorm(n,1,3^(1/2))

# This is for getting two graphs next to each other
oldpar <- par()
par(mfrow=c(1,2))
# The breaks argument specifies how many bars are in the histogram
hist(mean1Var3GaussianSampleA, breaks = 100)
hist(mean1Var3GaussianSampleB, breaks = 100)

x<-seq(-4, 5.9, 0.1)
mean1Var3GaussianSample<-dnorm(x,1,3^(1/2))
data.frame(x,mean1Var3GaussianSample,mean1Var3GaussianSampleA)%>%
  ggplot(aes(mean1Var3GaussianSampleA)) + geom_density(color="blue") +
  geom_line(aes(x=x, y=mean1Var3GaussianSample, color="red")) +
  geom_vline(xintercept=mean(mean1Var3GaussianSampleA), color="green") +
  geom_vline(xintercept=1, color="pink", ) 


# ggplot?

# 4

# probability mass function pZ(x) = P(Z = x) with Z ∼ Binom(50, 7/10)
x<-seq(0,50)
pmf<-dbinom(x,50,7/10)
binom_df<-data.frame(x, pmf)
head(dbinom_df)

x<-seq(0,50,0.01)
mean<-50*0.7
sd<-(50*0.7*(1-0.7))^(1/2)
pdf<-dnorm(x,mean,sd)
gaussian_df<-data.frame(x,pdf)
head(gd_df)

colors<-c("Gaussian pdf"="red", "Binomial pmf"="blue")
fill<-c("Gaussian pdf"="white", "Binomial pmf"="white")

ggplot()+labs(x="x",y="Probability")+theme_bw()+
  geom_line(data=gaussian_df,
            aes(x,y=pdf,color="Gaussian pdf"),size=2)+
  # create plot of Gaussian density
  geom_col(data=binom_df,
           aes(x=x,y=pmf,color="Binomial pmf",fill="Binomial pmf"))+
  scale_color_manual(name = "", values=colors)+
  scale_fill_manual(name = "", values=fill)+
  xlim(c(20,50))

# 5 exponential distribution
my_cdf_exp<-function(x,lambda){
  if(x<0){
    return(0)
  }
  else{
    return(1-(exp(1)^(-lambda*x)))
  }
}

lambda<-1/2
map_dbl(.x=seq(-1,4),.f=~my_cdf_exp(x=.x,lambda=lambda))%>%
  head()

test_inputs<-seq(-1,10,0.1)
my_cdf_output<-map_dbl(.x=test_inputs,.f=~my_cdf_exp(x=.x,lambda=lambda))
inbuilt_cdf_output<-map_dbl(.x=test_inputs,.f=~pexp(q=.x,rate=lambda))
all.equal(my_cdf_output,inbuilt_cdf_output)

my_quantile_exp<-function(p, lambda){
  if(p<0|p>1){
    return(0)
  }
  else{
    return(log(1-p, exp(1))/(-lambda))
  }
}

test_inputs<-seq(0.01, 0.99,0.01)
my_cdf_output<-map_dbl(.x=test_inputs,.f=~my_quantile_exp(p=.x,lambda=lambda))
inbuilt_cdf_output<-map_dbl(.x=test_inputs,.f=~qexp(p=.x,rate=lambda))
all.equal(my_cdf_output,inbuilt_cdf_output)





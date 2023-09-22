#----------------------------------> Homework 5 -- Question 2

library(coda)
library(ggplot2)

# DEFINE THE FUNCTION FOR THE VARIANCE
var <- function(u){
  return(1/(1 + u^2))
}
# Define the starting values of u1 and u2
u2 = 0; u1 = 0
N = 11000 # Number of Iterations
# Run the Gibbs Sampler
for(i in 2:(N + 1)){
  u1[i] = rnorm(n = 1, mean = 0, sd = sqrt(var(u2[i -1])))
  u2[i] = rnorm(n = 1, mean = 0, sd = sqrt(var(u1[i])))
}  

library(mcmcplots)
burn = 1000
u1 = u1[-1]; u2 = u2[-1] # Remove the initial points
samples = (burn + 1):N
u1.sample = u1[samples]; u2.sample = u2[samples]

u1.sample = data.frame(u1.sample); u2.sample = data.frame(u2.sample)
colnames(u1.sample) = "u1"; names(u2.sample) = "u2"

## TRACE PLOTS
traceplot(as.mcmc(u1.sample), main = "Trace plot of u1")
traceplot(as.mcmc(u2.sample), main = "Trace plot of u2")

u_sample = data.frame(u1.sample, u2.sample)
colnames(u_sample) = c(TeX(r'($u_1$)'), TeX(r'($u_2$)'))
traplot(as.mcmc(u_sample), plot.title = "Traceplots", greek = TRUE)


## TRACE PLOTS USING GGPLOT
a <- traplot(as.mcmc(u1.sample), main = TeX(r'(Trace plot of $u_1$)'))
b <- traplot(as.mcmc(u2.sample), main = TeX(r'(Trace plot of $u_2$)'))

## ACF PLOTS
par(mfrow = c(2,1))
acf(u1.sample, main = TeX(r'(ACF plot of $u_1$)'))
acf(u2.sample, main = TeX(r'(ACF plot of $u_2$)'))

# Defining the function
density_u1 <- function(x){
  return(exp(- 0.5 * (x^2))/sqrt(1 + (x^2)))
}
C = integrate(density_u1, lower = - Inf, upper = Inf)$value
K = 1/C

density_u1_normalized <- function(x){
  return(K * exp(- 0.5 * (x^2))/sqrt(1 + (x^2)))
}

par(mfrow = c(1,1))
hist(u1, prob = TRUE, main = "Histogram of u1 overlayed with density plot")
curve(density_u1_normalized, from = - 4, to = 4, add = TRUE, col = "red", lwd = 2)

## GGPLOT
library(tidyverse)
library(latex2exp)
ggplot(data.frame(x = u1), aes(x = x)) + geom_histogram(bins = 30, aes(y=..density..), colour = "black", fill = "lightgrey") + stat_function(fun = density_u1_normalized, col = "#0072B2", size = 1) + labs(x = TeX(r'($u_1$)'), title = TeX(r'(Plot of density of $u_1$ and histogram of MCMC samples)'))

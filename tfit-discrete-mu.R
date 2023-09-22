#tfit-problem - discretizing mu and putting a uniform prior on mu 


# Discretize mu (mu between 2 and 4)
L <- 2     
U <- 4   
Na <- 99 #no. of intervals
delta_mu <- (U - L) / Na #step size
mu_values <- L + seq(0, Na) * delta_mu #grid for mu values after discretization - 100 discretized mu values 


#simulate data
n = 1000 
x= 3 + 0.1*rt (n,6);  #generates data poitns from a t distribution with 6 degrees of freedom.

#plot histogram
hist(x)
# - - - define global parameters
MCMC = 20000; BURNIN = 10000; thin = 5; N=BURNIN+MCMC; effsamp=(N-BURNIN)/thin
# - - - set prior hyperparameters -"
lambda = 0.01; alpha = 0.01; beta = 0.01 # vague prior 
# ---- output files ---#
muout = rep (0,effsamp); sigmasqout = rep(0, effsamp) # 
#- - - initialize parameters#
mu = 0; sigmasq = 1; gammai = rep(1,n) ; nu = 6


# Start Gibbs sampling
for (g in 1:N) {
  # Update mu using log-sum-exp trick
  log_posterior = numeric(length(mu_values))
  for (i in 1:length(mu_values)) {
    mu_temp = mu_values[i]
    log_posterior[i] = sum(dnorm(x, mean=mu_temp, sd=sqrt(sigmasq), log=TRUE))
  }
  max_log_posterior = max(log_posterior)
  log_posterior = log_posterior - max_log_posterior
  posterior = exp(log_posterior)
  posterior = posterior / sum(posterior)
  mu = sample(mu_values, 1, prob=posterior)
  
  # Update sigmasq
  pshape = (n + alpha) / 2
  pscale = (sum((x - mu)^2) + beta) / 2
  sigmasq = 1 / rgamma(1, pshape, scale=1/pscale)
  
  # Update gammai
  pshape = nu/2 + 0.5
  pscalei = (x - mu)^2 / (2 * sigmasq) + nu/2
  gammai = rgamma(n, pshape, scale=1/pscalei)
  
  # Save output
  if (g > BURNIN & g%%thin == 0) {
    muout[(g - BURNIN) / thin] = mu
    sigmasqout[(g - BURNIN) / thin] = sigmasq
  }
}

#Draw traceplots of mu and sigma^2

library(coda)
library(mcmcplots)
muM = as.mcmc(muout); sigsqM = as.mcmc(sigmasqout)
traplot(muM, main = "Trace plot of mu")
traplot(sigsqM, main = "Trace Plot for sigmasq")
summary(muout)
summary(sigmasqout)


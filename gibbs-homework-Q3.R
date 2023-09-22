#----------HOMEWORK 5 - QUESTION 3 (part I)
U = matrix(c(1, 0.05, 0.05, 1), nrow = 2, byrow = TRUE)
sigma_u = as.numeric(1 - t(c(0.99, 0.1)) %*% solve(U) %*% c(0.99, 0.1) )
mu_u = t(c(0.99, 0.1)) %*% solve(U)

V = matrix(c(1, 0.1, 0.1, 1), nrow = 2, byrow = TRUE)
sigma_v = as.numeric(1 - t(c(0.99, 0.05)) %*% solve(V) %*% c(0.99, 0.05) )
mu_v = t(c(0.99, 0.05)) %*% solve(V)

W = matrix(c(1, 0.99, 0.99, 1), nrow = 2, byrow = TRUE)
sigma_w = as.numeric(1 - t(c(0.1, 0.05)) %*% solve(W) %*% c(0.1, 0.05) )
mu_w = t(c(0.1, 0.05)) %*% solve(W) 

u = 0; v = 0; w = 0
N = 50000
for(i in 2:(N + 1)){
  u[i] = rnorm(n = 1, mean = as.numeric(mu_u %*% c(v[i - 1], w[i - 1])), sd = sqrt(sigma_u))
  v[i] = rnorm(n = 1, mean = as.numeric(mu_v %*% c(u[i], w[i - 1])), sd = sqrt(sigma_v))
  w[i] = rnorm(n = 1, mean = as.numeric(mu_w %*% c(u[i], v[i])), sd = sqrt(sigma_w))
}
burn = 10000
library(mcmcplots)
# Get rid of the initial points
u = u[-1]; v = v[-1]; w = w[-1]

# Take the samples post the burn-in period
u_samples = data.frame(u = u[(burn + 1):N])
v_samples = data.frame(v = v[(burn + 1):N])
w_samples = data.frame(w = w[(burn + 1):N])

samples = data.frame(u = u_samples, v = v_samples, w = w_samples)
# Look at the traceplots of the samples
trace_u = traplot(as.mcmc(u_samples), main = "Traceplot of u")
trace_v = traplot(as.mcmc(v_samples), main = "Traceplot of v")
trace_w = traplot(as.mcmc(w_samples), main = "Traceplot of w")

traplot(as.mcmc(samples), plot.title = "Traceplots")

# Look at the ACF plots
par(mfrow = c(3, 1))
acf(u_samples, lag = 150, main = "ACF plot of u")
acf(v_samples, lag = 150, main = "ACF plot of v")
acf(w_samples, lag = 150, main = "ACF plot of w")

# Take a thinning of length 100
thin = seq(from = 1, to = length(u_samples$u), by = 100)

# Take as samples every 100th observation which become a new sample
u_samples_thin = data.frame(u_samples$u[thin])
v_samples_thin = data.frame(v_samples$v[thin])
w_samples_thin = data.frame(w_samples$w[thin])

# Look at the ACF plots
par(mfrow = c(3, 1))
acf(u_samples_thin, lag = 150, main = "ACF plot of u")
acf(v_samples_thin, lag = 150, main = "ACF plot of v")
acf(w_samples_thin, lag = 150, main = "ACF plot of w")

# Look at the traceplots of the samples
trace_u = traplot(as.mcmc(u_samples_thin), main = "Traceplot of u")
trace_v = traplot(as.mcmc(v_samples_thin), main = "Traceplot of v")
trace_w = traplot(as.mcmc(w_samples_thin), main = "Traceplot of w")



#----------------------------------------Homework 5 Questoin 3 Part(II)

U = matrix(c(1, 0.05, 0.05, 1), nrow = 2, byrow = TRUE)
sigma_u = as.numeric(1 - t(c(0.99, 0.1)) %*% solve(U) %*% c(0.99, 0.1) )
mu_u = t(c(0.99, 0.1)) %*% solve(U)

sigma_v = 1 - (0.05^2)

u = 0; v = 0; w = 0
N = 11000
for(i in 2:(N + 1)){
  v[i] = rnorm(n = 1, mean = (0.05 * w[i - 1]), sd = sqrt(sigma_v))
  u[i] = rnorm(n = 1, mean = as.numeric(mu_u %*% c(v[i], w[i - 1])), sd = sqrt(sigma_u))
  w[i] = rnorm(n = 1, mean = as.numeric(mu_w %*% c(u[i], v[i])), sd = sqrt(sigma_w))
}

burn = 1000
library(mcmcplots)
# Get rid of the initial points
u = u[-1]; v = v[-1]; w = w[-1]

# Take the samples post the burn-in period
u_samples = data.frame(u = u[(burn + 1):N])
v_samples = data.frame(v = v[(burn + 1):N])
w_samples = data.frame(w = w[(burn + 1):N])
samples = data.frame(u = u_samples, v = v_samples, w = w_samples)
# Look at the traceplots of the samples
trace_u = traplot(as.mcmc(u_samples), main = "Traceplot of u")
trace_v = traplot(as.mcmc(v_samples), main = "Traceplot of v")
trace_w = traplot(as.mcmc(w_samples), main = "Traceplot of w")

traplot(as.mcmc(samples), plot.title = "Traceplots")
# Look at the ACF plots
par(mfrow = c(3, 1))
acf(u_samples, lag = 10, main = "ACF plot of u")
acf(v_samples, lag = 10, main = "ACF plot of v")
acf(w_samples, lag = 10, main = "ACF plot of w")


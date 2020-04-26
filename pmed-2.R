# Modelling Probability Exercise 2

dat = read.csv(file = "ml-ex-2.csv")

neglogLikelihood <- function(theta, obs) {
  a = theta[1]
  b = theta[2]
  c = theta[3]
  d = theta[4]
  s = theta[5]
  n = obs[1] 
  x = obs[1+c(1:n)] 
  y = obs[1+c((n+1):(2*n))] 
  logF = dnorm(y, mean = (a + b*x + c*x^2 + d*x^3), sd = s, log = TRUE)
  return(-sum(logF)) 
}

n <- length(dat$time)
obs <- c(n, dat$time, dat$drug_usage)
theta_init = c(80, -450, 650, 300, -1)

out  = optim(theta_init, neglogLikelihood, gr = NULL, obs, method = "L-BFGS-B", lower = c(0.001, -Inf, 0.001, 0.001, 0.05), upper = c(Inf, -0.001, Inf, Inf, Inf))

a <- out$par[1]
b <- out$par[2]
c <- out$par[3]
d <- out$par[4]
s <- out$par[5]
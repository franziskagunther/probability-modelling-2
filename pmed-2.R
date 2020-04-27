# Modelling Probability Exercise 2

library(ggplot2)

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
theta_init = c(80, -450, 650, 300, -1) # change to theta_init = c(80, 0, 0, 0, 1)

out <- optim(theta_init, neglogLikelihood, gr = NULL, obs, method = "L-BFGS-B", lower = c(-Inf, -Inf, -Inf, -Inf, 0.05), upper = c(Inf, Inf, Inf, Inf, Inf))

a <- out$par[1]
b <- out$par[2]
c <- out$par[3]
d <- out$par[4]
s <- out$par[5]

# check spacing of time variable
differences <- integer(300)
for(i in 1:length(dat$time)) {
  differences[i] <- dat$time[i] - dat$time[i+1] 
}

# predict y between days 10 and 15
missing_x <- seq(10 + mean(differences[1:299]), 14.99999999998, length.out=50)

# compute error term
error_y <- obs[1+c((n+1):(2*n))] - (a + obs[1+c(1:n)]*b + obs[1+c(1:n)]^2*c + obs[1+c(1:n)]^3*d)
error_s <- sd(error_y)

missing_y <- a + missing_x*b + missing_x^2*c + missing_x^3*d + rnorm(length(missing_x), 0, error_s)

# correct for those who mistakenly were assigned negative drug usage
missing_y[missing_y < 0] <- 0

dat_pred <- data.frame(time=c(dat$time, missing_x), drug_usage=c(dat$drug_usage, missing_y))
plt = ggplot(dat_pred, aes(x=time, y=drug_usage))
plt = plt + geom_point()
print(plt)

# determine local minimum of function (number of days before patient relapses)

f <- function(x) {a + b*x + c*x^2 + d*x^3}
xmin <- optimize(f, interval = c(0, 30), tol = 0.0001)
print(xmin$minimum)


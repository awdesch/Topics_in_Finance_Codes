# clear all variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

set.seed(100)     # set pseudo random numbers

# parameter settings
n      = 1000     # number of observations
alpha  = 0.1    # alpha
sigma  = 0.1    # vola
mu     = 1.3       # mu

# simulates a mean reverting square root process around m
i      = 0
delta  = 0.1     # time step
x      = mu        # start value

while (i < (n * 10)) {
    i = i + 1
    d = alpha * (mu - x[length(x)]) * delta + sigma * sqrt(delta) * rnorm(1, 0, 1)
    x = rbind(x, x[length(x)] + d)
}

x = x[2:length(x)]
x = x[10 * (1:n)]

# plot
plot(x, type = "l", col = "blue3", lwd = 2, xlab = "x", ylab = "")
title(paste("Simulated OU process for alpha = ", alpha, "& sigma = ", sigma, " around mu= ", mu)) 





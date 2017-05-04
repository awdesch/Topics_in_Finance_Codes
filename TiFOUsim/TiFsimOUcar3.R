# clear all variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

set.seed(100)     # set pseudo random numbers

p = 3; q = 0
#p = 2; q = 0
 alpha = c(0.68, -0.17, 0.05) 
#alpha = c(0.68, 0.17) 

if(p == 2 & q == 0){
  # Matrix of CAR(2) coefficients
  alpha.1 = 2-alpha[1];
  alpha.2=alpha.1-alpha[2]-1;
  A=matrix(c(0, 1, -alpha.2, -alpha.1),2,2,byrow=T);
} else if(p==3 & q==0){
  # Matrix of CAR(3) coefficients
  alpha.1 = 3-alpha[1];
  alpha.2 = 2*alpha.1-alpha[2]-3;
  alpha.3 = alpha.2+1-alpha.1-alpha[3];
  A       = matrix(c(0, 1, 0, 0, 0, 1, -alpha.3, -alpha.2, -alpha.1),3,3,byrow=T);
} else if(p==4 & q==0){
  # Matrix of CAR(4) coefficients
  alpha.1 = 4- alpha[1]
  alpha.2 = 3*alpha.1 - alpha[2] - 6
  alpha.3 = 4 + 2*alpha.2 - alpha[3] - 3*alpha.1
  alpha.4 = alpha.3 - alpha[4] - alpha.2 + alpha.1 - 1
  A       = matrix(c(0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, -alpha.4, -alpha.3, -alpha.2, -alpha.1),4,4,byrow=T)
}
e1      = c(1, rep(0,p-1))
ep      = c(rep(0,p-1),1)
inversa = solve(A)
# parameter settings
n       = 1000           # number of observations
#alpha  = 0.1            # alpha
sigma   = 0.3            # vola
mu      = c(0.5)         # mu

# simulates a mean reverting square root process around m
i       = p - 1
delta   = 1/365            # time step
x       = c(rep(mu,p))     # start value

x = c(rep(NA, p-1), x)
xs  = embed(x, p)

#ds      = seq(delta,by=delta,length=n)

xx = xs
while (i < (n * 10)) {
    i = i + 1
    # i
    if((i < 5)&(i>3)){
    	xs  = embed(c(NA,xx[,1]), p)
    }else if(i >=5){
    	xs  = embed(c(xx[(nrow(xx)-4):nrow(xx),1]), p)
    }
    # xs
    d = (A%*%(xs-mu))[,1]*delta  + sqrt(delta)*t(sigma * rnorm(1, 0, 1)%*%ep)
    # d
    xs = rbind(xs, xs[nrow(xs),] + as.numeric(d[length(d):1]))
    # xs
    xx = rbind(xx, xx[nrow(xx),] + as.numeric(d[length(d):1]))
    # xx
}

plot(xx[,1],type="l")
# clear all variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

set.seed(100)     # set pseudo random numbers

# CARMA(p, q)
p = 3  # 3 AR lags
q = 2  # 2 MA lags

alpha = c(0.68, -0.17, 0.05) 
beta  = c(1, 0.23, 0.35)

# Matrix of CAR(3) coefficients
  alpha.1 = 3-alpha[1];
  alpha.2 = 2*alpha.1-alpha[2]-3;
  alpha.3 = alpha.2+1-alpha.1-alpha[3];
  A       = matrix(c(0, 1, 0, 0, 0, 1, -alpha.3, -alpha.2, -alpha.1),3,3,byrow=T);

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
ys = c()
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
    ys = c(ys, (beta[1]*xs[(nrow(xs)-p+1),1] + beta[2]*xs[(nrow(xs)-p+2),1] + beta[3]*xs[(nrow(xs)),1]))
    # xs
    xx = rbind(xx, xx[nrow(xx),] + as.numeric(d[length(d):1]))
    # xx
}


plot(xx[,1],type="l")
plot(ys,type="l")

library(xts)
ds = zooreg(c(ys),start=as.Date("1990-01-01"))
d.xts = xts(ds,order.by=time(ds))

install.packages("yuima")
library(yuima)

mod4 <- setCarma(p=3, q=2,Carma.var="y", measure=list(intensity="Lamb", df = list("dnorm(z,mu,sig)")),measure.type="CP")
 params = list(a1 = 0.1, a2 = 0.1, a3= 0.1, b0 = 1, b1=1, b2=1, Lamb=1, mu = 1, sig=1)
 
yuima.Data = setYuima(setData(d.xts,delta=1/365), model=mod4)

y.fit = qmle(yuima.Data,start=params)

# ARMA for Google
K = 4
aic = matrix(0, K,K)
  bic = matrix(0, K,K)
  for(i in 1:K){
  	j = 1
  	    for(j in 1:(K)){
	    	if(i>j){
    	  arma.LL = arima(c(ys),order=c(i-1,0,j-1),method="ML",include.mean=F)
	      aic[i,j] = AIC(arma.LL)
    	  bic[i,j] = BIC(arma.LL)
    	      }else if(j>=i){
    		aic[i,j] = 1e16
    		bic[i,j] = 1e16
      }
	    }

  } # auto.arima(YY.BC.DS.TF,max.q=0)#
  id.aic = which(aic==min(aic),arr.ind=T)-1
  id.bic = which(bic==min(bic),arr.ind=T)-1
library(lattice)
levelplot(bic)
levelplot(aic)
p.G = id.aic[1,1]
q.G = id.aic[1,2]
arma.G = arima(xx[,1],order=c(p.G,0,q.G),method="ML",include.mean=F)##

x = x[2:length(x)]
x = x[10 * (1:n)]

# plot
plot(x, type = "l", col = "blue3", lwd = 2, xlab = "x", ylab = "")
#title(paste("Simulated OU process for alpha = ", alpha, "& sigma = ", sigma, " around mu= ", mu)) 
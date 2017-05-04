# time series

rm(list=ls(all=TRUE))
graphics.off()

libraries = c("dygraphs", "quantmod")
lapply(libraries, function(x) if (!(x %in% installed.packages())) { install.packages(x) })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# STOCKS SELECTED:
# GOOG
# XOM (EXXON)
# GE (GENERAL ELECTRIC)
# KO The Coca-Cola Company (KO)
# F Ford
# GS Goldman Sachs
# AIG
# HPQ (HP Company)
# WMT (Walmart)
# MSI (Motorola Solutions)
start = "2005-01-01"
end   = Sys.Date()

tickers = c("GOOG", "XOM", "GE", "KO", "F", "GS", "AIG", "HPQ", "WMT", "MSI")

getSymbols(tickers, src="yahoo", from=start, to=end)

dygraph(MSI$MSI.Close)

ld = cbind(XOM$XOM.Close, GE$GE.Close,KO$KO.Close,F$F.Close,AIG$AIG.Close,HPQ$HPQ.CLose,WMT$WMT.Close,MSI$MSI.Close)

dygraph(ld) %>% dyRangeSelector()


#### Simple example
 G = GOOG$GOOG.Close
plot(1:length(G), G, type="l", ylab="stock price", xlab="time", frame=T, axes=FALSE)
axis(1, floor(seq(1, length(G), length=5)), time(GOOG)[floor(seq(1, length(G), length=5))])
axis(2, seq(0,1000,by=250))
abline(a=as.numeric(G[1]),b=auto.arima(G)$coef)


acf(G,lag.max=1000)
pacf(G,lag.max=1000)

G.logret = diff(log(G))
acf(G.logret[-1])
pacf(G.logret[-1])


# ARMA for Google
K = 9
aic = matrix(0, K,K)
bic = matrix(0, K,K)
for(i in 1:K){
  for(j in 1:(K)){
      arma.G = arima(G.logret, order=c(i-1,0,j-1), method="ML", include.mean=F)
      aic[i,j] = AIC(arma.G)
      bic[i,j] = BIC(arma.G)
  }
}
library(lattice)
levelplot(bic)
levelplot(aic)
id.aicG = which(aic==min(aic),arr.ind=T)
id.bicG = which(bic==min(bic),arr.ind=T)

p.G = id.bicG[1,1]-1
q.G = id.bicG[1,2]-1
arma.G = arima(G.logret,order=c(p.G,0,q.G),method="ML",include.mean=F)##

library(forecast)
auto.arima(G.logret)
plot(G.logret,type="l")

dygraph(G.logret)

G.arima = arima()





#### Simple example
X = XOM$XOM.Close
plot(1:length(X), X, type="l", ylab="stock price", xlab="time", frame=T, axes=FALSE)
axis(1, floor(seq(1, length(X), length=5)), time(X)[floor(seq(1, length(X), length=5))])
axis(2, seq(0,1000,by=20))
X1 = cbind(rep(1,length(X)),1:length(X))
b  = solve(t(X1)%*%X1)%*%t(X1)%*%X
abline(a=b[1],b=b[2])




acf(X,lag.max=1000)
pacf(X,lag.max=1000)

X.logret = diff(log(X))
acf(X.logret[-1])
pacf(X.logret[-1])


# ARMA for EXXON
K = 9
aic = matrix(0, K,K)
bic = matrix(0, K,K)
for(i in 1:K){
  for(j in 1:(K)){
      arma.X = arima(X.logret, order=c(i-1,0,j-1), method="ML", include.mean=F)
      aic[i,j] = AIC(arma.X)
      bic[i,j] = BIC(arma.X)
  }
}
library(lattice)
levelplot(bic)
levelplot(aic)
id.aicX = which(aic==min(aic),arr.ind=T)-1
id.bicX = which(bic==min(bic),arr.ind=T)-1

p.X = id.aicX[1,1]
q.X = id.aicX[1,2]
arma.X = arima(X.logret[-1],order=c(p.X,0,q.X),method="ML",include.mean=F)##
arma.X = Arima(X.logret[-1],order=c(p.X,0,q.X))##

# compare AIC results with ACF and PACF
id.aicX
par(mfrow=c(2,1), oma = c(0.5, 1, 0, 1), mar = c(2.5, 2.3, 1, 1), mgp = c(1.5, 0.5, 0))
acf(X.logret[-1])
pacf(X.logret[-1])


#simulation
S1<-simulate.Arima(arma.X,nsim=600,seed=1) #simulating 600 realisations of the model

# 95% percentile
fore1<-forecast(S1,h=40,level=0.95)
plot(fore1)






auto.arima(X.logret,max.p=10, max.q=10,max.D=1,ic="bic")

library(forecast)
auto.arima(X.logret)
plot(X.logret,type="l")

dygraph(G.logret)

G.arima = arima()
























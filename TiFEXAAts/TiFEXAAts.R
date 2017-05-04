rm(list=ls(all=TRUE))
graphics.off()

libraries = c("dygraphs", "zoo")
lapply(libraries, function(x) if (!(x %in% installed.packages())) { install.packages(x) })
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

EXAA         = read.csv("EXAA.csv",header=T,sep=";",dec=".")
data         = (as.numeric(as.character(EXAA[,26])))
names(data)  = time(zooreg(1:length(data),start=as.Date("2003-01-01")))

day1        = 1:length(data)
nll         = nls(data ~ a + b*day1 + c1*cos(2*pi*(day1)/365) + c2*cos(4*pi*(day1)/365) +c3*cos(2*pi*(day1)/7) + c4*cos(4*pi*(day1)/7) ,start=list(a = 0.1, b = 0.1, c1=0.1,  c2=0.1,c3 = 0.1, c4=0.1),trace=F,control=list(minFactor=1e-99)) 
coefgl      = coef(nll)
TFseas      = coefgl[1] + coefgl[2]*day1 + coefgl[3]*cos(2*pi*(day1)/365) +  coefgl[4]*cos(2*pi*(day1)/365)+ coefgl[5]*cos(2*pi*(day1)/7) +  coefgl[6]*cos(2*pi*(day1)/7)

dygraph(cbind(data,TFseas),ylab="EXAA spot price")
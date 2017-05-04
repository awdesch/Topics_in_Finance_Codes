# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# load data
x   = read.table("bostonh.dat")
xt  = x

boxplot(x) # highly asymmetric data

# Transformations
xt[, 1]   = log(x[, 1])
xt[, 2]   = x[, 2]/10
xt[, 3]   = log(x[, 3])
xt[, 5]   = log(x[, 5])
xt[, 6]   = log(x[, 6])
xt[, 7]   = ((x[, 7]^2.5))/10000
xt[, 8]   = log(x[, 8])
xt[, 9]   = log(x[, 9])
xt[, 10]  = log(x[, 10])
xt[, 11]  = exp(0.4 * x[, 11])/1000
xt[, 12]  = x[, 12]/100
xt[, 13]  = sqrt(x[, 13])
xt[, 14]  = log(x[, 14])

dev.new()
boxplot(xt) # more symmetric and more condensed


constant  = rep(1, length(xt[, 1]))                   # constant for intercept
Z         = as.matrix(cbind(constant, xt[,1:13]))     # variable matrix

y         = xt[, 14]                                  # median value of owner-occupied homes in $1000

n         = dim(Z)[1]                                 # number of observations
p         = dim(Z)[2]                                 # number of variables
df        = n - p                                     # calculate degrees of freedom

b         = solve(t(Z) %*% Z) %*% t(Z) %*% y          # estimate beta using OLS
yhat      = Z %*% b                                   # fitted values
r         = y - yhat                                  # residuals
mse       = t(r) %*% r/df                             # mean square error (unbiased)
covar     = solve(t(Z) %*% Z) %*% diag(rep(mse, 14))  # covariance matrix
se        = sqrt(diag(covar))                         # standard error
t         = b/se                                      # t-statistic
t2        = abs(t)^2                                  # squared absolute t-stat
k         = t2/(df + t2)	                              # distribution of squared-t-vlaues/(squared-t-vlaues + c) ~ chi^2/(chi^2 + c)
                                                      # chi^2/(chi^2 + c) ~ beta-distributed
p         = 0.5 * (1 + sign(t2) * pbeta(k, 0.5, 0.5 * df)) 
Pvalues   = 2 * (1 - p)                               # p-value (since 2-tailed)

tablex = cbind(round(b, 4), round(se, 4), round(t, 3), round(Pvalues, 4))
colnames(tablex) = c("coefficient", "standard error", "t-statistic", "p-value")
rownames(tablex) = c("intercept", "crime rate per capita", "proportion residential land", "proportion nonretail business", "Charles River", "Nitric oxides concentration", "average number of rooms", "proportion build before 1940", "distance to job-center", "index accessibility of highways", "tax rate per $10000", "pupil/teacher ratio", "proportion of African Americans","% lower status of population")
tablex

# alternative estimation:

X = as.matrix(xt[,1:13])
colnames(X) = c("crime rate per capita", "proportion residential land", "proportion nonretail business", "Charles River", "Nitric oxides concentration", "average number of rooms", "proportion build before 1940", "distance to job-center", "index accessibility of highways", "tax rate per $10000", "pupil/teacher ratio", "proportion of African Americans","% lower status of population")
y = as.numeric(xt[,14])
summary(lm(y~X))

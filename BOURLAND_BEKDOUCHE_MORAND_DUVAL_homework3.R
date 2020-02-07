library(lubridate)
library(fpp)
library(tseries)
library(ggfortify)
library(astsa)
library(zoo)
library(ggplot2)
library(FactoMineR)
library(missMDA)
library(vars)
library(imputeTS)
library(xts)
library(tidyr)
library(dplyr)
library(factoextra)
library(MARSS)

# set current working dir to where the file is
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# read data files in 
fred_qd <-read.csv('./FRED-QD.csv')
pc <- read.csv('./PC.csv',  sep=';')

names(pc)[1] <- "observation_date"

# separate fred to info and usuable data 
info <- fred_qd[1:2, ]
fred_qd <- fred_qd[3:nrow(fred_qd), ]

# fred_qd$sasdate <- quarter(fred_qd$sasdate, with_year = TRUE)
pc$observation_date <- quarter(pc$observation_date, with_year = TRUE)
fred_qd$sasdate <- as.Date(fred_qd$sasdate, format = "%m/%d/%Y")

dev.off()

#### QUESTION 1.a ####

# separate training and testing datasets
my_ts <- ts(pc$PCND_PCH,  frequency = 4, start = c(1947, 2))
my_ts_cum <- ts(pc$PCND_PCH,  frequency = 4, start = c(1947,2))
# cumulative next year forecast
for (i in c(4:(length(my_ts)))){
  my_ts_cum[i]=my_ts[i]+my_ts[i-1]+my_ts[i-2]+my_ts[i-3]
  
}

train_data <- window(my_ts, start = c(1947, 2), end = c(1989, 4))
test_data <- window(my_ts, start = c(1989, 4)) # start here because easier to predict later i.e we start predicting for 1990
test_data_cum <- window(my_ts_cum, start = c(1990, 1)) # start here because easier to predict later i.e we start predicting for 1990

## multistep forecast
fcast1 <- ts(start = c(1990, 1), frequency = 4) # transformed back to ts
fcast1_cum <- ts(start = c(1990, 3), frequency = 4)
fcast2_cum <- ts(start = c(1990, 3), frequency = 4)
sigmas <-ts( start = c(1990, 1), frequency = 4)
sigmas_bottom_interval <-ts( start = c(1990, 1), frequency = 4)
sigmas_top_interval <- ts( start = c(1990, 1), frequency = 4)

sigmas_cum <- ts( start = c(1990, 1), frequency = 4)
sigmas_bottom_interval_cum <- ts( start =  c(1990, 1), frequency = 4)
sigmas_top_interval_cum <- ts( start =  c(1990, 1), frequency = 4)
i=1

# predict as many times as there are elements in test data 
for(i in c(1:length(test_data))){
  
  date <- time(test_data)[i] # get the date 
  temp <- window(my_ts, end = date) 
  temp_cum <- window(my_ts_cum, end = date) 
  
  #fcast1.update <- auto.arima(temp) # fit an AR(1)
  #fcast1_cum.update <- auto.arima(temp_cum) # fit an AR(1)
  fcast1.update <- arima(temp, c(1, 0, 0)) # fit an AR(1)
  fcast1_cum.update <- arima(temp_cum, c(1, 0, 0)) # fit an AR(1)
  f = forecast(fcast1.update, 1, level = c(90))
  fc = forecast(fcast1.update, 4, level = c(90))
  fc2 = forecast(fcast1_cum.update, 1, level = c(90))
  if (i==1) {
    fcast1_cum <- ts(c(sum(fc$mean)), start = c(1990, 1), frequency = 4)
    fcast1 <- ts(c(f$mean), start = c(1990, 1), frequency = 4) #fcast1_cum <- ts(c(fcast1_cum, sum(window(fcast1, start =  date - 0.75, end = date))), start = time(fcast1)[1], frequency = 4) # maybe this should add the past 4 predicitons instead of predicting the next 4
    fcast2_cum <- ts(c(fc2$mean), start = c(1990, 1), frequency = 4)
    sigmas_bottom_interval <-ts(c(f$lower), start = c(1990, 1), frequency = 4)
    sigmas_top_interval<- ts(c(f$upper), start = c(1990, 1), frequency = 4)
    sigmas_bottom_interval_cum <- ts(c(sum(fc$lower)), start = c(1990, 1), frequency = 4)
    sigmas_top_interval_cum<- ts(c(sum(fc$upper)), start = c(1990, 1), frequency = 4)
  } else {
    fcast1_cum <- ts(c(fcast1_cum, sum(fc$mean)), start = c(1990, 1), frequency = 4)
    fcast1 <- ts(c(fcast1,f$mean), start =  c(1990, 1), frequency = 4)
    fcast2_cum <- ts(c(fcast2_cum,sum(fc2$mean)), start = c(1990, 1), frequency = 4)
    sigmas_bottom_interval <-ts(c(sigmas_bottom_interval,f$lower), start = c(1990, 1), frequency = 4)
    sigmas_top_interval<- ts(c(sigmas_top_interval,f$upper), start = c(1990, 1), frequency = 4)
    sigmas_bottom_interval_cum <- ts(c(sigmas_bottom_interval_cum,sum(fc$lower)), start = c(1990, 1), frequency = 4)
    sigmas_top_interval_cum<- ts(c(sigmas_top_interval_cum,sum(fc$upper)), start = c(1990, 1), frequency = 4)
  }
  # forecast and add to our forecast array
}

#### QUESTION 1. b ####
## One step ahead forecast
plot(test_data, col = 'black', xlab = 'Year', ylab = 'Rate of U.S personal consumption of non-durable goods', 
     main = 'Forecast and realisation from 1990:Q1 to 2019:Q3', 
     xlim = c(1990, 2020))
lines(fcast1, col = 'blue')
legend(x = 'bottomright', legend = c ('Realisation', 'Forecast'), col = c('black', 'blue'), lty = 1:1, cex = 1)

plot(test_data_cum, col = 'black', xlab = 'Year', ylab = 'Rate of U.S personal consumption of non-durable goods', 
     main = 'Forecast and realisation from 1990:Q1 to 2019:Q3', 
     xlim = c(1991, 2020),
     ylim = c(-19,19))
lines(fcast1_cum, col = 'blue')
legend(x = 'bottomright', legend = c ('Realisation', 'Forecast'), col = c('black', 'blue'), lty = 1:1, cex = 1)

plot(test_data_cum, col = 'black', xlab = 'Year', ylab = 'Rate of U.S personal consumption of non-durable goods', 
     main = 'Forecast and realisation from 1990:Q1 to 2019:Q3', 
     xlim = c(1991, 2020),
     ylim = c(-19,19))
lines(fcast2_cum, col = 'blue')
legend(x = 'bottomright', legend = c ('Realisation', 'Forecast'), col = c('black', 'blue'), lty = 1:1, cex = 1)

plot(test_data_cum, col = 'black', xlab = 'Year', ylab = 'Rate of U.S personal consumption of non-durable goods', 
     main = 'Forecast and realisation from 1990:Q1 to 2019:Q3', 
     xlim = c(1991, 2020),
     ylim = c(-19,19))
lines(fcast1_cum, col = 'red')
lines(fcast2_cum, col = 'blue')
legend(x = 'bottomright', legend = c ('Realisation', 'Forecast'), col = c('black', 'blue'), lty = 1:1, cex = 1)

#### QUESTION 1.c ####
fcast1 <- window(fcast1, start = 1989.75, end = 2019.5)
fcas1_cum <- window(fcast1_cum, start = 1989.75, end = 2019.5)

residuals_one_step = test_data - fcast1
residuals_cum = test_data_cum - fcast1_cum
residuals_cum2 = test_data_cum - fcast2_cum
# check if the residuals are uncorrelated if no -> good yes -> some information has not be captured in the data 
# check if the residuals have zero mean -> no: forecast is biased 
# check if normally distributed 
# check if constant variance 
plot(residuals_one_step)
plot(residuals_cum)
plot(residuals_cum2)

gghistogram(residuals_one_step) + ggtitle('Histogram of Residuals')
gghistogram(residuals_cum) + ggtitle('Histogram of Residuals')
gghistogram(residuals_cum2) + ggtitle('Histogram of Residuals')

ggAcf(residuals_one_step) # lack of correlation -> sign suggesting that the forecast is good 
ggAcf(residuals_cum) # there seems to be a correlation fore this forecast 
ggAcf(residuals_cum2) # there seems to be a correlation fore this forecast 

shapiro.test(residuals_one_step)

# check residuals does the three graphs in one go 
checkresiduals(residuals_one_step)
checkresiduals(residuals_cum)
checkresiduals(residuals_cum2)

mean(residuals_one_step) # mean is ~-0.3 i.e. pretty much unbiased
mean(residuals_cum) # mean is ~-4 ie biased 
mean(residuals_cum2) # mean is ~-4 ie biased 

## Performing a test 
t.test(residuals_one_step, y = NULL, alternative = "two.sided", mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(residuals_cum, y = NULL, alternative = "two.sided", mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(residuals_cum2, y = NULL, alternative = "two.sided", mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

### QUESTION 1.d PLOTS ####

## One step ahead forecast
plot(test_data, col = 'black', xlab = 'Year', ylab = 'Rate of U.S personal consumption of non-durable goods', 
     main = 'Forecast and realisation from 1990:Q1 to 2019:Q3', 
     xlim = c(1990, 2020), 
     ylim = c(-7, 5))
lines(fcast1, col = 'blue')
lines(sigmas_bottom_interval, lty = 'dashed', col = 'red')
lines(sigmas_top_interval, lty = 'dashed', col = 'red')
legend(x = 'bottomleft', legend = c ('Realisation', 'Forecast', '90% Confidence Interval'), col = c('black', 'blue', 'red'), lty = c(1,1,2), cex = 1)

## OTHER PLOT 

### QUESTION 1.e #### 
## Assessment of the prediction interval 
upper_bounds <- sigmas_top_interval
lower_bounds <- sigmas_bottom_interval
lower_bounds_cum <- sigmas_bottom_interval_cum
upper_bounds_cum <- sigmas_top_interval_cum

preds <- fcast1

results <- ts(0, start = c(1990,1), end = c(2019, 4), frequency = 4)
results_cum <- ts(0, start = c(1990,1), end = c(2019, 4), frequency = 4)

for(i in 1:length(results)){
  results[i] <- between(test_data[i], lower_bounds[i], upper_bounds[i]) 
  results_cum[i] <- between(test_data_cum[i], lower_bounds_cum[i], upper_bounds_cum[i]) 
} 

percentage <- sum(results, na.rm = TRUE) / length(results) * 100
percentage_cum <- sum(results_cum, na.rm = TRUE) / length(results_cum) * 100
print(percentage) # 97 % 
print(percentage_cum) # 96 % 

#Perform a Diebold-Mariano test for the null that the MSFE of cumct+4|t and cumc ]t+4|t are equal
dm.test(residuals_cum2, residuals_cum,alternative = c("two.sided", "less", "greater"), h = 1, power = 2)

#### QUESTION 2.a ####

## Dealing with NA -----
sapply(fred_qd, class)
colSums(is.na(fred_qd)) 

# 1. inputeTS package - only works for univariate but clear visualisation

# single line plot - example of 132 missing values in column ACOGNOx
ggplot(fred_qd, aes(x = sasdate, y = ACOGNOx)) + 
  geom_line(aes(color = ACOGNOx), size = 1, show.legend = FALSE) +
  theme_minimal()

# NA distribution plot
dummy = fred_qd[,c("sasdate", "TCU")]
TCU_ts = as.xts(dummy$TCU, order.by = dummy$sasdate)
plotNA.distribution(TCU_ts)

dummy = fred_qd[,c("sasdate", "ACOGNOx")]
ACONGNOx_ts = as.xts(dummy$ACOGNOx, order.by = dummy$sasdate)
plotNA.distribution(ACONGNOx_ts)

# replacing missing values 
# kalman
imp <- na_kalman(ACONGNOx_ts)
plotNA.imputations(ACONGNOx_ts, imp)

# interpolation
imp <- na_interpolation(ACONGNOx_ts)
plotNA.imputations(ACONGNOx_ts, imp)

# next observation carried backward
imp <- na_locf(ACONGNOx_ts, option = "nocf")
plotNA.imputations(ACONGNOx_ts, imp)

# moving average
imp <- na_ma(ACONGNOx_ts, weighting = "exponential", maxgap = Inf)
plotNA.imputations(ACONGNOx_ts, imp)

# 2. missMDA package - PCA and missing value for multivariate ---
# completes the data without impacting the outputted PCA
nb = estim_ncpPCA(fred_qd[,-1], ncp.max = 5)
res.imp = imputePCA(fred_qd[,-1],ncp=nb$ncp)
fred_pca = PCA(res.imp$completeObs)
summary(fred_pca) # only three dimensions necessary 

# visualisation
fviz_eig(fred_pca, addlabels = TRUE, ylim = c(0, 80))

#### QUESTION 2.b ####
factors = data.frame(fred_pca$ind$coord) # extract 5 principal components

# using pc
pc$observation_date = as.Date(as.yearqtr(pc$observation_date, format = "%y.%Q"))
PC <- pc %>% filter(observation_date >= "1959-01-01", observation_date < "2019-09-01")

# create the matrix used for the VAR model
variables <- cbind(PC, factors[1:4]) #factors[1:4] if reducing to four principal components
ts.matrix <- ts(variables, frequency = 4, start = c(1959, 1))

# verify stationarity
adf.test(ts.matrix[,6]) # stationary p < 0.05

# using Granger causality to chose the number of principal components to use
grangertest(PCND_PCH ~ Dim.4, order = 8, data = ts.matrix) # p = 0.215 > 0.05 Null hypothesis cannot be rejected

# selecting the correct VAR model
VARselect(ts.matrix, lag.max = 12, type = "const")
VARmodel <- VAR(ts.matrix, p=8) # from VARselect
summary(VARmodel)

#### QUESTION 2.c ####
fcast2 <- ts() # transformed back to ts
  
for(i in c(1:length(test_data))){
  date <- time(test_data)[i] # get the date 
  temp <- window(ts.matrix, end = date) 
  fcast2.update <- VAR(temp, p=8)
  # fcast2 <- ts(c(fcast2, forecast(fcast2.update, 1)$forecast$PCND_PCH$mean), start = time(fcast2)[1], frequency = 4) # forecast and add to our forecast array 
  if (i==1) {
    fcast2 <- ts(c(forecast(fcast2.update, 1)$forecast$PCND_PCH$mean), start = c(1989, 4), frequency = 4)
  } else{
    fcast2 <- ts(c(fcast2, forecast(fcast2.update, 1)$forecast$PCND_PCH$mean), start = time(fcast2)[1], frequency = 4)
  }
  }

## one step ahead forecast
plot(test_data, col = 'black', xlab = 'Year', ylab = 'Rate of U.S personal consumption of non-durable goods', 
     main = 'Forecast and realisation from 1990:Q1 to 2019:Q3', 
     xlim = c(1990, 2020))
lines(fcast2, col = 'blue')
legend(x = 'bottomright', legend = c ('Realisation', 'Forecast'), col = c('black', 'blue'), lty = 1:1, cex = 1)

#### QUESTION 2.d ####

## define errors
e1 = test_data - fcast1
e2 = test_data - fcast2

## check for bias
shapiro.test(e2) # check for normality of residuals
checkresiduals(e2) # plots
mean(e2)

## Performing a test 
# biased at a confidence level of 95%
t.test(e2, y = NULL, alternative = "two.sided", mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

## compare models 
accuracy(fcast1, test_data)
accuracy(fcast2, test_data)

# Diebold-Mariano test 
dm.test(e1, e2) #alternative = c("two.sided", "less", "greater"), h = 1, power = 2 

#### QUESTION 2.f ####
date <- time(test_data)[1] # get the date 
temp <- window(ts.matrix, end = date) 
dat <- ts.matrix
dat <- t(dat) # transpose
dat <- dat[-1,] # remove date

N_ts <- dim(dat)[1]

clr <- c("brown","blue","darkgreen","darkred","purple")

## get length of time series
TT <- dim(dat)[2] 
y_bar <- apply(dat, 1, mean, na.rm=TRUE)
dat_bar <- dat - y_bar
rownames(dat_bar) = rownames(dat)

Z_vals <- list("z11",  0  ,  0  ,
               "z21","z22",  0  ,
               "z31","z32","z33",
               "z41","z42","z43",
               "z51","z52","z53")
ZZ <- matrix(Z_vals, nrow=N_ts, ncol=3, byrow=TRUE)
ZZ

## 'aa' is the offset/scaling
aa <- "zero"
## 'DD' and 'd' are for covariates
DD <- "zero"  # matrix(0,mm,1)
dd <- "zero"  # matrix(0,1,wk_last)
## 'RR' is var-cov matrix for obs errors
RR <- "diagonal and unequal"

## number of processes
mm <- 3
## 'BB' is identity: 1's along the diagonal & 0's elsewhere
BB <- "identity"  # diag(mm)
## 'uu' is a column vector of 0's
uu <- "zero"  # matrix(0,mm,1)
## 'CC' and 'cc' are for covariates
CC <- "zero"  # matrix(0,mm,1)
cc <- "zero"  # matrix(0,1,wk_last)
## 'QQ' is identity
QQ <- "identity"  # diag(mm)

## list with specifications for model vectors/matrices
mod_list <- list(Z=ZZ, A=aa, D=DD, d=dd, R=RR,
                 B=BB, U=uu, C=CC, c=cc, Q=QQ)
## list with model inits
init_list <- list(x0 = matrix(rep(0, mm), mm, 1))
## list with model control parameters
con_list <- list(maxit = 3000, allow.degen = TRUE)

## fit MARSS
dfa_1 <- MARSS(y = dat_bar, model = mod_list, inits = init_list, control = con_list)
# check the articles for FAVAR and selection of components
# double check notes on state space
# calculate best model

get_DFA_fits <- function(MLEobj,dd=NULL,alpha=0.1) {
  ## empty list for results
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type="matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat
  ## check for covars
  if(!is.null(dd)) {
    DD <- coef(MLEobj, type="matrix")$D
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd
  } else {
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states
  }
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT
  VV <- NULL
  for(tt in 1:TT) {
    RZVZ <- coef(MLEobj, type="matrix")$R - ZZ%*%VtT[,,tt]%*%t(ZZ)
    SS <- Ey$yxtT[,,tt] - Ey$ytT[,tt,drop=FALSE] %*% t(MLEobj$states[,tt,drop=FALSE])
    VV <- cbind(VV,diag(RZVZ + SS%*%t(ZZ) + ZZ%*%t(SS)))
  }
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI
  fits$up <- qnorm(1-alpha/2)*SE + fits$ex
  fits$lo <- qnorm(alpha/2)*SE + fits$ex
  return(fits)
}

## get the estimated ZZ
Z_est <- coef(dfa_1, type="matrix")$Z
## get the inverse of the rotation matrix
H_inv <- varimax(Z_est)$rotmat
## rotate factor loadings
Z_rot = Z_est %*% H_inv   
## rotate processes
proc_rot = solve(H_inv) %*% dfa_1$states

ylbl <- ts.matrix[,2]
w_ts <- seq(dim(dat)[2])
layout(matrix(c(1,2,3,4,5,6),mm,2),widths=c(2,1))
## par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
par(mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))

## plot the processes
for(i in 1:mm) {
  ylm <- c(-1,1)*max(abs(proc_rot[i,]))
  ## set up plot area
  plot(w_ts,proc_rot[i,], type="n", bty="L",
       ylim=ylm, xlab="", ylab="", xaxt="n")
  ## draw zero-line
  abline(h=0, col="gray")
  ## plot trend line
  lines(w_ts,proc_rot[i,], lwd=2)
  lines(w_ts,proc_rot[i,], lwd=2)
  ## add panel labels
  mtext(paste("State",i), side=3, line=0.5)
  axis(1,12*(0:dim(dat)[2])+1,1990+0:dim(dat)[2])
}

## plot the loadings
minZ <- 0
ylm <- c(-1,1)*max(abs(Z_rot))
for(i in 1:mm) {
  plot(c(1:N_ts)[abs(Z_rot[,i])>minZ], as.vector(Z_rot[abs(Z_rot[,i])>minZ,i]), type="h",
       lwd=2, xlab="", ylab="", xaxt="n", ylim=ylm, xlim=c(0.5,N_ts+0.5), col=clr)
  for(j in 1:N_ts) {
    if(Z_rot[j,i] > minZ) {text(j, -0.03, ylbl[j], srt=90, adj=1, cex=1.2, col=clr[j])}
    if(Z_rot[j,i] < -minZ) {text(j, 0.03, ylbl[j], srt=90, adj=0, cex=1.2, col=clr[j])}
    abline(h=0, lwd=1.5, col="gray")
  } 
  mtext(paste("Factor loadings on state",i),side=3,line=0.5)
}

## get model fits & CI's
mod_fit <- get_DFA_fits(dfa_1)

## plot the fits
ylbl <- ts.matrix[,2]
par(mfrow=c(N_ts,1), mai=c(0.5,0.7,0.1,0.1), omi=c(0,0,0,0))
for(i in 1:N_ts) {
  up <- mod_fit$up[i,]
  mn <- mod_fit$ex[i,]
  lo <- mod_fit$lo[i,]
  plot(w_ts,mn,xlab="",ylab=ylbl[i],xaxt="n",type="n", cex.lab=1.2,
       ylim=c(min(lo),max(up)))
  axis(1,12*(0:dim(dat)[2])+1,1990+0:dim(dat)[2])
  points(w_ts,dat[i,], pch=16, col=clr[i])
  lines(w_ts, up, col="darkgray")
  lines(w_ts, mn, col="black", lwd=2)
  lines(w_ts, lo, col="darkgray")
}


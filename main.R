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
require(dplyr)

# set current working dir to where the file is
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# read data files in 
fred_qd <-read.csv('./data/FRED-QD.csv')
pc <- read.csv('./data/PC.csv',  sep=';')

names(pc)[1] <- "observation_date"

# separate fred to info and usuable data 
info <- fred_qd[1:2, ]
fred_qd <- fred_qd[3:nrow(fred_qd), ]

# fred_qd$sasdate <- quarter(fred_qd$sasdate, with_year = TRUE)
pc$observation_date <- quarter(pc$observation_date, with_year = TRUE)
fred_qd$sasdate <- as.Date(fred_qd$sasdate, format = "%m/%d/%Y")

#### QUESTION 1.a ####

# separate training and testing datasets

my_ts <- ts(pc$PCND_PCH,  frequency = 4, start = c(1947, 2))
train_data <- window(my_ts, start = c(1947, 2), end = c(1989, 4))
test_data <- window(my_ts, start = c(1989, 4), end = c(2019, 3)) # start here because easier to predict later i.e we start predicting for 1990

## multistep forecast
fcast1 <- as.ts(as.zoo(train_data)[(length(train_data) - 1):length(train_data)]) # transformed back to ts

fcast1_cum <- fcast1
# predict as many times as there are elements in test data 
for(i in c(1:length(test_data))){
  date <- time(test_data)[i] # get the date 
  temp <- window(my_ts, end = date) 
  fcast1.update <- arima(temp, c(1, 0, 0)) # fit an AR(1)
  fcast1 <- ts(c(fcast1, forecast(fcast1.update, 1)$mean), start = time(fcast1)[1], frequency = 4) # forecast and add to our forecast array 
  fcast1_cum <- ts(c(fcast1_cum, sum(forecast(fcast1.update, 4)$mean)), start = time(fcast1)[1], frequency = 4) # maybe this should add the past 4 predicitons instead of predicting the next 4
}

#### QUESTION 1.b ####

## One step ahead forecast
plot(test_data, col = 'black', xlab = 'Year', ylab = 'Rate of U.S personal consumption of non-durable goods', 
     main = 'Forecast and realisation from 1990:Q1 to 2019:Q3', 
     xlim = c(1990, 2020))
lines(fcast1, col = 'blue')
legend(x = 'bottomright', legend = c ('Realisation', 'Forecast'), col = c('black', 'blue'), lty = 1:1, cex = 1)

# cumulative next year forecast 
plot(test_data, col = 'black', xlab = 'Year', ylab = 'Rate of U.S personal consumption of non-durable goods', 
     main = 'Forecast and realisation from 1990:Q1 to 2019:Q3', 
     xlim = c(1990, 2020), 
     ylim = c(-7, 7))
lines(fcast1_cum, col = 'blue')
legend(x = 'bottomright', legend = c ('Realisation', 'Forecast'), col = c('black', 'blue'), lty = 1:1, cex = 1)

#### QUESTION 1.c ####

# checkresiduals(fit)  ## faire une analyise dessus --  https://otexts.com/fpp2/regression-evaluation.html

#### QUESTION 1.d ####


#### QUESTION 2 ####

## data visualisation
df <- fred_qd %>%
  gather(key = "variable", value = "value", -sasdate)

ggplot(df, aes(x = sasdate, y = value)) + 
  geom_line(aes(color = variable), size = 1, show.legend = FALSE) + 
  labs(x = "Time", y = "Value")

#### QUESTION 2.a ####

## Dealing with NA
sapply(fred_qd, class)
colSums(is.na(fred_qd)) 

# inputeTS package - only works for univariate but clear visualisation

# single line plot - example of 132 missing values in column ACOGNOx
ggplot(fred_qd, aes(x = sasdate, y = ACOGNOx)) + 
  geom_line(aes(color = ACOGNOx), size = 1, show.legend = FALSE) +
  theme_minimal()

# NA distribution plot
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

# missMDA package - PCA and missing value for multivariate
# completes the data without impacting the outputted PCA
nb = estim_ncpPCA(fred_qd[,-1], ncp.max = 5)
res.imp = imputePCA(fred_qd[,-1],ncp=nb$ncp)
fred_pca = PCA(res.imp$completeObs)
summary(fred_pca) # only three dimensions necessary 

## question 2.b
factors = data.frame(fred_pca$ind$coord) # extract 5 principal components

# using pc - WRONG SHOULD BE ct
# pc$observation_date = as.Date(as.yearqtr(pc$observation_date, format = "%y.%Q"))
# PC <- pc %>% filter(observation_date >= "1959-01-01", observation_date < "2019-09-01")
# variables <- cbind(PC, factors)

# using ct
ct <- forecast(fcast1.update, 1)
ct <- data.frame(Y=as.matrix(ct$x), date=as.Date(as.yearmon(time(ct$x))))
ct <- ct %>% filter(date >= "1959-01-01", date <= "2019-07-01")
variables <- cbind(ct, factors)

VARselect(variables[,-2], lag.max = 12, type = "const") # change to variables [,-1] if using pc
VARmodel <- VAR(variables[,-2], p=2) # from VARselect
summary(VARmodel)

# question 2.c
for(i in c(1:length(test_data))){
  date <- time(test_data)[i] # get the date 
  temp <- window(my_ts, end = date) 
  # fcast2.update <- arima(temp, c(1, 0, 0)) # fit an AR(1)
  fcast1 <- ts(c(fcast1, forecast(VARmodel, 1)$mean), start = time(fcast1)[1], frequency = 4) # forecast and add to our forecast array 
  fcast1_cum <- ts(c(fcast1_cum, sum(forecast(fcast1.update, 4)$mean)), start = time(fcast1)[1], frequency = 4) # maybe this should add the past 4 predicitons instead of predicting the next 4
}

a=forecast(VARmodel$datamat, 1)



prd <- predict(VARmodel, n.ahead = 10, ci = 0.95, dumvar = NULL)
print(prd)
plot(prd, "single")

FORCAST1 = data.frame(Y=as.matrix(fcast1), date=as.Date(as.yearmon(time(fcast1))))


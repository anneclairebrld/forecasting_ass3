library(lubridate)
library(fpp)
library(tseries)
library(ggfortify)
library(tidyverse)
library(astsa)
library(zoo)

# set current working dir to where the file is
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# read data files in 
fred_qd <-read.csv('./data/FRED-QD.csv')
pc <- read.csv('./data/PC.csv',  sep=';')

# separate fred to info and usuable data 
info <- fred_qd[1:2, ]
fred_qd <- fred_qd[3:nrow(fred_qd), ]

fred_qd$sasdate <- quarter(fred_qd$sasdate, with_year = TRUE)
pc$observation_date <- quarter(pc$observation_date, with_year = TRUE)

#### QUESTION 1.a ####

# separate training and testing datasets
my_ts <- ts(pc$PCND_PCH, frequency = 4, start = c(1942, 2))
train_data <- window(my_ts, start = c(1942, 2), end = c(1989, 4))
test_data <- window(my_ts, start = c(1989, 4)) # start here because easier to predict later i.e we start predicting for 1990

## multistep forecast
fcast1 <- as.ts(as.zoo(train_data)[(length(train_data) - 1):length(train_data)]) # transformed back to ts
fcast1_cum <- fcast1
# predict as many times as there are elements in test data 
for(i in c(1:length(test_data))){
  date <- time(test_data)[i] # get the date 
  temp <- window(my_ts, start = 1942.25, end = date) # date - 0.75 as we look back 4 quarters and date is included date = what we want to predict - 1 quarter
  fcast1.update <- arima(temp, c(1, 0, 0)) # fit an AR(1)
  fcast1 <- ts(c(fcast1, forecast(fcast1.update, 1)$mean), start = time(fcast1)[1], frequency = 4) # forecast and add to our forecast array 
  fcast1_cum <- ts(c(fcast1_cum, sum(forecast(fcast1.update, 4)$mean)), start = time(fcast1)[1], frequency = 4)
}


#### QUESTION 1.b ####
plot(train_data, col = 'blue', xlab = 'Year', 
     ylab = 'Quartely growth rate of U.S personal consumption\n of non-durable goods',  
     main='Multistep forecast of personal consumprion of non-durable goods', 
     xlim = c(1942, 2019))  ## add correct legend ?)
lines(fcast1, col = 'black') 
lines(test_data, col = 'red') 
lines(fcast1_cum, col = 'green')

plot(my_ts)


#### QUESTION 1.c ####
# checkresiduals(fit)  ## faire une analyise dessus --  https://otexts.com/fpp2/regression-evaluation.html

#### QUESTION 1.d ####






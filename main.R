library(lubridate)
library(fpp)
library(tseries)
library(ggfortify)
library(tidyverse)


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
my_ts <- ts(pc$PCND_PCH, frequency = 4, start = c(1942, 2), end = c(2019, 3))
train_data <- window(my_ts, start = c(1942, 2), end = c(1989, 4))
test_data <- window(my_ts, start = c(1990, 1), end = c(2019, 3))
# maybe do seasonal decomposition before 
  
# Forecasting AR 1 
fit <- Arima(train_data, c(1, 0, 0))
fcast1 <- forecast(fit, h = 76) # h is the number of predictions maybe should be 76?

#### QUESTION 1.b ####
plot(fcast1, xlab = 'Year', ylab = 'Quartely growth rate of U.S personal consumption\n of non-durable goods') + ## add correct legend ?
  lines(test_data, col = 'red', lty=2) 


  






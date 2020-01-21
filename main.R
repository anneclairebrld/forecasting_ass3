library(lubridate)
library(fpp)
library(tseries)
library(ggfortify)
library(astsa)
library(zoo)
library(dplyr)

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

my_ts <- ts(pc$PCND_PCH,  frequency = 4, start = c(1947, 2))
train_data <- window(my_ts, start = c(1947, 2), end = c(1989, 4))
test_data <- window(my_ts, start = c(1989, 4)) # start here because easier to predict later i.e we start predicting for 1990

## multistep forecast
fcast1 <- as.ts(as.zoo(train_data)[(length(train_data) - 4):length(train_data)]) # transformed back to ts
fcast1_cum <- fcast1
sigmas <- ts(0, start = c(1989,4), end = c(2019, 3), frequency = 4)
sigmas_bottom_interval <- ts(0, start = c(1989,4), end = c(2019, 3), frequency = 4)
sigmas_top_interval <- ts(0, start = c(1989,4), end = c(2019, 3), frequency = 4)

sigmas_cum <- ts(0, start = c(1989,4), end = c(2019, 3), frequency = 4)
sigmas_bottom_interval_cum <- ts(0, start = c(1989,4), end = c(2019, 3), frequency = 4)
sigmas_top_interval_cum <- ts(0, start = c(1989,4), end = c(2019, 3), frequency = 4)

# predict as many times as there are elements in test data 
for(i in c(1:length(test_data))){
  date <- time(test_data)[i] # get the date 
  temp <- window(my_ts, end = date) 
  
  fcast1.update <- arima(temp, c(1, 0, 0)) # fit an AR(1)
  fcast1 <- ts(c(fcast1, forecast(fcast1.update, 1)$mean), start = time(fcast1)[1], frequency = 4) # forecast and add to our forecast array
  fcast1_cum <- ts(c(fcast1_cum, sum(window(fcast1, start =  date - 0.75, end = date))), start = time(fcast1)[1], frequency = 4) # maybe this should add the past 4 predicitons instead of predicting the next 4
  
  # question 1.d 
  sigmas[i]  <- fcast1.update$sigma2 # this is for question 1. d
  sigmas_bottom_interval[i] <- forecast(fcast1.update, 1)$mean - 1.65*sigmas[i]
  sigmas_top_interval[i] <- forecast(fcast1.update, 1)$mean + 1.65*sigmas[i]
  
  
  ## might just have a problem for this part 
  sigmas[i]  <- fcast1.update$sigma2 # this is for question 1. d
  sigmas_bottom_interval[i] <- forecast(fcast1.update, 1)$mean - 1.65*sigmas[i]
  sigmas_top_interval[i] <- forecast(fcast1.update, 1)$mean + 1.65*sigmas[i]
}

#### QUESTION 1. b ####
## One step ahead forecast
plot(test_data, col = 'black', xlab = 'Year', ylab = 'Rate of U.S personal consumption of non-durable goods', 
     main = 'Forecast and realisation from 1990:Q1 to 2019:Q3', 
     xlim = c(1990, 2020))
lines(fcast1, col = 'blue')
legend(x = 'bottomright', legend = c ('Realisation', 'Forecast'), col = c('black', 'blue'), lty = 1:1, cex = 1)

# cumulative next year forecast 
plot(test_data, col = 'black', xlab = 'Year', ylab = 'Rate of U.S personal consumption of non-durable goods', 
     main = 'Forecast and realisation from 1990:Q1 to 2019:Q3', 
     xlim = c(1991, 2020), 
     ylim = c(-7, 7))
lines(fcast1_cum, col = 'blue')
legend(x = 'bottomright', legend = c ('Realisation', 'Forecast'), col = c('black', 'blue'), lty = 1:1, cex = 1)


#### QUESTION 1.c ####
fcast1 <- window(fcast1, start = 1989.75, end = 2019.5)
fcas1_cum <- window(fcast1_cum, start = 1989.75, end = 2019.5)

residuals_one_step = test_data - fcast1
residuals_cum = test_data - fcast1_cum   ## not sure this is correct, this should be test_data_cum - fcast1_cum?

# check if the residuals are uncorrelated if no -> good yes -> some information has not be captured in the data 
# check if the residuals have zero mean -> no: forecast is biased 
# check if normally distributed 
# check if constant variance 
plot(residuals_one_step)
plot(residuals_cum)

gghistogram(residuals_one_step) + ggtitle('Histogram of Residuals')
gghistogram(residuals_cum) + ggtitle('Histogram of Residuals')

ggAcf(residuals_one_step) # lack of correlation -> sign suggesting that the forecast is good 
ggAcf(residuals_cum) # there seems to be a correlation fore this forecast 

shapiro.test(forecast_error_one_step)

# check residuals does the three graphs in one go 
checkresiduals(residuals_one_step)
checkresiduals(residuals_cum)

mean(residuals_one_step) # mean is ~-0.3 i.e. pretty much unbiased
mean(residuals_cum) # mean is ~-4 ie biased 

## Performing a test 
#Not normally distributed residuals ?? 
t.test(residuals_one_step, y = NULL, alternative = "greater", mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)
#Biased at a confidence level of 95% ?? 
t.test(residuals_cum, y = NULL, alternative = "greater", mu = 0, 
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

preds <- fcast1
  
results <- ts(0, start = c(1989,4), end = c(2019, 3), frequency = 4)
results_cum <- ts(0, start = c(1989,4), end = c(2019, 3), frequency = 4)

for(i in 1:length(results)){
  results[i] <- between(test_data[i], lower_bounds[i], upper_bounds[i]) 
  # results_cum[i] <- between(test_data[i], lower_bounds_cum[i], upper_bounds_cum[i]) 
} 

percentage <- sum(results, na.rm = TRUE) / length(results) * 100
percentage_cum <- sum(results_cum, na.rm = TRUE) / length(results_cum) * 100
print(percentage) # 98 % 






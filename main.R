library(lubridate)
library(fpp)
library(tseries)


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
pc$year <- substring(pc$observation_date, 1, 4)

#### QUESTION 1.a ###

# separate training and testing datasets
test_data <- pc[(pc$year <= 2019) & (pc$year >= 1990), ]
train_data <- pc[pc$year < 1990, ]

# Forecasting AR 1 
fit <- Arima(train_data$PCND_PCH, c(1, 0, 0),  seasonal=FALSE)
fcast1 <- forecast(fit)

# And plot 1 
plot(fcast1) ## add correct legend ?





library(lubridate)

# set current working dir to where the file is
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# read data files in 
fred_qd <-read.csv('./data/FRED-QD.csv')
#pc <- read.delim('./data/PC.csv',';',  escape_double = FALSE, trim_ws = TRUE)

# separate fred to info and usuable data 
info <- fred_qd[1:2, ]
fred_qd <- fred_qd[3:nrow(fred_qd), ]

## creating date and quarter labels 
fred_qd$sasdate <- as.Date(fred_qd$sasdate, format='%m/%d/%Y')
fred_qd$year <- as.numeric(format(fred_qd$sasdate, '%Y'))

fred_qd$quarter[as.numeric(format(fred_qd$sasdate, '%m')) == 3] <- 'Q1'
fred_qd$quarter[as.numeric(format(fred_qd$sasdate, '%m')) == 6] <- 'Q2'
fred_qd$quarter[as.numeric(format(fred_qd$sasdate, '%m')) == 9] <- 'Q3'
fred_qd$quarter[as.numeric(format(fred_qd$sasdate, '%m')) == 12] <- 'Q4'

pc$

#### QUESTION 1.a ###

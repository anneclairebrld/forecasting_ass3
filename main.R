# set current working dir to where the file is
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# read data files in 
fred_qd <-read.csv('./data/FRED-QD.csv')
pc <- read.csv('./data/PC.csv')

# separate fred to info and usuable data 
info <- fred_qd[1:2, ]
fred_qd <- fred_qd[3:nrow(fred_qd), ]



#### QUESTION 1.a ###
head(pc)

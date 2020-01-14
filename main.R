# set current working dir to where the file is
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# read data files in 
fred_qd <-file('./data/FRED-QD.csv', 'r')
pc <-file('./data/PC.csv', 'r')


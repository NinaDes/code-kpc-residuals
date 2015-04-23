data = read.table(file='data.txt')
data = data[1:50,]


source('kPC_Residuals.R')

a = kPC_Residuals(data = data, alpha = 0.6, sig = 1, p = 100, test = 2)

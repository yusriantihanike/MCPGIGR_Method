data <- readxl::read_excel("data/2023.xlsx")

y1 = as.matrix(data[,3])
y2 = as.matrix(data[,4])
y  = cbind(y1,y2)
n  = length(y1)

constant = as.matrix(rep(1,n))
x        = as.matrix(data[,c(7:12)])
xb       = as.matrix(cbind(constant,x))
q1       = as.matrix(data[,5])#exposure q1
q2       = as.matrix(data[,6])
p        = ncol(xb)
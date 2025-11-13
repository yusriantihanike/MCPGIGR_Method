y_1 = as.matrix(data[,3])
y_2 = as.matrix(data[,4])
x_1 = as.matrix(data[,7])
x_2 = as.matrix(data[,8])
x_3 = as.matrix(data[,9])
x_4 = as.matrix(data[,10])
x_5 = as.matrix(data[,11])
x_6 = as.matrix(data[,12])
q1  = as.matrix(data[,5])
q2  = as.matrix(data[,6])

y3=y_2
for (i in 1:n){
  if(y3[i]>170){
    
    y3[i]<-0
  }else{
    y3[i]<-y3[i]
  }
  y3[i]<-y3[i]
}
maxim<-max(y3)


for (i in 1:n){
  if(y3[i]==0){
    
    y3[i]<-maxim
  }else{
    y3[i]<-y3[i]
  }
}

y_2=y3
y2=y3

data1 = as.data.frame(cbind(y_1,x_1,x_2,x_3,x_4,x_5,x_6,q1,q2))
data2 = as.data.frame(cbind(y_2,x_1,x_2,x_3,x_4,x_5,x_6,q1,q2))

#initial value beta dengan Poisson
pgig1 = glm(y_1 ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + offset(log(q1)), family=poisson(link="log"), data = data1)
pgig2 = glm(y_2 ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + offset(log(q2)), family=poisson(link="log"), data = data2)

beta10 = as.matrix(pgig1$coefficients)
beta20 = as.matrix(pgig2$coefficients)
beta_awal = as.matrix(c(beta10,beta20))

#initial value dispersi
tau_awal1 = (var(y1)-mean(y1))/(mean(y1)^2)
tau_awal2 = (var(y2)-mean(y2))/(mean(y2)^2)
tau_awal = mean(tau_awal1,tau_awal2)
psi_awal = 1
init_MCPGIGRe  = as.matrix(c(beta10,beta20,tau_awal,psi_awal))


pgig10 = glm(y_1 ~ constant + offset(log(q1)), family=poisson(link="log"), data = data1)
pgig20 = glm(y_2 ~ constant + offset(log(q2)), family=poisson(link="log"), data = data2)
beta1_H0 = as.matrix(pgig10$coefficients)
beta1_H0 = beta1_H0[1]
beta2_H0 = as.matrix(pgig20$coefficients)
beta2_H0 = beta2_H0[1]
beta_awalH0 = as.matrix(c(beta1_H0,beta2_H0))
tau_awalH0 = tau_awal
psi_awalH0 = psi_awal

init0_MCPGIGRe = as.matrix(c(beta1_H0, beta2_H0,tau_awalH0,psi_awalH0))

beta10      <- as.matrix(pgig1$coefficients)  # untuk Y1
beta20      <- as.matrix(pgig2$coefficients)  # untuk Y2
tau_awal    <- tau_awal
psi_awal    <- psi_awal

# ubah beta10 dan beta20 jadi baris, bukan kolom
beta1_row <- t(beta10)  # Y1
beta2_row <- t(beta20)  # Y2

# gabungkan jadi satu matrix 2×7
beta_table <- rbind(beta1_row, beta2_row)

# beri nama baris & kolom
rownames(beta_table) <- c("Y1", "Y2")
colnames(beta_table) <- paste0("beta_", 0:6)
disp_table <- data.frame(
  Parameter = c("tau", "psi"),
  Initial   = c(tau_awal, psi_awal)
)

beta_table
disp_table


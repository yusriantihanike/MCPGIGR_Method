Full_model_MCPGIGRe = MCPGIGR_este(init_MCPGIGRe,y1,y2,xb)
Null_model_MCPGIGR = MCPGIGR_este0(init0_MCPGIGRe,y1,y2,constant)
teta_MCPGIGR    = Full_model_MCPGIGRe$parameter
betaa1_MCPGIGR  = Full_model_MCPGIGRe$beta1
betaa2_MCPGIGR  = Full_model_MCPGIGRe$beta2
psi_MCPGIGR     = Full_model_MCPGIGRe$psi
tau_MCPGIGR     = Full_model_MCPGIGRe$tau
parameter = c(betaa1_MCPGIGR,betaa2_MCPGIGR,psi_MCPGIGR,tau_MCPGIGR)
parameter



yhat1_MCPGIGR = q1*(exp(xb%*%betaa1_MCPGIGR))
yhat2_MCPGIGR = q2*(exp(xb%*%betaa2_MCPGIGR))
yhat_MCPGIGR  = cbind(yhat1_MCPGIGR, yhat2_MCPGIGR)
res1_MCPGIGR  = y1-yhat1_MCPGIGR
res2_MCPGIGR  = y2-yhat2_MCPGIGR
res_MCPGIGR   = cbind(res1_MCPGIGR,res2_MCPGIGR)
MSE_MCPGIGR  = mean(t(res_MCPGIGR)%*%as.matrix(res_MCPGIGR))
RMSE_MCPGIGR = sqrt(MSE_MCPGIGR)
SSE1_MCPGIGR = sum((y1-yhat1_MCPGIGR)^2)
SSE2_MCPGIGR = sum((y2-yhat2_MCPGIGR)^2)
SSE_MCPGIGR  = SSE1_MCPGIGR+SSE2_MCPGIGR
residu        = matrix(NA,n,2)
residu[,1]    = res1_MCPGIGR; residu[,2]=res2_MCPGIGR; residu
p2   = ncol(x)
AICc = (-2*(Full_model_MCPGIGRe$Loglik)) + 2*p2 + (2*p2*(p2+1)/(n-p2-1))
outputY_MCPGIGR <- cbind(y, yhat_MCPGIGR=round(yhat_MCPGIGR,2), res=round(res_MCPGIGR,2))
colnames(outputY_MCPGIGR) <- c("Y1", "Y2", "Y hat 1", "Y hat 2", "Residual 1", "Residual 2")
outputY_MCPGIGR

beta1_hat <- parameter[1:7]
beta2_hat <- parameter[8:14]
psi_hat   <- parameter[15]
tau_hat   <- parameter[16]


param_table <- rbind(
  Y1 = beta1_hat,
  Y2 = beta2_hat
)

colnames(param_table) <- paste0("beta_", 0:6)


disp_table_hat <- data.frame(
  Parameter = c("psi", "tau"),
  Estimate  = c(psi_hat, tau_hat)
)

disp_table_hat$Estimate <- round(disp_table_hat$Estimate, 6)
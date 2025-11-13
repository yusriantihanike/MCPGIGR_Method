MCPGIGR_este0 = function(params,y1,y2,x)
{
  
  
  est = maxBHHH(loglik.MCPGIGR.inde0, grad.MCPGIGR.inde0, start=params, steptol=1e-3, y1=y1, 
                y2=y2, x=x,control=list(reltol=0, gradtol=0, iterlim=10000))
  
  #Parameter Model
  teta  = as.matrix(est$estimate)
  beta1 = teta[1]
  beta2 = teta[2]
  tau0  = teta[3]
  psi0  = teta[4]
  
  loglik   = est$maximum
  Deviance = -2*loglik
  Hessian  = est$hessian
  g        = est$gradient
  max.iter = est$iterations
  code     = est$returnCode
  message  = est$message
  laststep = est$last.step
  
  #Save Ouput
  return(list(parameter = teta,
              beta1 = beta1, beta2 = beta2, psi0 = psi0, tau0 = tau0,
              Loglik = loglik,
              Deviance = Deviance,
              Hessian = Hessian,
              gradien = g,
              iter = max.iter))#,message=message,code=code,laststep=laststep))
}
Null_model_MCPGIGR = MCPGIGR_este0(init0_MCPGIGRe,y1,y2,constant)


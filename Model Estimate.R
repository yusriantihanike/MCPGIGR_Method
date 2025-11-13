MCPGIGR_este=function(params,y1,y2,x)
{
  x       = xb
  n       = length(y1)
  m       = length(params)
  p       = ncol(x)
  k       = ncol(y)
  tau     = params[(2*p)+1]
  psi     = params[m]
  gamma   = -0.5
  btX1    = xb %*% params[1:p]
  btX2    = xb %*% params[(p+1):(2*p)]
  ebtX1   = q1*exp(btX1)
  ebtX2   = q2*exp(btX2)
  w       = (sqrt((tau^2)+(psi^2))-psi)
  h       = w+(2*(ebtX2))
  v       = sqrt((tau^2) + (psi^2))
  c12     = (y1 + (gamma))/2
  gamma32 = gamma - 1
  gamma12 = gamma + 1
  # Fungsi Bessel
  Kw1     = (besselK(w, gamma))
  Kw32    = (besselK(w, gamma32))
  Kw12    = (besselK(w, gamma12))
  z       = sqrt(w * (w + 2 * ebtX2))
  c1      = y1 + gamma
  c32     = y1 + gamma - 1
  c21     = y1 + gamma + 1
  K1      = besselK(z, c1)
  K2      = besselK(z, c32)
  K3      = besselK(z, c21)
  
  est=maxBHHH(loglik.MCPGIGRe, grad.MCPGIGR.inde, start=params, steptol=1e-3, y1=y1, 
              y2=y2, x=x,control=list(reltol=0, gradtol=0, iterlim=10000))
  
  
  #Parameter Model
  teta    = as.matrix(est$estimate)
  beta1   = teta[1:p]
  beta2   = teta[(p+1):(2*p)]
  tau     = teta[((2*p) + 1)]
  psi     = teta[m]
  
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
              beta1 = beta1, beta2 = beta2, tau = tau, psi =psi,
              Loglik   = loglik,
              Deviance = Deviance,
              Hessian  = Hessian,
              gradien  = g,
              iter     = max.iter))#,message=message,code=code,laststep=laststep))
}

Full_model_MCPGIGRe = MCPGIGR_este(init_MCPGIGRe,y1,y2,xb)

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


loglik.MCPGIGR.inde = function(params,y1,y2,x)
{
  x       = xb
  n       = length(y1)
  m       = length(params)
  p       = ncol(x)
  k       = ncol(y)
  tau     = params[(2*p)+1]
  psi     = params[m]
  gamma   = -0.5
  
  # PROTEKSI DOMAIN AWAL
  if (psi <= 0 | tau <= 0) return(rep(-1e10, n))
  
  btX1    = xb %*% params[1:p]
  btX2    = xb %*% params[(p+1):(2*p)]
  ebtX1   = q1*exp(btX1)
  ebtX2   = q2*exp(btX2)
  w       = (sqrt((tau^2)+(psi^2))-psi)
  h       = w+(2*(ebtX2))
  #pangkatyj
  c12 = (y1 + (gamma))/2
  #Fungsi Bessel pertama 
  Kw1  = log(besselK(w,gamma))
  #Fungsi Bessel kedua
  z    = sqrt(w*(w+2*(ebtX2)))
  c1   = y1 + gamma
  #K1   = besselK(z,c1)
  K1    = (besselK(z,c1))
  
  loglik = rep(0,n)
  
  for(i in 1:n)
  {
    
    term1 = log((sqrt(psi))/Kw1)
    term2 = log(((ebtX1[i]/psi)^y1[i])/factorial(y1[i]))
    term3 = log((((ebtX2[i]-ebtX1[i])/psi)^(y2[i]-y1[i]))/factorial(y2[i]-y1[i]))
    term4 = log((sqrt((w*(psi^2))/h[i]))^((y2[i]+gamma)/2))
    term5 = log(K1[i])
    
    loglik[i]= term1+term2+term3+term4+term5
    
    
  }
  return(loglik)
}

loglik.MCPGIGRe = function(params,y1,y2,x)
{
  loglik = loglik.MCPGIGR.inde(params,y1,y2,x)
  
  loglik[!is.finite(loglik)] <- NA
  loglikmin = min(loglik,na.rm=TRUE)
  loglik    = ifelse(is.na(loglik),loglikmin,loglik)
  
  return(sum(loglik))
}

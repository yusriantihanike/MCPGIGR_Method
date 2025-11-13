grad.MCPGIGR.inde0 = function(params,y1,y2,x)
{
  x       = constant
  n       = length(y1)
  m       = length(params)
  p       = ncol(x)
  k       = ncol(y)
  tau0     = params[3]
  psi0     = params[4]
  gamma   = -0.5
  
  # PROTEKSI DOMAIN AWAL
  
  btX10    = x %*% params[1]
  btX20    = x %*% params[2]
  ebtX10   = q1*exp(btX10)
  ebtX20   = q2*exp(btX20)
  w       = (sqrt((tau0^2)+(psi0^2))-psi0)
  h       = w+(2*(ebtX20))
  v = sqrt((tau0^2) + (psi0^2))
  c12 = (y1 + (gamma))/2
  gamma32 = gamma - 1
  gamma12 = gamma + 1
  # Fungsi Bessel
  Kw1 = log(besselK(w, gamma))
  Kw32 = log(besselK(w, gamma32))
  Kw12 = log(besselK(w, gamma12))
  z = sqrt(w * (w + 2 * ebtX20))
  c1 = y1 + gamma
  c32 = y1 + gamma - 1
  c21 = y1 + gamma + 1
  K1 = (besselK(z, c1))
  K2 = (besselK(z, c32))
  K3 = (besselK(z, c21))
  
  # Matriks Gradien
  g = matrix(0, n, m)  # Membuat matriks ukuran nxm untuk menyimpan hasil gradien
  
  for (i in 1:n) 
  {
    
    # Perhitungan untuk setiap term
    turunanbeta10 = ((x[i, ] * y1[i])
                     +(x[i, ] *(y2[i]-y1[i])*(ebtX10[i]/(ebtX20[i]-ebtX10[i]))))
    turunanbeta20 = ((x[i, ] *(y2[i]-y1[i])*(ebtX20[i]/(ebtX20[i]-ebtX10[i])))
                     -(x[i, ]*((y2[i]+gamma)/2)*(1/2)*(2*ebtX20[i]/h[i]))
                     -(x[i, ]*((1/(2*z[i]))+1)*(w/z[i])*(2*w*ebtX20[i])))
    turunanpsi0   = ((1/(2*psi0))
                     -((-Kw12+((-1/2)*(Kw1/w)))*((psi0/v)-1)/Kw1)
                     -y1[i]/psi0
                     -((y2[i]-y1[i])/psi0)
                     +(((y2[i]+gamma)/2)*(1/2)*((psi0/v)-1)/w)
                     +(((y2[i]+gamma)/2)*(1/psi0))
                     -(((y2[i]+gamma)/2)*(1/2)*((psi0/v)-1)*(1/h[i]))
                     +((-K2[i]+((gamma+y2[i])*K1[i]/z[i]))*((psi0/v)-1)*(2*(ebtX20[i]+w))/(2*z[i]*K1[i])))
    turunantau0   = (((-Kw12+((gamma)*(Kw1/w)))*(tau0)/(v*K1[i]))
                     +(((y2[i]+gamma)/2)*(1/2)*(tau0/v)/w)
                     -(((y2[i]+gamma)/2)*(1/2)*(tau0/v)/h[i])
                     +((-K2[i]+((gamma+y2[i])*K1[i]/z[i]))*(2*tau0*(w+ebtX20[i]))/(2*z[i]*K1[i])))
    
    g[i, 1] = turunanbeta10
    g[i, 2] = turunanbeta20
    g[i, 3] = turunantau0
    g[i, 4] = turunanpsi0
  }
  
  g[!is.finite(g)] <- NA
  gmin = min(g,na.rm=TRUE)
  g    = ifelse(is.na(g),gmin,g)
  
  return(g)
  
}



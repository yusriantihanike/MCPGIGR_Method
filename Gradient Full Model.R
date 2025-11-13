
grad.MCPGIGR.inde = function(params,y1,y2,x)
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
  v = sqrt((tau^2) + (psi^2))
  c12 = (y1 + (gamma))/2
  gamma32 = gamma - 1
  gamma12 = gamma + 1
  # Fungsi Bessel
  Kw1 = log(besselK(w, gamma))
  Kw32 = log(besselK(w, gamma32))
  Kw12 = log(besselK(w, gamma12))
  z = sqrt(w * (w + 2 * ebtX2))
  c1 = y1 + gamma
  c32 = y1 + gamma - 1
  c21 = y1 + gamma + 1
  K1 = (besselK(z, c1))
  K2 = (besselK(z, c32))
  K3 = (besselK(z, c21))
  
  # Matriks Gradien
  g = matrix(0, n, m)  # Membuat matriks ukuran nxm untuk menyimpan hasil gradien
  
  for (i in 1:n) {
    
    
    # Proteksi domain dalam loop
    if (psi <= 0 || w <= 0 || Kw1 <= 0 ||
        ebtX1[i] <= 0 || ebtX2[i] <= 0 || h[i] <= 0 || z[i] <= 0 || 
        K1[i] <= 0 || y1[i] < 0 || y2[i] < 0 || (y2[i]-y1[i]) < 0) {
      g[i, ] = -1e10
      next
    }
    
    
    # Perhitungan untuk setiap term
    turunanbeta1 = ((x[i, ] * y1[i])
                    +(x[i, ] *(y2[i]-y1[i])*(ebtX1[i]/(ebtX2[i]-ebtX1[i]))))
    turunanbeta2 = ((x[i, ] *(y2[i]-y1[i])*(ebtX2[i]/(ebtX2[i]-ebtX1[i])))
                    -(x[i, ]*((y2[i]+gamma)/2)*(1/2)*(2*ebtX2[i]/h[i]))
                    -(x[i, ]*((1/(2*z[i]))+1)*(w/z[i])*(2*w*ebtX2[i])))
    turunanpsi   = ((1/(2*psi))
                    -((-Kw12+((-1/2)*(Kw1/w)))*((psi/v)-1)/Kw1)
                    -y1[i]/psi
                    -((y2[i]-y1[i])/psi)
                    +(((y2[i]+gamma)/2)*(1/2)*((psi/v)-1)/w)
                    +(((y2[i]+gamma)/2)*(1/psi))
                    -(((y2[i]+gamma)/2)*(1/2)*((psi/v)-1)*(1/h[i]))
                    +((-K2[i]+((gamma+y2[i])*K1[i]/z[i]))*((psi/v)-1)*(2*(ebtX2[i]+w))/(2*z[i]*K1[i])))
    turunantau   = (((-Kw12+((gamma)*(Kw1/w)))*(tau)/(v*K1[i]))
                    +(((y2[i]+gamma)/2)*(1/2)*(tau/v)/w)
                    -(((y2[i]+gamma)/2)*(1/2)*(tau/v)/h[i])
                    +((-K2[i]+((gamma+y2[i])*K1[i]/z[i]))*(2*tau*(w+ebtX2[i]))/(2*z[i]*K1[i])))
    
    
    
    # Simpan hasil gradien ke dalam matriks g
    g[i, 1:p]            = turunanbeta1  # Turunan terhadap beta1
    g[i,((p+1):(2*p))]   = turunanbeta2
    g[i,((2*p) + 1)]     = turunantau  # Turunan terhadap tau
    g[i, m]              = turunanpsi  # Turunan terhadap psi
    
    
  }
  
  # Tangani nilai Inf atau NA dalam matriks gradien g
  g[!is.finite(g)] <- NA
  gmin = min(g, na.rm=TRUE)
  g = ifelse(is.na(g), gmin, g)
  
  return(g)
}


grad.MCPGIGRe=function(params,y1,y2,x)
{
  
  g    =grad.MCPGIGR.inde(params,y1,y2,x)
  g[!is.finite(g)] <- NA
  gmin = min(g,na.rm=TRUE)
  g    = ifelse(is.na(g),gmin,g)
  
  return(colSums(g))
}

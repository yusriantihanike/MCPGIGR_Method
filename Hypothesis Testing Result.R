
#Simultaneous Test
G2_MCPGIGR     = abs(2*(Full_model_MCPGIGRe$Loglik - Null_model_MCPGIGR$Loglik))
p1            = ncol(x) * 2
Sig.G2_MCPGIGR = 1-pchisq(G2_MCPGIGR,p1)
Simultaneous.Test  = cbind(G2_MCPGIGR, Sig.G2_MCPGIGR)
colnames(Simultaneous.Test) = c("Simultaneous.Test", "Pr(>|Z|)")
Simultaneous.Test
Chisq = qchisq(0.95,12)
Chisq

#Partial Test
# ====== PARTIAL TEST (Z-statistic per parameter) ======

H      <- Full_model_MCPGIGRe$Hessian
var    <- -ginv(H)
SE     <- sqrt(diag(var))

SE.beta1 <- SE[1:7]   # untuk beta1 (Y1)
SE.beta2 <- SE[8:14]  # untuk beta2 (Y2)
SE.psi   <- SE[15]
SE.tau   <- SE[16]

# Z-statistic
Zhit1   <- betaa1_MCPGIGR / SE.beta1
Zhit2   <- betaa2_MCPGIGR / SE.beta2
Zpsi    <- psi_MCPGIGR   / SE.psi
Ztau    <- tau_MCPGIGR   / SE.tau

# p-value (2-sided)
sig.beta1 <- 2 * (1 - pnorm(abs(Zhit1)))
sig.beta2 <- 2 * (1 - pnorm(abs(Zhit2)))
sig.psi   <- 2 * (1 - pnorm(abs(Zpsi)))
sig.tau   <- 2 * (1 - pnorm(abs(Ztau)))

# Gabungkan semua parameter dalam satu vector
Estimate_all <- c(betaa1_MCPGIGR, betaa2_MCPGIGR, psi_MCPGIGR,  tau_MCPGIGR)
SE_all       <- c(SE.beta1,       SE.beta2,       SE.psi,       SE.tau)
Z_all        <- c(Zhit1,          Zhit2,          Zpsi,         Ztau)
p_all        <- c(sig.beta1,      sig.beta2,      sig.psi,      sig.tau)

# Susun matrix: 16×4
Partial.test <- cbind(
  Estimate = Estimate_all,
  Std.Error = SE_all,
  Z.Value   = Z_all,
  `Pr(>|Z|)` = p_all
)

# Beri nama parameter: b01..b61 (Y1), b02..b62 (Y2), psi, tau
param_names <- c(
  paste0("b", 0:6, "1"),   # b01..b61 untuk Y1
  paste0("b", 0:6, "2"),   # b02..b62 untuk Y2
  "psi",
  "tau"
)

rownames(Partial.test) <- param_names

# (opsional) pembulatan untuk tampilan
Partial.test <- round(Partial.test, 7)

Partial.test

Y1_test         <- Partial.test[1:7, ]   # b01..b61
Y2_test         <- Partial.test[8:14, ]  # b02..b62
Dispersion_test <- Partial.test[15:16, ] # psi & tau

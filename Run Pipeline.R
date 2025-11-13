#############################################################
# MCPGIGR - Full Pipeline Execution Script
# This script runs the entire workflow:
# 1. Install packages
# 2. Load and process data
# 3. Descriptive statistics
# 4. Initial values
# 5. Likelihood & gradient
# 6. Model estimation (full & null)
# 7. Parameter results
# 8. Hypothesis testing output
#############################################################

rm(list = ls())
options(stringsAsFactors = FALSE)

# 1. Source all scripts in correct order
script_files <- c(
  "Install Packages.R",
  "Input Data.R",
  "Descriptive Data.R",
  "Initial Value.R",
  "LnLikelihood.R",
  "Gradient.R",
  "Model Estimate.R",
  "Parameter Result.R",
  "Hypothesis Testing Result.R"
)

for (f in script_files) {
  message("\n>>> Running: ", f)
  source(f)
}

#############################################################
# 2. Display key outputs for reproducibility
#############################################################

cat("\n\n==================== DESCRIPTIVE DATA ====================\n")
print(summary_y)
print(summary_x)

cat("\n\n==================== INITIAL VALUES ======================\n")
print(beta_table)
print(disp_table)

cat("\n\n==================== PARAMETER ESTIMATES =================\n")
print(round(param_table, 6))
print(disp_table_hat)
print(AICc)

cat("\n\n==================== SIMULTANEOUS TEST =====================\n")
print(Simultaneous.Test)

cat("\n\n==================== HYPOTHESIS TEST (DETAIL) ====================\n")

cat("\n--- Y1 Regression Coefficients (b01..b61) ---\n")
print(Y1_test)

cat("\n\n--- Y2 Regression Coefficients (b02..b62) ---\n")
print(Y2_test)

cat("\n\n--- Dispersion Parameters (psi, tau) ---\n")
print(Dispersion_test)

#############################################################
# End of pipeline
#############################################################

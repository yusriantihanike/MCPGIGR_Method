# MCPGIGR_Method
The code of data
# MCPGIGR – Reproducible R Code 

This repository contains the full R code and data required to reproduce the results of the MCPGIGR model described in our MethodsX article.

## Repository structure

- `run_pipeline.R`  
  Main driver script. Running this file executes the entire workflow:
  1. Load/install required packages
  2. Load and process data
  3. Compute descriptive statistics
  4. Compute initial values
  5. Evaluate log-likelihood and gradients 
  6. Estimate the MCPGIGR model 
  7. Report parameter estimates
  8. Perform hypothesis testing

- `R/`  
  Contains modular scripts for each step (data input, likelihood, gradients, estimation, parameter summaries, hypothesis testing).

- `data/`  
  Contains the dataset used in the empirical application (or a synthetic/anonymised version with the same structure).

## Requirements

- R version: (e.g. 4.3.1)
- Packages (automatically installed/loaded in `Install_Packages.R`):
  `gmp`, `Rmpfr`, `MASS`, `MixedPoisson`, `gamlss`, `Bessel`, `maxLik`, `COUNT`, `readxl`

## How to run

```r
# from within the project root directory
source("run_pipeline.R")

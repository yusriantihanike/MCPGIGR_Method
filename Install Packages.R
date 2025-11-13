rm(list = ls())

# daftar paket yang dibutuhkan
pkgs <- c(
  "gmp",
  "Rmpfr",
  "MASS",
  "MixedPoisson",
  "gamlss",
  "Bessel",
  "maxLik",
  "COUNT",
  "readxl"
)

# cek: kalau belum terinstal → install, lalu load
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

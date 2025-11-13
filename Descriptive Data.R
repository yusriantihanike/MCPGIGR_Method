mean(x, na.rm = TRUE)  
sd(x, na.rm = TRUE)    
mean_x <- colMeans(x, na.rm = TRUE)
sd_x   <- apply(x, 2, sd, na.rm = TRUE)


summary_x <- data.frame(
  Mean = round(mean_x, 2),
  Standard_Deviation = round(sd_x, 2)
)

rm(var)
mean_y <- colMeans(y, na.rm = TRUE)
sd_y   <- apply(y, 2, sd, na.rm = TRUE)
var_y  <- apply(y, 2, var, na.rm = TRUE)
cv_y   <- (sd_y / mean_y) * 100
min_y  <- apply(y, 2, min, na.rm = TRUE)
max_y  <- apply(y, 2, max, na.rm = TRUE)

summary_y <- data.frame(
  Mean = round(mean_y, 2),
  SD   = round(sd_y, 2),
  Variance = round(var_y, 2),
  CV_percent = round(cv_y, 2),
  Min  = round(min_y, 2),
  Max  = round(max_y, 2)
)


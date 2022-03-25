

###### prophet#######################
library(prophet)
library(V8)
library(readxl)

####set up new df############
setwd("~/Downloads")
df <- read_excel("df.xls")
m <- prophet(df)


##### plots and predictions#####

future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)

plot(m, forecast)

prophet_plot_components(m, forecast)

#### add regressor pandemic###########
data <- read_excel("headlouse.xls.xlsx")
df$pandemic <- data$pandemic
o <- prophet()
o <- add_regressor(o, 'pandemic')
o <- fit.prophet(o, df)
#regressor_coefficients(o)

#### debugging #######################

regressor_coefficients <- function(m){
  if (length(m$extra_regressors) == 0) {
    stop("No extra regressors found.")
  }
  regr_names <- names(m$extra_regressors)
  regr_modes <- unlist(lapply(m$extra_regressors, function(x) x$mode))
  regr_mus <- unlist(lapply(m$extra_regressors, function (x) x$mu))
  regr_stds <- unlist(lapply(m$extra_regressors, function(x) x$std))
  
  beta_indices <- which(m$train.component.cols[, regr_names, drop = FALSE] == 1, arr.ind = TRUE)[, "row"]
  betas <- m$params$beta[, beta_indices, drop = FALSE]
  # If regressor is additive, multiply by the scale factor to put coefficients on the original training data scale.
  y_scale_indicator <- matrix(
    data = ifelse(regr_modes == "additive", m$y.scale, 1),
    nrow = nrow(betas),
    ncol = ncol(betas),
    byrow = TRUE
  )
  coefs <- betas * y_scale_indicator  / regr_stds
  
  percentiles = c((1 - m$interval.width) / 2, 1 - (1 - m$interval.width) / 2)
  bounds <- apply(betas, 2, stats::quantile, probs = percentiles)
  
  df <- data.frame(
    regressor = regr_names,
    regressor_mode = regr_modes,
    center = regr_mus,
    coef_lower = bounds[1, ],
    coef = apply(betas, 2, mean),
    coef_upper = bounds[2, ],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  return(df)
}

#### get coefficeints############


regressor_coefficients(o)

########### attendance ###################

df_1 <- df[165:251,]

df_1$earlyyears <- data$earlyyears[165:251] # here it was 5 at the end here are na-s
df_1$primary <- data$primary2[165:251]  ## here it was 5 not 2, here are na-s
df_1$secondary <- data$secondary2[165:251]
#df_1$numdate_index <- data$seq_indexed[165:251]

p <- prophet()
p <- add_regressor(p, 'earlyyears')
p <- add_regressor(p, 'primary')
p <- add_regressor(p, 'secondary')
#p <- add_regressor(p, 'numdate_index') we dont need that
#p <- fit.prophet(p, df_1) # itwont wotk with NAs obviously

#regressor_coefficients(p)


df.cv <- cross_validation(m, initial = 160, period = 25, horizon = 52, units = 'weeks')
head(df.cv)

df.p <- performance_metrics(df.cv)
head(df.p)

plot_cross_validation_metric(df.cv, metric = 'mse')
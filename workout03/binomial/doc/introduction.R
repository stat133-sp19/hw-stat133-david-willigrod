## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(binomial)

## ------------------------------------------------------------------------
bin_var <- bin_variable(10, .4)
bin_var
bin_var$trials
bin_var$prob

## ------------------------------------------------------------------------
summary(bin_var)
bin_mean(10, .4)
bin_variance(10, .4)
bin_mode(10, .4)
bin_skewness(10, .4)
bin_kurtosis(10, .4)

## ------------------------------------------------------------------------
pdf_var <- bin_distribution(bin_var$trials, bin_var$prob)
pdf_var

cdf_var <- bin_cumulative(bin_var$trials, bin_var$prob)
cdf_var


## ------------------------------------------------------------------------
plot(pdf_var)
plot(cdf_var)


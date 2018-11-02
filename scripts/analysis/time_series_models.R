library(dplyr)
library(ggplot2)
library(forecast)
library(lmtest)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# ARIMA model on single species' yearly mass
one_species = occurrences_with_temp %>% 
  filter(species == "DM")

# Plot data and turn into time series object
ggplot(one_species, aes(x = yr, y = mass_mean)) +
  geom_line()
onesp_ts = ts(one_species$mass_mean, start = 1977, end = 2014, frequency = 1)

# Diagnose stationarity
# Is stationary (not autocorrelated), therefore do not need to difference
Acf(onesp_ts)

# Only peak at 1, therefore AR = 1 & MA = 1? 
Pacf(onesp_ts)

# Custom ARIMA model (p,d,q) = (1,0,1)
onesp_model_custom = Arima(onesp_ts, order = c(1, 0, 1))

# Compare with AICc for similar models
# Custom has lowest, so it's the best choice
onesp_model_alt1 = Arima(onesp_ts, order = c(2, 0, 1))
onesp_model_alt2 = Arima(onesp_ts, order = c(1, 0, 2))

# Automatically chosen ARIMA model
# Suggests (1,0,0), even if approximation = FALSE and/or stepwise = FALSE
onesp_model_auto = auto.arima(onesp_ts, seasonal = FALSE, stepwise = FALSE)

# More AICc comparisons
onesp_model_alt3 = Arima(onesp_ts, order = c(1, 0, 0))
onesp_model_alt4 = Arima(onesp_ts, order = c(0, 0, 1))

# Auto is best
onesp_model = onesp_model_alt3

# Look at residuals with tsdisplay()/checkresiduals() 
# Big p-value, ACF within bounds, and normal hist means residuals are like white noise
checkresiduals(onesp_model)
tsdisplay(residuals(onesp_model))

# Add dynamic regression
# Do model with mean annual temp as covariate
ggplot(one_species, aes(x = yr, y = avg_temp)) +
  geom_line()
temp_ts = ts(one_species$avg_temp, start = 1977, end = 2014, frequency = 1)
sptemp_model_custom = Arima(onesp_ts, xreg = temp_ts, order = c(1, 0, 0))
sptemp_model_auto = auto.arima(onesp_ts, xreg = temp_ts)

# Residuals look like white noise
checkresiduals(sptemp_model_custom)

# How to interpret effect of temperature on mass? 
# Look at coeff +/- 2*SE to see if overlaps zero? 
# "If coefficient is more than two SEs from zero, then p < .05"
# Range is within zero, so temp doesn't have significant impact
sptemp_model_coef = coef(sptemp_model_custom)[3]
sptemp_model_se = sqrt(diag(vcov(sptemp_model_custom)))[3]
sptemp_model_coef + (2 * sptemp_model_se)
sptemp_model_coef - (2 * sptemp_model_se)

# Z test to get p value for temperature
# source: https://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r
(1 - pnorm(abs(sptemp_model_coef) / sptemp_model_se)) * 2
coeftest(sptemp_model_custom)

# TODO: repeat this process to get one p-value per species
# TODO: do false discovery rate control on p-values

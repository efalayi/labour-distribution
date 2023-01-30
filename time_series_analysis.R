library(TTR)
library(forecast)

# Import base libraries, dataset and functions from utils.R file
source('base.R')

dataset_lmc_lpr <- dataset %>%
  filter(income_group_code == 'LMC') %>%
  group_by(year) %>%
  summarize(
    mean_lpr_lmc = mean(labour_participation_rate),
  ) %>% select(mean_lpr_lmc)

dataset_umc_lpr <- dataset %>%
  filter(income_group_code == 'UMC') %>%
  group_by(year) %>%
  summarize(
    mean_lpr_umc = mean(labour_participation_rate),
  ) %>% select(mean_lpr_umc)

lmc_matrix = as.matrix(dataset_lmc_lpr)
umc_matrix = as.matrix(dataset_umc_lpr)
combined_matrix = ts(cbind(lmc_matrix, umc_matrix), frequency = 1, start = c(2005))

# Plot time series
par(mfrow = c(1, 1)) 
plot(combined_matrix, plot.type = 'single', lty = 1:2, col = 3:4,
     ylab = 'Labour participation rate')
legend("topright",
       c("LMC", "UMC"),
       lty = 1:2,
       col = c(3, 4))

# Forecasting
lmc_forecasts <- HoltWinters(combined_matrix[, 1], gamma = FALSE)
umc_forecasts <- HoltWinters(combined_matrix[, 2], gamma = FALSE)

lmc_forecasts
lmc_forecasts$SSE
umc_forecasts
umc_forecasts$SSE

# Plot forecasts
par(mfrow = c(1, 2))
plot(lmc_forecasts, type="l", main = 'LMC', lty = 1, col = 3)
plot(umc_forecasts, type="l", main = 'UMC', lty = 1, col = 4)

# Forecast for 6 years
data_lmc_forecasts_two <- forecast(lmc_forecasts, h=6)
data_umc_forecasts_two <- forecast(umc_forecasts, h=6)
data_lmc_forecasts_two
data_umc_forecasts_two

par(mfrow = c(1, 2))  
plot(data_lmc_forecasts_two, type="l", main = 'LMC', lty = 1, col = 3)
plot(data_umc_forecasts_two, type="l", main = 'UMC', lty = 1, col = 4)

# Check correlation
par(mfrow = c(1, 2)) 
acf(data_lmc_forecasts_two$residuals, lag.max = 10, na.action = na.pass,
    main = 'LMC', col = 3)
acf(data_umc_forecasts_two$residuals, lag.max = 10, na.action = na.pass,
    main = 'UMC', col = 4)

Box.test(data_lmc_forecasts_two$residuals, lag = 10, type = 'Ljung-Box')
Box.test(data_umc_forecasts_two$residuals, lag = 10, type = 'Ljung-Box')

par(mfrow = c(1, 2)) 
plot(data_lmc_forecasts_two$residuals, main = 'LMC', lty = 1, col = 3,
     ylab = 'Forecast residuals')
plot(data_umc_forecasts_two$residuals, main = 'UMC', lty = 1, col = 4,
     ylab = 'Forecast residuals')

data_lmc_forecasts_two$residuals <- data_lmc_forecasts_two$residuals[
  !is.na(data_lmc_forecasts_two$residuals)
]

data_umc_forecasts_two$residuals <- data_umc_forecasts_two$residuals[
  !is.na(data_umc_forecasts_two$residuals)
]

shapiro.test(data_lmc_forecasts_two$residuals)
shapiro.test(data_umc_forecasts_two$residuals)

plotForecastErrors(data_lmc_forecasts_two$residuals, 'LMC')
plotForecastErrors(data_umc_forecasts_two$residuals, 'UMC')



# Regression Analysis
library(moments)
library(corrplot)
library(caret)

# Import base libraries, dataset and functions from utils.R file
source('base.R')

# Data exploration
sample_dataset <- getDataByYear(2019)
filtered_data <- getDataByColumns(sample_dataset)

filtered_data

model_one <- lm(labour_participation_rate ~ employment_to_population_ratio,
                filtered_data)
summary.lm(model_one)
model_one

plot(labour_participation_rate ~ employment_to_population_ratio,
     filtered_data, col = 'blue',
     main = 'Regression: Participation Rate & Employment Ratio',
     xlab = 'Employment to Population Ratio',
     ylab = 'Labour Participation Rate')
abline(model_one, col = 'red')

# Check residual's independence
cor(model_one$residuals, model_one$fitted.values)
plot(model_one, 1)

# Check normality of residuals
shapiro.test(model_one$residuals)
plot(model_one, 2)

# Check for Homoscedasticity
plot(model_one, 3)



# Correlation Analysis
library(moments)
library(corrplot)

# Import base libraries, dataset and functions from utils.R file
source('base.R')

# Data exploration
sample_dataset <- getDataByYear(2019)

# 1. correlation between labour_participation_rate and annual_gdp_growth
createScatterPlot(sample_dataset, sample_dataset$labour_participation_rate,
                  sample_dataset$annual_gdp_growth)
cor(sample_dataset$labour_participation_rate, sample_dataset$annual_gdp_growth)

# 2. correlation between labour_participation_rate and unemployment
createScatterPlot(sample_dataset, sample_dataset$labour_participation_rate,
                  sample_dataset$unemployment)
cor(sample_dataset$labour_participation_rate, sample_dataset$unemployment)


# Correlation matrix for variables of interest
selected_columns <- getDataByColumns(sample_dataset)

round(cor(selected_columns), digits = 2)
computed_coefficients = cor(selected_columns)

corrplot(computed_coefficients, method = 'number', type = 'upper',
        tl.col = 'black', tl.cex = 0.9, number.cex = 1,
        col = viridis_pal(option = 'H')(10)
      )

# Correlation Test
cor.test(dataset$employment_to_population_ratio,
         dataset$labour_participation_rate)


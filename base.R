library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(viridis)

dataset <- read_excel(
  'data/gdp_and_labour_distribution.xlsx',
  sheet='MIC'
)

# Convert total_poulation and total_labour_force variables to integer
dataset$total_population <- as.integer(dataset$total_population)
dataset$total_labour_force <- as.integer(dataset$total_labour_force)

# Convert income_group and region variables to factor
dataset$country <- as.factor(dataset$country)
dataset$country_code <- as.factor(dataset$country_code)
dataset$income_group <- as.factor(dataset$income_group)
dataset$income_group_code <- as.factor(dataset$income_group_code)
dataset$region <- as.factor(dataset$region)
dataset$region_code <- as.factor(dataset$region_code)

dim(dataset)
str(dataset)
summary(dataset)

analysed_columns <- c(
  'employment_to_population_ratio', 'labour_participation_rate',
  'unemployment','annual_gdp_growth')

# Functions
getDataByColumns <- function(dataframe = dataset, column_names = analysed_columns) {
  filter_dataset <- dataframe %>% select(all_of(column_names))
  return(filter_dataset)
}

getDataByIncomeGroup <- function(dataframe = dataset, group_name) {
  filter_dataset <- dataframe %>% filter(income_group == group_name)
  return(filter_dataset)
}

getDataByYear <- function(target_years = c(2019)) {
  filter_dataset = dataset %>% filter(year %in% target_years)
  return(filter_dataset)
}

createScatterPlot <- function(
    dataframe, x_axis, y_axis,group_colour, legend_label) {
  return (
    ggplot(dataframe) +
      aes(x = x_axis, y = y_axis) +
      geom_point(size = 3, alpha = 0.5) +
      theme_minimal() +
      scale_colour_viridis_d(option = 'plasma')
  )
}

plotForecastErrors <- function(forecasterrors, title) {
  # Make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mybinsize
  
  mysd	<- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3

  # Generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)

  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }

  # Make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col='red', freq=FALSE, breaks=mybins, main = title)

  # freq=FALSE ensures the area under the histogram = 1
  # Generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type='l', col='blue', lwd=2)
}



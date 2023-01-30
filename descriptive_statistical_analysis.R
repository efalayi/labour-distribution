# Descriptive Statistical Analysis
library(moments)

# Import dataset and functions from utils file
source('base.R')

# Get data for most recent year: 2019
dataset_2019 <- getDataByYear(2019)

# Verify that no missing are missing in the dataset
sum(is.na(dataset_2019))
mean(is.na(dataset_2019))

# Data exploration
dim(dataset_2019)
str(dataset_2019)
summary(dataset_2019)

# Descriptive Statistical Analysis
sample_means <- c()
sample_medians <- c()
sample_sd <- c()
sample_skewness_estimates <- c()
sample_kurtosis_estimates <- c()

for (index in 1:length(analysed_columns)) {
  values <- dataset_2019 %>% pull(analysed_columns[index])

  sample_means[index] <- signif(mean(values), digits = 4)
  sample_medians[index] <- signif(median(values), digits = 4)
  sample_sd[index] <- signif(sd(values), digits = 4)
  sample_skewness_estimates[index] <- round(skewness(values), digits = 4)
  sample_kurtosis_estimates[index] <- round(kurtosis(values), digits = 4)
}

# Display computed statistics for sample data
sample_statistics_summary <- data.frame('Mean' = sample_means, 'Median' = sample_medians,
                                'SD' = sample_sd, 'Skewness' = sample_skewness_estimates,
                                'Kurtosis' = sample_kurtosis_estimates)

rownames(sample_statistics_summary) <- analysed_columns
sample_statistics_summary

# Estimating means and standard deviations for income groups: LMC and UMC
# 1. LMC: Lower Middle Income
lmc_dataset_2019 <- getDataByIncomeGroup(dataset_2019, 'Lower Middle Income')
lmc_means <- c()
lmc_sd <- c()
lmc_skewness_estimates <- c()
lmc_kurtosis_estimates <- c()

for (index in 1:length(analysed_columns)) {
  values <- lmc_dataset_2019 %>% pull(analysed_columns[index])
  
  lmc_means[index] <- signif(mean(values), digits = 4)
  lmc_sd[index] <- signif(sd(values), digits = 4)
  lmc_skewness_estimates[index] <- round(skewness(values), digits = 4)
  lmc_kurtosis_estimates[index] <- round(kurtosis(values), digits = 4)
}

# Display computed statistics for LMC data
lmc_statistics_summary <- data.frame('Mean' = lmc_means, 'SD' = lmc_sd,
                                     'Skewness' = lmc_skewness_estimates,
                                     'Kurtosis' = lmc_kurtosis_estimates)

rownames(lmc_statistics_summary) <- analysed_columns
lmc_statistics_summary

# 2. UMC: Upper Middle Income
umc_dataset_2019 <- getDataByIncomeGroup(dataset_2019, 'Upper Middle Income')
umc_means <- c()
umc_sd <- c()
umc_skewness_estimates <- c()
umc_kurtosis_estimates <- c()

for (index in 1:length(analysed_columns)) {
  values <- umc_dataset_2019 %>% pull(analysed_columns[index])
  
  umc_means[index] <- signif(mean(values), digits = 4)
  umc_sd[index] <- signif(sd(values), digits = 4)
  umc_skewness_estimates[index] <- round(skewness(values), digits = 4)
  umc_kurtosis_estimates[index] <- round(kurtosis(values), digits = 4)
}

# Display computed statistics for UMC data
umc_statistics_summary <- data.frame('Mean' = umc_means, 'SD' = umc_sd,
                                     'Skewness' = umc_skewness_estimates,
                                     'Kurtosis' = umc_kurtosis_estimates)

rownames(umc_statistics_summary) <- analysed_columns
umc_statistics_summary

# Calculate t_score and confidence interval
# 1. Margin of error in mean labour_participation_rate between LMC and UMC
dim(lmc_dataset_2019)
dim(umc_dataset_2019)

# n1 = 35, n2 = 35
df_value <- 35 + 35 - 2
mid_df_value <- df_value / 2

t_score <- qt(p = (1 - 0.95) / 2, df = df_value, lower.tail = FALSE)
t_score

# Calculate margin of error for mean labour_participation_rate
margin_error <- t_score * sqrt((
  mid_df_value * lmc_statistics_summary['labour_participation_rate', 'SD']^2 +
    mid_df_value * umc_statistics_summary['labour_participation_rate', 'SD']^2
) / df_value)
mean_lpr_difference <- umc_statistics_summary['labour_participation_rate', 'Mean'] -
  lmc_statistics_summary['labour_participation_rate', 'Mean']

sprintf('The 95%% confidence interval for the difference in mean labour participation rate between LMC 
        and UMC income groups is from %.02f%% to %.02f%%',
        mean_lpr_difference - margin_error, mean_lpr_difference + margin_error
)

# Normality test
shapiro.test(dataset$employment_to_population_ratio)
shapiro.test(dataset$labour_participation_rate)
shapiro.test(dataset$unemployment)
shapiro.test(dataset$annual_gdp_growth)

# Visualisation of skewness
skewness_plot <- dataset %>%
  select(employment_to_population_ratio, labour_participation_rate,
         unemployment, annual_gdp_growth) %>%
  pivot_longer(everything()) %>% as.data.frame()

ggplot(skewness_plot, aes(x = value)) +
  geom_density() + facet_wrap(~ name, scales = 'free')

# Estimated mean test
# Non-parametric test
wilcox.test(employment_to_population_ratio ~ income_group, data = dataset)
wilcox.test(labour_participation_rate ~ income_group, data = dataset)
wilcox.test(unemployment ~ income_group, data = dataset)
wilcox.test(annual_gdp_growth ~ income_group, data = dataset)

# Data visualisation
ggplot(dataset, aes(income_group, employment_to_population_ratio)) +
  geom_boxplot()
ggplot(dataset, aes(income_group, labour_participation_rate)) +
  geom_boxplot()
ggplot(dataset, aes(income_group, unemployment)) +
  geom_boxplot()
ggplot(dataset, aes(income_group, annual_gdp_growth)) +
  geom_boxplot()


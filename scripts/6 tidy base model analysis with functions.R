# Script: tidy base model analysis with functions
# Author: Tina
# Date: 30.11.2023

# 1. Preliminaries
## Load Libraries
library(dplyr)     # For data transformations
library(tidyr)     # For tidy data
library(psych)     # For descriptive statistics
library(car)       # For multicollinearity checks with vif() function
library(plm)       # For panel data operations
library(lmtest)    # For heteroskedasticity check with bptest() function
library(sandwich)  # For Newey-West standard errors
library(ggplot2)   # For making plots

## Set Global Options
options(scipen = 999)  # Avoid scientific notation

## Define Directory Paths
data_dir <- "data/data_processed/"
output_dir <- "outputs/"
graph_dir <- paste(output_dir, "graphs/", sep = "")
result_dir_base_model <- paste(output_dir, "results/base_model_", sep = "")

## Load Processed Data
load(paste(data_dir, "data.RData", sep = ""))

# 2. Data Preparation and Inspection
## Data Selection and Transformation
df_model <- df %>% select(unique_id, municipality, year, year_squared,
                          perc_change_Pop_65_to_75_Meds, perc_change_GP_within_5km,
                          perc_Pop_aged_65_up, ln_avg_Standardized_Income,
                          ln_perc_Pop_migration_background, perc_Pop_Social_Security_excl_Pensioners,
                          perc_change_deaths_CVD, predominantly_Rural)

## Calculate and Save Descriptive Statistics
df_descriptive_stats <- describe(df_model[, sapply(df_model, is.numeric)])
write.csv(df_descriptive_stats, paste(result_dir_base_model, "descriptive_statistics.csv", sep = ""))
print(df_descriptive_stats)

# 3. Base Model Fitting
## Fit Base Model (PLM and LM)
df_panel <- pdata.frame(df_model, index = c("municipality", "year"))
base_model <- plm(perc_change_Pop_65_to_75_Meds ~ perc_change_GP_within_5km + 
                    perc_Pop_aged_65_up + ln_avg_Standardized_Income + 
                    predominantly_Rural + perc_change_deaths_CVD + 
                    ln_perc_Pop_migration_background + 
                    perc_Pop_Social_Security_excl_Pensioners + year + year_squared,
                  data = df_panel, model = "pooling")
base_model_lm <- lm(perc_change_Pop_65_to_75_Meds ~ perc_change_GP_within_5km + 
                      perc_Pop_aged_65_up + ln_avg_Standardized_Income + 
                      predominantly_Rural + perc_change_deaths_CVD + 
                      ln_perc_Pop_migration_background + 
                      perc_Pop_Social_Security_excl_Pensioners + year + year_squared,
                    data = df_model)

## Print Model Summary
summary_base_model <- summary(base_model)
print(summary_base_model)

# 4. Assumption Checking
#### Function Definitions ####
## -- Assumption Checking Function -- ##
assumption_function <- function(model, model_lm, df_panel, output_path) {
  # Multicollinearity Check
  model_mc <- update(model, . ~ . - year - year_squared)
  vif_values <- vif(model_mc)
  print(vif_values)
  
  # Residual Scatter Plot
  predicted_values <- predict(model)
  residuals_values <- residuals(model)
  df_with_predictions <- cbind(df_panel, predicted = predicted_values, residuals = residuals_values)
  residuals_plot <- ggplot(df_with_predictions, aes(x = predicted, y = residuals)) +
    geom_point() + geom_smooth(method = "loess", col = "red") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
    labs(title = "Residuals vs Predicted with LOESS Line", x = "Predicted Values", y = "Residuals") +
    theme_minimal()
  
  print(residuals_plot)
  ggsave(paste(output_path, "residuals_plot.png", sep = ""), residuals_plot, width = 10, height = 6)
  
  # Normality Check of Residuals
  qqnorm(residuals_values)
  qq_plot <- ggplot(data.frame(residuals = residuals_values), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "steelblue", lwd = 2) +
    labs(title = "Normal Q-Q Plot of Residuals") +
    theme_minimal()
  
  print(qq_plot)
  ggsave(paste(output_path, "qq_plot.png", sep = ""), qq_plot, width = 10, height = 6)
  
  # Heteroskedasticity Check
  heteroskedasticity_test <- bptest(model_lm)
  print(heteroskedasticity_test)
  
  # Autocorrelation Check
  autocorrelation_test <- pbgtest(model)
  print(autocorrelation_test)
}

## -- save non-ggplot-plots -- ##
save_plot <- function(plot_expr, filename, width = 10, height = 6) {
  png(filename, width = width * 100, height = height * 100)
  plot_expr
  dev.off()
}

## -- Influential Obs Investigation Function -- ##
influential_investigation <- function(model_lm, df_panel, output_path) {
  # Influential Observation Tests
  ## High Leverage Points
  hat_values <- hatvalues(model_lm)
  leverage_threshold = 2 * mean(hatvalues(model_lm))
  high_leverage_points = which(hatvalues(model_lm) > leverage_threshold)
  
  ## Cook's Distance
  cooks_d <- cooks.distance(model_lm)
  cooks_threshold = 4 / (length(model_lm$fitted.values) - length(model_lm$coefficients) - 2)
  high_cooks_points = which(cooks.distance(model_lm) > cooks_threshold)
  
  ## DFBETAS
  dfbetas_values <- dfbetas(model_lm)
  dfbetas_values = dfbetas(model_lm)
  high_dfbetas_points = which(abs(dfbetas_values) > (2 / sqrt(nrow(df_panel))))
  
  ## DFFITS
  dffits_values <- dffits(model_lm)
  plot(dffits_values, main = "DFFITS")
  dffits_threshold = 2 * sqrt(length(model_lm$coefficients) / nrow(df_panel))
  high_dffits_points = which(abs(dffits(model_lm)) > dffits_threshold)
  
  # Plot Influential Obs
  plot_hat <- plot(hat_values, main = "Leverage Values")
  plot_cooks <- plot(cooks_d, main = "Cook's Distance")
  plot_dfbetas <- plot(dfbetas_values, main = "DFBETAS")
  plot_dffits <- plot(dffits_values, main = "DFFITS")
  
  # Save Influential Obs Plots
  save_plot(plot(hat_values, main = "Leverage Values"), paste(output_path, "plot_hat.png", sep = ""))
  save_plot(plot(cooks_d, main = "Cook's Distance"), paste(output_path, "plot_cooks.png", sep = ""))
  save_plot(plot(dfbetas_values, main = "DFBETAS"), paste(output_path, "plot_dfbetas.png", sep = ""))
  save_plot(plot(dffits_values, main = "DFFITS"), paste(output_path, "plot_dffits.png", sep = ""))
  
  # Saving the influential obs as a table matched with unique ID
  influential_indices = unique(
    c(
      high_leverage_points,
      high_cooks_points,
      high_dfbetas_points,
      high_dffits_points
    )
  )
  
  influential_cases <- df_panel[influential_indices, 'unique_id']
  write.csv(influential_cases, paste(output_path, "influential_cases.csv", sep = ""))
}

#### Function Calls for Base Model ####
## Call Assumption Check for Base Model
assumption_function(
  model = base_model, 
  model_lm = base_model_lm, 
  df_panel, 
  output_path = result_dir_base_model)
## Call Influential Obs Function
influential_investigation(
  model_lm = base_model_lm, 
  df_panel, 
  output_path = result_dir_base_model)
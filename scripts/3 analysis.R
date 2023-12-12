# Script: Analysis
# Author: Tina
# Date: 12.12.2023

# 1. Preliminaries
## Load Libraries
library(dplyr)     # For data transformations
library(tidyr)     # For tidy data
library(psych)     # For descriptive statistics
library(car)       # For multicollinearity checks with vif() function
library(plm)       # For panel data operations
library(lmtest)    # For standard errors
library(ggplot2)   # For making plots
library(sf)        # for shape files ops and geographical representation
library(spdep)     # for moran's l test

## Define Directory Paths
data_dir <- "data/data_processed/"
## Load Data
load(paste(data_dir, "data.RData", sep = ""))
## Load Data with geometry
load(paste(data_dir, "data_spatial.RData", sep = "")) #with geometry
st_geometry(df_spatial) <- "geometry"

# 2. Data Selection and Inspection
## Data Selection and Transformation
df_model <- df %>% select(
  # ID, group, time for panel model
  unique_id,
  municipality,
  year,
  # quadratic time measure
  # year_centered,
  # year_squared,
  # dependent var
  perc_Pop_65_to_75_Meds,
  perc_change_Pop_65_to_75_Meds,
  perc_Pop_65_up_Meds,
  perc_change_Pop_65_up_Meds,
  #independent vars
  perc_change_GP_within_5km,        # proximity to GP changes
  ln_avg_Distance_GP,
  perc_Pop_65_up,                   # age demographic control
  perc_change_deaths_CVD,           # CVD control
  ln_perc_Pop_migration_background, # socioeconomic control
  ln_avg_Standardized_Income,       # prosperity control
  ln_pop_density_km2,               # rurality control
  # spatial divisions
  predominantly_Rural,              # binary rural var
  provinceNUTS                      # provinces as categorical
  # perc_Pop_Social_Security_excl_Pensioners, (excluding for multicol)
)

# Saving model with outliers included (and making it a panel df for later analysis)
df_model_with_outliers <- df_model
df_panel_with_outliers <- pdata.frame(df_model_with_outliers, index = c("municipality", "year"))

# making outlier list, filtering the df, and making it a panel df
outliers <- c("Vlieland", "Schiermonnikoog", "Rozendaal")
df_model <- df_model %>% filter(!municipality %in% outliers)
df_panel <- pdata.frame(df_model, index = c("municipality", "year"))

# Descriptive Statistics
df_descriptive_stats <-
  describe(df_model[, sapply(df_model, is.numeric)])
print(df_descriptive_stats)

### Model ####
# model fit
model <-
  plm(
    perc_change_Pop_65_up_Meds ~ perc_change_GP_within_5km + 
      perc_Pop_65_up + perc_change_deaths_CVD +
      ln_perc_Pop_migration_background + ln_avg_Standardized_Income + 
      ln_pop_density_km2 + year,
    data = df_panel,
    model = "pooling",
    index = c("municipality", "year")
  )
print(summary(model))

## Clustered Robust Standard Error Adjustment
clustered_robust_se <- vcovHC(model, method = "arellano", type = "HC1", cluster = "group")
summary(model, vcov = clustered_robust_se)

# assumption testing
## VIF
vif_values <- vif(model)
print(vif_values)

## residual plots
predicted_values <- predict(model)
residuals_values <- residuals(model)
df_with_predictions <- cbind(
  df_panel,
  predicted = predicted_values, 
  residuals = residuals_values
)

ggplot(df_with_predictions, aes(x = predicted, y = residuals)) +
  geom_point() + geom_smooth(method = "loess", col = "red") +
  labs(title = "Residuals vs Predicted of the Main Model with LOESS Line", x = "Predicted Values", y = "Residuals") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", colour = "white"))

## QQ Plot
qqnorm(residuals_values)

# bunch 'o tests
## Breusch–Pagan Heteroskedascity tests, see: Baltagi (2003)
plmtest(model, effect = "individual", type = "bp")
plmtest(model, effect = "time", type = "bp")
plmtest(model, effect = "twoways", type = "bp")

## Serial correlation Check with Durbin-Watson
pdwtest(model)

# Wooldridge’s Test for Unobserved Effects
pwtest(model)

# Spatial Investigation of Main Model #
# Moran's l for spatial correlation
## calculate neighbours from the municipal boundary shapefile
neighbors <- poly2nb(df_spatial)
lw <- nb2listw(neighbors)

# tests:
#outcome var perc_change_Pop_65_up_Meds
print(moran.test(df_spatial$perc_change_Pop_65_up_Meds, lw)) #significant
#perc_change_GP_within_5km
print(moran.test(df_spatial$perc_change_GP_within_5km, lw)) #not significant
#perc_Pop_65_up
print(moran.test(df_spatial$perc_Pop_65_up, lw)) #extremely significant
#perc_change_deaths_CVD
print(moran.test(df_spatial$perc_change_deaths_CVD, lw)) #p-value = 1 haha what
#ln_perc_Pop_migration_background
print(moran.test(df_spatial$ln_perc_Pop_migration_background, lw)) #extremely significant
#ln_avg_Standardized_Income
print(moran.test(df_spatial$ln_avg_Standardized_Income, lw)) #extremely significant
#ln_pop_density_km2
print(moran.test(df_spatial$ln_pop_density_km2, lw)) #extremely significant

## Urban and Rural stratified models ####
model_rural <-
  plm(
    perc_change_Pop_65_up_Meds ~ perc_change_GP_within_5km + 
      perc_Pop_65_up + perc_change_deaths_CVD +
      ln_perc_Pop_migration_background + ln_avg_Standardized_Income + 
      ln_pop_density_km2 + year,
    data = df_panel[df_panel$predominantly_Rural == 1, ],
    model = "pooling"
  )
clustered_robust_se_rural <- vcovHC(model_rural, method = "arellano", type = "HC1", cluster = "group")
summary(model_rural, vcov = clustered_robust_se_rural)

model_urban <-
  plm(
    perc_change_Pop_65_up_Meds ~ perc_change_GP_within_5km + 
      perc_Pop_65_up + perc_change_deaths_CVD +
      ln_perc_Pop_migration_background + ln_avg_Standardized_Income + 
      ln_pop_density_km2 + year,
    data = df_panel[df_panel$predominantly_Rural == 0, ],
    model = "pooling"
  )
clustered_robust_se_urban <- vcovHC(model_urban, method = "arellano", type = "HC1", cluster = "group")
summary(model_urban, vcov = clustered_robust_se_urban)

# assumption testing
# THESE MODELS ARE BASED ON UNBALANCED PANELS! This causes issues, the econometrics people say
## VIF
vif_values_rural <- vif(model_rural)
print(vif_values_rural)

vif_values_urban <- vif(model_urban)
print(vif_values_urban)
## residual plots
### rural
predicted_values_rural <- predict(model_rural)
residuals_values_rural <- residuals(model_rural)
### urban
predicted_values_urban <- predict(model_urban)
residuals_values_urban <- residuals(model_urban)

### plots
ggplot(
  data = data.frame(predicted_values_rural, residuals_values_rural),
  aes(x = predicted_values_rural, y = residuals_values_rural)) +
  geom_point() + geom_smooth(method = "loess", col = "red") +
  labs(title = "Rural model - Residuals vs Predicted with LOESS Line", x = "Predicted Values", y = "Residuals") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", colour = "white"))

ggplot(
  data = data.frame(predicted_values_urban, residuals_values_urban),
  aes(x = predicted_values_urban, y = residuals_values_urban)) +
  geom_point() + geom_smooth(method = "loess", col = "red") +
  labs(title = "Urban model - Residuals vs Predicted with LOESS Line", x = "Predicted Values", y = "Residuals") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", colour = "white"))

## QQ Plot
qqnorm(residuals_values_rural)
qqnorm(residuals_values_urban)

##### Other Models ######
### Model with outliers  ####
# model fit
model_with_outliers <-
  plm(
    perc_change_Pop_65_up_Meds ~ perc_change_GP_within_5km + 
      perc_Pop_65_up + perc_change_deaths_CVD +
      ln_perc_Pop_migration_background + ln_avg_Standardized_Income + 
      ln_pop_density_km2 + year,
    data = df_panel_with_outliers,
    model = "pooling",
    index = c("municipality", "year")
  )
print(summary(model_with_outliers))

## Clustered Robust Standard Error Adjustment
clustered_robust_se_outliers <- vcovHC(model_with_outliers, method = "arellano", type = "HC1", cluster = "group")
summary(model_with_outliers, vcov = clustered_robust_se_outliers)

# assumption testing
## VIF
vif_values_with_outliers <- vif(model_with_outliers)
print(vif_values_with_outliers)

## residual plots
predicted_values_with_outliers <- predict(model_with_outliers)
residuals_values_with_outliers <- residuals(model_with_outliers)
df_with_predictions_with_outliers <- cbind(
  df_panel_with_outliers,
  predicted_with_outliers = predicted_values_with_outliers, 
  residuals_with_outliers = residuals_values_with_outliers
)

ggplot(df_with_predictions_with_outliers, aes(x = predicted_with_outliers, y = residuals_with_outliers)) +
  geom_point() + geom_smooth(method = "loess", col = "red") +
  labs(title = "Residuals vs Predicted with LOESS Line of Model with Outliers", x = "Predicted Values", y = "Residuals") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", colour = "white"))

## QQ Plot
qqnorm(residuals_values_with_outliers)

###  Model mini with year dummies ####
# model fit
model_years <-
  plm(
    perc_change_Pop_65_up_Meds ~ perc_change_GP_within_5km + year,
    data = df_panel,
    model = "pooling"
  )
print(summary(model_years))

# assumption testing
## VIF
vif_values_years <- vif(model_years)
print(vif_values_years)

## residual plots
predicted_values_years <- predict(model_years)
residuals_values_years <- residuals(model_years)
df_with_predictions <- cbind(
  df_panel, 
  predicted_years = predicted_values_years, 
  residuals_years = residuals_values_years
)

ggplot(df_with_predictions, aes(x = predicted_years, y = residuals_years)) +
  geom_point() + geom_smooth(method = "loess", col = "red") +
  labs(title = "Residuals vs Predicted with LOESS Line", x = "Predicted Values", y = "Residuals") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", colour = "white"))

## QQ Plot
qqnorm(residuals_values_years)

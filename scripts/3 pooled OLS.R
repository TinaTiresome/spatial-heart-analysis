# Script: Pooled OLS
# Author: Tina
# Date: 30.11.2023

library(dplyr)     # for data tranformations
library(tidyr)     # for tidy data
library(psych)     # for nice descriptives table
library(car)       # for checking Multicollinearity with vif() function
library(plm)       # for panel data operations, Autocorrelation with pbgtest() function
library(lmtest)    # for Heteroskedascity check with bptest() function
library(sandwich)  # for Newey-West standard errors
library(ggplot2)   # for making plots

# loading balanced data
load("data/data_processed/data.RData")

# Data Selection
df_model <- df %>%
  select(
    c(
      "unique_id",
      # municipality+year unique case ID
      "municipality",
      # municipalities/group
      "year",
      # time var
      "year_squared",
      # squared time var as control
      "perc_change_Pop_65_to_75_Meds",
      # outcome/dependent var
      "perc_change_GP_within_5km",
      # changes in GP availability/main var of interest
      "perc_Pop_aged_65_up",
      # demographic control
      "ln_avg_Standardized_Income",
      # prosperity control var
      "ln_perc_Pop_migration_background",
      # socioeconomic control
      "perc_Pop_Social_Security_excl_Pensioners",
      # socioeconomic control
      "perc_change_deaths_CVD",
      # CVD control
      "predominantly_Rural"                        # spatial controls
    )
  )

# Descriptives
df_descriptive_stats <-
  describe(df_model[, sapply(df_model, is.numeric)])
print(df_descriptive_stats)

## Making it into a panel data frame
df_panel <- pdata.frame(df_model, index = c("municipality", "year"))

## Fit the panel base_model
base_model <-
  plm(
    perc_change_Pop_65_to_75_Meds ~ perc_change_GP_within_5km + perc_Pop_aged_65_up +
      ln_avg_Standardized_Income + predominantly_Rural +
      perc_change_deaths_CVD + ln_perc_Pop_migration_background +
      perc_Pop_Social_Security_excl_Pensioners + year + year_squared,
    data = df_panel,
    model = "pooling"
  )
summary(base_model)
notes_basemodel <- "
Model is significant, income and 2016 especially,
but also Rurality, benefit recipiants, 2015 and 2018.
Residuals are nice and even.
Adjusted R2 below 5%.
Balanced Panel: n = 371, T = 5, N = 1855
"
# normal linear model for assumption checks and testing:
base_model_lm <- lm(
  perc_change_Pop_65_to_75_Meds ~ perc_change_GP_within_5km + perc_Pop_aged_65_up +
    ln_avg_Standardized_Income + predominantly_Rural +
    perc_change_deaths_CVD + ln_perc_Pop_migration_background +
    perc_Pop_Social_Security_excl_Pensioners + year + year_squared,
  data = df_model
)
#### Assumptions and Validity Checks Base model ####
## - Multicollinearity
# Check VIF values to see if the factors are independent (below 5)
# the years break this, so to check this, running the model without the year var
base_model_Multicollinearity <-
  plm(
    perc_change_Pop_65_to_75_Meds ~ perc_change_GP_within_5km + perc_Pop_aged_65_up +
      ln_avg_Standardized_Income + predominantly_Rural +
      perc_change_deaths_CVD + ln_perc_Pop_migration_background +
      perc_Pop_Social_Security_excl_Pensioners,
    data = df_panel,
    model = "pooling"
  )
#summary(base_model_Multicollinearity)
vif_values <- vif(base_model_Multicollinearity)
print(vif_values) # All values are below 5, highest is 3.11

## - Linearity: The relationship between the independent variables and the dependent variable is linear
# > Inspect Residual Scatter Plot for checking Linearity (and Homoscedasticity)
predicted_values_base <- predict(base_model)
residuals_values_base <- residuals(base_model)
#Adding them to df to be able to label the outliers
df_with_predictions <-
  cbind(df_panel, predicted_values_base, residuals_values_base)
## Plot observed vs. predicted values
residuals_plot <-
  ggplot(df_with_predictions,
         aes(x = predicted_values_base, y = residuals_values_base)) +
  geom_point() +
  geom_smooth(method = "loess", col = "red") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "blue") +
  geom_text(aes(
    label = ifelse(
      abs(residuals_values_base) > 2.6 |
        predicted_values_base > 0.2 |
        predicted_values_base < -0.7,
      unique_id,
      ""
    )
  ), angle = 45) +
  labs(title = "Residuals vs Predicted with LOESS Line with Outliers labeled",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()
print(residuals_plot)
notes_basemodel_outliers <- "
Vlieland (least densely populated municipality) is predicted terribly
Laren (NH.) in 2016 was predicted highest (Laren is a very rich municipality)
Bloemendaal, Rozendaal and Wassenaar and Blaricum are the other very rich muncipalities
But they are less problematic now that the year is included in the model"
## - Normality of Errors
# QQ plot to check normality of residuals
qqnorm(residuals_values_base)
qq_plot <-
  qqline(residuals_values_base, col = "steelblue", lwd = 2) # not great
### - Homoscedasticity
# Inspect Residual Scatter Plot to check if the variance of residuals is constant
# AND Breusch-Pagan test for heteroskedascity
heteroskedasticity_test <- bptest(base_model)
print(heteroskedasticity_test)
notes_basemodel_heteroskedasticity <- "
Very significant, Definitely heteroskedascity. Is Clustered Robust SE enough?
BP = 67.733, df = 11, p-value = 3.287e-10"
## - Independence/Autocorrelation Observations are independent of each other
# Breusch-Godfrey/Woolridge test for serial correlation
autocorrelation_test <- pbgtest(base_model)
print(autocorrelation_test)
notes_basemodel_autocorrelation <- "
Not significant, yay!
chisq = 5.003, df = 5, p-value = 0.4155"

#### Influential Observation Investigation ####
# High Leverage Points
hat_values <- hatvalues(base_model_lm)
plot(hat_values, main = "Leverage Values")

leverage_threshold = 2 * mean(hatvalues(base_model_lm))
high_leverage_points = which(hatvalues(base_model_lm) > leverage_threshold)

# Cook's Distance
cooks_d <- cooks.distance(base_model_lm)
plot(cooks_d, main = "Cook's Distance")

cooks_threshold = 4 / (length(base_model_lm$fitted.values) - length(base_model_lm$coefficients) - 2)
high_cooks_points = which(cooks.distance(base_model_lm) > cooks_threshold)

# DFBETAS
dfbetas_values <- dfbetas(base_model_lm)
plot(dfbetas_values, main = "DFBETAS")

dfbetas_values = dfbetas(base_model_lm)
high_dfbetas_points = which(abs(dfbetas_values) > (2 / sqrt(nrow(df_model))))

# DFFITS
dffits_values <- dffits(base_model_lm)
plot(dffits_values, main = "DFFITS")

dffits_threshold = 2 * sqrt(length(base_model_lm$coefficients) / nrow(df_model))
high_dffits_points = which(abs(dffits(base_model_lm)) > dffits_threshold)

# Extracting the Influential observations
influential_cases = df_model[unique(
  c(
    high_leverage_points,
    high_cooks_points,
    high_dfbetas_points,
    high_dffits_points
  )
), 'unique_id']


#### Removing Residual Outliers abd Sensitivity Analysis ####
## Running model again, Excluding sparsely populated Vlieland and Schiermonnikoog
excluded_municipalities <-
  c("Vlieland",
    "Schiermonnikoog")
df_panel_outliers_removed <-
  df_panel %>% filter(!municipality %in% excluded_municipalities)

## Fitting the panel model again, with the outliers removed
model_outliers_removed <-
  plm(
    perc_change_Pop_65_to_75_Meds ~ perc_change_GP_within_5km + perc_Pop_aged_65_up +
      ln_avg_Standardized_Income + predominantly_Rural +
      perc_change_deaths_CVD + ln_perc_Pop_migration_background +
      perc_Pop_Social_Security_excl_Pensioners + year + year_squared,
    data = df_panel_outliers_removed,
    model = "pooling"
  )
summary(model_outliers_removed)

### Assumption Outliers removed ###
## Residual Plot for Heteroskedascity and Linearity check
predicted_values_outliers <- predict(model_outliers_removed)
residuals_values_outliers <- residuals(model_outliers_removed)
## Plot
residuals_plot_outliers <-
  ggplot(
    data = data.frame(predicted_values_outliers, residuals_values_outliers),
    aes(x = predicted_values_outliers, y = residuals_values_outliers)
  ) +
  geom_point() +
  geom_smooth(method = "loess", col = "red") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "blue") +
  labs(title = "Residuals vs Predicted with Outliers removed", x = "Predicted Values", y = "Residuals") +
  theme_minimal()
print(residuals_plot_outliers)
notes_outliers_residuals <- "looks good!"
# QQ plot for normality
qqnorm(residuals_values_outliers)
qq_plot <-
  qqline(residuals_values_outliers, col = "steelblue", lwd = 2) # BAD!
# Heteroskedascity test
heteroskedasticity_test <- bptest(model_outliers_removed)
print(heteroskedasticity_test)# significant
# Autocorrelation/Independence test
autocorrelation_test <- pbgtest(model_outliers_removed)
print(autocorrelation_test) # not significant

####  Clustered Robust Standard Error ####
# Base Model for comparison
model_clustered_robust3 <-
  coeftest(base_model, vcovHC(base_model, type = "HC2", cluster = "group"))
print(model_clustered_robust3)

# Outliers removed HC2
model_RSE_outliers_removed_cluster_group <-
  coeftest(
    model_outliers_removed,
    vcovHC(model_outliers_removed, type = "HC2", cluster = "group")
  )
print(model_RSE_outliers_removed_cluster_group)

model_RSE_outliers_removed_cluster_time <-
  coeftest(
    model_outliers_removed,
    vcovHC(model_outliers_removed, type = "HC3", cluster = "group")
  )
print(model_RSE_outliers_removed_cluster_group)
notes_outliers_RSE <- "
Using a group clustered RSE makes changes in distance to GP unsignificant"

#### Reverse Causality of CV mortality Stratified Analysis ####
# Stratifying into low/mid/high cardiovascular mortality rates
df_panel$cv_mortality_strata <-
  cut(
    df_panel$perc_change_deaths_CVD,
    breaks = quantile(df_panel$perc_change_deaths_CVD, probs = 0:3 / 3),
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE
  )

# Low CV mortality
model_low <- plm(
  perc_change_Pop_65_to_75_Meds ~ perc_change_GP_within_5km + perc_Pop_aged_65_up +
    ln_avg_Standardized_Income + predominantly_Rural +
    perc_change_deaths_CVD + ln_perc_Pop_migration_background +
    perc_Pop_Social_Security_excl_Pensioners + year + year_squared,
  data = df_panel[df_panel$cv_mortality_strata == "Low",],
  model = "pooling"
)

# Medium CV mortality
model_medium <- plm(
  perc_change_Pop_65_to_75_Meds ~ perc_change_GP_within_5km + perc_Pop_aged_65_up +
    ln_avg_Standardized_Income + predominantly_Rural +
    perc_change_deaths_CVD + ln_perc_Pop_migration_background +
    perc_Pop_Social_Security_excl_Pensioners + year + year_squared,
  data = df_panel[df_panel$cv_mortality_strata == "Medium",],
  model = "pooling"
)

# High CV mortality
model_high <- plm(
  perc_change_Pop_65_to_75_Meds ~ perc_change_GP_within_5km + perc_Pop_aged_65_up +
    ln_avg_Standardized_Income + predominantly_Rural +
    perc_change_deaths_CVD + ln_perc_Pop_migration_background +
    perc_Pop_Social_Security_excl_Pensioners + year + year_squared,
  data = df_panel[df_panel$cv_mortality_strata == "High",],
  model = "pooling"
)

summary(model_low)
summary(model_medium)
summary(model_high)
notes_CV_mortality <- "
GP Density's Role Varies across the strata, marginally significant in high CV mortality areas but not significant in low and medium CV mortality areas.
Economic Factors are pretty consistently important.
Year Effects (especially 2016) show significant effects in all models.
Rurality is significant in the medium CV mortality model.
NOTE: Not assumption checked"

#### Rural/Urban Investigation ####
## Pooled urban and rural models for comparison
model_rural <-
  plm(
    perc_change_Pop_65_to_75_Meds ~ perc_change_GP_within_5km + perc_Pop_aged_65_up +
      ln_avg_Standardized_Income + predominantly_Rural +
      perc_change_deaths_CVD + ln_perc_Pop_migration_background +
      perc_Pop_Social_Security_excl_Pensioners + year + year_squared,
    data = df_panel[df_panel$predominantly_Rural == 1, ],
    model = "pooling"
  )
summary(model_rural)
model_urban <-
  plm(
    perc_change_Pop_65_to_75_Meds ~ perc_change_GP_within_5km + perc_Pop_aged_65_up +
      ln_avg_Standardized_Income + predominantly_Rural +
      perc_change_deaths_CVD + ln_perc_Pop_migration_background +
      perc_Pop_Social_Security_excl_Pensioners + year + year_squared,
    data = df_panel[df_panel$predominantly_Rural == 0, ],
    model = "pooling"
  )
summary(model_urban)
notes_rurality_urbanity <- "
They are now unbalanced panels, which I think causes issues with assumptions.
Also not assumption checked.
urban model is not significant. rural model does show significance for changes in GP"

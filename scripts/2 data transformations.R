# Script: Data Transformations
# Author: Tina
# Date: 29.11.2023

library(dplyr)     # for data tranformations
library(tidyr)     # for tidy data

load("data/data_imports/imported_data.RData")
df <- df_import
rm("df_import")

#### Variable Calculation/Transformations ####
## Make column for each age groups medication percentage ##
df <- df %>%
  spread(key = c("Age_Group_with_meds"),
         value = c("perc_Pop_with_meds"))
# Rename new colomns
df <- df %>%
  rename(
    "perc_Pop_65_to_75_Meds" = "65 tot 75 jaar",
    "perc_Pop_75_up_Meds" = "75 jaar of ouder",
    "perc_Pop_Total_Meds" = "Totaal, gestandaardiseerd"
  )

## Calculation of "% population age 65+" variable ##
# from dataimport_region > add up the percentages of 65-80 and 80 to get % of pop 65+
df$perc_Pop_aged_65_up <-
  df$perc_Pop_aged_65_80 + df$perc_Pop_aged_80_up

## Calculating the share of population on benefits from totals ##
df$perc_Pop_Social_Security_excl_Pensioners <-
  (df$total_Social_Security_Recipiants_excl_Pensioners / df$total_Pop) * 100
# rounding calculated variable to two decimals
df <- df %>%
  mutate(
    perc_Pop_Social_Security_excl_Pensioners = round(perc_Pop_Social_Security_excl_Pensioners, 2)
  )

## Calculating share of deaths caused by cardiovascular from totals ##
df$perc_deaths_CVD <-
  (df$total_deaths_CVD / df$total_deaths) * 100
# rounding calculated variable to two decimals
df <- df %>%
  mutate(perc_deaths_CVD = round(perc_deaths_CVD, 2))

## Transforming Variables with log ##
# avg distance
df$ln_avg_Distance_GP <- log(df$avg_Distance_GP)
# income
df$ln_avg_Standardized_Income <- log(df$avg_Standardized_Income)
# wealth
df$ln_median_Household_Wealth <- log(df$median_Household_Wealth)
# perc_migration_background
df$ln_perc_Pop_migration_background <- log(df$perc_Pop_migration_background)

## Creating Binary variables ##
# 1 = Predominantly Rural Pop, 0 = Predominantly Urban Pop
df$predominantly_Rural <-
  ifelse(df$total_Pop_Rural > df$total_Pop_Urban, 1, 0)
# 1 = population shrunk/negative pop growth, 0 = positive relative pop growth
df$pop_shrinking <-
  ifelse(df$rel_pop_growth < 0, 1, 0)

## Defining Shrinkage Regions
# If more than 4 of 6 observed years are shrinking, 1 = shrinkage region
df <- df %>%
  group_by(municipality) %>%
  mutate(shrinkage_count = sum(pop_shrinking)) %>%
  mutate(shrinkage_region = ifelse(shrinkage_count >= 4, 1, 0)) %>%
  ungroup()
# -> 40 municipalities are 'shrinkage regions'

#### Year on Year Change Transformations ####
# Ordering dataframe
df <- df %>% 
  arrange(municipality, year)
# populate new column with NA
df$prev_GP_within_5km <- NA

# Loop through each row
for(i in 2:nrow(df)) {
  # Check if the current row and the previous row are from the same municipality
  if(df$municipality[i] == df$municipality[i-1]) {
    df$prev_GP_within_5km[i] <- df$GP_within_5km[i-1]
  }
}
# Calculate the percentage change
df$perc_change_GP_within_5km <- (df$GP_within_5km - df$prev_GP_within_5km) / df$prev_GP_within_5km * 100

### Medication Rate changes
df$prev_perc_Pop_65_to_75_Meds <- NA

# Loop through each row
for(i in 2:nrow(df)) {
  # Check if the current row and the previous row are from the same municipality
  if(df$municipality[i] == df$municipality[i-1]) {
    df$prev_perc_Pop_65_to_75_Meds[i] <- df$perc_Pop_65_to_75_Meds[i-1]
  }
}
# Calculate the percentage change
df$perc_change_Pop_65_to_75_Meds <- (df$perc_Pop_65_to_75_Meds - df$prev_perc_Pop_65_to_75_Meds)

### CVD Deaths
df$prev_perc_deaths_CVD <- NA

# Loop through each row
for(i in 2:nrow(df)) {
  # Check if the current row and the previous row are from the same municipality
  if(df$municipality[i] == df$municipality[i-1]) {
    df$prev_perc_deaths_CVD[i] <- df$perc_deaths_CVD[i-1]
  }
}
# Calculate the percentage change
df$perc_change_deaths_CVD <- (df$perc_deaths_CVD - df$prev_perc_deaths_CVD)

### Social Security excl Pensioners
df$prev_perc_Pop_Social_Security_excl_Pensioners <- NA

# Loop through each row
for(i in 2:nrow(df)) {
  # Check if the current row and the previous row are from the same municipality
  if(df$municipality[i] == df$municipality[i-1]) {
    df$prev_perc_Pop_Social_Security_excl_Pensioners[i] <- df$perc_Pop_Social_Security_excl_Pensioners[i-1]
  }
}
# Calculate the percentage change
df$perc_change_Pop_Social_Security_excl_Pensioners <- (df$perc_Pop_Social_Security_excl_Pensioners - df$prev_perc_Pop_Social_Security_excl_Pensioners)

### % of pop aged 65+
df$prev_perc_Pop_aged_65_up <- NA

# Loop through each row
for(i in 2:nrow(df)) {
  # Check if the current row and the previous row are from the same municipality
  if(df$municipality[i] == df$municipality[i-1]) {
    df$prev_perc_Pop_aged_65_up[i] <- df$perc_Pop_aged_65_up[i-1]
  }
}
# Calculate the percentage change
df$perc_change_Pop_aged_65_up <- (df$perc_Pop_aged_65_up - df$prev_perc_Pop_aged_65_up)


## get rid of 2013 as no previous data
df <- df %>%
  filter(year != 2013)

## Make quadratic term of year
df$year <- as.numeric(df$year)
df$year_squared <- df$year^2

#### Variable Selection ####
# Dropping unnecessary columns and ordering them
df_unbalanced <- df %>%
  select(
    c(
      "unique_id",               # municipality+year unique case ID
      "municipality",
      "year",
      "year_squared",
      # outcome/dependent var
      "perc_Pop_65_to_75_Meds",  # share of pop aged 65 to 75 with CV medication
      "perc_Pop_75_up_Meds",     # share of pop aged 75+ with CV medication
      "perc_change_Pop_65_to_75_Meds", # Year on Year Change
      # independent variable with alts
      "avg_Distance_GP",         # in km
      "ln_avg_Distance_GP",      # log of avg distance in km
      "GP_within_5km",           # amount of GP within 5km
      "perc_change_GP_within_5km",
      # demographic control var
      "perc_Pop_aged_65_up",     # share of pop age 65+
      "perc_change_Pop_aged_65_up",
      # properity control var
      "avg_Standardized_Income",
      "ln_avg_Standardized_Income",
      "median_Household_Wealth",
      "ln_median_Household_Wealth",
      # socioeconomic control var
      "perc_Pop_migration_background",
      "ln_perc_Pop_migration_background",
      "perc_Pop_Social_Security_excl_Pensioners",
      "perc_change_Pop_Social_Security_excl_Pensioners",
      # CVD control
      "perc_deaths_CVD",
      "perc_change_deaths_CVD",
      # extras
      "total_Pop",
      "rel_pop_growth",
      "shrinkage_region",
      "perc_Pop_aged_65_80",
      "perc_Pop_aged_80_up",
      "predominantly_Rural",
      "Regions"                # region code for shapefile join
    )
  )
# 2140 obs

#### Cleaning ####
# df is the subset with complete cases (missing values removed)
df <- df_unbalanced[complete.cases(df_unbalanced),]
# 1855 obs > muncipalities with missing values had years without data
# variable shrinkage_region requires 6 years of data too

# Subset with only cases with missing values
df_missing_cases <- df_unbalanced[!complete.cases(df_unbalanced),]
# this is all the municipalities with missing years, due to not existing in all 6 years

# checking that all included municipalities have all years
municipality_counts <- df %>%
  group_by(municipality) %>%
  summarise(count = n())

# saving the balanced and cleaned of NA as df
save(df, file = "data/data_processed/data.RData")
# saving df_unbalanced with missing values as df_
save(df_unbalanced, file = "data/data_processed/data_unbalanced.RData")
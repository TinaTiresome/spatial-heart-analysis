# Script: Data Transformations
# Author: Tina
# Date: 6.12.2024

library(dplyr)     # for data tranformations
library(tidyr)     # for tidy data

load("data/data_imports/imported_data.RData")
load("data/data_imports/shapefile_data.RData") # shapefile data merged at the end
df <- df_import
rm("df_import")

# Variable Calculation/Transformations
## Make column for each age groups medication percentage ##
df <- df %>%
  spread(key = c("Age_Group_with_meds"),
         value = c("perc_Pop_with_meds")) %>%
  rename(
    "perc_Pop_65_to_75_Meds" = "65 tot 75 jaar",
    "perc_Pop_75_up_Meds" = "75 jaar of ouder",
    "perc_Pop_Total_Meds" = "Totaal, gestandaardiseerd"
  )

## Calculate perc_Pop_65_up from totals  AND Population 65+ with MEDS ##
df <- df %>%
  mutate(
    # Calculate perc_Pop_65_up from totals
    total_Pop_65_up = total_Pop_65_75 + total_Pop_75_85 + total_Pop_85_up,
    perc_Pop_65_up = (total_Pop_65_up / total_Pop) * 100,
    
    # then, first get the total number of people aged 65 to 75 on medication
    total_Pop_65_to_75_Meds = floor(total_Pop_65_75 * (perc_Pop_65_to_75_Meds / 100)),
    
    # get total_Pop_75 up from total sum of 75-85 and 85+ to calc total_Pop_75_up_Meds
    total_Pop_75_up = total_Pop_75_85 + total_Pop_85_up,
    total_Pop_75_up_Meds = floor(total_Pop_75_up * (perc_Pop_75_up_Meds / 100)),
    
    # Then, add up these totals, ...
    total_Pop_65_up_Meds = total_Pop_65_to_75_Meds + total_Pop_75_up_Meds,
    
    # to get the % of pop aged 65+ with meds
    perc_Pop_65_up_Meds = round((total_Pop_65_up_Meds / total_Pop_65_up) * 100, 2)
  )

## Calculating the share of population from totals ##
df <- df %>%
  mutate(
    ## social security benefits
    perc_Pop_Social_Security_excl_Pensioners = round(
      (df$total_Social_Security_Recipiants_excl_Pensioners / df$total_Pop) * 100, 2),
    ## Deaths from CVD
    perc_deaths_CVD = round(
      (df$total_deaths_CVD / df$total_deaths) * 100, 2)
    )

## Transforming Variables with log ##
df <- df %>%
  mutate(
    ## avg distance
    ln_avg_Distance_GP = log(df$avg_Distance_GP),
    ## income
    ln_avg_Standardized_Income = log(df$avg_Standardized_Income),
    ## wealth
    ln_median_Household_Wealth = log(df$median_Household_Wealth),
    ## perc_migration_background
    ln_perc_Pop_migration_background = log(df$perc_Pop_migration_background),
    ## ambient_address_density_km2
    ln_pop_density_km2 = log(df$pop_density_km2)
  )

#### Year on Year Change Transformations ####
# Ordering dataframe
df <- df %>% 
  arrange(municipality, year)
# populate new column with NA for the calculations
df$prev_GP_within_5km <- NA
df$prev_perc_Pop_65_to_75_Meds <- NA
df$prev_perc_Pop_65_up_Meds <- NA
df$prev_perc_deaths_CVD <- NA

# Loop through each row
## GP within 5km
for(i in 2:nrow(df)) {
  # Check if the current row and the previous row are from the same municipality
  if(df$municipality[i] == df$municipality[i-1]) {
    df$prev_GP_within_5km[i] <- df$GP_within_5km[i-1]
  }
}
# Calculate the percentage change
df$perc_change_GP_within_5km <- (df$GP_within_5km - df$prev_GP_within_5km) / df$prev_GP_within_5km * 100

## Medication Rate changes 65-75
for(i in 2:nrow(df)) {
  # Check if the current row and the previous row are from the same municipality
  if(df$municipality[i] == df$municipality[i-1]) {
    df$prev_perc_Pop_65_to_75_Meds[i] <- df$perc_Pop_65_to_75_Meds[i-1]
  }
}
# Calculate the percentage change
df$perc_change_Pop_65_to_75_Meds <- (df$perc_Pop_65_to_75_Meds - df$prev_perc_Pop_65_to_75_Meds)

## Medication Rate changes 65+
for(i in 2:nrow(df)) {
  # Check if the current row and the previous row are from the same municipality
  if(df$municipality[i] == df$municipality[i-1]) {
    df$prev_perc_Pop_65_up_Meds[i] <- df$perc_Pop_65_up_Meds[i-1]
  }
}
# Calculate the percentage change
df$perc_change_Pop_65_up_Meds <- (df$perc_Pop_65_up_Meds - df$prev_perc_Pop_65_up_Meds)

## CVD Deaths
for(i in 2:nrow(df)) {
  # Check if the current row and the previous row are from the same municipality
  if(df$municipality[i] == df$municipality[i-1]) {
    df$prev_perc_deaths_CVD[i] <- df$perc_deaths_CVD[i-1]
  }
}
# Calculate the percentage change
df$perc_change_deaths_CVD <- (df$perc_deaths_CVD - df$prev_perc_deaths_CVD)

#### Region/spatial variables ####
## Creating Binary variables ##
# 1 = Predominantly Rural Pop, 0 = Predominantly Urban Pop
df$predominantly_Rural <-
  ifelse(df$total_Pop_Rural > df$total_Pop_Urban, 1, 0)
# 1 = population shrunk/negative pop growth, 0 = positive relative pop growth
df$pop_shrinking <-
  ifelse(df$rel_pop_growth < 0, 1, 0)

## Defining Shrinkage Regions ##
# If more than 4 of 7 observed years are shrinking, 1 = shrinkage region
df <- df %>%
  group_by(municipality) %>%
  mutate(shrinkage_count = sum(pop_shrinking)) %>%
  mutate(shrinkage_region = ifelse(shrinkage_count >= 4, 1, 0)) %>%
  ungroup()

## Province Factors ##
df$province <- trimws(df$province)
print(unique(df$province))
df <- df %>%
  mutate(provinceNUTS = case_when(
    province %in% c("Groningen", "Fryslân", "Drenthe") ~ "North",
    province %in% c("Overijssel", "Gelderland", "Flevoland") ~ "East",
    province %in% c("Utrecht", "Noord-Holland", "Zuid-Holland", "Zeeland") ~ "West",
    province %in% c("Noord-Brabant", "Limburg") ~ "South",
    TRUE ~ province # This line ensures that any province not listed retains its original value
  ))

df$provinceNUTS <- as.factor(df$provinceNUTS)

#### time vars ####
## Make quadratic term of year ##
df$year <- as.numeric(df$year)
# using quadratic time trend with centered/meaned year measure and square root of it
mean_year <- mean(df$year)
df$year_centered <- df$year - mean_year # center the year
df$year_squared <- df$year_centered^2  # square centered year

#### Variable Selection ####
# Dropping unnecessary columns and ordering them
df_unbalanced <- df %>%
  select(
    c(
      "unique_id",               # municipality+year unique case ID
      "municipality",
      "year",
      "year_centered",
      "year_squared",
      # outcome/dependent var
      "perc_Pop_65_to_75_Meds",        # % of pop aged 65-75 with CV medication
      "perc_change_Pop_65_to_75_Meds", # Year on Year Change
      "perc_Pop_65_up_Meds",           # % of pop aged 65+ with CV medication
      "perc_change_Pop_65_up_Meds",    # Year on Year Change
      # independent variable with alts
      "avg_Distance_GP",               # in km
      "ln_avg_Distance_GP",            # log of avg distance in km
      "GP_within_5km",                 # amount of GP within 5km
      "perc_change_GP_within_5km",     # YoY% change
      # demographic control var
      "perc_Pop_65_up",                # share of pop age 65+
      # properity control var
      "avg_Standardized_Income",
      "ln_avg_Standardized_Income",
      "median_Household_Wealth",
      "ln_median_Household_Wealth",
      # socioeconomic control var
      "perc_Pop_migration_background",
      "ln_perc_Pop_migration_background",
      "perc_Pop_Social_Security_excl_Pensioners",
      # CVD control
      "perc_deaths_CVD",
      "perc_change_deaths_CVD",
      #spatial control,
      "pop_density_km2",
      "ln_pop_density_km2",
      "predominantly_Rural",
      "provinceNUTS",          # for RSE clustering or as dummy
      "Regions",               # region code for shapefile join
      # extras
      "total_Pop",
      "rel_pop_growth",
      "shrinkage_region"
    )
  )
# 2140 obs, 31 col
# with extra year: 2996

#### Cleaning ####
# df is the subset with complete cases (missing values removed)
df <- df_unbalanced[complete.cases(df_unbalanced),]
# 1855 obs > muncipalities with missing values had years without data
# with extra year: 2022

# Subset with only cases with missing values
df_missing_cases <- df_unbalanced[!complete.cases(df_unbalanced),]
# this is all the municipalities with missing years, due to not existing in all 7 years

# checking that all included municipalities have all years
municipality_counts <- df %>%
  group_by(municipality) %>%
  summarise(count = n())
# 337 municipalities with full data

# saving the balanced and cleaned of NA as df_balanced
save(df, file = "data/data_processed/data.RData")
# saving df_unbalanced with missing values as df_unbalanced
save(df_unbalanced, file = "data/data_processed/data_unbalanced.RData")

# Joining shapefiles to df
df_spatial <- shapefile_data %>%
  inner_join(df, by = c("Regions", "year"))
# saving 
save(df_spatial, file = "data/data_processed/data_spatial.RData")

#and making one with missing values for maps
df_map <- shapefile_data %>%
  inner_join(df_unbalanced, by = c("Regions", "year"))
# saving 
save(df_map, file = "data/data_processed/data_map.RData")

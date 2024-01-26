# Script: Data Visualisations
# Author: Tina
# Date: 12.12.2023

library(dplyr)     # for data tranformations
library(tidyr)     # for tidy data
library(ggplot2)   # for datavis
library(sf)        # for shape files ops and geographical representation
library(scales)

#### Map Data Operations ####
# loading the data
load("data/data_processed/data.RData")
load("data/data_processed/data_map.RData") # unbalanced data with geometry
st_geometry(df_map) <- "geometry"

outliers <- c("Vlieland", "Schiermonnikoog", "Rozendaal")
df <-
  df %>% filter(!municipality %in% outliers)

######### -- Function Defs -- ##################
# -- mapmaker -- #
mapmaker <-
  function(data,
           indicator_variable,
           title_base,
           color_palette,
           color_limits = NULL) {
    years <- 2014:2019
    
    # Determine color scale by looking at the range of values across the years
    if (is.null(color_limits)) {
      color_limits <- range(data[[indicator_variable]], na.rm = TRUE)
    }
    # making temp df filtered for each year
    for (year in years) {
      temp_df <- data %>%
        filter(year == !!year)
      
      #making sure the polygons are read as a sf object
      if (!inherits(temp_df, "sf")) {
        temp_df <- st_as_sf(temp_df)
      }
      
      # Dynamic variable mapping
      fill_aes <- aes_string(fill = indicator_variable)
      
      # Plotting the map
      p <- ggplot(temp_df) +
        geom_sf(fill_aes, color = NA) +
        scale_fill_gradientn(colors = color_palette,
                             limits = color_limits,
                             oob = scales::squish) +
        labs(title = paste(title_base, year),
             fill = title_base) +
        theme_void() +
        theme(
          legend.position = "bottom",
          plot.background = element_rect(fill = "white", colour = "white"),
          plot.title = element_text(size = 14)
        )
      
      # Saving the plots
      ggsave(
        paste0("outputs/maps/map_", indicator_variable, "_", year, ".png"),
        plot = p,
        width = 10,
        height = 8
      )
    }
  }

# -- scattermaker -- #
scattermaker <- function(data, x_var, y_var, x_lab, y_lab) {
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    ggtitle(paste("Scatter plot of", y_var, "vs.", x_var)) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", colour = "white"),
      plot.title = element_text(size = 14)
    )
  
  ggsave(paste0("outputs/graphs/scatter_", x_var, "_vs_", y_var, ".png"), plot = p, width = 10, height = 8)
}


## Custom Colour Palettes
# Custom color palette
palette_gradient <- c("#c2f9b6",
                      "#868686",
                      "#5f0a64")

#################################
## Split Map of Pop with meds ####
# Data preparation
temp_df <- df_map %>%
  filter(year == 2016)

temp_df$AreaType <- ifelse(temp_df$predominantly_Rural == 1, "Rural", "Urban")

# Color scale limits
color_limits <- range(temp_df[["perc_Pop_65_up_Meds"]], na.rm = TRUE)

# Custom color palette
color_palette <- c("#FAF8FF", "#200A64")

# Combined plot with facets
ggplot(temp_df) +
  geom_sf(aes(fill = perc_Pop_65_up_Meds)) +
  scale_fill_gradientn(colors = color_palette, limits = color_limits, oob = scales::squish) +
  facet_wrap(~AreaType, strip.position = "bottom") +
  labs(title = "Share of Population aged 65+ with Cardiovascular Medication in 2016",
       fill = "Population 65+ with Medication (%)") +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 18),
    strip.text = element_text(size = 14),
    strip.background = element_rect(fill = "gray90", color = NA),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


# By year plots ####
## perc_change_Pop_65_up_Meds
ggplot(df, aes(x = as.factor(year), y = perc_change_Pop_65_up_Meds, fill = as.factor(year))) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title = "% Change in Share of Population 65+ with CV Medication per Year", x = "Year", y = "perc_change_Pop_65_up_Meds", fill = "Year") +
  theme (
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 14)
  )

## perc_change_GP_within_5km
ggplot(df, aes(x = as.factor(year), y = perc_change_GP_within_5km, fill = as.factor(year))) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title = "% Change in GP practices within 5km per Year", x = "Year", y = "perc_change_GP_within_5km", fill = "Year") +
  theme (
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 14)
  )

## perc_Pop_aged_65_up
ggplot(df, aes(x = as.factor(year), y = perc_Pop_65_up, fill = as.factor(year))) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Share of Population in Municipalities aged 65+ per Year", x = "Year", y = "perc_Pop_aged_65_up", fill = "Year") +
  theme (
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 14)
  )

## perc_change_deaths_CVD
ggplot(df, aes(x = as.factor(year), y = perc_change_deaths_CVD, fill = as.factor(year))) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title = "V% Change in Deaths attributed to CVD per Year", x = "Year", y = "perc_change_deaths_CVD", fill = "Year") +
  theme (
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 14)
  )

## ln_perc_Pop_migration_background
ggplot(df, aes(x = as.factor(year), y = ln_perc_Pop_migration_background, fill = as.factor(year))) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Log of % of Population with Migration Background per Year", x = "Year", y = "ln_perc_Pop_migration_background", fill = "Year") +
  theme (
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 14)
  )

## ln_avg_Standardized_Income
ggplot(df, aes(x = as.factor(year), y = ln_avg_Standardized_Income, fill = as.factor(year))) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Log of Average Income per Year", x = "Year", y = "ln_avg_Standardized_Income", fill = "Year") +
  theme (
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 14)
  )

## ln_pop_density_km2
ggplot(df, aes(x = as.factor(year), y = ln_pop_density_km2, fill = as.factor(year))) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Log of Population Density per km2 per Year", x = "Year", y = "ln_pop_density_km2", fill = "Year") +
  theme (
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 14)
  )

## ln_avg_Distance_GP
ggplot(df, aes(x = as.factor(year), y = ln_avg_Distance_GP, fill = as.factor(year))) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Log of Average Distance to nearest GP Practise per Year", x = "Year", y = "ln_avg_Distance_GP", fill = "Year") +
  theme (
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 14)
  )


# -- Map Maker Calls -- #
mapmaker(
  data = df_map,
  indicator_variable = "perc_change_Pop_65_up_Meds",
  title_base = "Year on Year % Change in Share of Population 65-75 on CV Medication",
  color_palette = palette_gradient,
)

mapmaker(
  data = df_map,
  indicator_variable = "perc_change_GP_within_5km",
  title_base = "Year on Year Change of GP practises within 5km by Municipality",
  color_palette = palette_gradient,
)

mapmaker(
  data = df_map,
  indicator_variable = "avg_Distance_GP",
  title_base = "Average Distance to GP within Municipalities in KM",
  color_palette = palette_gradient,
)

mapmaker(
  data = df_map,
  indicator_variable = "predominantly_Rural",
  title_base = "predominantly Rural Municipalities in the Netherlands",
  color_palette = palette_gradient,
)
# -- Scatter Maker Calls -- #
scattermaker(
  df,
  "perc_change_GP_within_5km",
  "perc_change_Pop_65_up_Meds",
  "Y-o-Y % Change of number of GPs within 5km of Municipality",
  "Percentage of Population Aged 65+ with Cardiovascular Meds"
)

scattermaker(
  df,
  "perc_Pop_aged_65_up",
  "perc_change_Pop_65_up_Meds",
  "% of Population aged 65+",
  "Percentage of Population Aged 65+ with Cardiovascular Meds"
)

scattermaker(
  df,
  "ln_avg_Standardized_Income",
  "perc_change_Pop_65_up_Meds",
  "Log of Average Standardized Income",
  "Percentage of Population Aged 65+ with Cardiovascular Meds"
)

scattermaker(
  df,
  "perc_change_deaths_CVD",
  "perc_change_Pop_65_up_Meds",
  "Y-o-Y % Change in Deaths attributed to Cardiovascular Disease",
  "Percentage of Population Aged 65+ with Cardiovascular Meds"
)

scattermaker(
  df,
  "ln_perc_Pop_migration_background",
  "perc_change_Pop_65_up_Meds",
  "Log of % of Population with Migration Background",
  "Percentage of Population Aged 65+ with Cardiovascular Meds"
)

scattermaker(
  df,
  "perc_Pop_Social_Security_excl_Pensioners",
  "perc_change_Pop_65_up_Meds",
  "% of Population on Social Security Benefits (up to Pension age)",
  "Percentage of Population Aged 65+ with Cardiovascular Meds"
)

scattermaker(
  df,
  "ln_pop_density_km2",
  "perc_change_Pop_65_up_Meds",
  "% of Population on Social Security Benefits (up to Pension age)",
  "Percentage of Population Aged 65+ with Cardiovascular Meds"
)

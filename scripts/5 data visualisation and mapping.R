# Script: Data Visualisations
# Author: Tina
# Date: 29.11.2023

library(dplyr)     # for data tranformations
library(tidyr)     # for tidy data
library(ggplot2)   # for datavis
library(sf)        # for shape files ops and geographical representation
library(scales)

#### Map Data Operations ####
# loading the data
load("data/data_imports/shapefile_data.RData")
load("data/data_processed/data.RData")

map_data <- shapefile_data %>%
  left_join(df, by = c("Regions", "year"))

######### -- Function Defs -- ##################
# 'mapmaker()' function to create maps for different indicators
mapmaker <-
  function(data,
           indicator_variable,
           title_base,
           color_limits = NULL) {
    years <- 2014:2018
    
    # Determine color scale by looking at the range of values across the years
    if (is.null(color_limits)) {
      color_limits <- range(data[[indicator_variable]], na.rm = TRUE)
    }
    # making temp df filtered for each year
    for (year in years) {
      temp_map_data <- data %>%
        filter(year == !!year)
      
      #making sure the polygons are read as a sf object
      if (!inherits(temp_map_data, "sf")) {
        temp_map_data <- st_as_sf(temp_map_data)
      }
      
      # Dynamic variable mapping
      fill_aes <- aes_string(fill = indicator_variable)
      
      # Custom color palette
      custom_palette <- c("#c2f9b6",
                           "#868686",
                           "#5f0a64")
      
      # Plotting the map
      p <- ggplot(temp_map_data) +
        geom_sf(fill_aes, color = NA) +
        scale_fill_gradientn(
          colors = custom_palette, 
          limits = color_limits, 
          oob = scales::squish
          ) +
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

#################################
# -- Map Maker Calls -- #
mapmaker(
  map_data,
  "perc_change_Pop_65_to_75_Meds",
  "Year on Year Change in Share of Population 65-75 on CV Medication"
)

mapmaker(
  map_data,
  "perc_change_GP_within_5km",
  "Year on Year Change of GP practises within 5km by Municipality"
)
mapmaker(map_data,
         "avg_Distance_GP",
         "Average Distance to GP within Municipalities in KM")


###### Scatter Plots
# Scatter plot for main effect variable meds
ggplot(df, aes(x = avg_Distance_GP, y = perc_Pop_65_to_75_Meds)) +
  geom_point() +
  ggtitle("Scatter plot of avg_Distance_GP vs. perc_Pop_65_to_75_Meds") +
  xlab("Average Distance to GP") +
  ylab("Percentage of Population Aged 65 - 75 on Meds") +
  theme_minimal()

# Scatter plot for main effect variable meds - now natural logged
ggplot(df, aes(x = ln_avg_Distance_GP, y = perc_Pop_65_to_75_Meds)) +
  geom_point() +
  ggtitle("Scatter plot of LN_avg_Distance_GP vs. perc_Pop_65_to_75_Meds") +
  xlab("Average Distance to GP") +
  ylab("Percentage of Population Aged 65 - 75 on Meds") +
  theme_minimal()

# perc_Pop_aged_65_up
ggplot(df, aes(x = perc_Pop_aged_65_up, y = perc_Pop_65_to_75_Meds)) +
  geom_point() +
  ggtitle("Scatter plot of perc_Pop_aged_65_up vs. perc_Pop_65_to_75_Meds") +
  xlab("% of Population aged 65+") +
  ylab("Percentage of Population Aged 65 - 75 on Meds") +
  theme_minimal()

#ln avg income
ggplot(df,
       aes(x = ln_avg_Standardized_Income, y = perc_Pop_65_to_75_Meds)) +
  geom_point() +
  ggtitle("Scatter plot of ln_avg_Standardized_Income vs. perc_Pop_65_to_75_Meds") +
  xlab("Log of Average Standardized Income") +
  ylab("Percentage of Population Aged 65 - 75 on Meds") +
  theme_minimal()

# perc_Pop_migration_background
ggplot(df,
       aes(x = perc_Pop_migration_background, y = perc_Pop_65_to_75_Meds)) +
  geom_point() +
  ggtitle("Scatter plot of perc_Pop_migration_background vs. perc_Pop_65_to_75_Meds") +
  xlab("% of Population with migration background") +
  ylab("Percentage of Population Aged 65 - 75 on Meds") +
  theme_minimal()

# perc_Pop_Social_Security_excl_Pensioners
ggplot(df,
       aes(x = perc_Pop_Social_Security_excl_Pensioners, y = perc_Pop_65_to_75_Meds)) +
  geom_point() +
  ggtitle(
    "Scatter plot of perc_Pop_Social_Security_excl_Pensioners vs. perc_Pop_65_to_75_Meds"
  ) +
  xlab("% of Population on Social Security Benefits (up to Pension age)") +
  ylab("Percentage of Population Aged 65 - 75 on Meds") +
  theme_minimal()

# predominantly_Rural
ggplot(df, aes(x = predominantly_Rural, y = perc_Pop_65_to_75_Meds)) +
  geom_point() +
  ggtitle("Scatter plot of predominantly_Rural (binary) vs. perc_Pop_65_to_75_Meds") +
  xlab("Log of Average Standardized Income") +
  ylab("Percentage of Population Aged 65 - 75 on Meds") +
  theme_minimal()

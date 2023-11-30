# Script: Data Visualisations
# Author: Tina
# Date: 30.11.2023

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
# -- mapmaker -- #
mapmaker <-
  function(data,
           indicator_variable,
           title_base,
           color_palette,
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
      
      # Plotting the map
      p <- ggplot(temp_map_data) +
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
# -- Map Maker Calls -- #
mapmaker(
  data = map_data,
  indicator_variable = "perc_change_Pop_65_to_75_Meds",
  title_base = "Year on Year % Change in Share of Population 65-75 on CV Medication",
  color_palette = palette_gradient,
)

mapmaker(
  data = map_data,
  indicator_variable = "perc_change_GP_within_5km",
  title_base = "Year on Year Change of GP practises within 5km by Municipality",
  color_palette = palette_gradient,
)

mapmaker(
  data = map_data,
  indicator_variable = "avg_Distance_GP",
  title_base = "Average Distance to GP within Municipalities in KM",
  color_palette = palette_gradient,
)

# -- Scatter Maker Calls -- #
scattermaker(
  df,
  "perc_change_GP_within_5km",
  "perc_change_Pop_65_to_75_Meds",
  "Y-o-Y % Change of number of GPs within 5km of Municipality",
  "Percentage of Population Aged 65 - 75 with Cardiovascular Meds"
)

scattermaker(
  df,
  "perc_Pop_aged_65_up",
  "perc_change_Pop_65_to_75_Meds",
  "% of Population aged 65+",
  "Percentage of Population Aged 65 - 75 with Cardiovascular Meds"
)

scattermaker(
  df,
  "ln_avg_Standardized_Income",
  "perc_change_Pop_65_to_75_Meds",
  "Log of Average Standardized Income",
  "Percentage of Population Aged 65 - 75 with Cardiovascular Meds"
)

scattermaker(
  df,
  "predominantly_Rural",
  "perc_change_Pop_65_to_75_Meds",
  "Predominantly Rural (binary)",
  "Percentage of Population Aged 65 - 75 with Cardiovascular Meds"
)

scattermaker(
  df,
  "perc_change_deaths_CVD",
  "perc_change_Pop_65_to_75_Meds",
  "Y-o-Y % Change in Deaths attributed to Cardiovascular Disease",
  "Percentage of Population Aged 65 - 75 with Cardiovascular Meds"
)

scattermaker(
  df,
  "ln_perc_Pop_migration_background",
  "perc_change_Pop_65_to_75_Meds",
  "Log of % of Population with Migration Background",
  "Percentage of Population Aged 65 - 75 with Cardiovascular Meds"
)

scattermaker(
  df,
  "perc_Pop_Social_Security_excl_Pensioners",
  "perc_change_Pop_65_to_75_Meds",
  "% of Population on Social Security Benefits (up to Pension age)",
  "Percentage of Population Aged 65 - 75 with Cardiovascular Meds"
)

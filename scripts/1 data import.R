# Script: 1 Data Import
# Author: Tina
# Date: 23.01.2024

library(dplyr)     # for data tranformations
library(tidyr)     # for tidy data
library(cbsodataR) # for CBS data imports
library(sf)        # for shapefiles operations and geographical representation
library(purrr)

#### Data 83251 - Prescription Medication ####
## CBS Import
# import metadata
dataimport_meds_meta <- cbs_get_meta("83251NED")
# import filtered data
dataimport_meds <- cbs_get_data(
  id = "83251NED",
  Geslacht = c ("T001038"),
  # combined totals
  Leeftijd = c("53925  ", "21600  ", "T001249"),
  # pop 65-75, 75+ and totals
  GeneesmiddelengroepATC = "ATC1079",
  # cardiovascular meds group
  Perioden = c(
    "2013JJ00",
    "2014JJ00",
    "2015JJ00",
    "2016JJ00",
    "2017JJ00",
    "2018JJ00",
    "2019JJ00"
  ),
  RegioS = has_substring("GM"),
  select = c(
    "Geslacht",
    "Leeftijd",
    "GeneesmiddelengroepATC",
    "Perioden",
    "RegioS",
    "PersonenMetGeneesmiddelenRelatief_2"
  ),
  base_url = "http://opendata.cbs.nl"
)

## Translation and matching Keys and Titles to replace values for readability
# Creating temporary tables to hold the Titles to replace the Keys
tmp_meds_Periods <- dataimport_meds_meta$Perioden
tmp_meds_RegioS <- dataimport_meds_meta$RegioS
tmp_meds_ATC <- dataimport_meds_meta$GeneesmiddelengroepATC
tmp_meds_Sex <- dataimport_meds_meta$Geslacht
tmp_meds_Age <- dataimport_meds_meta$Leeftijd

# Matching and replacing Keys for Titles
dataimport_meds$Perioden <-
  tmp_meds_Periods$Title[match(dataimport_meds$Perioden, tmp_meds_Periods$Key)]
dataimport_meds$RegioS <-
  tmp_meds_RegioS$Title[match(dataimport_meds$RegioS, tmp_meds_RegioS$Key)]
dataimport_meds$GeneesmiddelengroepATC <-
  tmp_meds_ATC$Title[match(dataimport_meds$GeneesmiddelengroepATC, tmp_meds_ATC$Key)]
dataimport_meds$Geslacht <-
  tmp_meds_Sex$Title[match(dataimport_meds$Geslacht, tmp_meds_Sex$Key)]
dataimport_meds$Leeftijd <-
  tmp_meds_Age$Title[match(dataimport_meds$Leeftijd, tmp_meds_Age$Key)]

# Renaming columns
dataimport_meds <- dataimport_meds %>%
  rename(
    "Age_Group_with_meds" = "Leeftijd",
    "ATC_Medication_Group" = "GeneesmiddelengroepATC",
    "year" = "Perioden",
    "municipality" = "RegioS",
    "perc_Pop_with_meds" = "PersonenMetGeneesmiddelenRelatief_2",
  )

#### Data 80305 - Proximity to GP ####
## CBS Import
# import metadata
dataimport_GP_meta <- cbs_get_meta("80305ENG")
# import filtered data
dataimport_GP <- cbs_get_data(
  id = "80305ENG",
  Periods = c(
    "2013JJ00",
    "2014JJ00",
    "2015JJ00",
    "2016JJ00",
    "2017JJ00",
    "2018JJ00",
    "2019JJ00"
  ),
  Regions = has_substring("GM"),
  select = c("Periods",
             "Regions",
             "DistanceToGPPractice_1",
             "Within5Km_4"),
  base_url = "http://opendata.cbs.nl"
)

## Translation and matching Keys and Titles to replace values for readability
# temp tables
tmp_GP_Periods <- dataimport_GP_meta$Periods
tmp_GP_Regions <- dataimport_GP_meta$Regions

# Matching and replacing Keys for Titles
dataimport_GP$Periods <-
  tmp_GP_Periods$Title[match(dataimport_GP$Periods, tmp_GP_Periods$Key)]
dataimport_GP$municipality <-
  tmp_GP_Regions$Title[match(dataimport_GP$Regions, tmp_GP_Regions$Key)]

# Renaming columns
dataimport_GP <- dataimport_GP %>%
  rename(
    "year" = "Periods",
    "avg_Distance_GP" = "DistanceToGPPractice_1",
    "GP_within_5km" = "Within5Km_4"
  )

#### Data 70072 - Regional Statistics ####
## CBS import
# import metadata
dataimport_region_meta <- cbs_get_meta("70072NED")
# import filtered data
dataimport_region <- cbs_get_data(
  id = "70072ned",
  Perioden = c(
    "2013JJ00",
    "2014JJ00",
    "2015JJ00",
    "2016JJ00",
    "2017JJ00",
    "2018JJ00",
    "2019JJ00"
  ),
  RegioS = has_substring("GM"),
  select = c(
    "Perioden",
    "RegioS",
    "TotaleBevolking_1",
    "Mannen_2", 
    "Vrouwen_3",
    "k_65Tot80Jaar_20",
    "k_80JaarOfOuder_21",
    "TotaalMetMigratieachtergrond_44",
    "Bevolkingsdichtheid_57", # pop density
    "Overledenen_60",
    # number of deceased
    "ZiektenVanHartEnVaatstelsel_65",
    # number of deceased by CVD
    "BevolkingsgroeiRelatief_80",
    # relative pop growth
    "ParticuliereHuishoudensExclStudenten_131",
    # avg standardised income (excl students)
    "ParticuliereHuishoudensExclStudenten_141",
    # avg household wealth (excl students)
    "TotDeAOWLeeftijd_152",
    "Naam_290" # province
  ),
  base_url = "http://opendata.cbs.nl"
)

## Translation and matching Keys and Titles to replace values for readability
# temp tables
tmp_region_Periods <- dataimport_region_meta$Perioden
tmp_region_Regions <- dataimport_region_meta$RegioS

# Matching and replacing Keys for Titles
dataimport_region$Perioden <-
  tmp_region_Periods$Title[match(dataimport_region$Perioden, tmp_region_Periods$Key)]
dataimport_region$RegioS <-
  tmp_region_Regions$Title[match(dataimport_region$RegioS, tmp_region_Regions$Key)]

# Renaming columns
dataimport_region <- dataimport_region %>%
  rename(
    "year" = "Perioden",
    "municipality" = "RegioS",
    "total_Pop" = "TotaleBevolking_1",
    "total_Men" = "Mannen_2", 
    "total_Women" ="Vrouwen_3",
    "perc_Pop_aged_65_80" = "k_65Tot80Jaar_20",
    "perc_Pop_aged_80_up" = "k_80JaarOfOuder_21",
    "perc_Pop_migration_background" = "TotaalMetMigratieachtergrond_44",
    "total_deaths" = "Overledenen_60",
    "total_deaths_CVD" = "ZiektenVanHartEnVaatstelsel_65",
    "rel_pop_growth" =  "BevolkingsgroeiRelatief_80",
    "avg_Standardized_Income" = "ParticuliereHuishoudensExclStudenten_131",
    "median_Household_Wealth" = "ParticuliereHuishoudensExclStudenten_141",
    "total_Social_Security_Recipiants_excl_Pensioners" = "TotDeAOWLeeftijd_152",
    "pop_density_km2" = "Bevolkingsdichtheid_57",
    "province" = "Naam_290"
  )

#### Data 60039 - Rurality ####
## CBS import
#import metadata
dataimport_rurality_meta <- cbs_get_meta("60039fvw")
#import data
dataimport_rurality <- cbs_get_data(
  id = "60039fvw",
  StatusCijfer = "A048407 ",
  # Final numbers only
  Perioden = c(
    "2013JJ00",
    "2014JJ00",
    "2015JJ00",
    "2016JJ00",
    "2017JJ00",
    "2018JJ00",
    "2019JJ00"
  ),
  RegioS = has_substring("GM"),
  select = c(
    "StatusCijfer",
    "Perioden",
    "RegioS",
    "k_65Tot75Jaar_4", 
    "k_75Tot85Jaar_5", 
    "k_85JaarOfOuder_6",
    "TotaalStedelijkGebied_19",
    "TotaalLandelijkGebied_23"
  ),
  base_url = "http://opendata.cbs.nl"
)

## Translation and matching Keys and Titles to replace values for readability
# temp tables
tmp_rurality_Periods <- dataimport_rurality_meta$Perioden
tmp_rurality_Regions <- dataimport_rurality_meta$RegioS

# Matching and replacing Keys for Titles
dataimport_rurality$Perioden <-
  tmp_rurality_Periods$Title[match(dataimport_rurality$Perioden, tmp_rurality_Periods$Key)]
dataimport_rurality$RegioS <-
  tmp_rurality_Regions$Title[match(dataimport_rurality$RegioS, tmp_rurality_Regions$Key)]

# Renaming columns
dataimport_rurality <- dataimport_rurality %>%
  rename(
    "year" = "Perioden",
    "municipality" = "RegioS",
    "total_Pop_65_75" = "k_65Tot75Jaar_4",
    "total_Pop_75_85" = "k_75Tot85Jaar_5",
    "total_Pop_85_up" = "k_85JaarOfOuder_6",
    "total_Pop_Urban" = "TotaalStedelijkGebied_19",
    "total_Pop_Rural" = "TotaalLandelijkGebied_23"
  )

#### Data Joining and tidying data, and storing data ####
## Joining the four datasets along municipality and year
df_import <- dataimport_meds %>%
  inner_join(dataimport_GP, by = c("municipality", "year")) %>%
  inner_join(dataimport_region, by = c("municipality", "year")) %>%
  inner_join(dataimport_rurality, by = c("municipality", "year"))

## Create unique id variable (Muncipality_year)
df_import <- df_import %>%
  mutate(unique_id = paste0(municipality, "_", year))

# removing all created temp tables used for values
rm(list = ls(pattern = "^tmp"))
# removing the now unnecessary data import files
rm(list = ls(pattern = "^dataimport"))

### Saving the df_import in the directory
save(df_import, file = "data/data_imports/imported_data.RData")

#### Shapefile import and joining Mapping Data ####
# Function to read and process shapefile year by year
read_and_process_shapefile <- function(year) {
  url <- sprintf("https://service.pdok.nl/cbs/gebiedsindelingen/%s/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&outputFormat=application%%2Fjson%%3B%%20subtype%%3Dgeojson&typeName=gebiedsindelingen:gemeente_gegeneraliseerd", year)
  
  municipalBoundaries <- st_read(url)
  municipalBoundaries <- municipalBoundaries %>%
    select(statcode, geometry) %>%
    mutate(year = year) # Add a year column
  
  return(municipalBoundaries)
}
# Years to process
years <- 2014:2019

# Read and process each shapefile and combine
shapefile_data <- do.call(rbind, lapply(years, read_and_process_shapefile))
shapefile_data <- shapefile_data %>% rename(
  "Regions" = "statcode"
)

# Saving the combined shapefile dataframe
save(shapefile_data, file = "data/data_imports/shapefile_data.RData")

# Script: other data analysis
# Author: Tina
# Date: 30.11.2023

# load packages
library(dplyr)     # for data tranformations
library(tidyr)     # for tidy data

# load data
load("data/data_processed/data.RData")

#### Comparing Shrinking VS non-shrinking T-test ####
# Splitting the dataframe into two groups
# I don't remember why I used only one year?
df_2018 <- df %>% filter(year == "2018")
shrinkage_df = df_2018[df_2018$shrinkage_region == TRUE,]
non_shrinkage_df = df_2018[df_2018$shrinkage_region == FALSE,]

#Shapiro Wilk test for Normality
shapiro.test(shrinkage_df$perc_change_Pop_65_to_75_Meds) # normal
shapiro.test(non_shrinkage_df$perc_change_Pop_65_to_75_Meds) # not normal ugh

# QQ Plot for Normality
qqnorm(shrinkage_df$perc_change_Pop_65_to_75_Meds)

qqline(shrinkage_df$perc_change_Pop_65_to_75_Meds) #wobbly

qqnorm(non_shrinkage_df$perc_change_Pop_65_to_75_Meds)

qqline(non_shrinkage_df$perc_change_Pop_65_to_75_Meds) # wobbly at the ends
# F test for variances
var.test(
  shrinkage_df$perc_change_Pop_65_to_75_Meds,
  non_shrinkage_df$perc_change_Pop_65_to_75_Meds
)
# not significant, but maybe problematic
# t test comparing group means
t.test_result = t.test(
  shrinkage_df$perc_change_Pop_65_to_75_Meds,
  non_shrinkage_df$perc_change_Pop_65_to_75_Meds
)
print(t.test_result)
# also not significant

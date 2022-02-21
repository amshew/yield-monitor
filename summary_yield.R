library(dplyr)

# Summarize the dataset
dat_clean <- read.csv("clean_yield.csv")
fields <- read.csv("data/HarvestID_to_FieldID.csv")
dat_clean <- merge(dat_clean, fields, by.x = "harvestID", by.y = "ID")

# Get observations by Field ID
field_n <- dat_clean %>%
              group_by(FieldID) %>%
              summarise(field_yield_obs = n(),
                        n_crops = n_distinct(CropName),
                        n_years = n_distinct(CropSeason),
                        min_year = min(CropSeason, na.rm = T),
                        max_year = max(CropSeason, na.rm = T),
                        n_seedvar = n_distinct(Seeding_Variety),
                        sd_slope = sd(Slope1, na.rm = T),
                        sd_nccpi2 = sd(nccpi2all, na.rm = T),
                        sd_clay3060 = sd(clay_mean_30_60, na.rm = T))

write.csv(field_n, "output/field_summaries.csv")

crop_yield <- dat_clean %>%
                group_by(CropName, Seeding_Variety) %>%
                summarise(mean_yield = mean(Yield, na.rm =T),
                          min_yield = min(Yield, na.rm = T),
                          max_yield = max(Yield, na.rm = T),
                          sd_yield = sd(Yield, na.rm = T),
                          n_years = n_distinct(CropSeason),
                          min_year = min(CropSeason, na.rm = T),
                          max_year = max(CropSeason, na.rm = T),
                          n_fields = n_distinct(FieldID))

write.csv(crop_yield, "output/crop_yields.csv")

#########################################################
library(fixest)
library(modelsummary)

# log yields
dat_clean$l_yield <- log(dat_clean$Yield)

corn <- dat_clean[dat_clean$CropName == "CORN_WET",]

# Exploratory Regressions
yield_mod <- feols(Yield ~ nccpi2all + Seeding_Variety | CropSeason, 
                  corn,
                  vcov = ~CropSeason)
modelsummary(yield_mod)

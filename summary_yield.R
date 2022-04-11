library(dplyr)
library(ggplot2)
library(ggridges)
library(ggformula)

# Summarize the dataset
dat_clean <- read.csv("clean_yield_fields.csv")
# fields <- read.csv("data/HarvestID_to_FieldID.csv")
# dat_clean <- merge(dat_clean, fields, by.x = "harvestID", by.y = "ID")
# write.csv(dat_clean, "clean_yield_fields.csv")

## Field summaries
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

## Crop variety yield summaries with years
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
#################################################################################

## Summary of field - crop - year yield and inputs
input_var <- dat_clean %>%
  group_by(FieldID, CropName, Seeding_Variety, CropSeason) %>%
  summarise(mean_yield = mean(Yield, na.rm =T),
            min_yield = min(Yield, na.rm = T),
            max_yield = max(Yield, na.rm = T),
            sd_yield = sd(Yield, na.rm = T),
            mean_relative_elevation = mean(Relative_Elevation1, na.rm = T),
            sd_relative_elevation = sd(Relative_Elevation1, na.rm = T),
            mean_slope = mean(Slope1, na.rm = T),
            sd_slope = sd(Slope1, na.rm = T),
            mean_tri = mean(TRI1, na.rm = T),
            sd_tri = sd(TRI1, na.rm = T),
            mean_tpi = mean(TPI1, na.rm = T),
            sd_tpi = sd(TPI1, na.rm = T),
            mean_Seed = mean(SeedingDensity, na.rm=T),
            sd_Seed = sd(SeedingDensity, na.rm=T),
            mean_plant = mean(PlantingDay1, na.rm=T),
            sd_plant = sd(PlantingDay1, na.rm = T),
            mean_harvest = mean(HarvestDay, na.rm=T),
            sd_harvest = sd(HarvestDay, na.rm=T),
            mean_App1 = mean(Application_1_rate, na.rm=T),
            sd_App1 = sd(Application_1_rate, na.rm=T),
            ID_App1 = unique(Application_1_ID),
            date_App1 = mean(Application_1_date, na.rm=T),
            mean_App2 = mean(Application_2_rate, na.rm=T),
            sd_App2 = sd(Application_2_rate, na.rm=T),
            ID_App2 = unique(Application_2_ID),
            date_App2 = mean(Application_2_date, na.rm=T),
            mean_App3 = mean(Application_3_rate, na.rm=T),
            sd_App3 = sd(Application_3_rate, na.rm=T),
            ID_App3 = unique(Application_3_ID),
            date_App3 = mean(Application_3_date, na.rm=T),
            mean_App4 = mean(Application_4_rate, na.rm=T),
            sd_App4 = sd(Application_4_rate, na.rm=T),
            ID_App4 = unique(Application_4_ID),
            date_App4 = mean(Application_4_date, na.rm=T),
            mean_App5 = mean(Application_5_rate, na.rm=T),
            sd_App5 = sd(Application_5_rate, na.rm=T),
            ID_App5 = unique(Application_5_ID),
            date_App5 = mean(Application_5_date, na.rm=T),
            mean_App6 = mean(Application_6_rate, na.rm=T),
            sd_App6 = sd(Application_6_rate, na.rm=T),
            ID_App6 = unique(Application_6_ID),
            date_App6 = mean(Application_6_date, na.rm=T),
            mean_App7 = mean(Application_7_rate, na.rm=T),
            sd_App7 = sd(Application_7_rate, na.rm=T),
            ID_App7 = unique(Application_7_ID),
            date_App7 = mean(Application_7_date, na.rm=T),
            mean_App8 = mean(Application_8_rate, na.rm=T),
            sd_App8 = sd(Application_8_rate, na.rm=T),
            ID_App8 = unique(Application_8_ID),
            date_App8 = mean(Application_8_date, na.rm=T),
            mean_App9 = mean(Application_9_rate, na.rm=T),
            sd_App9 = sd(Application_9_rate, na.rm=T),
            ID_App9 = unique(Application_9_ID),
            date_App9 = mean(Application_9_date, na.rm=T),
            mean_App10 = mean(Application_10_rate, na.rm=T),
            sd_App10 = sd(Application_10_rate, na.rm=T),
            ID_App10 = unique(Application_10_ID),
            date_App10 = mean(Application_10_date, na.rm=T))
            #mean_GDD1 = mean())
            # across(.cols=GDD1:Precipitation12, .fns = mean, .names = "{.col}_{.fn}"))
write.csv(input_var, "output/field-year-crop-inputs.csv")

#########################################################
# Summary Plots

### Application SD Histograms
# App 1 Histogram
ggplot(input_var, aes(sd_App1)) +
  geom_histogram(binwidth = 10) +
  xlim(0,100) +
  ylim(0, 150) +
  ggtitle("SD of Application 1")

# App 2 Histogram
ggplot(input_var, aes(sd_App2)) +
  geom_histogram(binwidth = 10) +
  xlim(0,100) +
  ylim(0, 250)+
  ggtitle("SD of Application 2")


# App 3 Histogram
ggplot(input_var, aes(sd_App3)) +
  geom_histogram(binwidth = 10) +
  xlim(0,100) +
  ylim(0, 250)+
  ggtitle("SD of Application 3")

# App 4 Histogram
ggplot(input_var, aes(sd_App4)) +
  geom_histogram(binwidth = 10) +
  xlim(0,100) +
  ylim(0, 250)+
  ggtitle("SD of Application 4")

# App 5 Histogram
ggplot(input_var, aes(sd_App5)) +
  geom_histogram(binwidth = 10) +
  xlim(0,100) +
  ylim(0, 250)+
  ggtitle("SD of Application 5")

# App 6 Histogram
ggplot(input_var, aes(sd_App6)) +
  geom_histogram(binwidth = 10) +
  xlim(0,100) +
  ylim(0, 250)+
  ggtitle("SD of Application 6")

# App 7 Histogram
ggplot(input_var, aes(sd_App7)) +
  geom_histogram(binwidth = 10) +
  xlim(0,100) +
  ylim(0, 250)+
  ggtitle("SD of Application 7")

# App 8 Histogram
ggplot(input_var, aes(sd_App8)) +
  geom_histogram(binwidth = 10) +
  xlim(0,100) +
  ylim(0, 250)+
  ggtitle("SD of Application 8")

# App 9 Histogram
ggplot(input_var, aes(sd_App9)) +
  geom_histogram(binwidth = 10) +
  xlim(0,100) +
  ylim(0, 250)+
  ggtitle("SD of Application 9")

# App 10 Histogram
ggplot(input_var, aes(sd_App10)) +
  geom_histogram(binwidth = 10) +
  xlim(0,100) +
  ylim(0, 250)+
  ggtitle("SD of Application 10")

### Yields and Apps Plots

# Yield by Planting Date
ggplot(input_var, aes(x=mean_plant, y=mean_yield, color = factor(CropSeason))) +
  geom_point() +
  scale_color_viridis_d(option = "magma") +
  geom_smooth() + 
  ylim(0,300) +
  facet_wrap(~ CropName)+
  ggtitle("Mean Planting DOY by Mean Yield")

# Yield by SD of Application 1
ggplot(input_var, aes(x=sd_App1, y=mean_yield, color = factor(CropSeason))) +
  geom_point() +
  scale_color_viridis_d(option = "magma") +
  geom_smooth() + 
  ylim(0,500) +
  xlim(0,100)+
  facet_wrap(~ CropName)

# SD Yield by SD of Application 1
ggplot(input_var, aes(x=sd_App1, y=sd_yield, color = factor(CropSeason))) +
  geom_point() +
  scale_color_viridis_d(option = "magma") +
  geom_smooth() + 
  ylim(0,500) +
  xlim(0,100)+
  facet_wrap(~ CropName)

# Mean by SD of Application 1
ggplot(input_var, aes(x=sd_App1, y=mean_App1, color = factor(CropSeason))) +
  geom_point() +
  scale_color_viridis_d(option = "magma") +
  geom_smooth() + 
  ylim(0,500) +
  xlim(0,100)+
  facet_wrap(~ CropName)

# Yield by SD of Application 2
ggplot(input_var, aes(x=sd_App2, y=mean_yield, color = factor(CropSeason))) +
  geom_point() +
  scale_color_viridis_d(option = "magma") +
  geom_smooth() + 
  ylim(0,500) +
  xlim(0,100)+
  facet_wrap(~ CropName)

# Yield by SD of Application 3
ggplot(input_var, aes(x=sd_App3, y=mean_yield, color = factor(CropSeason))) +
  geom_point() +
  scale_color_viridis_d(option = "magma") +
  geom_smooth() + 
  ylim(0,500) +
  xlim(0,100)+
  facet_wrap(~ CropName)



### Corn
corn <- dat_clean[dat_clean$CropName == "CORN_WET",]
ggplot(dat_clean) +
  geom_point(aes(x=PlantingDate1, y=Yield, color = CropName)) +
  geom_spline() +
  facet_grid(rows = vars(CropSeason))

#########################################################
### Filter to fields with at least 2 years of data

fields_sum <- read.csv("output/field_summaries.csv")

fields_2 <- fields_sum[fields_sum$n_years > 1,]

cleaned2 <- dat_clean[dat_clean$FieldId %in% fields_2$FieldID,]

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

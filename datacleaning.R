# Clean all yield monitor files

rm(list=ls()) # clear env vars

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  curl,
  sf,
  raster, 
  viridis, cowplot, ggplot2, ggrepel, RColorBrewer,
  ggspatial,rnaturalearth, rnaturalearthdata,
  gstat, fields, interp, mgcv, automap, patchwork, ggmap,
  concaveman, plyr
)

# Get data

# how many files do we expect
# length(list.files("yieldmodelv4/"))

# generate list of all csv files

files_to_read = list.files(
  path = "yieldmodelv4/",        # directory to search within
  pattern = ".*.*csv", # regex pattern, some explanation below
  recursive = TRUE,   # search subdirectories
  full.names = TRUE
)

data_ls = lapply(files_to_read, function(x) {
  tryCatch(read.csv(x), error=function(e) NULL)
}) # read all the matching files

# clean nulls




clean_fields <- function(x){
  dat_sf <- st_as_sf(x, coords = c("x", "y"), crs = 4326, agr = "constant")
  dat_utm <- st_transform(dat_sf, crs = 32614)
  utm_crs <- crs(dat_utm)
  
  # Get bounding polygon containing all points
  bounds <- concaveman(dat_utm, concavity = 2)
  
  # Create buffer of field edges
  buffer <- st_buffer(bounds, dist = -40)
  
  # Clip to buffer
  dat_utm_c <- st_intersection(buffer, dat_utm)
  
  sd.y <- sd(dat_utm_c$Yield)*2.5
  m.y <- mean(dat_utm_c$Yield)
  out.up <- m.y+sd.y
  out.down <- m.y-sd.y
  dat_clean <- dat_utm_c[dat_utm_c$Yield > out.down & 
                           dat_utm_c$Yield < out.up,]
}

clean_ls <- lapply(dat_ls, clean_fields)

dat_df <- bind_rows(clean_ls)

write.csv()
rm(list=ls()) # clear env

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  curl,
  sf,
  raster, 
  viridis, cowplot, ggplot2, ggrepel, 
  ggspatial,rnaturalearth, rnaturalearthdata,
  gstat, fields, interp, mgcv, automap, patchwork, ggmap
)

### Get Data
id <- "1Xy838-GIKSau5VLFsT99gxgP98vvDGOA" # Google file ID
sURL <- sprintf("https://docs.google.com/uc?id=%s&export=download", id) # google drive export/download link
con <- curl(sURL) # open the drive connection to R
dat <- read.csv(con) # read in the csv via the connection

###############################################################################
### Convert to sf and utm
dat_sf <- st_as_sf(dat, coords = c("x", "y"), crs = 4326, agr = "constant")
dat_sf_utm <- st_transform(dat_sf, crs = 32614)
utm_crs <- crs(dat_sf_utm)
utm_coords <- do.call(rbind, st_geometry(dat_sf_utm)) %>% 
  as_tibble() %>% setNames(c("x_u","y_u"))
dat$x_u <- utm_coords$x_u
dat$y_u <- utm_coords$y_u

# Initial plot of raw yields in WGS84
pt_gg <- ggplot() + 
  geom_sf(data = dat_sf_utm, aes(color = Yield)) +
  scale_color_viridis_c(option="magma")
pt_gg

# Generate 10m raster for interpolations
bbox <- c("xmin"=min(dat$x_u), "ymin"=min(dat$y_u),"xmax"=max(dat$x_u), "ymax"=max(dat$y_u))
grd.temp <- expand.grid(
  X = seq(from = bbox["xmin"], to = bbox["xmax"], by = 20),
  Y = seq(from = bbox["ymin"], to = bbox["ymax"], by = 20) # 20 m resolution
)

# Take a look at the gridded points for interpolation
grid_plot <- ggplot() +
  geom_point(data = grd.temp, aes(x = X, y = Y), size = 0.01) +
  geom_point(data = dat,
             mapping = aes(x = x_u, y = y_u, color = Yield), size = 2) +
  scale_color_viridis_c(option="magma")+
  theme_bw()
grid_plot

# Set the raster CRS
crs_raster_format <- utm_crs
grd_template_raster <- grd.temp %>% 
  dplyr::mutate(Z = 0) %>% 
  raster::rasterFromXYZ( 
    crs = crs_raster_format)

################################################
### Fit some models for interpolation
# Neirest Neighbor
fit_NN <- gstat::gstat(
  formula = Yield ~1,
  data = as(dat_sf_utm, "Spatial"),
  nmax = 10, nmin = 3
)

# Inverse Distance Weighting
fit_IDW <- gstat::gstat(
  formula = Yield ~1,
  data = as(dat_sf_utm, "Spatial"),
  nmax = 10, nmin = 3,
  set = list(idp = 0.5)
)

# Thin Plate Spline
fit_TPS <- fields::Tps(
  X = as.matrix(dat[,c('x_u','y_u')]),
  Y = dat$Yield,
  miles = F
)

fit_GAM <- mgcv::gam( # using {mgcv}
  Yield ~ s(x_u, y_u),      # here come our X/Y/Z data - straightforward enough
  data = dat     # specify in which object the data is stored
)

#library(Rcpp) # error with Rcpp and when runs it times out.
# Triangular Irregular Network
# fit_TIN <- interp::interp(
#   x = dat$x_u,
#   y = dat$y_u,
#   z = dat$Yield,
#   xo = grd.temp$X,     # here we already define the target grid
#   yo = grd.temp$Y,
#   output = "points"
# ) %>% bind_cols()

# Automated Kriging
fit_KRIG <- automap::autoKrige(      # using {automap}
  formula = Yield ~ 1,                 # The interface is similar to {gstat} but
  input_data = as(dat_sf_utm, "Spatial") # {automap} makes a lot of assumptions for you
) %>% 
  .$krige_output %>%  # the function returns a complex object with lot's of metainfo
  as.data.frame() %>% # we keep only the data we are interested in
  dplyr::select(X = x1, Y = x2, Z = var1.pred) 

########################################################################
### Interpolate and Plot

# Nearest Neighbor
interp_NN <- interpolate(grd_template_raster, fit_NN)

# Inverse Distance Weighting
interp_IDW <- interpolate(grd_template_raster, fit_IDW)

# Thin Plate Spline Regression
interp_TPS <- interpolate(grd_template_raster, fit_TPS)

# TIN Interpolation
interp_TIN <- raster::rasterFromXYZ(fit_TIN, crs = crs_raster_format)

# Krigging Interpolation
interp_KRIG <- raster::rasterFromXYZ(fit_KRIG, crs = crs_raster_format)
plot(interp_KRIG)

########################################################################
### Generalized Additive Model
interp_GAM <- grd.temp %>% 
  mutate(Z = predict(fit_GAM, .)) %>% 
  rasterFromXYZ(crs = crs_raster_format)

########################################################################
### Visualize Outputs
plot_my_rasters <- function(raster_object, raster_name){
  
  df <- rasterToPoints(raster_object) %>% as_tibble()
  colnames(df) <- c("X", "Y", "Z")
  
  ggplot(df, aes(x = X, y = Y, fill = Z)) +
    geom_raster() +
    ggtitle(label = raster_name) +
    scale_fill_viridis(option = "C") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}

rasterlist <- list(
  "Nearest Neighbor" = interp_NN, 
  "Inverse Distance Weighted" = interp_IDW, 
  "Kriging" = interp_KRIG, 
  "Thin Plate Spline Regression" = interp_TPS,
  "Triangular Irregular Surface" = interp_TIN, 
  "Generalized Additive Model" = interp_GAM
)

plotlist <- map2(
  rasterlist,
  names(rasterlist),
  plot_my_rasters
)

# Note that the next trick only works because of library(patchwork)
(plotlist[[1]] + plotlist[[2]]) /
  (plotlist[[3]] + plotlist[[4]]) /
  (plotlist[[5]] + plotlist[[6]])

##########################################################################
### Cleaning Yield Data

# Drop zeros
dat_cl <- dat[dat$Yield==0,]

# remove outliers above or below 3 standard deviations from mean
dat_cl <- 
  dat_cl[Yield > (mean(Yield) - 3*sd(Yield)) & 
           Yield < (mean(Yield) + 3*sd(Yield))]
# proportion removed
length(dat_cl) / length(Yield)

### Questions: 

# Is there any information on yield monitor calibration?

# Grab soil type and elevation changes across the field

# Nathans Hendricks Thoughts:
# optimal crop rotation - estimate how yields are affected by the previous crop
# 1300 fields - polygons to grab historical rotations from CDL
# sufficient for cross-sectional analysis

# Demonstrate the impact of corn hybrid choice on yield 
# Single farmer can tell if their seed choice is best but exploit cross-farm yields
# Demonstrate yield stabiliity

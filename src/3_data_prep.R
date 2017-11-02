# This script is to produce exploratory figures of the WUI-land ownership- fire type
# These will be used in the 2018 Smith Fellowship
# Key layers are the Protected Areas Database, Short ignitions, Radeloff WUI product, MTBS data

# Libraries ---------------------------------------------------------------
library(sf)
library(lubridate)
library(tidyverse)
library(gridExtra)

source("src/functions/st_par.R")
ncores <- detectCores()/2

# Set directories and projections -----------------------------------------

#EPSG:2163 USA_Contiguous_Albers_Equal_Area_Conic
proj_ea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#EPSG:102005 USA_Contiguous_Equidistant_Conic
proj_ed <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0+y_0=0 +datum=NAD83 +units=m +no_defs"

# Clean the USA States layer ---------------------------------------------
usa <- st_read(dsn = file.path(prefix, "bounds/state"),
               layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  st_transform("+init=epsg:2163") %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
  st_simplify(., preserveTopology = TRUE) 

# Dissolve to the USA Boundary
conus <- usa %>%
  st_union()

# Clean the FPA database class
fpa_fire <- st_read(dsn = file.path(fpa_prefix, "Data", "FPA_FOD_20170508.gdb"),
                    layer = "Fires", quiet= FALSE) %>%
  filter(!(STATE %in% c("Alaska", "Hawaii", "Puerto Rico") & FIRE_SIZE >= 0.1)) %>%
  dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME,
                FIRE_YEAR, DISCOVERY_DATE, DISCOVERY_DOY, STAT_CAUSE_DESCR, FIRE_SIZE, STATE) %>%
  mutate(IGNITION = ifelse(STAT_CAUSE_DESCR == "Lightning", "Lightning", "Human"),
         FIRE_SIZE_m2 = FIRE_SIZE*4046.86,
         FIRE_SIZE_km2 = FIRE_SIZE_m2/1000000,
         FIRE_SIZE_ha = FIRE_SIZE_m2*10000,
         DISCOVERY_DAY = day(DISCOVERY_DATE),
         DISCOVERY_MONTH = month(DISCOVERY_DATE),
         DISCOVERY_YEAR = FIRE_YEAR)  %>%
  st_make_valid()
fpa_fire <- st_transform(fpa_fire, "+init=epsg:2163")

st_write(shrt_fire, "../data/fire/Short_9215/gpkg/short_clean_conus.gpkg",
         driver = "GPKG",
         update=TRUE)

# Clean the PAD Land Ownership layer -------------------------------------

landowner <- st_read(dsn = "../data/bounds/public_private_lands/shp/",
                     layer = "PADUS1_4Combined") %>%
  filter(!(d_State_Nm %in% c("United States Virgin Islands", "Hawaii", "Puerto Rico",
                             "Alaska", "Marshall Islands", "U.S. Minor Outlying Islands", 
                             "Palau", "Not Applicable", "Marshall Islands", "Guam",
                             "Mariana Islands", "Federated States of Micronesia", "American Samoa"))) %>%
  st_transform("+init=epsg:2163") %>%
  mutate(OwnArea_m2 = as.numeric(st_area(geometry)),
         OwnArea_km2 = OwnArea_m2/1000000) %>%
  st_make_valid(.) %>% st_cast("MULTIPOLYGON")

st_write(landowner, "../data/bounds/public_private_lands/gpkg/pad_conus.gpkg", 
         driver = "GPKG",
         update=TRUE)

# Import the Wildland-Urban Interface and process
wui <- st_read(dsn = file.path(wui_prefix, "us_wui_2010.gdb"),
               layer = "us_wui_2010") %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 0.001) %>%
  mutate(Class = classify_wui(WUICLASS10)) %>%
  filter(Class %in% c("Urban" ,"WUI", "VLD", "Wildlands")) %>%
  st_transform("+init=epsg:2163") %>%
  st_make_valid()

st_write(wui, file.path(prefix, "anthro/gpkg/wui_us.gpkg"), 
         driver = "GPKG",
         update=TRUE)

# Intersect FPA with WUI
fpa_wui <- fpa_fire %>%
  st_intersection(., wui) %>%
  st_make_valid()

if (!file.exists(file.path(fpa_out, "fpa_wui_conus.gpkg"))) {
  st_write(fpa_wui, file.path(fpa_out, "fpa_wui_conus.gpkg"),
           driver = "GPKG",
           update=TRUE)}

# Intersect FPA data with PAD-US -------------------------------------

fire_landowner <- 
  st_par(fpa_wui, st_join, 
         n_cores = ncores, 
         y = landowner,
         join = st_intersects)

st_write(fire_landowner, file.path(prefix, "bounds/public_private_lands/gpkg/shrt_pad_us.gpkg"),
         driver = "GPKG")
#


# Libraries ---------------------------------------------------------------
library(sf)
library(lubridate)
library(tidyverse)
library(gridExtra)
library(ggthemes)

# Set directories and projections -----------------------------------------

#EPSG:102003 USA_Contiguous_Albers_Equal_Area_Conic
proj_ea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 
+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#EPSG:102005 USA_Contiguous_Equidistant_Conic
proj_ed <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 
+y_0=0 +datum=NAD83 +units=m +no_defs"

# Clean the USA States layer ---------------------------------------------
usa <- st_read(dsn = file.path(prefix, "bounds/state"),
               layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  st_transform("+init=epsg:2163") %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
  st_simplify(., preserveTopology = TRUE) 

# Dissolve to the USA Boundary
conus <- usa %>%
  st_union()

# Import the Level 3 Ecoregions ---------------------------------------------
ecoreg <- st_read(dsn = file.path(prefix, "/bounds/us_eco_l3"),
                  layer = "us_eco_l3", quiet= TRUE) %>%
  st_transform(., proj_ea) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) 
# Import the FPA-FOD  ---------------------------------------------

fpa_wui <- st_read(
  dsn = file.path(fpa_out, "fpa_wui_conus.gpkg")) %>%
  st_transform(crs = "+init=epsg:2163")

# Import the PAD-US  ---------------------------------------------

landowner <- st_read(
  dsn = file.path(prefix, "bounds", "public_private_lands", "pad_us", "pad_conus.gpkg"), 
  quiet= FALSE) %>%
  st_transform(crs = "+init=epsg:2163")
landowner <- st_make_valid(landowner)
# Import the WUI  ---------------------------------------------

wui <- st_read(
  dsn = file.path(prefix, "anthro", "wui_conus.gpkg"),
  quiet= FALSE) %>%
  st_transform(crs = "+init=epsg:2163")

# Intersect FPA data with PAD-US -------------------------------------

fire_landowner <- 
  st_join(fpa_wui, landowner,
         join = st_intersects)
names(fire_landowner) %<>% tolower
names(landowner) %<>% tolower

fwrite(as.data.frame(fire_landowner), "../data/csv/fire_landowner.csv", sep = ",")
fwrite(as.data.frame(fpa_wui), "../data/csv/fpa_wui.csv", sep = ",")

# Create exploratory figs -------------------------------------------------

# What are the totals of ignition type across the land owner types
area_norm_own <- as.data.frame(landowner) %>%
  group_by(d_own_type) %>%
  summarise(landarea = sum(ownarea_km2))

shrt_cause_own <- as.data.frame(fire_landowner) %>%
  group_by(ignition, d_own_type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  na.omit() %>%
  left_join(., area_norm_own, by = "d_own_type") %>%
  mutate(cnt_norm = (count/landarea)*100)

shrtcause_own <- shrt_cause_own %>%
  ggplot() + 
  geom_bar(aes(x =  reorder(d_own_type, -cnt_norm), y = cnt_norm, fill = ignition), stat = "identity", position = "dodge") +
  theme_pub()  + 
  xlab("") + ylab("Wildfire ignition count") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.position = "right")

ggsave(file = "results/Cause_Landowner.jpeg", shrtcause, width = 10, height = 4, dpi=1200, scale = 3, units = "cm")


# What are the totals of ignition name across the land management names
area_norm_name <- as.data.frame(landowner) %>%
  group_by(d_own_name) %>%
  summarise(landarea = sum(ownarea_km2))

shrt_cause_name <- as.data.frame(fire_landowner) %>%
  group_by(ignition, d_own_name) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  na.omit() %>%
  left_join(., area_norm_name, by = "d_own_name") %>%
  mutate(cnt_norm = (count/landarea)*100)

shrtcause_name <- shrt_cause_name %>%
  ggplot() + 
  geom_bar(aes(x =  reorder(d_own_name, -cnt_norm), y = cnt_norm, fill = ignition), stat = "identity", position = "dodge") +
  theme_pub()  + 
  xlab("") + ylab("Wildfire ignition count") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right")

ggsave(file = "results/Cause_MngType.jpeg", shrtcause, width = 10, height = 6, dpi=1200, scale = 3, units = "cm")

# What is the temporal 
shrt_ff_yr <- as.data.frame(fire_landowner) %>%
  group_by(ignition, discovery_year, d_own_type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  na.omit() %>%
  left_join(., area_norm_own, by = "d_own_type") %>%
  mutate(cnt_norm = (count/landarea)*100)

shrtyr <- shrt_ff_yr %>%
  ggplot() + 
  geom_point(aes(x = discovery_year, y = cnt_norm, 
                 color = d_own_type), stat = "identity", alpha = 0.5) +
  geom_smooth(aes(x = discovery_year, y = cnt_norm, color = d_own_type), 
              se = FALSE, method="glm", 
              method.args = list(family = "poisson"), size = 0.75) +
  theme_pub()  + 
  xlab("Year") + ylab("% ignition per year normalized by land area") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right") +
  facet_wrap(~ignition)

ggsave(file = "results/cause_landowner_temporal.jpeg", shrtyr, width = 10, height = 6, dpi=1200, scale = 3, units = "cm")

shrt_doy <- as.data.frame(fire_landowner) %>%
  group_by(discovery_doy, ignition, class) %>%
  summarise(count = n())

shrtdoy <- shrt_doy %>%
  transform(class = factor(class, levels=c("Urban", "WUI", "VLD", "Wildlands"))) %>%
  ggplot() + 
  geom_bar(aes(x =  discovery_doy, y = count, 
               color = ignition, fill = ignition), stat = "identity") +
  theme_pub()  + 
  xlab("Discovery day of year") + ylab("Fire frequency") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") +
  facet_wrap(~ class)

ggsave("results/class_freq_doy.jpeg", shrtyr, width = 5, height = 5, dpi=600, scale = 3, units = "cm") #saves g

shrt_freq_yr <- as.data.frame(fire_landowner) %>%
  group_by(ignition, class, discovery_year) %>%
  summarise(count = n())

shrtyr <- shrt_freq_yr %>%
  transform(class = factor(class, levels=c("Urban", "WUI", "VLD", "Wildlands"))) %>%
  ggplot() + 
  geom_point(aes(x = discovery_year, y = count, 
                 color = ignition), stat = "identity", alpha = 0.5) +
  geom_smooth(aes(x = discovery_year, y = count, color = ignition), 
              method="glm", method.args = list(family = "poisson"), size = 0.75) +
  theme_pub()  + 
  xlab("Year") + ylab("Fire frequency") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") +
  facet_wrap(~ class)

ggsave("results/class_freq_yr.jpeg", shrtyr, width = 5, height = 5, dpi=600, scale = 3, units = "cm") #saves g


shrt_doycause <- as.data.frame(fire_landowner) %>%
  transform(class = factor(class, levels=c("Urban", "WUI", "VLD", "Wildlands"))) %>%
  group_by(discovery_doy, class, stat_cause_descr) %>%
  summarise(count = n())

shrtdoycause <- shrt_doycause %>%
  ggplot() + 
  geom_bar(aes(x =  discovery_doy, y = count, 
               color = class, fill = class), stat = "identity") +
  theme_pub()  + 
  xlab("Discovery day of year") + ylab("Wildfire ignition count") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right") + 
  facet_wrap(~ stat_cause_descr, ncol = 3, scales = "free")

ggsave("results/doy_statcause_class_ff.jpeg", shrtdoycause, width = 8, height = 7, dpi=300, scale = 3, units = "cm") #saves g

shrt_areaburn_yr <- as.data.frame(fire_landowner) %>%
  transform(class = factor(class, levels=c("Urban", "WUI", "VLD", "Wildlands"))) %>%
  group_by(ignition, class, discovery_year) %>%
  summarise(count = sum(fire_size_km2))

shrtyr <- shrt_areaburn_yr %>%
  ggplot() + 
  geom_point(aes(x = discovery_year, y = count, 
                 color = ignition), stat = "identity", alpha = 0.5) +
  geom_smooth(aes(x = discovery_year, y = count, color = ignition), 
              method="glm", method.args = list(family = "poisson"), size = 0.75) +
  theme_pub()  + 
  xlab("Year") + ylab("Area burn (km2)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") +
  facet_wrap(~ class, scales = "free")

ggsave("results/class_areaburn_yr.jpeg", shrtyr, width = 8, height = 7, dpi=600, scale = 3, units = "cm") #saves g


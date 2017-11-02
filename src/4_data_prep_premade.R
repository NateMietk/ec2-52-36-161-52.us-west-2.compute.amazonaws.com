
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
eco = "../data/bounds/us_eco_l3"
ecoreg <- st_read(dsn = eco, layer = "us_eco_l3", quiet= TRUE) %>%
  st_transform(., proj_ea) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) 

# Import the FPA-FOD  ---------------------------------------------

fpa_wui <- st_read(dsn = file.path(fpa_out, "fpa_wui_conus.gpkg"))

# Import the PAD-US  ---------------------------------------------

landowner <- st_read(
  dsn = file.path(prefix, "bounds", "public_private_lands", "pad_us", "pad_conus.gpkg"), 
  quiet= FALSE)

# Import the WUI  ---------------------------------------------

wui <- st_read(
  dsn = file.path(prefix, "anthro", "wui_conus.gpkg"),
  quiet= FALSE) 

# Intersect FPA data with PAD-US -------------------------------------

fire_landowner <- st_par(
  fpa_wui, st_join, n_cores = 4, y = landowner, join = st_intersects)

 # Create exploratory figs -------------------------------------------------

# What are the totals of ignition type across the land owner types
shrt_cause <- fire_landowner %>%
  group_by(IGNITION, d_Own_Type) %>%
  summarise(count = n()) %>%
  ungroup()

shrtcause <- shrt_cause %>%
  ggplot() + 
  geom_bar(aes(x =  reorder(d_Own_Type, -count), y = count, fill = IGNITION), stat = "identity", position = "dodge") +
  theme_pub()  + 
  xlab("") + ylab("Wildfire ignition count") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right")
ggsave(file = "../Smith-Fellow/figs/Cause_Landowner.pdf", shrtcause, width = 6, height = 4, dpi=1200, scale = 3, units = "cm")


# What are the totals of ignition type across the land management types
shrt_cause <- fire_landowner %>%
  group_by(IGNITION, d_Own_Name) %>%
  summarise(count = n()) %>%
  ungroup()

shrtcause <- shrt_cause %>%
  ggplot() + 
  geom_bar(aes(x =  reorder(d_Own_Name, -count), y = count, fill = IGNITION), stat = "identity", position = "dodge") +
  theme_pub()  + 
  xlab("") + ylab("Wildfire ignition count") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right")
ggsave(file = "../Smith-Fellow/figs/Cause_MngType.pdf", shrtcause, width = 10, height = 4, dpi=1200, scale = 3, units = "cm")

# What is the temporal 
shrt_areaburn_yr <- fire_landowner %>%
  group_by(IGNITION, FIRE_YEAR, d_Own_Type) %>%
  summarise(count = sum(FIRE_SIZE_ha))

shrtyr <- shrt_areaburn_yr %>%
  ggplot() + 
  geom_point(aes(x = FIRE_YEAR, y = count, color = d_Own_Type), stat = "identity") +
  geom_smooth(aes(x = FIRE_YEAR, y = count, color = d_Own_Type), 
              method="glm", method.args = list(family = "poisson")) +
  theme_pub()  + 
  xlab("Year") + ylab("") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right") +
  facet_wrap(~IGNITION)

shrt_doy <- shrt_fire %>%
  group_by(DISCOVERY_DOY, IGNITION) %>%
  summarise(count = n())

shrtdoy <- shrt_doy %>%
  ggplot() + 
  geom_bar(aes(x =  DISCOVERY_DOY, y = count, color = IGNITION, fill = IGNITION), stat = "identity") +
  theme_pub()  + 
  xlab("Discovery day of year") + ylab("") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none")

shrt_freq_yr <- shrt_fire %>%
  group_by(IGNITION, FIRE_YEAR) %>%
  summarise(count = n())

shrtyr <- shrt_freq_yr %>%
  ggplot() + 
  geom_point(aes(x = FIRE_YEAR, y = count, color = IGNITION), stat = "identity") +
  geom_smooth(aes(x = FIRE_YEAR, y = count, color = IGNITION), 
              method="glm", method.args = list(family = "poisson")) +
  theme_pub()  + 
  xlab("Year") + ylab("") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none")

grid.arrange(shrtcause, shrtdoy, shrtyr, ncol =3)
g <- arrangeGrob(shrtcause, shrtdoy, shrtyr, ncol = 3)
ggsave("/Users/NateM/Google Drive/Proposal/Smith/figs/sum.png", g, width = 8, height = 5, dpi=300, scale = 3, units = "cm") #saves g


shrt_doycause <- shrt_fire %>%
  group_by(DISCOVERY_DOY, STAT_CAUSE_DESCR) %>%
  summarise(count = n())

shrtdoycause <- shrt_doycause %>%
  ggplot() + 
  geom_bar(aes(x =  DISCOVERY_DOY, y = count), stat = "identity") +
  theme_pub()  + 
  xlab("Discovery day of year") + ylab("Wildfire ignition count") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") + 
  facet_wrap(~ STAT_CAUSE_DESCR, ncol = 3, scales = "free")

ggsave("/Users/NateM/Google Drive/Proposal/Smith/figs/cause.png", shrtdoycause, width = 8, height = 7, dpi=300, scale = 3, units = "cm") #saves g

p1 <- shrt_fire %>% 
  ggplot(aes(x=LONGITUDE, y=LATITUDE)) +
  geom_point(color = "red", size = 0.45, stroke = 0) +
  theme_nothing(legend = TRUE) +
  facet_wrap(~IGNITION, ncol =1)
ggsave("/Users/NateM/Google Drive/Proposal/Smith/figs/sp_cause.png", p1, width = 4.5, height = 7, dpi=300, scale = 3, units = "cm") #saves g

ggplot() +
  geom_sf(data = ny_shp) +
  geom_sf(data = shrt_fire)


p2 <- ggplot() +
  geom_point(data = shrt_fire, aes(x=LONGITUDE, y=LATITUDE), 
             color = "red", size = 0.45, stroke = 0) +
  geom_sf(data = ny_shp, color='black', fill = "gray99") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~STAT_CAUSE_DESCR, ncol =3)

ggsave("/Users/NateM/Dropbox/Professional/Proposals/Smith/figs/sp_cause_facet.png", p2, width = 6, height = 7, dpi=300, scale = 3, units = "cm") #saves g

shrt_areaburn_yr <- shrt_fire %>%
  group_by(IGNITION, FIRE_YEAR) %>%
  summarise(count = sum(FIRE_SIZE_ha))

shrtyr <- shrt_areaburn_yr %>%
  ggplot() + 
  geom_point(aes(x = FIRE_YEAR, y = count, color = IGNITION), stat = "identity") +
  geom_smooth(aes(x = FIRE_YEAR, y = count, color = IGNITION), 
              method="glm", method.args = list(family = "poisson")) +
  theme_pub()  + 
  xlab("Year") + ylab("") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none")




#
#
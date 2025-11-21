
# ----------------------- #
# Load packages ----
# ----------------------- #
# If it's your first time, here are 3 ways to download packages

# install.packages("pkgname")

# install.packages("remotes")
# remotes::install_github("junkka/histmaps")

# install.packages("devtools")
# devtools::install_github("yutannihilation/ggsflabel")
# devtools::install_github("jessecambon/tidygeocoder")
# devtools::install_github("borstell/swemapdata")



library(tidyverse)      # Makes R nice and easy. Always use it. See cheatsheet. 
library(sf)             # Makes Rstudio a good GIS
library(ggthemes)
library(showtext)        # plotting fonts
library(sysfonts)        # selecting fonts 
library(ggrepel) 
library(ggsflabel)
library(readxl)
library(histmaps)        # geodata - Swedish administrative geometry by Johan Junkka of Umeå University. 
library(tidygeocoder)    # geodata - automatic geocoding based on OSM
library(swemapdata)      # geodata - Swedish adm. data
#library(swemaps2)        # geodata - Swedish city data
#library(leaflet)         # geodata
#library(ggspatial)       # geodata
#library(ggspatial)       # etc. etc.
#library(ggpattern)
#library(gridExtra)
# library(maps)
# library(viridis)
# library(fuzzyjoin)
# etc...




# ---------------------- #
# Set up  ---- 
# ---------------------- #

# Set your working directory
setwd("/Users/ann-ida/Library/CloudStorage/OneDrive-UniversityofGothenburg/ASG/phd/presentations/Techup_2025-11-21")

# Prep font for later use
# https://fonts.google.com/
sysfonts::font_add_google("EB Garamond", "garamond")  # or something else
showtext::showtext_auto()



# Load a couple of basic sf objects

# Swedish county boundaries from Swemapdata
# + Low resolution, data from SCB. - Not dated
sweden <- swemapdata::lan 

# Swedish historical county boundaries from Histmaps
# + low resolution, data from National Arhcive, dated
sweden1860 <- histmaps::get_boundaries(1860, "county") 


# Choose the right map for the right time
swe1860 <- ggplot() +
  geom_sf(data = sweden1860)

swe2000s <- ggplot() +
  geom_sf(data = sweden)

grid.arrange(swe1860, swe2000s, ncol = 2)





# ---------------------------------- #
# Geocode with historical data ---- 
# ---------------------------------- #
# Usecase: Visualize big cities in Sweden in 1860.
# Data source: Ortshistoria.se (it has population so you can try out plotting frequencies)
# How: Geocode points with tidygeocoder and build a map


# Load data over all Swedish cities 1880-1960.
cities18601960 <- read_excel("DATA_cit18601960.xlsx")


# Prep df for geocoding
cities1860 <- cities18601960 %>% 
  filter(year == "1860") %>%   # Filter to keep only 1860
  mutate(country = "Sweden")   # Add a country-column and fill it with the value "Sweden"


# Add geometry to the city data by geocoding 
cities1860_latlong <- cities1860 %>% 
  tidygeocoder::geocode(
    city = city,            # The city variable is the city 
    country = country,      # The country variable is the country 
    method = "osm",         # Method is Open Street Map 
    lat = latitude,         # Save latitude in variable "latitude"
    long = longitude)       # Save longitude in variable "longitude"


# Set coords as point
# 4326 is WGS84, 3006 is SWEREF99
cities1860_geompoint <- cities1860_latlong %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% # correct for long/lat
  st_transform(crs = 3006) # transform to match Sweden


# Plot: Plot cities on the map of Sweden
ggplot() +
  geom_sf(data = sweden1860) +
  geom_sf(data = cities1860_geompoint)


# Plot: Put labels on the cities
ggplot() +
  geom_sf(data = sweden1860) +
  geom_sf(data = cities1860_geompoint) +
  geom_sf_text(data = cities1860_geompoint, aes(label = city)) 
# i prefer geom_sf_text over geom_sf_label, but that's just aesthethics
# try exchanging 'text' for 'label'



# Plot: Select the biggest cities, and mark them in red
ggplot() +
  geom_sf(data = sweden1860) +
  geom_sf(data = cities1860_geompoint, size = 3) +
  geom_sf_text(         
    data = filter(cities1860_geompoint, city_pop > 5000), 
    aes(label = city), size = 4,
    nudge_y = 0.2,      # Nudge the labels slightly from their points
    color = "red") +    # Only cities with a population over 5000 in 1860 gets a label
  geom_sf(
    data = filter(cities1860_geompoint, city_pop > 5000), 
    color = "red", size = 4) + # Add red dots for the +5k cities
  theme_minimal()       # or try with theme_map



### Plot of Big Cities 1860 ----
# Better visibility, more reasonable geographic focus

sweden1860 %>%  ggplot() + # Instead of specifying "data =" we use the pipe operator here
  geom_sf(fill = "grey80", color = "grey90", size = 0.2) + 
  geom_sf(data = cities1860_geompoint, size = 2, color = "grey60", alpha = 0.5) +
  geom_sf_text_repel(          # adding 'repel'
    data = filter(cities1860_geompoint, city_pop > 5000), # only cities with a population over 5000 in 1860 gets a label
    aes(label = city),
    max.overlaps = Inf,         # we need this with repel
    family = "garamond",        # add font
    size = 4.5) +                 # size
  geom_sf(data = filter(cities1860_geompoint, city_pop > 5000), color = "black", size = 3) +  # add red dots for the +5k cities
  coord_sf(xlim = c(200000, 800000), ylim = c(6100000, 6750000), expand = F) + # Zoom in on the south of Sweden (use st_bbox or ask ChatGPT)
  labs(caption = "Figure 1. Cities With a Population over 5000 in 1860.") +
  theme_map() +               
  theme(
    text = element_text(family = "garamond"), # Add a font
    plot.caption = element_text(size = 16, hjust = 0.5, vjust = 1))

# If you have certain dimensions in mind, you can save with set dimensions
#ggsave("PLOT_cities.png", width = 2, height = 2.5)







# ---------------------------------- #
# Spatial operations ---- 
# ---------------------------------- #


### Unite geometries ----
sweden_union <- swemapdata::lan %>% 
  st_union() 

sweden_union %>%
  ggplot() +
  geom_sf()





### Measure distance ----
# Usecase: You want a distance variable in your dataset
counties1860 <- get_boundaries(1860, "county")

# Get centroid of Stockholm county
sthlm_centroid <- counties1860 %>%
  filter(name == "Stockholms överståthållarskap") %>%
  st_centroid()
# The warning message tells us that we're assuming that centroid has the same attributes as the polygon

# Distance from each county centroid to Stockholm centroid
countydist <- counties1860 %>%
  mutate(
    centroid = st_centroid(geometry), # create centroid variable for each county
    dist_to_sthlm = as.numeric(st_distance(centroid, sthlm_centroid))) # create a numeric distance variable

# Plot
countydist %>% 
  ggplot() +
  geom_sf(aes(fill = dist_to_sthlm), show.legend = F) +
  scale_fill_viridis_c(          # this is the color scale that I used for the secularization maps!
    option = "mako", 
    direction = 1) + # -1 is default
  theme_map()




### Spatial join 1: Points with polygons ----
# Usecase: You want X geometry with Y attributes

cities_within_county <- st_join(
  cities1860_geompoint, counties1860, join = st_within) %>% 
  select(year,city, city_pop,county=name) %>% 
  arrange(county, city)

# Try to find out why some cities ended up NA! Look at the map for a hint:

counties1860 %>% 
  ggplot() +
  #geom_sf(aes(fill = name), show.legend = F) +
  geom_sf(fill = "darkblue", show.legend = F) +
  theme_map()

# Check what your data is referencing!



### Spatial join 2a: Polygons with polygons ----
# Usecase: You want X geometry with Y attributes, many-to-many

counties1910 <- get_boundaries(1910, "county")
parishes1910 <- get_boundaries(1910, "parish")


parish_intersect_county <- st_join(
  parishes1910, counties1910, join = st_intersects)

# Many parishes intersect with several counties.




### Spatial join 2b: Polygons with polygons ----
# Usecase: You want X geometry with Y attributes, but one-to-one

parish_intersect_county_l <- st_join(
  parishes1910, counties1910, join = st_intersects, largest = TRUE)

# The largest intersection of each parish into a county.


# Let's compare the two join-by-intersection methods
intersect1 <- parish_intersect_county %>% 
  ggplot() +
  geom_sf(aes(fill = name.y), linewidth = 0.08, show.legend = F) +
  labs(title = "st_intersects") + 
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5))

intersect2 <- parish_intersect_county_l %>% 
  ggplot() +
  geom_sf(aes(fill = name.y), linewidth = 0.08, show.legend = F) +
  labs(title = "st_intersects, largest") + 
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5))

truemap <- counties1910 %>% 
  ggplot() +
  geom_sf(aes(fill = name), linewidth = 0.08, show.legend = F) +
  labs(title = "real counties") + 
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(intersect1, truemap, intersect2, ncol=3)




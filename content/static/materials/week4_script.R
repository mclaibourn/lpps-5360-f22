# Data Viz in R
# 2021-11-01
# Practicing with R


# ..................................................
# Load libraries and data ----
# Libraries

library(tidyverse)
library(patchwork)
library(ggridges) # for ridge plot
library(viridis)

# For spatial stuff
library(sf) # the primary spatial package for today
library(rnaturalearth) # quick access to a world map (also states)
library(rnaturalearthdata) # and some data points about countries
library(tigris) # to call Census boundary files

options(tigris_use_cache = TRUE)
theme_set(theme_bw()) # globally change the ggplot theme

# Albemarle County property data
property <- readRDS("data/property_cleaned.RDS")

# Data from rnaturalearth/rnaturalearthdata
world <- ne_countries(scale = "medium", returnclass = "sf")


# ..................................................
# Annotating Figures  ----

# dumbbell plot
# recreate needed long data frame for dumbbell plot: 
#   tract, type (assessed/sale), median value
tract_sell_assess <- property %>% 
  filter(last_sale_year == "2020",
         lastsaleprice > 0) %>% 
  group_by(fips_tract) %>% 
  summarize(median_assessed = median(totalvalue),
            median_sale = median(lastsaleprice)) %>% 
  mutate(diff = median_sale - median_assessed,
         diff10 = ifelse(diff > 10000, "diff", "nodiff")) %>%
  pivot_longer(-c(fips_tract, diff, diff10), names_to = "value_type", values_to = "value",
               names_prefix = "median_")

# last week's version
ggplot(tract_sell_assess, aes(x= value, y= fct_reorder(fips_tract, value))) +
  geom_line(aes(group = fips_tract), color = "grey")+
  geom_point(aes(color=value_type, size = fct_rev(diff10))) +
  scale_size_discrete(range = c(2,3)) +
  scale_x_continuous(breaks = seq(2e+05, 8e+05, 1e+05),
                     labels = paste0(seq(200,800,100), "K")) +
  guides(color = guide_legend("Value"), size = "none") +
  labs(x = "", y = "",
       title = "Median Assessed Value vs. Median Sale Value",
       subtitle = "By Census Tract, 2020 Sales and Assessments") +
  theme_minimal() +
  theme(legend.position = "bottom")

# With annotations instead
ggplot(tract_sell_assess, aes(x= value, y= fct_reorder(fips_tract, value))) +
  geom_line(aes(group = fips_tract), color = "grey")+
  geom_point(aes(color=value_type, size = fct_rev(diff10))) +
  scale_size_discrete(range = c(2,3)) +
  scale_x_continuous(breaks = seq(2e+05, 8e+05, 1e+05),
                     labels = paste0(seq(200,800,100), "K")) +
  guides(color = "none", size = "none") +
  labs(x = "", y = "",
       title = "Median Assessed Value vs. Median Sale Value",
       subtitle = "By Census Tract, 2020 Sales and Assessments") +
  annotate("text", x = 5.5e+05, y = 13, label = "$16,600 gap", 
           hjust = 0, size = 3) +
  annotate("segment", x = 5.5e+05, xend = 5e+05, y = 13, yend = 14.75, 
           color = "grey50", 
           arrow = arrow(angle = 15, length = unit(0.5, "lines"))) +
  annotate("text", x = c(1.1e+05, 3.6e+05), y = 4, label = c("Assessed", "Sale"), 
           hjust = 0, size = 3) +
  annotate("segment", x = c(1.9e+05, 3.55e+05), xend = c(2.45e+05, 3.1e+05), y = 4, yend = 4, 
           color = "grey50", arrow = arrow(angle =  15,  length = unit(0.5, "lines"))) +
  theme_minimal() 


# ..................................................
# Mapping the World ----
## MAPPING WITH GGPLOT/SF ----

# just the map
ggplot(data = world) +
  geom_sf()

# We can add line and fill colors
ggplot(data = world) +
  geom_sf(fill = "#669438", color = "#32481B") +
  theme_void()


## PROJECTIONS ----
# Default is the CRS of the data
st_crs(world) # st_crs is from sf

# Robinson
ggplot(data = world) + 
  geom_sf(fill = "#669438", color = "#32481B") +
  coord_sf(crs = "+proj=robin")

# Sinusoidal
ggplot(data = world) + 
  geom_sf(fill = "#669438", color = "#32481B") +
  coord_sf(crs = st_crs("ESRI:54008")) # or use the CRS code

# Mollweide
ggplot(data = world) + 
  geom_sf(fill = "#669438", color = "#32481B") +
  coord_sf(crs = "+proj=moll")

# Goode homolosine
ggplot(data = world) + 
  geom_sf(fill = "#669438", color = "#32481B") +
  coord_sf(crs = "+proj=igh")


## EXTENT ----
ggplot(data = world) + 
  geom_sf(fill = "#669438", color = "#32481B") +
  coord_sf(xlim = c(-172.54, -47.74), 
           ylim = c(23.81, 86.46))
# lat and lon boundaries given the North America WGS84 CRS
# I had to look these up -- unless you work with GIS data
# regularly, you'd probably need to look these up, too


## COLORS ----
# Color the countries to distinguish adjacent countries 
ggplot(data = world) + 
  geom_sf(aes(fill = as.factor(mapcolor7))) +
  scale_fill_viridis_d(option = "plasma") +
  guides(fill = "none") 
# The Natural Earth dataset comes with columns to assign
# a coloring scheme with 7–13 colors (e.g., mapcolor7) 
# so that no countries with a shared border share a color. 

# Color the countries by an attribute: Choropleths!
ggplot(data = world) + 
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma") +
  theme(legend.position = "bottom")

ggplot(data = world) + 
  geom_sf(aes(fill = income_grp)) +
  scale_fill_viridis_d(option = "viridis", name = "") 


# ..................................................
# Mapping the US ----
## STATES ----
# Pull states from US CENSUS
states <- states(cb = TRUE) # cb = TRUE generates a smaller/more approximate shapefile
st_crs(states) # NAD83

not48 <- c("02", "15", "60", "66", "69", "72", "78")

states48 <- states %>% 
  filter(!(GEOID %in% not48))

ggplot(states48) +
  geom_sf() +
  theme_void()

ggplot(states48) +
  geom_sf() +
  coord_sf(crs = st_crs("EPSG:4326")) +
  theme_void()

ggplot(states48) +
  geom_sf() +
  coord_sf(crs = st_crs("EPSG:5070")) +
  theme_void()


## VIRGINIA ----
va <- states48 %>% 
  filter(GEOID == "51")

ggplot(va) +
  geom_sf() +
  theme_void()

# get VA counties
va_counties <- counties(state = 51, cb = TRUE) # tigris again

ggplot() +
  geom_sf(data = va, color = "#EC8E55", size = 3) +
  geom_sf(data = va_counties, fill = "#669438", color = "white") +
  theme_void()

## ALBEMARLE COUNTY ----
albco_tracts <- tracts(state = 51, county = 003, cb = TRUE) # still tigris

ggplot(data = albco_tracts) +
  geom_sf() +
  theme_void()

## JOINS ----
# Let's summarize house values by tract and join to our spatial data
property_tracts <- property %>% 
  group_by(fips_tract) %>% 
  summarize(median_value = median(totalvalue, na.rm = TRUE),
            median_land_value = median(landvalue, na.rm = TRUE),
            median_imp_value = median(improvementsvalue, na.rm = TRUE))

albco_prop_tracts <- albco_tracts %>% 
  left_join(property_tracts, by = c("GEOID" = "fips_tract"))

ggplot(data = albco_prop_tracts) +
  geom_sf(aes(fill = median_value)) +
  theme_void()

# Let's add tract labels at the polygon centroids
tract_points <- st_centroid(albco_tracts)

# the centroids are in the geometry list; we need them as
# latitude and longitude columns for plotting
tract_points <- tract_points %>% 
  mutate(lat = st_coordinates(.)[,1],
         lon = st_coordinates(.)[,2])

value_map <- ggplot(data = albco_prop_tracts) +
  geom_sf(aes(fill = median_value)) +
  geom_text(data = tract_points, 
            aes(x = lat, y = lon, label = NAME), 
            color = "white", size = 3,
            check_overlap = TRUE) +
  scale_fill_viridis_c(option = "viridis", 
                       name = "Median Value") +
  theme_void()
value_map 


# ..................................................
# Patchwork again ----
# Make a ridge plot to pair with the map
# ridge plots
value_ridge <- property %>% 
  group_by(tract) %>% 
  mutate(medianvalue = median(totalvalue, na.rm = TRUE)) %>% 
  filter(totalvalue < 1e+07,
         !is.na(tract)) %>% 
  
  ggplot(aes(x = totalvalue, y = fct_reorder(tract, medianvalue),
             fill = medianvalue)) +
  geom_density_ridges() +
  coord_cartesian(xlim = c(100,1000000)) +
  scale_fill_viridis_c(option = "viridis") +
  guides(fill = "none") +
  labs(x = "Assessed Value",
       y = "Census Tract",
       title = "Home Values by Census Tract") +
  theme_classic() +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 11)) 
value_ridge + 
  annotate("text", x = 750000, y = 1.5, label = "Look Here")

# And patch them together 
value_ridge + value_map


# ..................................................
# Bonus: Mapping with Leaflet ----
# For interactive online maps
# https://rstudio.github.io/leaflet/
library(leaflet)

# 1. Basic map using Albemarle County Census Tracts

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = albco_prop_tracts,
              fillColor = "orange",
              fillOpacity = 0.8,
              weight = 2, 
              opacity = 1,
              color = "white")

albco_prop_tracts <- st_transform(albco_prop_tracts, 4326)

# 2. Map the fill color to median assesed value
# create a palette function

pal <- colorNumeric("viridis", reverse = TRUE, domain = albco_prop_tracts$median_value) # viridis

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = albco_prop_tracts,
              fillColor = ~pal(median_value),
              fillOpacity = 0.8,
              weight = 2, 
              opacity = 1,
              color = "white")

# 3. Add a legend

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = albco_prop_tracts,
              fillColor = ~pal(median_value),
              fillOpacity = 0.8,
              weight = 2, 
              opacity = 1,
              color = "white") %>% 
  addLegend("bottomright", pal = pal, values = albco_prop_tracts$median_value, 
            title = "Median Assessment", opacity = 0.7)

# 4. Add some hovering functionality

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = albco_prop_tracts,
              fillColor = ~pal(median_value),
              fillOpacity = 0.86,
              weight = 1, 
              opacity = 1,
              color = "white",
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T)) %>% 
  addLegend("bottomright", pal = pal, values = albco_prop_tracts$median_value, 
            title = "Median Assessment", opacity = 0.7)

# 5. Add some popup information

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = albco_prop_tracts,
              fillColor = ~pal(median_value),
              fillOpacity = 0.86,
              weight = 1, 
              opacity = 1,
              color = "white",
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("Tract Number: ", albco_prop_tracts$NAME, "<br>",
                             "Median Value: ", albco_prop_tracts$median_value, 2)) %>% 
  addLegend("bottomright", pal = pal, values = albco_prop_tracts$median_value, 
            title = "Median Assessment", opacity = 0.7)

# 6. Get rid of the NA?
# Only by getting rid of polygon altogether

albco_prop_tracts_nouva <- albco_prop_tracts %>% 
  filter(TRACTCE != "010903")

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = albco_prop_tracts_nouva,
              fillColor = ~pal(median_value),
              fillOpacity = 0.86,
              weight = 1, 
              opacity = 1,
              color = "white",
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("Tract Number: ", albco_prop_tracts_nouva$NAME, "<br>",
                             "Median Value: ", albco_prop_tracts_nouva$median_value, 2)) %>% 
  addLegend("bottomright", pal = pal, values = albco_prop_tracts_nouva$median_value, 
            title = "Median Assessment", opacity = 0.7)




###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_
##Migration chronology for Gamebirds in British Columbia
## ##
## Objectives
## ## 
## This code does the following:
## 0) Download e-bird 3*3 km data for species of interest 
## i)
## ii) 
## iii) 
##
## Updated and annotated by Jenny Munoz
## Last updated: February 2026
###_###_####_###_###_###_###_###_###_###_###_###_###_###_###_###_###_

# documentation AND video Check this : https://ebird.github.io/ebirdst/articles/applications.html
# ================================
# 0) SETUP & ACCESS KEY
# ================================

# ---- Install required libraries ----
# Data Manipulation
install.packages("tidyverse")
install.packages("janitor")
install.packages("glue")   # String Manipulation
install.packages("fs")     # File Operations
install.packages("png")    # Image Handling

# Data Visualization
install.packages("viridis")
install.packages("scales")
install.packages("fields")
install.packages("readr")  # Data Input/Output

# Geospatial Data
install.packages("rnaturalearth")
install.packages("sf")
install.packages("raster")
install.packages("ebirdst")
install.packages("rmapshaper")
install.packages("terra")

# ---- Load libraries ----
library(dplyr)        # Data manipulation
library(janitor)      # Data cleaning
library(glue)         # String interpolation
library(fs)           # File operations
library(png)          # Read/write PNG images
library(viridis)      # Color scales
library(scales)       # Graphical scales
library(fields)       # Spatial tools
library(readr)        # Read CSV, rectangular data
library(rnaturalearth)# Map data
library(sf)           # Simple features for geospatial data
library(raster)       # Raster data analysis
library(ebirdst)      # Access eBird Status and Trends data
library(rmapshaper)   # Simplify shapes
library(terra)        # Raster/vector spatial analysis
library(ggplot2)      # Plots

# ---- eBird S&T access key ----
# An access key is required to download eBird Status & Trends data.
# 1) Request a key hereor check you active key here : https://ebird.org/st/request
# 2) Save the key for this session with set_ebirdst_access_key().
#    NOTE: Do NOT hard-code real keys in scripts stored in repos. Prefer ~/.Renviron.
#    usethis::edit_r_environ(); add a line like: EBIRDST_KEY="your-key"
#    Then call set_ebirdst_access_key(Sys.getenv("EBIRDST_KEY")).

set_ebirdst_access_key("f6me7thr51ul")  # <- 
# Where am I running this from (useful for path debugging)?
getwd()
# ebirdst package version (useful for reproducibility)
ebirdst_version()

# ================================
# 1) CHOOSING THE DATA DIRECTORY FOR EBIRD DATA 
# ================================
# ebirdst_data_dir() resolves the download directory using:
# 1) EBIRDST_DATA_DIR env var if set, otherwise
# 2) tools::R_user_dir("ebirdst", which = "data")
ebirdst_data_dir()

# If you want to override the default for THIS SESSION ONLY:
# (Pick a fast local SSD or a managed project folder.)

Sys.setenv(EBIRDST_DATA_DIR ="C:/Users/jmunoz/Documents/BirdsCanada/1_jv_science_coordinator_role/1_projects/10_migration_chronologies/data")
ebirdst::ebirdst_data_dir()


# ================================
# 1) DOWNLOADING DATA FOR THE SPECIES OF INTEREST 
# ================================

species_list<-read.csv("data/list/request_species_list.csv")





# ================================
# 1) READING TEH POLYGONS ADN FILTERING THEM
# ================================
# check the class to decide how to work with them 
 class( fraser_river<- sf::st_read("data/conservation_polygon/Fraser_Skagit_Valley/SnowGooseSurveyArea.shp"))
# tehre are sf and datframes so sf package is fine probably even dplyr 

#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# River Deltas ( 2 River deltas)
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# for the river deltas I needed to filter by lenght because the otehr attributes were not different between the two 

fraser_river<- sf::st_read("data/conservation_polygon/Fraser_Skagit_Valley/SnowGooseSurveyArea.shp")  %>%
  st_transform(8857) %>% 
  filter(Shape_Leng<"300000")

skagit_river<- sf::st_read("data/conservation_polygon/Fraser_Skagit_Valley/SnowGooseSurveyArea.shp")  %>%
  st_transform(8857) %>% 
  filter(Shape_Leng>"400000")

print(names(fraser_river))

# Rain check for a couple of them 
# st_geometry_type(skagit_river) # should be a polygoon no multipolygon
# nrow(skagit_river)# should have one layer
# plot(skagit_river)

#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# BC Harvest zones (2 zones) 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

harvest_zone1<- sf::st_read("data/conservation_polygon/Harvest_Survey_Zones/Harvest_Survey_Zones_2017.shp") %>%
  st_transform(8857) %>% 
  filter(Zonename=="British Columbia - Zone 1") %>% 
  st_make_valid() # Fixes invalid geometries for example when islands are disconected they facilitate operations later on 

harvest_zone2<- sf::st_read("data/conservation_polygon/Harvest_Survey_Zones/Harvest_Survey_Zones_2017.shp") %>%
  st_transform(8857) %>% 
  filter(Zonename=="British Columbia - Zone 2")%>%
  st_make_valid()

# Rain check for a couple of them 
st_geometry_type(harvest_zone1) # should be a polygoon no multipolygon
# nrow(skagit_river)# should have one layer
 plot(harvest_zone1)


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# BC Harvest districts ( 8 districts)
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

hunting_district_van_island<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% # transforms to WGS84
  filter(REG_R_NAME =="Vancouver Island") %>% 
  st_union() %>% #dissolves the polygons into one 
  st_as_sf # make sure the object is a dataframe with spatial geometry

print(names(hunting_districts))

hunting_district_lower_mainland<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>%  # transforms to WGS84
  filter(REG_R_NAME =="Lower Mainland") %>% 
  st_union() %>% # dissolves the polygons into one 
  st_as_sf # make sure the object is a dataframe with spatial geometry
plot(hunting_district_lower_mainland)

hunting_district_thompson<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Thompson")%>% 
  st_union() %>% 
  st_as_sf

hunting_district_kootenay<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Kootenay")%>% 
  st_union() %>% 
  st_as_sf

hunting_district_cariboo<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Cariboo")%>% 
  st_union() %>% 
  st_as_sf

hunting_district_skeena<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Skeena")%>% 
  st_union() %>% 
  st_as_sf

hunting_district_omineca<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Omineca")%>% 
  st_union() %>% 
  st_as_sf

hunting_district_okanagan<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Okanagan")%>% 
  st_union() %>% 
  st_as_sf


# Rain check for a couple of them 
st_geometry_type(hunting_district_van_island) # should be a polygoon no multipolygon
nrow(hunting_district_van_island)# should have one layer
plot(hunting_district_van_island)


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For one species at the time using per
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
#Please note that although we are running afunction to calcultae chronologies for all species
# Values are not comparable between them as tehy are relative abundances







# ================================
# 0) SETUP
#region_boundary <-st_read("data/conservation_polygon/BC/BC_boundary_layer.shp")

harvest_zone1<- sf::st_read("data/conservation_polygon/Harvest_Survey_Zones/Harvest_Survey_Zones_2017.shp") %>%
  st_transform(8857) %>% 
  filter(Zonename=="British Columbia - Zone 1") %>% 
  st_make_valid() # Fixes invalid geometries for example when islands are disconected they facilitate operations later on 


#ebirdst_download_status( "American Wigeon",pattern = "abundance_(median|lower|upper)_3km", download_occurrence = TRUE,dry_run = FALSE,force = TRUE)

chronologies_abundance <- NULL

for (species in species_list) {

# load the median weekly relative abundance and lower/upper confidence limits
abd_median <- load_raster(species, product = "abundance", metric = "median")
abd_lower <- load_raster(species, product = "abundance", metric = "lower")
abd_upper <- load_raster(species, product = "abundance", metric = "upper")

# project region boundary to match raster data
#region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))

# extract values within region and calculate the mean
abd_median_region <- extract(abd_median, harvest_zone1,
                             fun = "mean", na.rm = TRUE, ID = FALSE)
abd_lower_region <- extract(abd_lower, harvest_zone1,
                            fun = "mean", na.rm = TRUE, ID = FALSE)
abd_upper_region <- extract(abd_upper, harvest_zone1,
                            fun = "mean", na.rm = TRUE, ID = FALSE)

# transform to data frame format with rows corresponding to weeks
chronology <- data.frame(species=species, 
                         week = as.Date(names(abd_median)),
                         median = as.numeric(abd_median_region),
                         lower = as.numeric(abd_lower_region),
                         upper = as.numeric(abd_upper_region))

# combine with other species

chronologies_abundance <- bind_rows(chronologies_abundance, chronology)

}

# Plot but want to do one for esach specie separately 

# ggplot(chronology) +
#   aes(x = week, y = median) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#   geom_line() +
#   scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#   labs(x = "Week", 
#        y = "Mean relative abundance in Bc",
#        title = "Migration chronology for American Wigeon in Bc")


species_list_plot <- unique(chronologies_abundance$species)

for (sp in species_list_plot) {
  
  p <- ggplot(chronologies %>% filter(species == sp)) +
    aes(x = week, y = median, color = species, fill = species) +
    geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
    geom_line(linewidth = 1) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Mean relative abundance ",
      title = paste("Migration chronology Harvest Zone1-", sp),
      color = NULL, fill = NULL
    ) +
    theme(legend.position = "none")
  
  ggsave(
    filename = paste0("chronologyAbundance_HarvestZone1_2023", gsub(" ", "_", sp), ".png"),
    plot = p,
    width = 8,
    height = 5
  )
  
}
# Save each plot automatically
# 
# Add this inside the loop:
  
  ggsave(
    filename = paste0("chronology_", gsub(" ", "_", sp), ".png"),
    plot = p,
    width = 8,
    height = 5
  )


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportionof population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

# directory for ebird 

Sys.setenv(EBIRDST_DATA_DIR ="C:/Users/jmunoz/Documents/BirdsCanada/1_jv_science_coordinator_role/1_projects/10_migration_chronologies/data/migration_chronologies/data/species_rasters")
ebirdst::ebirdst_data_dir()

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")

# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

species_list <- c("American Coot","American Wigeon","Barrow's Goldeneye")

#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# Harvest zone 1
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

harvest_zone1<- sf::st_read("data/conservation_polygon/Harvest_Survey_Zones/Harvest_Survey_Zones_2017.shp") %>%
  st_transform(8857) %>% 
  filter(Zonename=="British Columbia - Zone 1") %>% 
  st_make_valid() # Fixes invalid geometries for example when islands are disconected they facilitate operations later on 


chronologies <- NULL
for (species in species_list) {
  # download weekly 27km relative abundance, median and confidence limits
  # ebirdst_download_status(species,
  #                         pattern = "abundance_(median|upper|lower)_3km")
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species)
  abd_lower <- load_raster(species, metric = "lower")
  abd_upper <- load_raster(species, metric = "upper")
  
  # total relative abundance across the entire modeled range of the species
  abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
  
  # total abundance within the region of interest
  abd_median_region <- extract(abd_median, harvest_zone1,
                               fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, harvest_zone1,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, harvest_zone1,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # proportion of population within the region of interest
  prop_pop_median <- as.numeric(abd_median_region) / abd_total
  prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
  prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
  
  # transform to data frame format with rows corresponding to weeks
  chronology <- data.frame(species = species,
                           week = as.Date(names(abd_median)),
                           median = prop_pop_median,
                           lower = prop_pop_lower,
                           upper = pmin(prop_pop_upper, 1))
  
  # combine with other species
  chronologies <- bind_rows(chronologies, chronology)
}

#Finally, we can use this data frame to generate migration chronologies for these species.

ggplot(chronologies) +
  aes(x = week, y = median, color = species, fill = species) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
  geom_line(linewidth = 1) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = NULL,
       y = "Percent of population in Harvest zone 1",
       title = "Migration chronologies for Game bird species of interest in BC-Harvest zone 1",
       color = NULL, fill = NULL) +
  theme(legend.position = "bottom")


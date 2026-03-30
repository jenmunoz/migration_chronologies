
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
# -----0) SETUP & ACCESS KEY-----
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
install.packages("scico")

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
extract<-terra::extract
library(scico)
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

Sys.setenv(EBIRDST_DATA_DIR="C:/Users/jmunoz/Local_BirdsCanada/1_JV_science_coordinator_role_local/1_Projects/10_migration_chronologies/migration_chronologies/data/species_rasters")
ebirdst::ebirdst_data_dir()


# _#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##_
# 1) --DOWNLOADING DATA FOR THE SPECIES OF INTEREST ---------
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
species_list<-read.csv("data/list/request_species_list.csv")

#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# 2)---READING TEH POLYGONS ADN FILTERING THEM------
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# check the class to decide how to work with them 
# class( fraser_river<- sf::st_read("data/conservation_polygon/Fraser_Skagit_Valley/SnowGooseSurveyArea.shp"))
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

hunt_district_van_island<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% # transforms to WGS84
  filter(REG_R_NAME =="Vancouver Island") %>% 
  st_union() %>% #dissolves the polygons into one 
  st_as_sf # make sure the object is a dataframe with spatial geometry

print(names(hunt_districts))

hunt_district_lower_mainland<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>%  # transforms to WGS84
  filter(REG_R_NAME =="Lower Mainland") %>% 
  st_union() %>% # dissolves the polygons into one 
  st_as_sf # make sure the object is a dataframe with spatial geometry
plot(hunting_district_lower_mainland)

hunt_district_thompson<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Thompson")%>% 
  st_union() %>% 
  st_as_sf

hunt_district_kootenay<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Kootenay")%>% 
  st_union() %>% 
  st_as_sf

hunt_district_cariboo<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Cariboo")%>% 
  st_union() %>% 
  st_as_sf

hunt_district_skeena<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Skeena")%>% 
  st_union() %>% 
  st_as_sf

hunt_district_omineca<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Omineca")%>% 
  st_union() %>% 
  st_as_sf

hunt_district_okanagan<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Okanagan")%>% 
  st_union() %>% 
  st_as_sf


# Rain check for a couple of them 
st_geometry_type(hunting_district_van_island) # should be a polygoon no multipolygon
nrow(hunting_district_van_island)# should have one layer
plot(hunting_district_van_island)


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For each zone 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
#Please note that although we are running afunction to calcultae chronologies for all species
# Values are not comparable between them as they are relative abundances

#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# Harvest Zone 1 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

# #_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# 0) SETUP
#region_boundary <-st_read("data/conservation_polygon/BC/BC_boundary_layer.shp")

harvest_zone1<- sf::st_read("data/conservation_polygon/Harvest_Survey_Zones/Harvest_Survey_Zones_2017.shp") %>%
  st_transform(8857) %>% 
  filter(Zonename=="British Columbia - Zone 1") %>% 
  st_make_valid() # Fixes invalid geometries for example when islands are disconected they facilitate operations later on 


#ebirdst_download_status( "American Wigeon",pattern = "abundance_(median|lower|upper)_3km", download_occurrence = TRUE,dry_run = FALSE,force = TRUE)

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")

# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)


# #_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# ----Chronologies------
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# Notes: There are so far three different ways I seen people approach the chronologies, here I am using two different ways but the third one might be usefull too
# 1)[Matt]Plot the chronology using weekly relative abundance estimates cropping to a given geography, CALCULATING the MEAN ( why not the sum?) weekly value for that geography. 
# Relative abundances can be more than 1, why? need to refresh this concept, I know they are counts but often they are less than 1, and sometimes way bigger than 1.
# Pros of this approach You are using numbers that can have an interpretation of a given species, e.g areas with more individuals
# 2)[Matt] Plot the chronology using weekly relative abundance estimates cropping to a given geography, and divided by the total global population (Percentage of population)
# Pros you can compare the importance of a given area for different species
# 3)[Joe]Plot the chronology using weekly relative abundance estimates cropping to a given geography, CALCULATING the SUM( why not the mean?) weekly value for that geography. 
# This are also also smoothed to daily values, instead of weeky using a spline model 
# Use anchor points to scale based on values in teh field, this gives you estimates?
# need to look at the presentation fro this 

# In our case we are Plot the chronology using weekly relative abundance estimates cropping to a given geography, CALCULATING the SUM( at the geograohy) weekly value for that geography [JOE approach]
# and also using the percentage of population 
# I want to compare the output of the three approaches and see what is more useful for Megan !

# WARNING The chronologies dont do well for rare species
# WARNING As we increase scale we get more acceptable level of accuracy in the chronology 

#---- Function to generate the chronology dataset -------------
chronologies_abundance <- NULL

for (species in species_list) {

# load the median weekly relative abundance and lower/upper confidence limits
abd_median <- load_raster(species, product = "abundance", metric = "median")
abd_lower <- load_raster(species, product = "abundance", metric = "lower")
abd_upper <- load_raster(species, product = "abundance", metric = "upper")

# project region boundary to match raster data
#region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))

# extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
abd_median_region <- extract(abd_median, harvest_zone1,
                             fun = "mean", na.rm = TRUE, ID = FALSE)
abd_median_region_sum <- extract(abd_median, harvest_zone1,
                             fun = "sum", na.rm = TRUE, ID = FALSE)
abd_lower_region <- extract(abd_lower, harvest_zone1,
                            fun = "mean", na.rm = TRUE, ID = FALSE)
abd_lower_region_sum <- extract(abd_lower, harvest_zone1,
                            fun = "sum", na.rm = TRUE, ID = FALSE)
abd_upper_region <- extract(abd_upper, harvest_zone1,
                            fun = "mean", na.rm = TRUE, ID = FALSE)
abd_upper_region_sum <- extract(abd_upper, harvest_zone1,
                            fun = "sum", na.rm = TRUE, ID = FALSE)

# transform to data frame format with rows corresponding to weeks
chronology <- data.frame(species=species, 
                         week = as.Date(names(abd_median)),
                         median = as.numeric(abd_median_region),
                         lower = as.numeric(abd_lower_region),
                         upper = as.numeric(abd_upper_region),
                         median_sum = as.numeric(abd_median_region_sum),
                         lower_sum = as.numeric(abd_lower_region_sum),
                         upper_sum = as.numeric(abd_upper_region_sum))

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
out_dir <- "outputs/plots/harvest_zone1/rel_abundance_zone1"

# In the first option [THE ONE I PREFER]
#which for me it is more interpretative we will use the sum of the relative abundance in the area of interest for example in Harvest zone 1

for (sp in species_list_plot) {
p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
  aes(x = week, y = median_sum) +
  geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
  
  geom_line(linewidth = 1, color="steelblue") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    x = NULL, 
    y = "Relative abundance in the area  ",
    title = paste("Migration chronology Harvest Zone1-", sp),
    color = NULL, fill = NULL
  ) +
  theme_classic()+
  theme(legend.position = "bottom")
# Save each plot automatically 
ggsave(
  filename = file.path(out_dir, paste0("chronologyRelAbundance_HarvestZone1_2023", gsub(" ", "_", sp), ".png")),
  plot = p,
  width = 8,
  height = 5
)

}

# Alternatively you can use the mean relative abundance in the given area, but this seems more difficult to interprete 

# for (sp in species_list_plot) {
#   
#   p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
#     aes(x = week, y = median, color = species, fill = species) +
#     geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
#     geom_line(linewidth = 1) +
#     scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#     labs(
#       x = NULL, 
#       y = "MEAN relative abundance in the area",
#       title = paste("Migration chronology Harvest Zone1-", sp),
#       color = NULL, fill = NULL
#     ) +
#     theme(legend.position = "none")
#   # Save each plot automatically 
#   ggsave(
#     filename = file.path(out_dir, paste0("chronologyAbundance_HarvestZone1_2023_", gsub(" ", "_", sp), ".png")),
#     plot = p,
#     width = 8,
#     height = 5
#   )
#   
# }
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

# directory for ebird 

Sys.setenv(EBIRDST_DATA_DIR="C:/Users/jmunoz/Local_BirdsCanada/1_JV_science_coordinator_role_local/1_Projects/10_migration_chronologies/migration_chronologies/data/species_rasters")
ebirdst::ebirdst_data_dir()

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")

# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#species_list <- c("American Coot","American Wigeon","Barrow's Goldeneye")

harvest_zone1<- sf::st_read("data/conservation_polygon/Harvest_Survey_Zones/Harvest_Survey_Zones_2017.shp") %>%
  st_transform(8857) %>% 
  filter(Zonename=="British Columbia - Zone 1") %>% 
  st_make_valid() # Fixes invalid geometries for example when islands are disconected they facilitate operations later on 

#----------Function to generate the chronology for percentage of pop dataset-----------

chronologies <- NULL
for (species in species_list) {
  # download weekly 27km relative abundance, median and confidence limits
  # ebirdst_download_status(species,
  #                         pattern = "abundance_(median|upper|lower)_3km")
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric="median")
  abd_lower <- load_raster(species, metric = "lower")
  abd_upper <- load_raster(species, metric = "upper")
  
  # total relative abundance across the entire modeled range of the species, extract as a vector
  abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
  
  # total abundance within the region of interest # extract= extract values for a given loiocation 
  # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too 
  abd_median_region <- extract(abd_median, harvest_zone1,
                               fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, harvest_zone1,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, harvest_zone1,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # proportion of global population within the region of interest
  prop_pop_median <- as.numeric(abd_median_region) / abd_total
  prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
  prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
  
  # transform to data frame format with rows corresponding to weeks
  # median give as teh median proportion of poplaton within teh area of interest 
  chronology<- data.frame(species = species,
                           week = as.Date(names(abd_median)),
                           median = prop_pop_median,
                           lower = prop_pop_lower,
                           upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions 
  
  # combine with other species
  chronologies <- bind_rows(chronologies, chronology)
}



#Finally, we can use this data frame to generate migration chronologies for these species.

out_dir <- "outputs/plots/harvest_zone1/percent_pop_zone1"


for (sp in species_list_plot) {
  p <- ggplot(chronologies%>% filter(species == sp)) +
    aes(x = week, y = median) +
    geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="yellow4") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
    labs(x = NULL,
         y = "Percent of population Harvest zone 1",
         title =paste( "Migration chronologies BC-Harvest zone 1-", sp),
         color = NULL, fill = NULL) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chronology_PERCENTAGE_POP_HarvestZone1_2023_", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
  }


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
#--------------Harvest zone 2---------
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

harvest_zone2<- sf::st_read("data/conservation_polygon/Harvest_Survey_Zones/Harvest_Survey_Zones_2017.shp") %>%
  st_transform(8857) %>% 
  filter(Zonename=="British Columbia - Zone 2")%>%
  st_make_valid()

#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# 0) SETUP

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")

# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#----z2 Function to generate the chronology -----------

# Function to generate the chronology dataset 
chronologies_abundance <- NULL

for (species in species_list) {
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric = "median")
  abd_lower <- load_raster(species, product = "abundance", metric = "lower")
  abd_upper <- load_raster(species, product = "abundance", metric = "upper")
  
  # project region boundary to match raster data
  #region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
  
  # extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
  abd_median_region <- extract(abd_median, harvest_zone2,
                               fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_median_region_sum <- extract(abd_median, harvest_zone2,
                                   fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, harvest_zone2,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_lower_region_sum <- extract(abd_lower, harvest_zone2,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, harvest_zone2,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_upper_region_sum <- extract(abd_upper, harvest_zone2,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # transform to data frame format with rows corresponding to weeks
  chronology <- data.frame(species=species, 
                           week = as.Date(names(abd_median)),
                           median = as.numeric(abd_median_region),
                           lower = as.numeric(abd_lower_region),
                           upper = as.numeric(abd_upper_region),
                           median_sum = as.numeric(abd_median_region_sum),
                           lower_sum = as.numeric(abd_lower_region_sum),
                           upper_sum = as.numeric(abd_upper_region_sum))
  
  # combine with other species
  
  chronologies_abundance <- bind_rows(chronologies_abundance, chronology)
  
}

# Plot but want to do one for esach specie separately 

species_list_plot <- unique(chronologies_abundance$species)
out_dir <- "outputs/plots/harvest_zone2/rel_abundance_zone2"

for (sp in species_list_plot) {
  p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
    aes(x = week, y = median_sum) +
    geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="steelblue") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Relative abundance in the area  ",
      title = paste("Migration chronology Harvest Zone2-", sp),
      color = NULL, fill = NULL
    ) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chronologyRelAbundance_HarvestZone2_2023", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
  
}


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

# directory for ebird 

Sys.setenv(EBIRDST_DATA_DIR="C:/Users/jmunoz/Local_BirdsCanada/1_JV_science_coordinator_role_local/1_Projects/10_migration_chronologies/migration_chronologies/data/species_rasters")
ebirdst::ebirdst_data_dir()

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")

# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#species_list <- c("American Coot","American Wigeon","Barrow's Goldeneye")

# ---z2 Function to generate the chronology for percentage of pop dataset-----

chronologies <- NULL
for (species in species_list) {
  # download weekly 27km relative abundance, median and confidence limits
  # ebirdst_download_status(species,
  #                         pattern = "abundance_(median|upper|lower)_3km")
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, metric="median")
  abd_lower <- load_raster(species, metric = "lower")
  abd_upper <- load_raster(species, metric = "upper")
  
  # total relative abundance across the entire modeled range of the species, extract as a vector
  abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
  
  # total abundance within the region of interest # extract= extract values for a given loiocation 
  # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too 
  abd_median_region <- extract(abd_median, harvest_zone2,
                               fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, harvest_zone2,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, harvest_zone2,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # proportion of global population within the region of interest
  prop_pop_median <- as.numeric(abd_median_region) / abd_total
  prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
  prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
  
  # transform to data frame format with rows corresponding to weeks
  # median give as teh median proportion of poplaton within teh area of interest 
  chronology <- data.frame(species = species,
                           week = as.Date(names(abd_median)),
                           median = prop_pop_median,
                           lower = prop_pop_lower,
                           upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions 
  
  # combine with other species
  chronologies <- bind_rows(chronologies, chronology)
}

#Finally, we can use this data frame to generate migration chronologies for these species.

out_dir <- "outputs/plots/harvest_zone2/percent_pop"


for (sp in species_list_plot) {
  p <- ggplot(chronologies%>% filter(species == sp)) +
    aes(x = week, y = median) +
    geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="yellow4") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
    labs(x = NULL,
         y = "Percent of population Harvest zone 1",
         title =paste( "Migration chronologies BC-Harvest zone 1-", sp),
         color = NULL, fill = NULL) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chronology_PERCENTAGE_POP_Harvestzone2_2023_", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
  
}


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# ---------hunting_district_van_island----------
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

hunt_district_van_island<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% # transforms to WGS84 because ebord is in Wgs84
  filter(REG_R_NAME =="Vancouver Island") %>% 
  st_union() %>% #dissolves the polygons into one 
  st_as_sf # make sure the object is a dataframe with spatial geometry

# #_#_##_#_#_#_#_##_#_#_#_#_#
# 0) SETUP

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#---hd1 Function to generate the chronology dataset -----
chronologies_abundance <- NULL

for (species in species_list) {
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric = "median")
  abd_lower <- load_raster(species, product = "abundance", metric = "lower")
  abd_upper <- load_raster(species, product = "abundance", metric = "upper")
  
  # project region boundary to match raster data
  #region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
  
  # extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
  abd_median_region <- extract(abd_median, hunt_district_van_island,
                               fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_median_region_sum <- extract(abd_median, hunt_district_van_island,
                                   fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, hunt_district_van_island,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_lower_region_sum <- extract(abd_lower, hunt_district_van_island,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, hunt_district_van_island,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_upper_region_sum <- extract(abd_upper, hunt_district_van_island,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # transform to data frame format with rows corresponding to weeks
  chronology_abd <- data.frame(species=species, 
                           week = as.Date(names(abd_median)),
                           median = as.numeric(abd_median_region),
                           lower = as.numeric(abd_lower_region),
                           upper = as.numeric(abd_upper_region),
                           median_sum = as.numeric(abd_median_region_sum),
                           lower_sum = as.numeric(abd_lower_region_sum),
                           upper_sum = as.numeric(abd_upper_region_sum))
  
  # combine with other species
  
  chronologies_abundance <- bind_rows(chronologies_abundance, chronology_abd)
  
}

# plots 
species_list_plot <- unique(chronologies_abundance$species)
out_dir <- "outputs/plots/hunt_district_van_island/rel_abd_hd_vIs"

for (sp in species_list_plot) {
  p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
    aes(x = week, y = median_sum) +
    geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="steelblue") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Relative abundance Hunting district Vancouver Island ",
      title = paste("Migration chronology-Hunting district Vancouver Island-", sp),
      color = NULL, fill = NULL
    ) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chrono_RelAbun_hd_van23", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
  }


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#species_list <- c("American Coot","American Wigeon","Barrow's Goldeneye")

chronologies <- NULL

#---hd1 Function to generate the chronology for percentage of pop dataset -----

for (species in species_list) {
  # download weekly 27km relative abundance, median and confidence limits
  # ebirdst_download_status(species,
  #                         pattern = "abundance_(median|upper|lower)_3km")
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, metric="median")
  abd_lower <- load_raster(species, metric = "lower")
  abd_upper <- load_raster(species, metric = "upper")
  
  # total relative abundance across the entire modeled range of the species, extract as a vector
  abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
  
  # total abundance within the region of interest # extract= extract values for a given loiocation 
  # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too 
  abd_median_region <- extract(abd_median, hunt_district_van_island,
                               fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, hunt_district_van_island,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, hunt_district_van_island,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # proportion of global population within the region of interest
  prop_pop_median <- as.numeric(abd_median_region) / abd_total
  prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
  prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
  
  # transform to data frame format with rows corresponding to weeks
  # median give as teh median proportion of poplaton within teh area of interest 
  chronology <- data.frame(species = species,
                           week = as.Date(names(abd_median)),
                           median = prop_pop_median,
                           lower = prop_pop_lower,
                           upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions 
  
  # combine with other species
  chronologies <- bind_rows(chronologies, chronology)
}

#Finally, we can use this data frame to generate migration chronologies for these species.

out_dir <- "outputs/plots/hunt_district_van_island/pp_hd_van_isl"


for (sp in species_list_plot) {
  p <- ggplot(chronologies%>% filter(species == sp)) +
    aes(x = week, y = median) +
    geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="yellow4") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
    labs(x = NULL,
         y = "Percent of population Hunting district Vancouver Island",
         title =paste( "Migration chronologies BC-Hunting district Vancouver Island-", sp),
         color = NULL, fill = NULL) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chrono_pp_hd_van_is_23", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
  
}




# ---------hunting_district_lower_mainland----------
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

hunt_district_lower_mainland<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>%  # transforms to WGS84
  filter(REG_R_NAME =="Lower Mainland") %>% 
  st_union() %>% # dissolves the polygons into one 
  st_as_sf # make sure the object is a dataframe with spatial geometry
# #_#_##_#_#_#_#_##_#_#_#_#_#
# 0) SETUP

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#---hd2 Function to generate the chronology dataset -----
chronologies_abundance <- NULL

for (species in species_list) {
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric = "median")
  abd_lower <- load_raster(species, product = "abundance", metric = "lower")
  abd_upper <- load_raster(species, product = "abundance", metric = "upper")
  
  # project region boundary to match raster data
  #region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
  
  # extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
  abd_median_region <- extract(abd_median, hunt_district_lower_mainland,
                               fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_median_region_sum <- extract(abd_median,hunt_district_lower_mainland,
                                   fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, hunt_district_lower_mainland,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_lower_region_sum <- extract(abd_lower, hunt_district_lower_mainland,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, hunt_district_lower_mainland,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_upper_region_sum <- extract(abd_upper, hunt_district_lower_mainland,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # transform to data frame format with rows corresponding to weeks
  chronology_abd <- data.frame(species=species, 
                               week = as.Date(names(abd_median)),
                               median = as.numeric(abd_median_region),
                               lower = as.numeric(abd_lower_region),
                               upper = as.numeric(abd_upper_region),
                               median_sum = as.numeric(abd_median_region_sum),
                               lower_sum = as.numeric(abd_lower_region_sum),
                               upper_sum = as.numeric(abd_upper_region_sum))
  
  # combine with other species
  
  chronologies_abundance <- bind_rows(chronologies_abundance, chronology_abd)
  
}

# plots 
species_list_plot <- unique(chronologies_abundance$species)
out_dir <- "outputs/plots/hunt_district_lower_mainland/rel_abd_hd_lm"

for (sp in species_list_plot) {
  p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
    aes(x = week, y = median_sum) +
    geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="steelblue") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Relative abundance Hunting district Lower mainland ",
      title = paste("Migration chronology-Hunting district Lower mainland-", sp),
      color = NULL, fill = NULL
    ) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chrono_RelAbd_hd_lm_23", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
}


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#species_list <- c("American Coot","American Wigeon","Barrow's Goldeneye")

chronologies_h2<- NULL

#---hd2 Function to generate the chronology for percentage of pop dataset -----

for (species in species_list) {
  # download weekly 27km relative abundance, median and confidence limits
  # ebirdst_download_status(species,
  #                         pattern = "abundance_(median|upper|lower)_3km")
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, metric="median")
  abd_lower <- load_raster(species, metric = "lower")
  abd_upper <- load_raster(species, metric = "upper")
  
  # total relative abundance across the entire modeled range of the species, extract as a vector
  abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
  
  # total abundance within the region of interest # extract= extract values for a given loiocation 
  # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too 
  abd_median_region <- extract(abd_median,hunt_district_lower_mainland,
                               fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, hunt_district_lower_mainland,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, hunt_district_lower_mainland,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # proportion of global population within the region of interest
  prop_pop_median <- as.numeric(abd_median_region) / abd_total
  prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
  prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
  
  # transform to data frame format with rows corresponding to weeks
  # median give as teh median proportion of poplaton within teh area of interest 
  chronology <- data.frame(species = species,
                           week = as.Date(names(abd_median)),
                           median = prop_pop_median,
                           lower = prop_pop_lower,
                           upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions 
  
  # combine with other species
  chronologies_h2 <- bind_rows(chronologies_h2, chronology)
}

#Finally, we can use this data frame to generate migration chronologies for these species.

graphics.off()

out_dir <- "outputs/plots/hunt_district_lower_mainland/perc_pop_hd_lm"


for (sp in species_list_plot) {
  p <- ggplot(chronologies_h2%>% filter(species == sp)) +
    aes(x = week, y = median) +
    geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
    geom_line(linewidth = 1, color="yellow4") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
    labs(x = NULL,
         y = "Percent of population Hunting district Lower mainland",
         title =paste( "Migration chronologies BC-Hunting district Lower mainland-", sp),
         color = NULL, fill = NULL) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("crono_Percpop-hd_lm_23_", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
  
}





# ---------hunting_district_thompson---------
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

hunt_district_thompson<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Thompson")%>% 
  st_union() %>% 
  st_as_sf

# #_#_##_#_#_#_#_##_#_#_#_#_#
# 0) SETUP

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#---hd3 Function to generate the chronology dataset -----
chronologies_abundance <- NULL

for (species in species_list) {
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric = "median")
  abd_lower <- load_raster(species, product = "abundance", metric = "lower")
  abd_upper <- load_raster(species, product = "abundance", metric = "upper")
  
  # project region boundary to match raster data
  #region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
  
  # extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
  abd_median_region <- extract(abd_median, hunt_district_thompson,
                               fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_median_region_sum <- extract(abd_median,hunt_district_thompson,
                                   fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, hunt_district_thompson,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_lower_region_sum <- extract(abd_lower, hunt_district_thompson,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, hunt_district_thompson,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_upper_region_sum <- extract(abd_upper, hunt_district_thompson,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # transform to data frame format with rows corresponding to weeks
  chronology_abd <- data.frame(species=species, 
                               week = as.Date(names(abd_median)),
                               median = as.numeric(abd_median_region),
                               lower = as.numeric(abd_lower_region),
                               upper = as.numeric(abd_upper_region),
                               median_sum = as.numeric(abd_median_region_sum),
                               lower_sum = as.numeric(abd_lower_region_sum),
                               upper_sum = as.numeric(abd_upper_region_sum))
  
  # combine with other species
  
  chronologies_abundance <- bind_rows(chronologies_abundance, chronology_abd)
  
}

# plots 
species_list_plot <- unique(chronologies_abundance$species)
out_dir <- "outputs/plots/hunt_district_thompson/rel_abd_hd_t"

for (sp in species_list_plot) {
  p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
    aes(x = week, y = median_sum) +
    geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="steelblue") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Relative abundance Hunting district Thompson ",
      title = paste("Migration chronology-Hunting district Thompson-", sp),
      color = NULL, fill = NULL
    ) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chrono_RelAbd_hd_t_23", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
}


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# 
# # Vector of species names we’ll potentially loop over later
# species_list <- unique(species_list_requested$common_name)
# 
# #species_list <- c("American Coot","American Wigeon","Barrow's Goldeneye")
# 
# chronologies_h3<- NULL
# 
#---hd3 Function to generate the chronology for percentage of pop dataset -----
# 
# for (species in species_list) {
#   # download weekly 27km relative abundance, median and confidence limits
#   # ebirdst_download_status(species,
#   #                         pattern = "abundance_(median|upper|lower)_3km")
#   
#   # load the median weekly relative abundance and lower/upper confidence limits
#   abd_median <- load_raster(species, metric="median")
#   abd_lower <- load_raster(species, metric = "lower")
#   abd_upper <- load_raster(species, metric = "upper")
#   
#   # total relative abundance across the entire modeled range of the species, extract as a vector
#   abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
#   
#   # total abundance within the region of interest # extract= extract values for a given loiocation 
#   # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too 
#   abd_median_region <- extract(abd_median, hunt_district_thompson,
#                                fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_lower_region <- extract(abd_lower,  hunt_district_thompson,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_upper_region <- extract(abd_upper,  hunt_district_thompson,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   
#   # proportion of global population within the region of interest
#   prop_pop_median <- as.numeric(abd_median_region) / abd_total
#   prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
#   prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
#   
#   # transform to data frame format with rows corresponding to weeks
#   # median give as teh median proportion of poplaton within teh area of interest 
#   chronology <- data.frame(species = species,
#                            week = as.Date(names(abd_median)),
#                            median = prop_pop_median,
#                            lower = prop_pop_lower,
#                            upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions 
#   
#   # combine with other species
#   chronologies_h3 <- bind_rows(chronologies_h3, chronology)
# }
# 
# #Finally, we can use this data frame to generate migration chronologies for these species.
# 
# graphics.off()
# 
# out_dir <- "outputs/plots/hunt_district_thompson/perc_pop_hd_t"
# 
# 
# for (sp in species_list_plot) {
#   p <- ggplot(chronologies_h3%>% filter(species == sp)) +
#     aes(x = week, y = median) +
#     geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
#     geom_line(linewidth = 1, color="yellow4") +
#     scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#     scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
#     labs(x = NULL,
#          y = "Percent of population Hunting district Thompson",
#          title =paste( "Migration chronologies BC-Hunting district Thompson-", sp),
#          color = NULL, fill = NULL) +
#     theme_classic()+
#     theme(legend.position = "bottom")
#   # Save each plot automatically 
#   ggsave(
#     filename = file.path(out_dir, paste0("crono_Percpop-hd_t_23_", gsub(" ", "_", sp), ".png")),
#     plot = p,
#     width = 8,
#     height = 5
#   )
#   
# }

# ---------hunting_district_kootenay---------
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#

hunt_district_kootenay<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Kootenay")%>% 
  st_union() %>% 
  st_as_sf

# #_#_##_#_#_#_#_##_#_#_#_#_#
# 0) SETUP

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#---hd4 Function to generate the chronology dataset -----
chronologies_abundance <- NULL

for (species in species_list) {
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric = "median")
  abd_lower <- load_raster(species, product = "abundance", metric = "lower")
  abd_upper <- load_raster(species, product = "abundance", metric = "upper")
  
  # project region boundary to match raster data
  #region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
  
  # extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
  abd_median_region <- extract(abd_median, hunt_district_kootenay,
                               fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_median_region_sum <- extract(abd_median,hunt_district_kootenay,
                                   fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, hunt_district_kootenay,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_lower_region_sum <- extract(abd_lower, hunt_district_kootenay,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, hunt_district_kootenay,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_upper_region_sum <- extract(abd_upper, hunt_district_kootenay,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # transform to data frame format with rows corresponding to weeks
  chronology_abd <- data.frame(species=species, 
                               week = as.Date(names(abd_median)),
                               median = as.numeric(abd_median_region),
                               lower = as.numeric(abd_lower_region),
                               upper = as.numeric(abd_upper_region),
                               median_sum = as.numeric(abd_median_region_sum),
                               lower_sum = as.numeric(abd_lower_region_sum),
                               upper_sum = as.numeric(abd_upper_region_sum))
  
  # combine with other species
  
  chronologies_abundance <- bind_rows(chronologies_abundance, chronology_abd)
  
}

# plots 
species_list_plot <- unique(chronologies_abundance$species)
out_dir <- "outputs/plots/hunt_district_kootenay/rel_abd_k"

for (sp in species_list_plot) {
  p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
    aes(x = week, y = median_sum) +
    geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="steelblue") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Relative abundance Hunting district Kootenay ",
      title = paste("Migration chronology-Hunting district Kootenay-", sp),
      color = NULL, fill = NULL
    ) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chrono_RelAbd_hd_k_23", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
}


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# 
# # Vector of species names we’ll potentially loop over later
# species_list <- unique(species_list_requested$common_name)
# 
# #species_list <- c("American Coot","American Wigeon","Barrow's Goldeneye")
# 
# chronologies_h4<- NULL
# 
#---hd4 Function to generate the chronology for percentage of pop dataset -----
# 
# for (species in species_list) {
#   # download weekly 27km relative abundance, median and confidence limits
#   # ebirdst_download_status(species,
#   #                         pattern = "abundance_(median|upper|lower)_3km")
#   
#   # load the median weekly relative abundance and lower/upper confidence limits
#   abd_median <- load_raster(species, metric="median")
#   abd_lower <- load_raster(species, metric = "lower")
#   abd_upper <- load_raster(species, metric = "upper")
#   
#   # total relative abundance across the entire modeled range of the species, extract as a vector
#   abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
#   
#   # total abundance within the region of interest # extract= extract values for a given loiocation 
#   # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too 
#   abd_median_region <- extract(abd_median, hunt_district_kootenay,
#                                fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_lower_region <- extract(abd_lower,  hunt_district_kootenay,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_upper_region <- extract(abd_upper,  hunt_district_kootenay,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   
#   # proportion of global population within the region of interest
#   prop_pop_median <- as.numeric(abd_median_region) / abd_total
#   prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
#   prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
#   
#   # transform to data frame format with rows corresponding to weeks
#   # median give as teh median proportion of poplaton within teh area of interest 
#   chronology <- data.frame(species = species,
#                            week = as.Date(names(abd_median)),
#                            median = prop_pop_median,
#                            lower = prop_pop_lower,
#                            upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions 
#   
#   # combine with other species
#   chronologies_h4 <- bind_rows(chronologies_h4, chronology)
# }
# 
# #Finally, we can use this data frame to generate migration chronologies for these species.
# 
# graphics.off()
# 
# out_dir <- "outputs/plots/hunt_district_kootenay/perc_pop_hd_t"
# 
# 
# for (sp in species_list_plot) {
#   p <- ggplot(chronologies_h4%>% filter(species == sp)) +
#     aes(x = week, y = median) +
#     geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
#     geom_line(linewidth = 1, color="yellow4") +
#     scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#     scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
#     labs(x = NULL,
#          y = "Percent of population Hunting district Kootenay",
#          title =paste( "Migration chronologies BC-Hunting district Kootenay-", sp),
#          color = NULL, fill = NULL) +
#     theme_classic()+
#     theme(legend.position = "bottom")
#   # Save each plot automatically 
#   ggsave(
#     filename = file.path(out_dir, paste0("crono_Percpop-hd_k_23_", gsub(" ", "_", sp), ".png")),
#     plot = p,
#     width = 8,
#     height = 5
#   )
#   
# }




# ---------hunting_district_cariboo---------
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
hunt_district_cariboo<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Cariboo")%>% 
  st_union() %>% 
  st_as_sf


# #_#_##_#_#_#_#_##_#_#_#_#_#
# 0) SETUP

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#---hd5 Function to generate the chronology dataset -----
chronologies_abundance <- NULL

for (species in species_list) {
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric = "median")
  abd_lower <- load_raster(species, product = "abundance", metric = "lower")
  abd_upper <- load_raster(species, product = "abundance", metric = "upper")
  
  # project region boundary to match raster data
  #region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
  
  # extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
  abd_median_region <- extract(abd_median, hunt_district_cariboo,
                               fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_median_region_sum <- extract(abd_median,hunt_district_cariboo,
                                   fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, hunt_district_cariboo,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_lower_region_sum <- extract(abd_lower, hunt_district_cariboo,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, hunt_district_cariboo,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_upper_region_sum <- extract(abd_upper, hunt_district_cariboo,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # transform to data frame format with rows corresponding to weeks
  chronology_abd <- data.frame(species=species, 
                               week = as.Date(names(abd_median)),
                               median = as.numeric(abd_median_region),
                               lower = as.numeric(abd_lower_region),
                               upper = as.numeric(abd_upper_region),
                               median_sum = as.numeric(abd_median_region_sum),
                               lower_sum = as.numeric(abd_lower_region_sum),
                               upper_sum = as.numeric(abd_upper_region_sum))
  
  # combine with other species
  
  chronologies_abundance <- bind_rows(chronologies_abundance, chronology_abd)
  
}

# plots 
species_list_plot <- unique(chronologies_abundance$species)
out_dir <- "outputs/plots/hunt_district_cariboo/rel_abd_c"

for (sp in species_list_plot) {
  p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
    aes(x = week, y = median_sum) +
    geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="steelblue") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Relative abundance Hunting district Cariboo",
      title = paste("Migration chronology-Hunting district Cariboo-", sp),
      color = NULL, fill = NULL
    ) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chrono_RelAbd_hd_c_23", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
}


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# 
# # Vector of species names we’ll potentially loop over later
# species_list <- unique(species_list_requested$common_name)
# 
# #species_list <- c("American Coot","American Wigeon","Barrow's Goldeneye")
# 
# chronologies_h5<- NULL
# 
#---hd5 Function to generate the chronology for percentage of pop dataset -----
# 
# for (species in species_list) {
#   # download weekly 27km relative abundance, median and confidence limits
#   # ebirdst_download_status(species,
#   #                         pattern = "abundance_(median|upper|lower)_3km")
#   
#   # load the median weekly relative abundance and lower/upper confidence limits
#   abd_median <- load_raster(species, metric="median")
#   abd_lower <- load_raster(species, metric = "lower")
#   abd_upper <- load_raster(species, metric = "upper")
#   
#   # total relative abundance across the entire modeled range of the species, extract as a vector
#   abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
#   
#   # total abundance within the region of interest # extract= extract values for a given loiocation 
#   # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too 
#   abd_median_region <- extract(abd_median, hunt_district_cariboo,
#                                fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_lower_region <- extract(abd_lower,  hunt_district_cariboo,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_upper_region <- extract(abd_upper,  hunt_district_cariboo,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   
#   # proportion of global population within the region of interest
#   prop_pop_median <- as.numeric(abd_median_region) / abd_total
#   prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
#   prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
#   
#   # transform to data frame format with rows corresponding to weeks
#   # median give as teh median proportion of poplaton within teh area of interest 
#   chronology <- data.frame(species = species,
#                            week = as.Date(names(abd_median)),
#                            median = prop_pop_median,
#                            lower = prop_pop_lower,
#                            upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions 
#   
#   # combine with other species
#   chronologies_h5 <- bind_rows(chronologies_h5, chronology)
# }
# 
# #Finally, we can use this data frame to generate migration chronologies for these species.
# 
# graphics.off()
# 
# out_dir <- "outputs/plots/hunt_district_cariboo/perc_pop_c"
# 
# 
# for (sp in species_list_plot) {
#   p <- ggplot(chronologies_h5%>% filter(species == sp)) +
#     aes(x = week, y = median) +
#     geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
#     geom_line(linewidth = 1, color="yellow4") +
#     scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#     scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
#     labs(x = NULL,
#          y = "Percent of population Hunting district Cariboo",
#          title =paste( "Migration chronologies BC-Hunting district Cariboo-", sp),
#          color = NULL, fill = NULL) +
#     theme_classic()+
#     theme(legend.position = "bottom")
#   # Save each plot automatically 
#   ggsave(
#     filename = file.path(out_dir, paste0("crono_Percpop-hd_c_23_", gsub(" ", "_", sp), ".png")),
#     plot = p,
#     width = 8,
#     height = 5
#   )
#   
# }


# ---------hunting_district_skeena---------

hunt_district_skeena<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Skeena")%>% 
  st_union() %>% 
  st_as_sf


# #_#_##_#_#_#_#_##_#_#_#_#_#
# 0) SETUP

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#---hd6 Function to generate the chronology dataset -----
chronologies_abundance <- NULL

for (species in species_list) {
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric = "median")
  abd_lower <- load_raster(species, product = "abundance", metric = "lower")
  abd_upper <- load_raster(species, product = "abundance", metric = "upper")
  
  # project region boundary to match raster data
  #region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
  
  # extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
  abd_median_region <- extract(abd_median, hunt_district_skeena,
                               fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_median_region_sum <- extract(abd_median,hunt_district_skeena,
                                   fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, hunt_district_skeena,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_lower_region_sum <- extract(abd_lower, hunt_district_skeena,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, hunt_district_skeena,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_upper_region_sum <- extract(abd_upper, hunt_district_skeena,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # transform to data frame format with rows corresponding to weeks
  chronology_abd <- data.frame(species=species, 
                               week = as.Date(names(abd_median)),
                               median = as.numeric(abd_median_region),
                               lower = as.numeric(abd_lower_region),
                               upper = as.numeric(abd_upper_region),
                               median_sum = as.numeric(abd_median_region_sum),
                               lower_sum = as.numeric(abd_lower_region_sum),
                               upper_sum = as.numeric(abd_upper_region_sum))
  
  # combine with other species
  
  chronologies_abundance <- bind_rows(chronologies_abundance, chronology_abd)
  
}

# plots 
species_list_plot <- unique(chronologies_abundance$species)
out_dir <- "outputs/plots/hunt_district_skeena/rel_abd_s"

for (sp in species_list_plot) {
  p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
    aes(x = week, y = median_sum) +
    geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="steelblue") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Relative abundance Hunting district Skeena",
      title = paste("Migration chronology-Hunting district Skeena-", sp),
      color = NULL, fill = NULL
    ) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chrono_RelAbd_hd_s_23", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
}


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# 
# # Vector of species names we’ll potentially loop over later
# species_list <- unique(species_list_requested$common_name)
# 
# #species_list <- c("American Coot","American Wigeon","Barrow's Goldeneye")
# 
# chronologies_h6<- NULL
# 
#---hd6 Function to generate the chronology for percentage of pop dataset -----
# 
# for (species in species_list) {
#   # download weekly 27km relative abundance, median and confidence limits
#   # ebirdst_download_status(species,
#   #                         pattern = "abundance_(median|upper|lower)_3km")
#   
#   # load the median weekly relative abundance and lower/upper confidence limits
#   abd_median <- load_raster(species, metric="median")
#   abd_lower <- load_raster(species, metric = "lower")
#   abd_upper <- load_raster(species, metric = "upper")
#   
#   # total relative abundance across the entire modeled range of the species, extract as a vector
#   abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
#   
#   # total abundance within the region of interest # extract= extract values for a given loiocation 
#   # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too 
#   abd_median_region <- extract(abd_median, hunt_district_skeena,
#                                fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_lower_region <- extract(abd_lower,  hunt_district_skeena,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_upper_region <- extract(abd_upper,  hunt_district_skeena,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   
#   # proportion of global population within the region of interest
#   prop_pop_median <- as.numeric(abd_median_region) / abd_total
#   prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
#   prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
#   
#   # transform to data frame format with rows corresponding to weeks
#   # median give as teh median proportion of poplaton within teh area of interest 
#   chronology <- data.frame(species = species,
#                            week = as.Date(names(abd_median)),
#                            median = prop_pop_median,
#                            lower = prop_pop_lower,
#                            upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions 
#   
#   # combine with other species
#   chronologies_h6 <- bind_rows(chronologies_h6, chronology)
# }
# 
# #Finally, we can use this data frame to generate migration chronologies for these species.
# 
# graphics.off()
# 
# out_dir <- "outputs/plots/hunt_district_skeena/perc_pop_s"
# 
# 
# for (sp in species_list_plot) {
#   p <- ggplot(chronologies_h6%>% filter(species == sp)) +
#     aes(x = week, y = median) +
#     geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
#     geom_line(linewidth = 1, color="yellow4") +
#     scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#     scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
#     labs(x = NULL,
#          y = "Percent of population Hunting district Skeena",
#          title =paste( "Migration chronologies BC-Hunting district Skeena-", sp),
#          color = NULL, fill = NULL) +
#     theme_classic()+
#     theme(legend.position = "bottom")
#   # Save each plot automatically 
#   ggsave(
#     filename = file.path(out_dir, paste0("crono_Percpop-hd_s_23_", gsub(" ", "_", sp), ".png")),
#     plot = p,
#     width = 8,
#     height = 5
#   )
#   
# }

# new 




# ---------hunting_district_omineca---------

hunt_district_omineca<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Omineca")%>% 
  st_union() %>% 
  st_as_sf


# #_#_##_#_#_#_#_##_#_#_#_#_#
# 0) SETUP

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#---hd7 Function to generate the chronology dataset -----
chronologies_abundance <- NULL

for (species in species_list) {
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric = "median")
  abd_lower <- load_raster(species, product = "abundance", metric = "lower")
  abd_upper <- load_raster(species, product = "abundance", metric = "upper")
  
  # project region boundary to match raster data
  #region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
  
  # extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
  abd_median_region <- extract(abd_median, hunt_district_omineca,
                               fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_median_region_sum <- extract(abd_median,hunt_district_omineca,
                                   fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, hunt_district_omineca,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_lower_region_sum <- extract(abd_lower, hunt_district_omineca,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, hunt_district_omineca,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_upper_region_sum <- extract(abd_upper, hunt_district_omineca,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # transform to data frame format with rows corresponding to weeks
  chronology_abd <- data.frame(species=species, 
                               week = as.Date(names(abd_median)),
                               median = as.numeric(abd_median_region),
                               lower = as.numeric(abd_lower_region),
                               upper = as.numeric(abd_upper_region),
                               median_sum = as.numeric(abd_median_region_sum),
                               lower_sum = as.numeric(abd_lower_region_sum),
                               upper_sum = as.numeric(abd_upper_region_sum))
  
  # combine with other species
  
  chronologies_abundance <- bind_rows(chronologies_abundance, chronology_abd)
  
}

# plots 
species_list_plot <- unique(chronologies_abundance$species)
out_dir <- "outputs/plots/hunt_district_omineca/rel_abd_om"

for (sp in species_list_plot) {
  p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
    aes(x = week, y = median_sum) +
    geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="steelblue") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Relative abundance Hunting district Omineca",
      title = paste("Migration chronology-Hunting district Omineca-", sp),
      color = NULL, fill = NULL
    ) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chrono_RelAbd_hd_om_23", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
}


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# 
# # Vector of species names we’ll potentially loop over later
# species_list <- unique(species_list_requested$common_name)
# 
# #species_list <- c("American Coot","American Wigeon","Barrow's Goldeneye")
# 
# chronologies_h7<- NULL
# 
#---hd7 Function to generate the chronology for percentage of pop dataset -----
# 
# for (species in species_list) {
#   # download weekly 27km relative abundance, median and confidence limits
#   # ebirdst_download_status(species,
#   #                         pattern = "abundance_(median|upper|lower)_3km")
#   
#   # load the median weekly relative abundance and lower/upper confidence limits
#   abd_median <- load_raster(species, metric="median")
#   abd_lower <- load_raster(species, metric = "lower")
#   abd_upper <- load_raster(species, metric = "upper")
#   
#   # total relative abundance across the entire modeled range of the species, extract as a vector
#   abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
#   
#   # total abundance within the region of interest # extract= extract values for a given loiocation 
#   # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too 
#   abd_median_region <- extract(abd_median, hunt_district_omineca,
#                                fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_lower_region <- extract(abd_lower,  hunt_district_omineca,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_upper_region <- extract(abd_upper,  hunt_district_omineca,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   
#   # proportion of global population within the region of interest
#   prop_pop_median <- as.numeric(abd_median_region) / abd_total
#   prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
#   prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
#   
#   # transform to data frame format with rows corresponding to weeks
#   # median give as teh median proportion of poplaton within teh area of interest 
#   chronology <- data.frame(species = species,
#                            week = as.Date(names(abd_median)),
#                            median = prop_pop_median,
#                            lower = prop_pop_lower,
#                            upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions 
#   
#   # combine with other species
#   chronologies_h7 <- bind_rows(chronologies_h7, chronology)
# }
# 
# #Finally, we can use this data frame to generate migration chronologies for these species.
# 
# graphics.off()
# 
# out_dir <- "outputs/plots/hunt_district_omineca/perc_pop_om"
# 
# 
# for (sp in species_list_plot) {
#   p <- ggplot(chronologies_h7%>% filter(species == sp)) +
#     aes(x = week, y = median) +
#     geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
#     geom_line(linewidth = 1, color="yellow4") +
#     scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#     scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
#     labs(x = NULL,
#          y = "Percent of population Hunting district Omineca",
#          title =paste( "Migration chronologies BC-Hunting district Omineca-", sp),
#          color = NULL, fill = NULL) +
#     theme_classic()+
#     theme(legend.position = "bottom")
#   # Save each plot automatically 
#   ggsave(
#     filename = file.path(out_dir, paste0("crono_Percpop-hd_om_23_", gsub(" ", "_", sp), ".png")),
#     plot = p,
#     width = 8,
#     height = 5
#   )
#   
# }





# ---------hunting_district_okanagan---------

hunt_district_okanagan<- sf::st_read("data/conservation_polygon/WAA_wildlifeMGMT_units/WAAWMU_SVW_polygon.shp") %>%
  sf::st_transform(8857) %>% 
  filter(REG_R_NAME =="Okanagan")%>% 
  st_union() %>% 
  st_as_sf


# #_#_##_#_#_#_#_##_#_#_#_#_#
# 0) SETUP

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv")
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#---hd8 Function to generate the chronology dataset -----
chronologies_abundance <- NULL

for (species in species_list) {
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric = "median")
  abd_lower <- load_raster(species, product = "abundance", metric = "lower")
  abd_upper <- load_raster(species, product = "abundance", metric = "upper")
  
  # project region boundary to match raster data
  #region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
  
  # extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
  abd_median_region <- extract(abd_median, hunt_district_okanagan,
                               fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_median_region_sum <- extract(abd_median,hunt_district_okanagan,
                                   fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, hunt_district_okanagan,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_lower_region_sum <- extract(abd_lower, hunt_district_okanagan,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, hunt_district_okanagan,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_upper_region_sum <- extract(abd_upper, hunt_district_okanagan,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # transform to data frame format with rows corresponding to weeks
  chronology_abd <- data.frame(species=species, 
                               week = as.Date(names(abd_median)),
                               median = as.numeric(abd_median_region),
                               lower = as.numeric(abd_lower_region),
                               upper = as.numeric(abd_upper_region),
                               median_sum = as.numeric(abd_median_region_sum),
                               lower_sum = as.numeric(abd_lower_region_sum),
                               upper_sum = as.numeric(abd_upper_region_sum))
  
  # combine with other species
  
  chronologies_abundance <- bind_rows(chronologies_abundance, chronology_abd)
  
}

# plots 
species_list_plot <- unique(chronologies_abundance$species)
out_dir <- "outputs/plots/hunt_district_okanagan/rel_abd_oka"

for (sp in species_list_plot) {
  p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
    aes(x = week, y = median_sum) +
    geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="steelblue") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Relative abundance Hunting district Okanagan",
      title = paste("Migration chronology-Hunting district Okanagan-", sp),
      color = NULL, fill = NULL
    ) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chrono_RelAbd_hd_oka_23", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
}


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# 
# # Vector of species names we’ll potentially loop over later
# species_list <- unique(species_list_requested$common_name)
# 
# #species_list <- c("American Coot","American Wigeon","Barrow's Goldeneye")
# 
# chronologies_h8<- NULL
# 
#---hd8 Function to generate the chronology for percentage of pop dataset -----
# 
# for (species in species_list) {
#   # download weekly 27km relative abundance, median and confidence limits
#   # ebirdst_download_status(species,
#   #                         pattern = "abundance_(median|upper|lower)_3km")
#   
#   # load the median weekly relative abundance and lower/upper confidence limits
#   abd_median <- load_raster(species, metric="median")
#   abd_lower <- load_raster(species, metric = "lower")
#   abd_upper <- load_raster(species, metric = "upper")
#   
#   # total relative abundance across the entire modeled range of the species, extract as a vector
#   abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
#   
#   # total abundance within the region of interest # extract= extract values for a given loiocation 
#   # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too 
#   abd_median_region <- extract(abd_median, hunt_district_okanagan,
#                                fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_lower_region <- extract(abd_lower,  hunt_district_okanagan,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   abd_upper_region <- extract(abd_upper,  hunt_district_okanagan,
#                               fun = "sum", na.rm = TRUE, ID = FALSE)
#   
#   # proportion of global population within the region of interest
#   prop_pop_median <- as.numeric(abd_median_region) / abd_total
#   prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
#   prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
#   
#   # transform to data frame format with rows corresponding to weeks
#   # median give as teh median proportion of poplaton within teh area of interest 
#   chronology <- data.frame(species = species,
#                            week = as.Date(names(abd_median)),
#                            median = prop_pop_median,
#                            lower = prop_pop_lower,
#                            upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions 
#   
#   # combine with other species
#   chronologies_h8 <- bind_rows(chronologies_h8, chronology)
# }
# 
# #Finally, we can use this data frame to generate migration chronologies for these species.
# 
# graphics.off()
# 
# out_dir <- "outputs/plots/hunt_district_okanagan/perc_pop_oka"
# 
# 
# for (sp in species_list_plot) {
#   p <- ggplot(chronologies_h8%>% filter(species == sp)) +
#     aes(x = week, y = median) +
#     geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
#     geom_line(linewidth = 1, color="yellow4") +
#     scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#     scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
#     labs(x = NULL,
#          y = "Percent of population Hunting district Okanagan",
#          title =paste( "Migration chronologies BC-Hunting district Okanagan-", sp),
#          color = NULL, fill = NULL) +
#     theme_classic()+
#     theme(legend.position = "bottom")
#   # Save each plot automatically 
#   ggsave(
#     filename = file.path(out_dir, paste0("crono_Percpop-hd_oka_23_", gsub(" ", "_", sp), ".png")),
#     plot = p,
#     width = 8,
#     height = 5
#   )
#   
# }




#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
#------ River Deltas ( 2 River deltas)---------------
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# for the river deltas I needed to filter by lenght because the otehr attributes were not different between the two 



# ---------Delta-Fraser River---------

fraser_river<- sf::st_read("data/conservation_polygon/Fraser_Skagit_Valley/SnowGooseSurveyArea.shp")  %>%
  st_transform(8857) %>% 
  filter(Shape_Leng<"300000")

# #_#_##_#_#_#_#_##_#_#_#_#_#
# 0) SETUP

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv") %>% 
  filter(polygon_fraser_river_delta=="yes")
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#---hd8 Function to generate the chronology dataset -----
chronologies_abundance <- NULL

for (species in species_list) {
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric = "median")
  abd_lower <- load_raster(species, product = "abundance", metric = "lower")
  abd_upper <- load_raster(species, product = "abundance", metric = "upper")
  
  # project region boundary to match raster data
  #region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
  
  # extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
  abd_median_region <- extract(abd_median, fraser_river,
                               fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_median_region_sum <- extract(abd_median,fraser_river,
                                   fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, fraser_river,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_lower_region_sum <- extract(abd_lower, fraser_river,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, fraser_river,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_upper_region_sum <- extract(abd_upper, fraser_river,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # transform to data frame format with rows corresponding to weeks
  chronology_abd <- data.frame(species=species, 
                               week = as.Date(names(abd_median)),
                               median = as.numeric(abd_median_region),
                               lower = as.numeric(abd_lower_region),
                               upper = as.numeric(abd_upper_region),
                               median_sum = as.numeric(abd_median_region_sum),
                               lower_sum = as.numeric(abd_lower_region_sum),
                               upper_sum = as.numeric(abd_upper_region_sum))
  
  # combine with other species
  
  chronologies_abundance <- bind_rows(chronologies_abundance, chronology_abd)
  
}

# plots 
species_list_plot <- unique(chronologies_abundance$species)
out_dir <- "outputs/plots/fraser_river_delta/rel_abd_fraser"

for (sp in species_list_plot) {
  p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
    aes(x = week, y = median_sum) +
    geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="steelblue") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Relative abundance Fraser River Delta",
      title = paste("Migration chronology-Fraser River Delta-", sp),
      color = NULL, fill = NULL
    ) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chrono_RelAbd_fraser_23", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
}


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# 
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)


chronologies_delta1<- NULL

#---Delta1 Function to generate the chronology for percentage of pop dataset -----

for (species in species_list) {
  # download weekly 27km relative abundance, median and confidence limits
  # ebirdst_download_status(species,
  #                         pattern = "abundance_(median|upper|lower)_3km")

  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, metric="median")
  abd_lower <- load_raster(species, metric = "lower")
  abd_upper <- load_raster(species, metric = "upper")

  # total relative abundance across the entire modeled range of the species, extract as a vector
  abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum

  # total abundance within the region of interest # extract= extract values for a given loiocation
  # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too
  abd_median_region <- extract(abd_median, fraser_river,
                               fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower,  fraser_river,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper,  fraser_river,
                              fun = "sum", na.rm = TRUE, ID = FALSE)

  # proportion of global population within the region of interest
  prop_pop_median <- as.numeric(abd_median_region) / abd_total
  prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
  prop_pop_upper <- as.numeric(abd_upper_region) / abd_total

  # transform to data frame format with rows corresponding to weeks
  # median give as teh median proportion of poplaton within teh area of interest
  chronology <- data.frame(species = species,
                           week = as.Date(names(abd_median)),
                           median = prop_pop_median,
                           lower = prop_pop_lower,
                           upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions

  # combine with other species
  chronologies_delta1 <- bind_rows(chronologies_delta1, chronology)
}

#Finally, we can use this data frame to generate migration chronologies for these species.

#graphics.off()

out_dir <- "outputs/plots/fraser_river_delta/perc_pop_fraser"


for (sp in species_list_plot) {
  p <- ggplot(chronologies_delta1%>% filter(species == sp)) +
    aes(x = week, y = median) +
    geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
    geom_line(linewidth = 1, color="yellow4") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
    labs(x = NULL,
         y = "Percent of population Fraser River Delta",
         title =paste( "Migration chronologies BC-Fraser River Delta-", sp),
         color = NULL, fill = NULL) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically
  ggsave(
    filename = file.path(out_dir, paste0("crono_Percpop-fraser_23_", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )

}






# ---------Skagit-Fraser River---------

skagit_river<- sf::st_read("data/conservation_polygon/Fraser_Skagit_Valley/SnowGooseSurveyArea.shp")  %>%
  st_transform(8857) %>% 
  filter(Shape_Leng>"400000")


# #_#_##_#_#_#_#_##_#_#_#_#_#
# 0) SETUP

# data 
species_list_requested<- read.csv("data/list/requested_species_list.csv") %>% 
  filter(polygon_skagit_river_delta=="yes")
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)

#---hd8 Function to generate the chronology dataset -----
chronologies_abundance <- NULL

for (species in species_list) {
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, product = "abundance", metric = "median")
  abd_lower <- load_raster(species, product = "abundance", metric = "lower")
  abd_upper <- load_raster(species, product = "abundance", metric = "upper")
  
  # project region boundary to match raster data
  #region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
  
  # extract values within region and calculate the mean, I also calculated the sum because it seems mo easy to interprete 
  abd_median_region <- extract(abd_median, skagit_river,
                               fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_median_region_sum <- extract(abd_median,skagit_river,
                                   fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower, skagit_river,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_lower_region_sum <- extract(abd_lower, skagit_river,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper, skagit_river,
                              fun = "mean", na.rm = TRUE, ID = FALSE)
  abd_upper_region_sum <- extract(abd_upper, skagit_river,
                                  fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # transform to data frame format with rows corresponding to weeks
  chronology_abd <- data.frame(species=species, 
                               week = as.Date(names(abd_median)),
                               median = as.numeric(abd_median_region),
                               lower = as.numeric(abd_lower_region),
                               upper = as.numeric(abd_upper_region),
                               median_sum = as.numeric(abd_median_region_sum),
                               lower_sum = as.numeric(abd_lower_region_sum),
                               upper_sum = as.numeric(abd_upper_region_sum))
  
  # combine with other species
  
  chronologies_abundance <- bind_rows(chronologies_abundance, chronology_abd)
  
}

# plots 
species_list_plot <- unique(chronologies_abundance$species)
out_dir <- "outputs/plots/skagit_river_delta/rel_abd_skagit"

for (sp in species_list_plot) {
  p <- ggplot(chronologies_abundance %>% filter(species == sp)) +
    aes(x = week, y = median_sum) +
    geom_ribbon(aes(ymin = lower_sum, ymax = upper_sum),  fill="steelblue", color = NA, alpha = 0.2) +
    
    geom_line(linewidth = 1, color="steelblue") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      x = NULL, 
      y = "Relative abundance Skagit River Delta",
      title = paste("Migration chronology-Skagit River Delta-", sp),
      color = NULL, fill = NULL
    ) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically 
  ggsave(
    filename = file.path(out_dir, paste0("chrono_RelAbd_Skagit_23", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
}


#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# For multiple species using proportion of population
# Corrects for detectability differences between species 
#_#_##_#_#_#_#_##_#_#_#_#_#__##_#__##_#__#_#_#_#_#_#_##__#
# 
# Vector of species names we’ll potentially loop over later
species_list <- unique(species_list_requested$common_name)


chronologies_delta2<- NULL

#---Delta1 Function to generate the chronology for percentage of pop dataset -----

for (species in species_list) {
  # download weekly 27km relative abundance, median and confidence limits
  # ebirdst_download_status(species,
  #                         pattern = "abundance_(median|upper|lower)_3km")
  
  # load the median weekly relative abundance and lower/upper confidence limits
  abd_median <- load_raster(species, metric="median")
  abd_lower <- load_raster(species, metric = "lower")
  abd_upper <- load_raster(species, metric = "upper")
  
  # total relative abundance across the entire modeled range of the species, extract as a vector
  abd_total <- global(abd_median, fun = sum, na.rm = TRUE)$sum
  
  # total abundance within the region of interest # extract= extract values for a given loiocation
  # here we extract the values firs and then we calculate the proportion of population, we could do twh opossite way too
  abd_median_region <- extract(abd_median, skagit_river,
                               fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_lower_region <- extract(abd_lower,  skagit_river,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  abd_upper_region <- extract(abd_upper,  skagit_river,
                              fun = "sum", na.rm = TRUE, ID = FALSE)
  
  # proportion of global population within the region of interest
  prop_pop_median <- as.numeric(abd_median_region) / abd_total
  prop_pop_lower <- as.numeric(abd_lower_region) / abd_total
  prop_pop_upper <- as.numeric(abd_upper_region) / abd_total
  
  # transform to data frame format with rows corresponding to weeks
  # median give as teh median proportion of poplaton within teh area of interest
  chronology <- data.frame(species = species,
                           week = as.Date(names(abd_median)),
                           median = prop_pop_median,
                           lower = prop_pop_lower,
                           upper = pmin(prop_pop_upper, 1)) # the 1 ensure does not go over 1 as tehy are proportions
  
  # combine with other species
  chronologies_delta2 <- bind_rows(chronologies_delta2, chronology)
}

#Finally, we can use this data frame to generate migration chronologies for these species.

#graphics.off()

out_dir <- "outputs/plots/skagit_river_delta/perc_pop_skagit"


for (sp in species_list_plot) {
  p <- ggplot(chronologies_delta2%>% filter(species == sp)) +
    aes(x = week, y = median) +
    geom_ribbon(aes(ymin = lower, ymax = upper),  fill="yellow3", color = NA, alpha = 0.2) +
    geom_line(linewidth = 1, color="yellow4") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::label_percent()) + # It converts numeric values into percentage strings for the axis.
    labs(x = NULL,
         y = "Percent of population Skagit River Delta",
         title =paste( "Migration chronologies Skagit River Delta-", sp),
         color = NULL, fill = NULL) +
    theme_classic()+
    theme(legend.position = "bottom")
  # Save each plot automatically
  ggsave(
    filename = file.path(out_dir, paste0("crono_Percpop_skagit_23_", gsub(" ", "_", sp), ".png")),
    plot = p,
    width = 8,
    height = 5
  )
  
}


#### The end of teh script
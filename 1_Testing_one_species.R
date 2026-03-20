
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
# 1) CHOOSING THE DATA DIRECTORY
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
# ================================
# 0) SETUP
region_boundary <-st_read("data/conservation_polygon/BC/BC_boundary_layer.shp")



# download data if they haven't already been downloaded
# only weekly 3km relative abundance, median and confidence limits


ebirdst_download_status( "American Wigeon",pattern = "abundance_(median|lower|upper)_3km", download_occurrence = TRUE,dry_run = FALSE,force = TRUE)


# load the median weekly relative abundance and lower/upper confidence limits
abd_median <- load_raster("amewig", product = "abundance", metric = "median")
abd_lower <- load_raster("amewig", product = "abundance", metric = "lower")
abd_upper <- load_raster("amewig", product = "abundance", metric = "upper")

# project region boundary to match raster data
region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))

# extract values within region and calculate the mean
abd_median_region <- extract(abd_median, region_boundary_proj,
                             fun = "mean", na.rm = TRUE, ID = FALSE)
abd_lower_region <- extract(abd_lower, region_boundary_proj,
                            fun = "mean", na.rm = TRUE, ID = FALSE)
abd_upper_region <- extract(abd_upper, region_boundary_proj,
                            fun = "mean", na.rm = TRUE, ID = FALSE)

# transform to data frame format with rows corresponding to weeks
chronology <- data.frame(week = as.Date(names(abd_median)),
                         median = as.numeric(abd_median_region),
                         lower = as.numeric(abd_lower_region),
                         upper = as.numeric(abd_upper_region))
ggplot(chronology) +
  aes(x = week, y = median) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Week", 
       y = "Mean relative abundance in Bc",
       title = "Migration chronology for American Wigeon in Bc")

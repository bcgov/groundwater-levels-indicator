# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

################################################################################
# This script uses bcgroundwater to download ground water level data from  BC
# Ministry of environment's Groundwater Observation Well data access tool 
# (http://www.env.gov.bc.ca/wsd/data_searches/obswell/index.html), and processes
# the data for annual trend analysis, following the methods documented here: 
# http://www.env.gov.bc.ca/soe/archive/print_ver/water/2014_GWL_Trends_methods.pdf
################################################################################

# Install the packages we will need from CRAN:
package_list <- c("dplyr", "rgdal", "sp", "lubridate", "zoo", "ggplot2", "stringr",
                  "grid", "scales", "ggmap", "devtools", "rvest", "RColorBrewer",
                  "purrr", "sf", "gridExtra", "bcmaps")
package_new <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(package_new)) install.packages(package_new)

# Install the packages we will need from GitHub:
package_github <- c("bcgroundwater", "envreportutils")
package_new <- package_github[!(package_github %in% installed.packages()[,"Package"])]
if(length(package_new)) devtools::install_github(paste0("bcgov/", package_new))

library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(bcgroundwater)
library(sp)
library(rgdal)
library(rvest)
library(stringr)
library(bcmaps)

## Source a few handy functions we will need
source("func.R")

################################################################################
# Get well attributes from 
# - Ground Water Wells data (from DataBC) https://catalogue.data.gov.bc.ca/dataset/ground-water-wells
# - NR region info: https://catalogue.data.gov.bc.ca/dataset/generalized-natural-resource-nr-regions
# Here we automatically grab it from online or bcmaps package
################################################################################

obs_wells <- bcdc_map("WHSE_WATER_MANAGEMENT.GW_WATER_WELLS_WRBC_SVW", 
                      query = "OBSERVATION_WELL_NUMBER IS NOT NULL") %>%
  filter(!is.na(MINISTRY_OBSERVATION_WELL_STAT) | 
           !is.na(OBSERVATION_WELL_NUMBER)) %>% 
  mutate(LONGITUDE = -LONGITUDE) %>% # LONGITUDE needs to be negative
  sf::st_join(nr_regions()) %>%
  as_tibble() %>%
  select(OBSERVATION_WELL_NUMBER, CHEMISTRY_SITE_ID, WELL_TAG_NUMBER, 
         CONSTRUCTION_END_DATE, GENERAL_REMARKS, OTHER_INFORMATION,
         MINISTRY_OBSERVATION_WELL_STAT, 
         AQUIFER_TYPE = AQUIFER_LITHOLOGY_CODE, 
         DEPTH_WELL_DRILLED, WATER_DEPTH, LONGITUDE, LATITUDE,
         REGION_NAME, SITE_AREA) %>%
  mutate(REGION_NAME = str_replace(REGION_NAME, " Natural Resource Region", ""),
         REGION_NAME = str_replace(REGION_NAME, "-", " / "), # To fit labeller later
         AQUIFER_TYPE = as.character(AQUIFER_TYPE),
         AQUIFER_TYPE = replace(AQUIFER_TYPE, is.na(AQUIFER_TYPE), "Unknown"),
         AQUIFER_TYPE = factor(AQUIFER_TYPE, levels = c("BED", "UNC", "Unknown"), 
                               labels = c("Bedrock", "Sand and Gravel", "Unknown")))

## Check for duplicate Well numbers:
dup_wells <- obs_wells$OBSERVATION_WELL_NUMBER[duplicated(obs_wells$OBSERVATION_WELL_NUMBER)]
obs_wells[obs_wells$OBSERVATION_WELL_NUMBER %in% dup_wells,]

## Looking at the comments in GENERAL_REMARKS and OTHER_NIFORMATION, they are 
## deep and shallow variants of the same obs well number
## Omit the shallow version.
obs_wells <- filter(obs_wells, WELL_TAG_NUMBER != 93712)

###############################################################################
# Download and process the groundwater level data using bcgroundwater
# WARNING: This takes quite a bit of time
###############################################################################

# Raw well data (warnings reflect wells with no data)
wells_raw <- get_gwl(wells = obs_wells$OBSERVATION_WELL_NUMBER, which = "all")
dir.create("tmp", showWarnings = FALSE)
save(wells_raw, file = "./tmp/raw_well_data.RData") #backup, just in case

# Nest data by Well_Num
# As we don't have EMS_IDS, use Well_Num so we get a clear idea of which well has convergences issues
wells_prep <- wells_raw %>%
  mutate(EMS_ID = Well_Num) %>%      
  group_by(Well_Num1 = Well_Num) %>%
  nest()

# Create monthly time series for each well
wells_month <- mutate(wells_prep, data = map(data, ~monthlyValues(.x)))

# Get timeseries interpolate over missing values
wells_ts <- mutate(wells_month, data = map(data, ~makeWellTS(.x)))

# Remove consecutive strings of missing (interpolated) values from the
# beginning and end of each time series

wells_ts_trimmed <- mutate(wells_ts, 
                           start_end = map(data,
                                           ~as.data.frame(trimConsRuns(.x$nReadings, val = 0, head = 0.1,
                                                                       tail = 0.9, n_consec = 4))),
                           data = map2(data, start_end, ~.x[.y$start:.y$end,]))

# Unnest data for full timeseries
monthlywells_ts <- unnest(wells_ts_trimmed, data) %>%
  select(-Well_Num1) %>%
  mutate(Well_Num = as.numeric(Well_Num),
         EMS_ID = NA)

# Check the problems with convergence:
problems <- c("196", "060", "303", "203", "154", "065", "117", "185", 
              "256", "081", "204", "007", "292", "176", "228")

filter(monthlywells_ts, Well_Num %in% as.numeric(problems)) %>% summary()



## These processes all take a long time, so it's a good idea to save them in a
## temporary directory:
dir.create("tmp", showWarnings = FALSE)
save(monthlywells_ts, file = "./tmp/raw_data.RData")
save(obs_wells, file = "./tmp/raw_attr_data.RData")


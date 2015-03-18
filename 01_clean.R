# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

################################################################################
# This script takes one or more csv files downloaded from the BC Ministry of 
# environment's Groundwater Observation Well data access tool 
# (http://www.env.gov.bc.ca/wsd/data_searches/obswell/index.html), and processes
# the data for annual trend analysis, following the methods documented here: 
# http://www.env.gov.bc.ca/soe/archive/print_ver/water/2014_GWL_Trends_methods.pdf
################################################################################

downloadMonthly <- TRUE # Set to TRUE to use monthly data from DataBC

# if downloadMonthly is FALSE, (i.e., using raw data), set the directory where
# your raw data files are stored:
data_raw_dir <- "data/well_files"

# First, install the packages we will need:
list.of.packages <- c("dplyr", "rgdal", "sp", "lubridate", "zoo", "ggplot2", 
                      "grid", "scales", "ggmap", "devtools", "rvest", "RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Install the bcgroundwater package from the bcgov GitHub repo:
devtools::install_github("bcgov/bcgroundwater")

library(dplyr)
library(bcgroundwater)
library(sp)
library(rgdal)
library(rvest)

## Source a few handy functions we will need
source("func.R")

################################################################################
# Get attribute data from Ground Water Wells data (from DataBC). 
# http://catalogue.data.gov.bc.ca/dataset/ground-water-wells-spatial-view-with-attribute-info
################################################################################
attr_dir <- file.path("data", "BCGW_Wells")
if (!file.exists(file.path(attr_dir, "GW_WW_WRBC", "GW_WW_WRBC.csv"))) {
  unzip(file.path("data", "BCGW_GW_Wells_csv.zip"), exdir =attr_dir)
}
wells_attr <- read.csv(file.path(attr_dir, "GW_WW_WRBC", "GW_WW_WRBC.csv"), 
                       na.strings = "", stringsAsFactors = FALSE)

## Get the details about this data from the DataBC metadata, using rvest:
tbls <- html("data/BCGW_Wells/metadata_GW_WW_WRBC.html") %>% 
  html_nodes("#object-description .table") %>% 
  html_table
metadata <- tbls[[1]]
names(metadata) <- gsub("\\s+", "_", names(metadata))

## Create a named vector of long names to rename the columns, and select the 
## the columns we want:
col_names <- setNames(metadata$Short_Name, metadata$Column_Name)
cols <- c("OBSERVATION_WELL_NUMBER", "CHEMISTRY_SITE_ID", "WELL_TAG_NUMBER", 
          "CONSTRUCTION_END_DATE", "GENERAL_REMARKS", "OTHER_INFORMATION",
          "MINISTRY_OBSERVATION_WELL_STAT", "AQUIFER_LITHOLOGY_CODE", 
          "DEPTH_WELL_DRILLED", "WATER_DEPTH", "LONGITUDE", "LATITUDE")

wells_attr <- wells_attr[, col_names[cols]]
names(wells_attr) <- cols

obs_wells_attr <- wells_attr %>%
  filter(!is.na(wells_attr$MINISTRY_OBSERVATION_WELL_STAT) | 
           !is.na(wells_attr$OBSERVATION_WELL_NUMBER)) %>% 
  mutate(LONGITUDE = -LONGITUDE) # LONGITUDE needs to be negative

## Check for duplicate Well numbers:
dup_wells <- obs_wells_attr$OBSERVATION_WELL_NUMBER[duplicated(obs_wells_attr$OBSERVATION_WELL_NUMBER)]
obs_wells_attr[obs_wells_attr$OBSERVATION_WELL_NUMBER %in% dup_wells,]
## Looking at the comments in GENERAL_REMARKS and OTHER_NIFORMATION, they are 
## deep and shallow variants of the same obs well number. Name them as such (manually):
obs_wells_attr$OBSERVATION_WELL_NUMBER[obs_wells_attr$WELL_TAG_NUMBER == 85623] <- "365 deep"
obs_wells_attr$OBSERVATION_WELL_NUMBER[obs_wells_attr$WELL_TAG_NUMBER == 93712] <- "365 shallow"
obs_wells_attr$OBSERVATION_WELL_NUMBER[obs_wells_attr$WELL_TAG_NUMBER == 90212] <- "381 shallow"
obs_wells_attr$OBSERVATION_WELL_NUMBER[obs_wells_attr$WELL_TAG_NUMBER == 90213] <- "381 deep"

################################################################################
# Get NRO region information into the attributes file.
# http://catalogue.data.gov.bc.ca/dataset/natural-resource-operations-regions
################################################################################
reg_dir <- file.path("data", "BCGW_NRO_reg")
if (!file.exists(file.path(reg_dir, "NRO_REG", "NRO_REG_polygon.shp"))) {
  unzip(file.path("data", "BCGW_NRO_reg_shp.zip"), exdir = reg_dir)
}
regions.poly <- readOGR(file.path(reg_dir, "NRO_REG"), "NRO_REG_polygon", 
                        stringsAsFactors = FALSE)

# Make obs_wells_attr spatial
coordinates(obs_wells_attr) <- ~LONGITUDE+LATITUDE
proj4string(obs_wells_attr) <- CRS(proj4string(regions.poly))

well_reg_info <- over(obs_wells_attr, regions.poly)
obs_wells_attr <- data.frame(obs_wells_attr, well_reg_info, stringsAsFactors = FALSE)

## Fix region names
obs_wells_attr$REGION_NM <- sapply(strsplit(obs_wells_attr$REGION_NM, " Region")
                                  , function(x) x[[1]])

###############################################################################
# Import and process the groundwater level data. When you download the data from 
# the GWL tool, name each file with the well's EMS_ID like so: "[EMS_ID].csv",
# so you can then pass in the EMS_ID to readGWLdata. Save them all in the same 
# directory (data_raw_dir set below).
#
# If you do not want to download the # raw data, a copy of the cleaned, monthly 
# data is available at: 
# http://www.data.gov.bc.ca/dbc/catalogue/detail.page?config=dbc&P110=recorduid:179324.
# Set download_monthly to TRUE if you want to start with the aggregated monthly 
# data from DataBC
###############################################################################

if (downloadMonthly) {
  monthlycsv <- "data/GWL_monthly.csv"
  download.file("http://pub.data.gov.bc.ca/datasets/179324/GWL_monthly.csv", 
                destfile = monthlycsv)
  monthlywells <- read.csv(monthlycsv, stringsAsFactors = FALSE)
  monthlywells <- split(monthlywells, monthlywells$EMS_ID)
  
} else {
  # Get a list of all the files in the directory
  wellfiles <- list.files(data_raw_dir, pattern="\\.csv", full.names = TRUE)
  
  # import each csv using the 'readGWLdata' function.
  wells_raw <- lapply(wellfiles, function(x) {
    tryCatch(
      readGWLdata(path=x, emsID=gsub("\\.csv", "", basename(x)))
      , error=function(e) NULL)
  }
  )
  
  ################################################################################
  # Create monthly time series for each well.
  ################################################################################
  
  # Use 'monthlyValues' function to create monthly values
  monthlywells <- lapply(compact(wells_raw), monthlyValues)
}

# Create full monthly time series, interpolating missing values
monthlywellts <- lapply(monthlywells, function(x)
  tryCatch(makeWellTS(x), error=function(e) NULL))

# Remove consecutive strings of missing (interpolated) values from the
# beginning and end of each time series
monthlyts_strip <- lapply(compact(monthlywellts), function(x) {
  start_end <- trimConsRuns(x$nReadings, val=0, head=0.1, tail=0.9, n_consec=4)
  x[start_end$start:start_end$end,]
})

# Combine the list of data frames into a single long data frame. This data frame 
# can then be converted to annual data and analysed.
monthlywells.ts <- rbind_all(monthlyts_strip)

## There are Well_Nums that are denoted deep or shallow in the raw files that
## are just numbers in the attribute data from DataBC. Find them and just use the numbers:
to_simple <- grepl("deep|shallow", monthlywells.ts$Well_Num) & 
  !monthlywells.ts$Well_Num %in% obs_wells_attr$OBSERVATION_WELL_NUMBER

monthlywells.ts$Well_Num[to_simple] <- gsub("([0-9]+)\\s[a-z]+", "\\1", 
                                            monthlywells.ts$Well_Num[to_simple])

## These processes all take a long time, so it's a good idea to save them in a
## temporary directory:
dir.create("tmp", showWarnings = FALSE)
save.image("tmp/raw_data.RData")


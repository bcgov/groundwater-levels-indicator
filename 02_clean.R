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


###############################################################################
# This script works on raw groundwater level data downloaded using
# the bcgroundwater R package in the 01_load.R script
###############################################################################

## Source package libraries
if (!exists(".header_sourced")) source("header.R")

## Load saved raw data if necessary
if (!exists("wells_data_raw")) load("./tmp/raw_well_data.RData")

## Clean raw groundwater level data

# Nest data by Well_Num. As we don't have EMS_IDS, use Well_Num
# so we get a clear idea of which well has convergence issues
wells_prep <- wells_data_raw %>%
  mutate(Date = as.Date(Time)) %>% 
  filter(Date <= as.POSIXct("2019-01-11")) %>% 
  rename(Well_Num = myLocation) %>% 
  mutate(EMS_ID = Well_Num,
         GWL = Value) %>%      
  group_by(Well_Num1 = Well_Num) %>%
  nest()

# # Original script:
# # Create monthly time series for each well. 

# Get time series, remove consecutive strings of missing values from the
# beginning and end of each time series, interpolate over missing values
wells_month <- mutate(wells_prep[1:10,], data = map(data, ~monthly_values(.x)))
wells_ts <- mutate(wells_month[1:10,], data = map(data, ~make_well_ts(.x)))
# NOTE: You can skip the above lines, as they take a long time (20-30 minutes)
# and you use the map() function at line 56 for identical results.

# {bcgroundwater} function 'make_well_ts' outputs to the console whether or not a well 
# has data gaps that are sufficiently large to be a problem. The below function recreates
# the data gap checking logic and adds a column to the wells_ts object 
# indicating whether or not the well had data gaps.
wells_ts = wells_ts$data %>% 
  map( ~ {
    .x %>% 
      # Add a column that indicates if either the top 10% or bottom 10% of records for a well
      # has NA for the groundwater level. This new column 'data_missing' is TRUE if data gaps
      # are identified in the top 10% or bottom 10% of records (we glance at top or bottom 10%
      # as a good estimate of data completeness in general for each well)
      cbind(.x %>% slice_head(prop = 0.1) %>% 
              bind_rows(.x %>% slice_tail(prop = 0.1)) %>% 
              filter(is.na(med_GWL)) %>%
              filter(Date %m+% months(1) == lead(Date)) %>% 
              summarise(data_missing = n()) > 1
      )
  }) %>% 
  bind_rows() %>% 
  group_by(EMS_ID) %>% 
  nest()

# Unnest data for full timeseries
monthlywells_ts <- unnest(wells_ts, data)

# # Check the problems with convergence:
# problems <- c("284", "125", "232", "303", "173", "291", "102", "185", "220", 
#               "287", "007", "100", "414")

wells_with_data_issues = monthlywells_ts %>% 
  filter(data_missing == T) %>% 
  select(EMS_ID) %>% 
  distinct() %>% 
  pull(EMS_ID)

monthlywells_ts = monthlywells_ts %>% 
  filter(!EMS_ID %in% wells_with_data_issues)

## Save clean data object in a temporary directory
save(monthlywells_ts, file = "./tmp/clean_well_data.RData")

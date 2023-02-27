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
  mutate(Well_Num = str_extract(myLocation,'[0-9]+')) %>% 
  rename(EMS_ID = myLocation,
         GWL = Value) %>% 
  group_by(Well_Num1 = Well_Num) %>%
  nest()

# Create monthly time series for each well
wells_month <- mutate(wells_prep[c(1:20),], data = map(data, ~monthly_values(.x)))

# Get time series, remove consecutive strings of missing values from the
# beginning and end of each time series, interpolate over missing values
wells_ts <- mutate(wells_month, data = map(data, ~make_well_ts(.x)))

## CHRIS ADDITION - START ##
# The following code applies a function to each row of the dataset. This function repeats most of the logic
# of the {bcgroundwater} function 'make_well_ts', which outputs to the console whether or not a well 
# has data gaps that are sufficiently large to be a problem. The issue is that if we use
# the current {bcgroundwater} function, then the user must 
# write (by hand!) the list of well identity numbers and then filter them out... we can do better!
# The function below adds a column to each well's dataframe indicating whether or not such a data gap exists,
# which we can easily use in the following code to filter out such problematic wells.
wells_ts = wells_ts$data %>% 
  map( ~ {
    .x %>%
      # Identify wells that have 2+ sequential months without groundwater level records
      # in the top 10% (i.e. oldest 10%) of the recorded years.
      cbind(.x %>% slice_head(prop = 0.1) %>% 
                   filter(is.na(dev_med_GWL)) %>% 
              # Filter for 1 value per month
                   filter(Date %m+% months(1) == lead(Date)) %>% 
                   summarise(data_missing_oldest_10_percent = n()) > 1
    ) %>% 
      # Identify wells that have 2+ sequential months without groundwater level records
      # in the top 10% (i.e. oldest 10%) of the recorded years.
      cbind(.x %>% slice_tail(prop = 0.1) %>% 
              filter(is.na(dev_med_GWL)) %>% 
              # Filter for 1 value per month
              filter(Date %m+% months(1) == lead(Date)) %>% 
              summarise(data_missing_recent_10_percent = n()) > 1)
  }) %>% 
  bind_rows() %>% 
  group_by(EMS_ID) %>% 
  nest()

# If we choose to drop all stations that had data gaps in the first 10% or last 10%
# of records...
wells_ts = wells_ts %>% 
  unnest(data) %>% 
  filter(data_missing_oldest_10_percent + data_missing_recent_10_percent == 0) %>% 
  group_by(EMS_ID) %>% 
  nest()

## CHRIS ADDITION - END ##

# Unnest data for full timeseries
monthlywells_ts <- unnest(wells_ts, data) %>%
  select(-Well_Num1) %>%
  mutate(Well_Num = as.numeric(Well_Num),
         EMS_ID = NA)

# Check the problems with convergence:
problems <- c("284", "125", "232", "303", "173", "291", "102", "185", "220", 
              "287", "007", "100", "414")

filter(monthlywells_ts, Well_Num %in% as.numeric(problems)) %>% 
  summary()


## Save clean data object in a temporary directory
save(monthlywells_ts, file = "./tmp/clean_well_data.RData")




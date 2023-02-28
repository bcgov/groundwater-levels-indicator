# Copyright 2023 Province of British Columbia
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
library("lubridate") 

## Load saved raw data if necessary
if (!exists("wells_data_raw")) load("./tmp/raw_well_data.RData")

## Clean raw groundwater level data

#Rename attributes
wells_data_raw <- wells_data_raw %>%
  rename("Date" = Time, "GWL" = Value) %>%
  mutate(Well_Num = substring(myLocation, 3,5)) %>%
  filter(Approval=="Approved")

#Keep only wells with >= 10 years of data
wells_over_10 <- wells_data_raw %>%
  group_by(Well_Num) %>% 
  summarise(dataStart = as.Date(min(Date)), 
            dataEnd = as.Date(max(Date)), 
            dataYears = as.numeric(dataEnd - dataStart) / 365) %>%
  filter(dataYears >= 10) %>%
  pull(Well_Num)

wells_data_filtered <- wells_data_raw %>%
  filter(Well_Num %in% wells_over_10)

# Nest data by Well_Num. Rename Well_Num to EMS_ID for wells_prep() {bcgroundwater} function.
#Filter data by previous year.
wells_prep <- wells_data_filtered %>%
  filter(Date <= as.POSIXct("2022-01-01")) %>% 
  mutate(EMS_ID = Well_Num) %>%  
  group_by(Well_Num1 = Well_Num) %>%
  nest()

# Filter data for last 10 years for recent metric
wells_prep_10 <- wells_data_filtered %>%
  filter(Date <= as.POSIXct("2022-01-01") & Date >= as.POSIXct("2012-01-01") ) %>% 
  mutate(EMS_ID = Well_Num) %>%  
  group_by(Well_Num1 = Well_Num) %>%
  nest()

# Create monthly time series of monthly median values for each well
wells_month <- mutate(wells_prep, data = map(data, ~monthly_values(.x)))
wells_month_10 <- mutate(wells_prep_10, data = map(data, ~monthly_values(.x)))

# Get time series, remove consecutive strings of missing values from the
# beginning and end of each time series, interpolate over missing values
wells_ts <- mutate(wells_month, data = map(data, ~make_well_ts(.x)))
wells_ts_10 <- mutate(wells_month_10, data = map(data, ~make_well_ts(.x)))


# The following code applies a function to each row of the dataset. This function repeats most of the logic
# of the {bcgroundwater} function 'make_well_ts', which outputs to the console whether or not a well 
# has data gaps that are sufficiently large to be a problem. 
# The function below adds a column to each well's dataframe indicating whether or not such a data gap exists,
# which we can easily use in the following code to filter out such problematic wells.
wells_ts = wells_ts$data %>% 
  map( ~ {
    .x %>% cbind(.x %>% slice_head(prop = 0.1) %>% 
                   bind_rows(.x %>% slice_tail(prop = 0.1)) %>% 
                   filter(is.na(dev_med_GWL)) %>% 
                   filter(Date %m+% months(1) == lead(Date)) %>% 
                   summarise(data_missing = n()) > 1
    )
  }) %>% 
  bind_rows() %>% 
  group_by(EMS_ID) %>% 
  nest()

wells_ts_10 = wells_ts_10$data %>% 
  map( ~ {
    .x %>% cbind(.x %>% slice_head(prop = 0.1) %>% 
                   bind_rows(.x %>% slice_tail(prop = 0.1)) %>% 
                   filter(is.na(dev_med_GWL)) %>% 
                   filter(Date %m+% months(1) == lead(Date)) %>% 
                   summarise(data_missing = n()) > 1
    )
  }) %>% 
  bind_rows() %>% 
  group_by(EMS_ID) %>% 
  nest()

# Unnest data for full timeseries
monthlywells_ts <- unnest(wells_ts, data) %>%
  ungroup() %>% 
  select(-EMS_ID)

monthlywells_ts_10 <- unnest(wells_ts_10, data) %>%
  ungroup() %>% 
  select(-EMS_ID)

### Chris said to remove this
## Check the problems with convergence:
#problems <- c("284", "125", "232", "303", "173", "291", "102", "185", "220", 
#              "287", "007", "100", "414")
#
##Ekaterina's initial list
#problems <- c("007", "047", "060", "100", "102", "173", "220", "236", "287", "337", "414")
#
##Ekaterina's new list
#problems <- c("047", "080", "117", "125", "154", "236", "283", "295", "304", "312", "314")
#
#filter(monthlywells_ts, Well_Num %in% as.numeric(problems)) %>% 
#  summary()

## Save clean data object in a temporary directory
save(monthlywells_ts, file = "./tmp/clean_well_data.RData")
save(monthlywells_ts_10, file = "./tmp/clean_well_data_10.RData")


## Temp - Ekaterina change
save(monthlywells_ts, file = "./tmp/monthlywells_ts_unfiltered.RData")
save(monthlywells_ts_10, file = "./tmp/monthlywells_ts_10_unfiltered.RData")




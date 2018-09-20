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


## Source package libraries and a few handy functions we will need
source("header.R")
source("func.R")

## Load saved raw data if necessary
if (!exists("wells_raw")) load("./tmp/raw_well_data.RData")
if (!exists("obs_wells_raw")) load("./tmp/raw_attr_data.RData")


## Clean raw groundwater level data
# Nest data by Well_Num
# As we don't have EMS_IDS, use Well_Num so we get a clear idea of which well has convergence issues
wells_prep <- wells_raw %>%
  mutate(EMS_ID = Well_Num) %>%      
  group_by(Well_Num1 = Well_Num) %>%
  nest()

# Create monthly time series for each well
wells_month <- mutate(wells_prep, data = map(data, ~monthly_values(.x)))

# Get time series, remove consecutive strings of missing values from the
# beginning and end of each time series, interpolate over missing values
wells_ts <- mutate(wells_month, data = map(data, ~make_well_ts(.x)))

# Unnest data for full timeseries
monthlywells_ts <- unnest(wells_ts, data) %>%
  select(-Well_Num1) %>%
  mutate(Well_Num = as.numeric(Well_Num),
         EMS_ID = NA)

# Check the problems with convergence:
problems <- c("196", "060", "303", "203", "154", "065", "117", "185", 
              "256", "081", "204", "007", "292", "176", "228")

filter(monthlywells_ts, Well_Num %in% as.numeric(problems)) %>% summary()


## Clean groundwater level metadata

# Check for duplicate Well numbers in the well metadata
dup_wells <- obs_wells_raw$OBSERVATION_WELL_NUMBER[duplicated(obs_wells_raw$OBSERVATION_WELL_NUMBER)]
obs_wells_raw[obs_wells_raw$OBSERVATION_WELL_NUMBER %in% dup_wells,]

# Looking at the comments in GENERAL_REMARKS and OTHER_INFORMATION, they are 
# deep and shallow variants of the same obs well number, omit the shallow version
obs_wells <- filter(obs_wells_raw, WELL_TAG_NUMBER != 93712)


## Save clean data objects in a temporary directory
save(monthlywells_ts, file = "./tmp/raw_data.RData")
save(obs_wells, file = "./tmp/raw_attr_data.RData")



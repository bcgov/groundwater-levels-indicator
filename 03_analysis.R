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
# This script uses on ground water level data which has been processed to have
# 1 observation per month using the 02_clean.R script
###############################################################################


## Source package libraries
if (!exists(".header_sourced")) source("header.R")


# Load saved clean data objects if necessary
if (!exists("monthlywells_ts")) load("./tmp/clean_well_data.RData")
if (!exists("obs_wells")) load("./tmp/clean_attr_data.RData")


## Generate summary data for each well
welldata_attr <- monthlywells_ts %>%
  group_by(EMS_ID, Well_Num) %>%
  summarise(dataStart = as.Date(min(Date)), 
            dataEnd = as.Date(max(Date)), 
            dataYears = as.numeric(dataEnd - dataStart) / 365, 
            nObs = n(), 
            nMissing = length(med_GWL[nReadings == 0]), 
            percent_missing = round(nMissing/nObs*100, 1))


## Analysis of mean annual groundwater levels, using a Mann-Kendall trend test
## with trend-free prewhitening, as implemented in the package 'zyp'. Methods are 
## documented here: 
## http://www.env.gov.bc.ca/soe/indicators/water/groundwater-levels.html


## Only use wells with relatively current data, more than 10 years of data, and 
## less than 25% missing monthly observations
latest_date <- "2009-01-01"

wells_nums <- filter(welldata_attr, 
                     dataYears >= 10, 
                     percent_missing < 25, 
                     dataEnd > latest_date) %>% 
  pull(Well_Num)

## Summarise as mean annual values and filter to subset of wells
annualwells_ts <- monthlywells_ts %>%
  group_by(EMS_ID, Well_Num, Year) %>%
  summarize(mean_GWL = mean(med_GWL), SD = sd(med_GWL), n_months = n()) %>%
  filter(Well_Num %in% wells_nums, n_months == 12)

## Perform the analysis
results_annual <- gwl_zyp_test(dataframe = annualwells_ts, byID = "Well_Num", 
                             col = "mean_GWL", method = "both") %>%
  mutate(Well_Num = as.numeric(Well_Num)) %>%
  filter(test_type == "yuepilon")

## Join the analysis results to the well summary data
## Full join to add in missing and old data so we can mark it as such below
wells_results <- full_join(results_annual, welldata_attr, by = "Well_Num")

## Assign each well to a trend category according to the slope and significance 
## of the trend
wells_results <- mutate(wells_results,
                        state = case_when(trend >= 0.1 & sig < 0.05 ~ "Large Rate of Decline",
                                          trend >= 0.03 & trend < 0.1 & sig < 0.05 ~ "Moderate Rate of Decline",
                                          trend <= -0.03 & sig < 0.05 ~ "Increasing",
                                          TRUE ~ "Stable"))

## Join this to the well attribute data
results_out <- right_join(obs_wells, wells_results, 
                          by = c("OBSERVATION_WELL_NUMBER" = "Well_Num")) %>%
  mutate(Lat = round(LATITUDE, 4), 
         Long = round(LONGITUDE, 4), 
         wellDepth_m = round(DEPTH_WELL_DRILLED * 0.3048), 
         waterDepth_m = round(WATER_DEPTH * 0.3048), 
         dataYears = round(dataYears, 1),
         trend_line_int = round(intercept, 4), 
         trend_line_slope = round(trend, 4),
         sig = round(sig, 4), 
         percent_missing = round(percent_missing, 1)) %>%
  select(EMS_ID = CHEMISTRY_SITE_ID, 
         Well_Num = OBSERVATION_WELL_NUMBER, 
         Aquifer_Type = AQUIFER_TYPE,
         REGION_NAME, 
         aquifer_id,
         Lat, Long, 
         wellDepth_m, waterDepth_m, 
         start_date = dataStart, 
         last_date = dataEnd, 
         nYears = dataYears, 
         percent_missing, trend_line_int, trend_line_slope, sig, state) %>%
  mutate(Well_Name = paste0("Observation Well #", Well_Num), 
         state = case_when(is.na(trend_line_int) & (nYears < 10 | is.na(last_date)) ~ 
                             "Recently established well; time series too short for trend analysis",
                           is.na(trend_line_int) & (percent_missing >= 25 | last_date < latest_date) ~
                             "Too many missing observations to perform trend analysis",
                           TRUE ~ state),
         category = case_when(state %in% c("Increasing", "Stable") ~ "Stable or Increasing", 
                              grepl("Recently|missing", state) ~ "N/A",
                              TRUE ~ state)) %>% 
  filter(!(state == "Too many missing observations to perform trend analysis" & last_date < latest_date)) %>% 
  select(EMS_ID, Well_Num, Well_Name, everything())

## CHRIS ADDITION - START ## 

# CHRIS: I don't recall why I changed the above function.
# In case it's useful, here's 'my' version.

## Join this to the well attribute data
results_out <- obs_wells %>% 
  mutate(observation_well_number = as.numeric(observation_well_number)) %>% 
  right_join(wells_results, 
             by = c("observation_well_number" = "Well_Num")) %>%
  # Reproject the coordinates of the wells into lat/long, add them to dataframe
  st_transform(crs = 4326) %>% 
  mutate(Long = st_coordinates(.)[,1],
         Lat = st_coordinates(.)[,2]) %>%
  mutate(Lat = round(Lat, 4), 
         Long = round(Long, 4), 
         wellDepth_m = round(finished_well_depth * 0.3048), 
         waterDepth_m = round(static_water_level * 0.3048), 
         dataYears = round(dataYears, 1),
         trend_line_int = round(intercept, 4), 
         trend_line_slope = round(trend, 4),
         sig = round(sig, 4), 
         percent_missing = round(percent_missing, 1)) %>%
  select(EMS_ID = id, 
         Well_Num = observation_well_number, 
         Aquifer_Type = aquifer_type,
         region_name, 
         aquifer_id,
         Lat, Long, 
         wellDepth_m, waterDepth_m, 
         start_date = dataStart, 
         last_date = dataEnd, 
         nYears = dataYears, 
         percent_missing, trend_line_int, trend_line_slope, sig, state) %>%
  mutate(Well_Name = paste0("Observation Well #", Well_Num), 
         state = case_when(is.na(trend_line_int) & (nYears < 10 | is.na(last_date)) ~ 
                             "Recently established well; time series too short for trend analysis",
                           is.na(trend_line_int) & (percent_missing >= 25 | last_date < latest_date) ~
                             "Too many missing observations to perform trend analysis",
                           TRUE ~ state),
         category = case_when(state %in% c("Increasing", "Stable") ~ "Stable or Increasing", 
                              grepl("Recently|missing", state) ~ "N/A",
                              TRUE ~ state)) %>% 
  filter(!(state == "Too many missing observations to perform trend analysis" & last_date < latest_date)) %>% 
  select(EMS_ID, Well_Num, Well_Name, everything())

## CHRIS ADDITION - END ##

## Save results in a temporary directory
save(results_out, file = "./tmp/analysis_data.RData")
save(welldata_attr, file = "./tmp/well_data_attributes.RData")


## Write out clean data and attributes file
attr.out.file <- "out/GW_Well_Attributes.csv"
write.csv(results_out, attr.out.file, row.names = FALSE)


## Write out clean groundwater level data file
# remove interpolated values and NA values and make sure only
# keep wells which match results_out dataset

monthly_out <- monthlywells_ts %>%
  filter(nReadings > 0, 
         Well_Num %in% results_out$Well_Num) %>%
  select(EMS_ID, Well_Num, Date, Year, Month, med_GWL, dev_med_GWL, nReadings)

gwl.out.file <- "out/GWL_Monthly.csv"

write.table(monthly_out, file = gwl.out.file, sep = ",", row.names = FALSE)


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
library("lubridate")

# Load saved clean data objects if necessary
if (!exists("monthlywells_ts")) load("./tmp/clean_well_data.RData")
if (!exists("monthlywells_ts_10")) load("./tmp/clean_well_data_10.RData")
if (!exists("monthlywells_ts_20")) load("./tmp/clean_well_data_20.RData")
if (!exists("monthlywells_ts_mean")) load("./tmp/clean_well_data_mean.RData")
if (!exists("monthlywells_ts_10_mean")) load("./tmp/clean_well_data_10_mean.RData")
if (!exists("monthlywells_ts_20_mean")) load("./tmp/clean_well_data_20_mean.RData")
if (!exists("obs_wells_clean")) load("./tmp/clean_well_attr.RData")

## Get the coordinates from the obs_wells object, revert from sf to table.
obs_wells_sf = obs_wells_clean %>% 
  bind_cols(
    obs_wells_clean %>% st_transform(crs=4326) %>% 
      st_coordinates()) %>%
  mutate(Lat = round(Y, 4), 
         Long = round(X, 4), 
         wellDepth_m = round(finished_well_depth * 0.3048), 
         waterDepth_m = round(static_water_level * 0.3048)) %>%
  select(EMS_ID = ems_id, 
         Well_Num = observation_well_number, 
         Aquifer_Type = aquifer_type,
         region_name, 
         aquifer_id,
         Lat, Long, 
         wellDepth_m, waterDepth_m, 
         start_year, end_year)

obs_wells <- obs_wells_sf %>%
  st_drop_geometry() 

## Define function to produce annual mean trend results
summary_function_annual <- function(df, latest_date, MK_method, time_period, well_attributes){
  
  #Produce summary statistics
    welldata_attr <- df %>%
    group_by(EMS_ID, Well_Num) %>%
    summarise(dataStart = as.Date(min(Date)), 
              dataEnd = as.Date(max(Date)), 
              dataYears = as.numeric(dataEnd - dataStart) / 365, 
              nObs = n(), 
              nMissing = length(med_GWL[nReadings == 0]), 
              percent_missing = round(nMissing/nObs*100, 1))

    # Only use wells with relatively current data and less than 25% missing monthly observations
    # Note wells were previously filtered by more than 10 years of data prior to the
    # time series analysis in the 02_clean.R script
  wells_nums <- filter(welldata_attr, 
                       percent_missing < 25, 
                       dataEnd > latest_date) %>% 
                       pull(Well_Num)
  
  #Calculate annual means for trend analysis
  annualwells_ts <- df %>%
    group_by(EMS_ID, Well_Num, Year) %>%
    summarize(mean_GWL = mean(med_GWL), SD = sd(med_GWL), n_months = n()) %>%
    filter(Well_Num %in% wells_nums, n_months == 12)
  
  ## Perform the analysis
  results_annual <- gwl_zyp_test(dataframe = annualwells_ts, byID = "Well_Num", 
                                 col = "mean_GWL", method = "both") %>%
    mutate(Well_Num = Well_Num) %>%
    filter(test_type == MK_method) 
  
  wells_results <- full_join(results_annual, welldata_attr, by = "Well_Num")
  
  wells_results <- mutate(wells_results,
                          state = case_when(trend >= 0.1 & sig < 0.05 ~ "Large Rate of Decline",
                                            trend >= 0.03 & trend < 0.1 & sig < 0.05 ~ "Moderate Rate of Decline",
                                            trend <= -0.03 & sig < 0.05 ~ "Increasing",
                                            TRUE ~ "Stable")) %>%
    mutate(Well_Num = str_remove(Well_Num,'[A-Z]*'))
  
  left_join(well_attributes, wells_results, by=c("Well_Num"="Well_Num")) %>%
    mutate(dataYears = round(dataYears, 1),
           trend_line_int = round(intercept, 4), 
           trend_line_slope = round(trend, 4),
           sig = round(sig, 4), 
           start_date = dataStart, 
           last_date = dataEnd, 
           nYears = dataYears, 
           percent_missing = round(percent_missing, 1)) %>%
    mutate(Well_Name = paste0("Observation Well #", Well_Num), 
           state = case_when(is.na(trend_line_int) & (start_year > latest_date | is.na(last_date)) ~ 
                               "Recently established well; time series too short for trend analysis",
                             is.na(trend_line_int) & (percent_missing >= 25 | last_date < latest_date) ~
                               "Too many missing observations to perform trend analysis",
                             TRUE ~ state),
           category = case_when(state %in% c("Increasing", "Stable") ~ "Stable or Increasing", 
                                grepl("Recently|missing", state) ~ "N/A",
                                TRUE ~ state)) %>% 
    filter(!(state == "Too many missing observations to perform trend analysis" & last_date < latest_date)) %>% 
    select(Well_Num, 
           region_name, 
           start_year, end_year,
           start_date, last_date, nYears, percent_missing, trend_line_int, trend_line_slope, sig, state, category) %>%
    mutate(time_scale = time_period, period = "Yearly", month = "NA") %>%
    select(Well_Num, everything())

  
}


## Define function to produce monthly trend results
summary_function_monthly <- function(df, latest_date, MK_method, time_period, well_attributes){
  
  #Produce summary statistics
  welldata_attr <- df %>%
    group_by(EMS_ID, Well_Num) %>%
    summarise(dataStart = as.Date(min(Date)), 
              dataEnd = as.Date(max(Date)), 
              dataYears = as.numeric(dataEnd - dataStart) / 365, 
              nObs = n(), 
              nMissing = length(mean_GWL[nReadings == 0]), #Changed to mean_GWL
              percent_missing = round(nMissing/nObs*100, 1))
  
  ## Only use wells with relatively current data and 
  ## less than 25% missing monthly observations
  # Note wells were previously filtered by more than 10 years of data prior to the
  # time series analysis in the 02_clean.R script
  wells_nums <- filter(welldata_attr, 
                       percent_missing < 25, 
                       dataEnd > latest_date) %>% 
    pull(Well_Num)
  
  #Separate out wells by month
  # all months
  months = c(1:12)
  
  bymonth_ts <- lapply(1:length(months), function(x) {
    
    df %>%
      filter(., Month==x) 
    
  })
  
  ## Perform the analysis
  # by month
  results_bymonth <- lapply(1:length(bymonth_ts), function(x) {
    
    gwl_zyp_test(dataframe = bymonth_ts[[x]], byID = "Well_Num", 
                 col = "mean_GWL", method = "both") %>%
      mutate(Well_Num = Well_Num) %>%
      filter(test_type == MK_method)
    
  })
  
  wells_results_bymonth <- lapply(1:length(bymonth_ts), function(x) {
    
    full_join(results_bymonth[[x]], welldata_attr, by="Well_Num")
    
  })
  
  wells_results_bymonth <- lapply(1:length(bymonth_ts), function(x) {
    
    mutate(wells_results_bymonth[[x]],
           state = case_when(trend >= 0.1 & sig < 0.05 ~ "Large Rate of Decline",
                             trend >= 0.03 & trend < 0.1 & sig < 0.05 ~ "Moderate Rate of Decline",
                             trend <= -0.03 & sig < 0.05 ~ "Increasing",
                             TRUE ~ "Stable"))
    
  })
  
  results_out_bymonth <- lapply(1:length(bymonth_ts), function(x) {
    
    left_join(well_attributes, wells_results_bymonth[[x]], by=c("Well_Num"="Well_Num")) %>%
      mutate(dataYears = round(dataYears, 1),
             trend_line_int = round(intercept, 4), 
             trend_line_slope = round(trend, 4),
             sig = round(sig, 4), 
             start_date = dataStart, 
             last_date = dataEnd, 
             nYears = dataYears, 
             percent_missing = round(percent_missing, 1)) %>%
      mutate(Well_Name = paste0("Observation Well #", Well_Num), 
             state = case_when(is.na(trend_line_int) & (start_year > latest_date | is.na(last_date)) ~ 
                                 "Recently established well; time series too short for trend analysis",
                               is.na(trend_line_int) & (percent_missing >= 25 | last_date < latest_date) ~
                                 "Too many missing observations to perform trend analysis",
                               TRUE ~ state),
             category = case_when(state %in% c("Increasing", "Stable") ~ "Stable or Increasing", 
                                  grepl("Recently|missing", state) ~ "N/A",
                                  TRUE ~ state)) %>% 
      filter(!(state == "Too many missing observations to perform trend analysis" & last_date < latest_date)) %>% 
      select(Well_Num, 
             region_name, 
             start_year, end_year,
             start_date, last_date, nYears, percent_missing, trend_line_int, trend_line_slope, sig, state, category) %>%
      mutate(time_scale = time_period, period = "Monthly", month = month.abb[x]) %>%
      select(Well_Num, everything())
    
  })
  
  well_results_out <<- do.call("rbind", results_out_bymonth) 
}

#Produce annual result data set for all years
results_annual <- summary_function_annual(monthlywells_ts, "2013-01-01", "zhang", "All", obs_wells)

#Filter out wells with too many missing observations and end date earlier than the defined "latest date"
#Note this step is necessary because the 10 and 20 year data subsets do not have the necessary
#attributes to pick out the filter conditions in the function (already filtered for date less than 10 years old)
wells_list <- results_annual %>% pull(Well_Num)

#Produce remaining results data sets
results_annual_10 <- summary_function_annual(monthlywells_ts_10, "2013-01-01", "zhang", "10 Years", obs_wells) %>%
  filter(Well_Num %in% wells_list)
results_annual_20 <- summary_function_annual(monthlywells_ts_20, "2013-01-01", "zhang", "20 Years", obs_wells) %>%
  filter(Well_Num %in% wells_list)

results_monthly <- summary_function_monthly(monthlywells_ts_mean, "2013-01-01", "zhang", "All", obs_wells) %>%
  filter(Well_Num %in% wells_list)
results_monthly_10 <- summary_function_monthly(monthlywells_ts_10_mean, "2013-01-01", "zhang", "10 Years", obs_wells) %>%
  filter(Well_Num %in% wells_list)
results_monthly_20 <- summary_function_monthly(monthlywells_ts_20_mean, "2013-01-01", "zhang", "20 Years", obs_wells) %>%
  filter(Well_Num %in% wells_list)

#Produce output files
#Results table for shiny app
results_for_app <- rbind(results_annual, results_annual_10, results_annual_20, results_monthly, results_monthly_10, results_monthly_20) %>%
  select(Well_Num, region_name, trend_line_slope, trend_line_int, state, category, period, month, time_scale) 

#Results table for RMarkdown charts (all data annual means only)
results_out <- left_join(results_annual, obs_wells, by=c("Well_Num" = "Well_Num", "region_name"="region_name"))

#Results table for RMarkdown
pivot_table <- rbind(results_annual, results_annual_10, results_annual_20) %>%
  mutate(sig_symbol = case_when(sig < 0.01 ~"**",
                                sig >= 0.01 & sig < 0.05 ~"*",
                                sig >= 0.05 | is.na(sig) ~"")) %>%
  mutate(state_short = ifelse(state == "Recently established well; time series too short for trend analysis",
                              "Recently established well", ifelse(state == "Too many missing observations to perform trend analysis",
                                                                  "Too many missing observations", state))) %>% 
  mutate(state_sig = paste0(state_short, sig_symbol)) %>%
  mutate(time_nm = case_when(time_scale == "All" ~ "Results_All",
                             time_scale == "10 Years" ~ "Results_10yrs",
                             time_scale == "20 Years" ~ "Results_20yrs")) %>%
  select(Well_Num, time_nm, state_sig) %>%
  pivot_wider(., names_from = time_nm, values_from = state_sig) %>%
  mutate(no_results = ifelse((Results_All == "Too many missing observations" | Results_All == "Recently established well") &
                               (Results_10yrs == "Too many missing observations" | Results_10yrs == "Recently established well") &
                               (Results_20yrs == "Too many missing observations" | Results_20yrs == "Recently established well"),
                             "remove", "keep"))

results_for_table <- left_join(results_annual, obs_wells, by=c("Well_Num" = "Well_Num", "region_name"="region_name")) %>%
  mutate(Well_Name = paste0("Observation Well #", Well_Num)) %>%
  select(EMS_ID, Well_Num, Well_Name, Aquifer_Type, 
         REGION_NAME = region_name, 
         aquifer_id, Lat, Long, wellDepth_m, waterDepth_m,
         start_date, last_date, nYears, percent_missing) %>%
  right_join(., pivot_table, by=c("Well_Num" = "Well_Num")) %>%
  arrange(REGION_NAME, Well_Num)

#Results spatial file for shiny app and RMarkdown
results_sf <- obs_wells_sf %>%
  select(Well_Num) %>%
  right_join(., results_for_table, by=c("Well_Num" = "Well_Num")) %>%
  select(-no_results)

#Remove wells with no data for RMarkdown table
results_for_table <- results_for_table %>%
  filter(no_results == "keep") %>%
  select(-no_results)

#Write output files
write.csv(results_out, "out/annual_results_all_data.csv")
write.csv(results_for_app, "out/gw_well_results.csv")
write.csv(results_for_table, "out/gw_well_table.csv")
write.csv(monthlywells_ts, "out/GWL_Monthly_Medians.csv")
write.csv(monthlywells_ts_mean, "out/GWL_Monthly_Means.csv")
write.sf(results_sf, "out/gw_well_attributes.gpkg")

## Save results in a temporary directory
save(results_out, file = "./tmp/analysis_data.RData")
save(results_for_app, file = "./tmp/analysis_data_for_app.RData")
save(results_for_table, file = "./tmp/well_data_attributes.RData")
save(results_sf, file = "./tmp/well_data_attributes_sf.RData")




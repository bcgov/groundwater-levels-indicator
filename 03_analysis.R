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

# This script uses on ground water level data which has been processed to have
# 1 observation per month using the 02_clean.R script


# Library & data load, spatial data prep ----------------------------------

## Source package libraries
if (!exists(".header_sourced")) source("header.R")

# Create output folder for shinyapp
if (!dir.exists('app/www')) dir.create('app/www')

# Load saved clean data objects if necessary
if (!exists("monthlywells_ts")) load("./tmp/clean_well_data.RData")
if (!exists("monthlywells_ts_10")) load("./tmp/clean_well_data_10.RData")
if (!exists("monthlywells_ts_20")) load("./tmp/clean_well_data_20.RData")
if (!exists("monthlywells_ts_mean")) load("./tmp/clean_well_data_mean.RData")
if (!exists("monthlywells_ts_10_mean")) load("./tmp/clean_well_data_10_mean.RData")
if (!exists("monthlywells_ts_20_mean")) load("./tmp/clean_well_data_20_mean.RData")
if (!exists("obs_wells")) load("./tmp/clean_well_attr.RData")


## Get the coordinates from the obs_wells object, revert from sf to table

obs_wells_sf = obs_wells_clean %>% 
  bind_cols(
    obs_wells_clean %>% st_transform(crs=4326) %>% 
      st_coordinates()) %>%
  mutate(Lat = round(Y, 4), 
         Long = round(X, 4), 
         wellDepth_m = round(finished_well_depth * 0.3048), #ft to m
         waterDepth_m = round(static_water_level * 0.3048)) %>%
  select(EMS_ID = ems_id, 
         Well_Num = observation_well_number, 
         Aquifer_Type = aquifer_type,
         region_name, 
         aquifer_id,
         Lat, Long, 
         wellDepth_m, waterDepth_m, 
         start_year, end_year)

obs_well_viz <- obs_wells_sf %>%
  st_drop_geometry() 

# Define function to produce annual mean MK trend results -----------------
## 
summary_function_annual <- function(df, latest_date, MK_method, time_period, 
                                    well_attributes, minimum_years, complete_years){
  
  options(dplyr.summarise.inform = FALSE)
  #Produce summary statistics
  welldata_attr <- df %>%
    group_by(EMS_ID, Well_Num) %>%
    summarise(
      dataStart = as.Date(min(Date)),
      dataEnd = as.Date(max(Date)),
      dataYears = as.numeric(dataEnd - dataStart) / 365,
      nObs = n(),
      nMissing = length(med_GWL[nReadings == 0]),
      percent_missing = round(nMissing / nObs * 100, 1)) |> 
    mutate(max_missing_years = case_when(dataYears <= 10 ~ 1,
                                         dataYears > 10 & dataYears <= 14.9  ~ 1,
                                         dataYears > 14.9 & dataYears <= 19.9  ~ 3,
                                         dataYears > 19.9 ~ 5
                     ))


    # Only use wells with relatively current data and less than 25% missing monthly observations
    # Note wells were previously filtered by more than 10 years of data prior to the
    # time series analysis in the 02_clean.R script
    wells_nums <- filter(welldata_attr, 
                         percent_missing < 15, 
                         dataEnd > latest_date,
                         dataYears > minimum_years) %>% 
      pull(Well_Num)
  
  # pull missing years
  missing_years_data <- df |> 
    group_by(EMS_ID, Well_Num, Year) %>%
    mutate(n_months_missing = length(med_GWL[nReadings == 0])) |> 
    ungroup() |> 
    group_by(EMS_ID, Well_Num, Year, n_months_missing) |> 
    summarize(n_months = n()) |> 
    filter(n_months >= 8) |> 
    mutate(year_complete = (n_months_missing/n_months)*100) |> 
    ungroup() |> 
    group_by(EMS_ID, Well_Num) |> 
    mutate(count_exceeds = sum(year_complete > complete_years),
           max = max(year_complete)) |> 
    summarise(max_count = max(count_exceeds)) |> 
    left_join(select(welldata_attr, Well_Num, max_missing_years)) |> 
    ungroup()
  
  missing_years <- missing_years_data |> 
    filter(max_count <= max_missing_years) |> 
    pull(Well_Num)
    
  #Calculate annual means for trend analysis
  annualwells_ts <- df %>%
    filter(Well_Num %in% wells_nums) |> 
    filter(Well_Num %in% missing_years) |> 
    group_by(EMS_ID, Well_Num, Year) |> 
    summarize(mean_GWL = mean(med_GWL), SD = sd(med_GWL), 
              n_months = n()) 
  
  ## Perform the analysis
  results_annual <- gwl_zyp_test(dataframe = annualwells_ts, byID = "Well_Num", 
                                 col = "mean_GWL", method = "both") %>%
    mutate(Well_Num = Well_Num) %>%
    filter(test_type == MK_method) 
  
  wells_results <- full_join(results_annual, welldata_attr, by = "Well_Num")
  
  wells_results <- mutate(wells_results,
                          state = case_when(trend >= 0.1 & sig <= 0.05 ~ "Large Rate of Decline",
                                            trend >= 0.03 & trend < 0.1 & sig <= 0.05 ~ "Moderate Rate of Decline",
                                            trend <= -0.03 & sig <= 0.05 ~ "Increasing",
                                            (trend > -0.03 & trend < 0.03) | sig > 0.05 ~ "Stable and/or Non-significant",
                                            TRUE ~ "NA")) %>%
    mutate(Well_Num = str_remove(Well_Num,'[A-Z]*')) 
  
  b <- left_join(well_attributes, wells_results, by=c("Well_Num"="Well_Num")) %>%
    mutate(trend_line_int = round(intercept, 4), 
           trend_line_slope = round(trend, 4),
           sig = round(sig, 4), 
           start_date = dataStart, 
           last_date = dataEnd, 
           nYears = round(dataYears, 1), 
           percent_missing = round(percent_missing, 1)) %>%
    left_join(select(missing_years_data, Well_Num, max_count),
              by=c("Well_Num"="Well_Num")) |>
    mutate(Well_Name = paste0("Observation Well #", Well_Num), 
           state = case_when(is.na(trend_line_int) & (start_year > latest_date) ~ 
                               "Recently established well; time series too short for trend analysis",
                             last_date < latest_date ~ "Well not active in 2013", 
                             end_year < 2013 ~ "Well not active in 2013",
                             is.na(trend_line_int) & (nYears < minimum_years) ~
                                                        "Insufficient Data",
                             is.na(trend_line_int) & (percent_missing >= 15) ~
                               "Too many missing observations to perform trend analysis",
                             max_count > max_missing_years ~
                               "Large data gaps present in time series",
                             TRUE ~ state)) %>% 
    select(Well_Num, 
           region_name, 
           start_year, end_year,
           start_date, last_date, nYears, percent_missing, trend_line_int, trend_line_slope, sig, state) %>%
    mutate(time_scale = time_period, period = "Yearly", month = "NA") %>%
    select(Well_Num, everything())
  
  b
}

# Define function to produce monthly MK trend results ---------------------

summary_function_monthly <- function(df, latest_date, MK_method, time_period, 
                                     well_attributes, minimum_years){
  
  options(dplyr.summarise.inform = FALSE)
  
  #Produce summary statistics
  welldata_attr <- df %>%
    group_by(EMS_ID, Well_Num, Month) %>%
    summarise(dataStart = as.Date(min(Date)), 
              dataEnd = as.Date(max(Date)), 
              dataYears = as.numeric(dataEnd - dataStart) / 365, 
              nObs = n(), 
              nMissing = length(mean_GWL[nReadings == 0]), #Changed to mean_GWL
              percent_missing = round(nMissing/nObs*100, 1)) |>
    ungroup() |> 
    mutate(max_missing_years = case_when(dataYears <= 10 ~ 1,
                                         dataYears > 10 & dataYears <= 14.9  ~ 1,
                                         dataYears > 14.9 & dataYears <= 19.9  ~ 3,
                                         dataYears > 19.9 ~ 5)) |> 
    unite(well_month, c("Well_Num", "Month"), remove = FALSE) |> 
    left_join(well_attributes, by = "Well_Num")
  
  ## Only use wells with relatively current data and 
  ## less than 25% missing monthly observations
  # Note wells were previously filtered by more than 10 years of data prior to the
  # time series analysis in the 02_clean.R script
  wells_nums <- filter(welldata_attr, 
                       percent_missing < 15, 
                       dataEnd > latest_date,
                       dataYears > minimum_years,
                       nMissing <= max_missing_years) %>% 
    pull(well_month)
  
    bymonth_ts  <- df |> 
      unite(well_month, c("Well_Num", "Month"), remove = FALSE) |>
      filter(well_month %in% wells_nums) 
  
  ## Perform the analysis
  # by month
  results_bymonth <- gwl_zyp_test(dataframe = bymonth_ts, byID = "well_month", 
                 col = "mean_GWL", method = "both") %>%
      #mutate(Well_Num = Well_Num) %>%
      filter(test_type == MK_method)
  
  well_results_bymonth <- full_join(results_bymonth, welldata_attr, by = "well_month") |> 
    mutate(state = case_when(trend >= 0.1 & sig <= 0.05 ~ "Large Rate of Decline",
                             trend >= 0.03 & trend < 0.1 & sig <= 0.05 ~ "Moderate Rate of Decline",
                             trend <= -0.03 & sig <= 0.05 ~ "Increasing",
                             (trend > -0.03 & trend < 0.03) | sig > 0.05 ~ "Stable and/or Non-significant",
                             TRUE ~ "NA")) %>%
    mutate(Well_Num = str_remove(Well_Num,'[A-Z]*'),
           dataYears = round(dataYears, 1),
           trend_line_int = round(intercept, 2),
           trend_line_slope = round(trend, 2),
           sig = round(sig, 4), 
           start_date = dataStart, 
           last_date = dataEnd, 
           nYears = dataYears, 
           percent_missing = round(percent_missing, 1)) %>%
      mutate(Well_Name = paste0("Observation Well #", Well_Num), 
             state = case_when(is.na(trend_line_int) & (start_year > latest_date | is.na(last_date)) ~ 
                                 "Recently established well; time series too short for trend analysis",
                               last_date < latest_date ~ "Well not active in 2013", 
                               end_year < 2013 ~ "Well not active in 2013",
                               is.na(trend_line_int) & (nYears < minimum_years) ~
                                                          "Insufficient Data", 
                               is.na(trend_line_int) & (percent_missing >= 15 | last_date < latest_date) ~
                                 "Too many missing observations to perform trend analysis",
                               nMissing > max_missing_years ~
                                 "Large data gaps present in time series",
                               TRUE ~ state)) %>% 
      select(Well_Num, Month,
             region_name,
             start_year, end_year,
             start_date, last_date, nYears, 
             percent_missing, trend_line_int, trend_line_slope, sig, state) %>%
      mutate(time_scale = time_period, period = "Monthly", month = month.abb[Month]) %>%
      select(-Month) |> 
      select(Well_Num, everything())
      
      well_results_bymonth
}


# Conduct MK trending analysis  ---------------------------------------------------

## Produce annual result data set for all years
results_annual <- summary_function_annual(monthlywells_ts, "2012-12-31", "zhang",
                                          "All", obs_well_viz, 9.9, 34)

## Produce remaining results data sets
results_annual_10 <- summary_function_annual(monthlywells_ts_10, "2012-12-31",
                                             "zhang", "10 Years", obs_well_viz, 9.9, 34) 
results_annual_20 <- summary_function_annual(monthlywells_ts_20, "2012-12-31",
                                             "zhang", "20 Years", obs_well_viz, 17.9, 34) 

results_monthly <- summary_function_monthly(monthlywells_ts_mean, "2012-12-31",
                                            "zhang", "All", obs_well_viz, 9.9) 
results_monthly_10 <- summary_function_monthly(monthlywells_ts_10_mean, "2012-12-31",
                                               "zhang", "10 Years", obs_well_viz, 9.9) 
results_monthly_20 <- summary_function_monthly(monthlywells_ts_20_mean, "2012-12-31",
                                               "zhang", "20 Years", obs_well_viz, 17.9) 


# Produce output files ----------------------------------------------------

#Results table for shiny app
results_for_app <- rbind(results_annual, results_annual_10, results_annual_20, results_monthly, results_monthly_10, results_monthly_20) %>%
  select(Well_Num, region_name, trend_line_slope, trend_line_int, sig, state, period, month, time_scale) |> 
  mutate(sig_state = case_when(sig <= 0.05 ~"Significant",
                               sig > 0.05 ~ "Not Significant"))

#Results table for RMarkdown charts (all data annual means only)
results_out <- left_join(results_annual, obs_well_viz, by=c("Well_Num" = "Well_Num", "region_name"="region_name"))

#Results table for RMarkdown
pivot_table <- rbind(results_annual, results_annual_10, results_annual_20) %>%
  mutate(sig_symbol = case_when(sig < 0.01 ~"**",
                                sig >= 0.01 & sig <= 0.05 ~"*",
                                sig > 0.05 | is.na(sig) ~"")) %>%
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

results_for_table <- left_join(results_annual, obs_well_viz, by=c("Well_Num" = "Well_Num", "region_name"="region_name")) %>%
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
## CHRIS CODE ADDENDUM - adding this to the www/ folder for shiny app
write_csv(results_for_app, "app/www/gw_well_results.csv")
## CHRIS CODE ADDENDUM end
write.csv(results_for_table, "out/gw_well_table.csv")
write.csv(monthlywells_ts, "app/www/GWL_Monthly_Medians.csv")
write.csv(monthlywells_ts_mean, "app/www/GWL_Monthly_Means.csv")
write_sf(results_sf, "app/www/gw_well_attributes.gpkg")

## Save results in a temporary directory
save(results_out, file = "./tmp/analysis_data.RData")
save(results_for_app, file = "./tmp/analysis_data_for_app.RData")
save(results_for_table, file = "./tmp/well_data_attributes.RData")
save(results_sf, file = "./tmp/well_data_attributes_sf.RData")
save(results_monthly, file = "./tmp/monthly_results_all.RData")
save(results_annual_10, file = "./tmp/results_annual_10.RData")
save(obs_well_viz, file = "./tmp/obs_well_viz.RData")

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

## Source package libraries
if (!exists(".header_sourced")) source("header.R")

## Load saved raw data if necessary
if (!exists("wells_data_raw")) load("./tmp/raw_well_data.RData")
if (!exists("obs_wells")) load("./tmp/clean_attr_data.RData")

## Clean raw groundwater level data
wells_data_prep <- wells_data_raw %>%
  rename("Date" = Time, "GWL" = Value) %>%
  mutate(Well_Num = substring(myLocation, 3,5)) %>%
  filter(Approval == "Approved" | Approval == "Validated")

#Calculate start and end dates
well_data_range <- wells_data_prep %>%
  group_by(Well_Num) %>% 
  summarise(dataStart = as.Date(min(Date)), 
            dataEnd = as.Date(max(Date)), 
            dataYears = as.numeric(dataEnd - dataStart) / 365) 

#Keep only wells with >= 10 years of data for trend analysis
#This prevents errors with the make_wells_ts function
wells_over_10 <- well_data_range %>%
  filter(dataYears >= 10) %>%
  pull(Well_Num)

wells_data_filtered <- wells_data_prep %>%
  filter(Well_Num %in% wells_over_10)

# Save list of wells with data for use in filtering well attribute data set
obs_wells_clean <- right_join(obs_wells, well_data_range, by=c("observation_well_number" = "Well_Num")) %>%
  mutate(start_year = year(dataStart), end_year = year(dataEnd)) 

# Nest data by Well_Num. As we don't have EMS_IDS, use Well_Num
# so we get a clear idea of which well has convergence issues
wells_prep <- wells_data_filtered %>%
  filter(Date <= as.POSIXct("2023-01-01")) %>% 
  mutate(EMS_ID = Well_Num) %>%  
  group_by(Well_Num1 = Well_Num) %>%
  nest()

# well data in last 10 years
wells_prep_10 <- wells_data_filtered %>%
  filter(Date <= as.POSIXct("2023-01-01") & Date >= as.POSIXct("2013-01-01") ) %>% 
  mutate(EMS_ID = Well_Num) %>%  
  group_by(Well_Num1 = Well_Num) %>%
  nest()

# well data in last 20 years
wells_prep_20 <- wells_data_filtered %>%
  filter(Date <= as.POSIXct("2023-01-01") & Date >= as.POSIXct("2003-01-01") ) %>% 
  mutate(EMS_ID = Well_Num) %>%  
  group_by(Well_Num1 = Well_Num) %>%
  nest()

# # Original script:
# # Create monthly time series for each well. 

# Get time series, remove consecutive strings of missing values from the
# beginning and end of each time series, interpolate over missing values

#This step takes ~20-30 minutes

wells_month <- mutate(wells_prep, data = map(data, ~monthly_values(.x)))
wells_ts <- mutate(wells_month, data = map(data, ~make_well_ts(.x)))

# =========================================================================== #
# CHRIS CODE ADDENDUM - I think we can skip the following bits - just filter
# the dataset we've created above #

# ## last 10 years
# wells_month_10 <- mutate(wells_prep_10, data = map(data, ~monthly_values(.x)))
# wells_ts_10 <- mutate(wells_month_10, data = map(data, ~make_well_ts(.x)))
# 
# ## last 20 years
# wells_month_20 <- mutate(wells_prep_20, data = map(data, ~monthly_values(.x)))
# wells_ts_20 <- mutate(wells_month_20, data = map(data, ~make_well_ts(.x)))

# Take the 'data' dataframe that was nested out and filter for last 10 years.
wells_month_10 = wells_month$data |> 
  bind_rows() |> 
  filter(Year >= lubridate::year(Sys.Date()) - 10) |> 
  mutate(Well_Num1 = Well_Num) |>
  group_by(Well_Num1) |>
  # group_by(Well_Num) |> 
  dplyr::group_nest() #Unsure if necessary, but this makes the data like it was before.

wells_ts_10 <- mutate(wells_month_10, data = map(data, ~make_well_ts(.x)))

# Same thing but for last 20 years.
wells_month_20 = wells_month$data |> 
  bind_rows() |> 
  filter(Year >= lubridate::year(Sys.Date()) - 20) |> 
  mutate(Well_Num1 = Well_Num) |>
  group_by(Well_Num1) |>
  # group_by(Well_Num) |> 
  dplyr::group_nest() #Unsure if necessary, but this makes the data like it was before.

wells_ts_20 <- mutate(wells_month_20, data = map(data, ~make_well_ts(.x)))


# CHRIS CODE ADDENDUM END #
# =========================================================================== #

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

## last 10 years
wells_ts_10 = wells_ts_10$data %>% 
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

## last 20 years
wells_ts_20 = wells_ts_20$data %>% 
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

# last 10 years
monthlywells_ts_10 <- unnest(wells_ts_10, data)

# last 20 years
monthlywells_ts_20 <- unnest(wells_ts_20, data)

#Do not filter for now

# wells_with_data_issues = monthlywells_ts %>% 
#   filter(data_missing == T) %>% 
#   select(EMS_ID) %>% 
#   distinct() %>% 
#   pull(EMS_ID)
# 
# wells_with_data_issues_10 = monthlywells_ts_10 %>% 
#   filter(data_missing == T) %>% 
#   select(EMS_ID) %>% 
#   distinct() %>% 
#   pull(EMS_ID)
# 
# monthlywells_ts = monthlywells_ts %>% 
#   filter(!EMS_ID %in% wells_with_data_issues)
# 
# monthlywells_ts_10 = monthlywells_ts_10 %>% 
#   filter(!EMS_ID %in% wells_with_data_issues_10)

###########
#Add calculation of mean monthly values for monthly summaries in addition to median values for annual mean

#Define new function based on monthly_values function (add to bcgroundwater package?)
monthly_values_mean <- function(df) {
  
  if (!is.data.frame(df)) stop("df must be a dataframe")
  
  monthlywell <- df %>%
    dplyr::group_by(.data$EMS_ID, .data$Well_Num, 
                    Year = lubridate::year(.data$Date),
                    Month = lubridate::month(.data$Date)) %>%
    dplyr::mutate(Date = dplyr::case_when(length(.data$Well_Num) < 5 ~ 
                                            lubridate::round_date(.data$Date, "month"),
                                          length(.data$Well_Num) >= 5 ~ 
                                            lubridate::floor_date(.data$Date, "month"))) %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(.data$EMS_ID, .data$Well_Num, .data$Date) %>% 
    dplyr::summarize(mean_GWL = mean(.data$GWL), #Changed from median to mean here
                     nReadings = length(.data$Well_Num)) %>% 
    dplyr::mutate(Year = lubridate::year(.data$Date), 
                  Month = lubridate::month(.data$Date)) %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(.data$EMS_ID, .data$Well_Num, .data$Year) %>%
    dplyr::mutate(dev_mean_GWL = .data$mean_GWL - mean(.data$mean_GWL)) %>% #Changed from median to mean here
    dplyr::ungroup()
  
  #   TODO: May want to make these values flipped in sign, then would have to 
  #   remove the scale_y_reverse in gwlMonthlyPlot 
  
  return(monthlywell)
}

#Define new function based on make_well_ts function (add to bcgroundwater package?)
make_well_ts_mean <- function(df, trim = TRUE, head = 0.1, tail = 0.9 , n_consec = 4) {
  
  well <- df[1, "EMS_ID"]
  
  ## Turn monthlies into yearmon data type from package {zoo}
  df$yearmonth <- zoo::as.yearmon(df$Date)
  
  ## Create a full sequence of months for the timespan of the data
  well.seq <- data.frame(yearmonth =
                           zoo::as.yearmon(seq(from = zoo::as.Date(min(df$yearmonth)),
                                               to = zoo::as.Date(max(df$yearmonth)),
                                               by = "month")))
  
  ## Join the monthly sequence to the well level data to fill missing values 
  ## with NAs so we can create a time series
  well.ts <- merge(well.seq, df, by = "yearmonth", all = TRUE)
  
  if (trim) {
    missings <- as.integer(is.na(well.ts$mean_GWL)) #Changed from med_GWL to mean_GWL here
    start_end <- trim_cons_runs(missings, val = 1L, head = head, tail = tail, n_consec = n_consec)
    well.ts <- well.ts[start_end$start:start_end$end, , drop = FALSE]
  }
  
  # Interpolate missing values - see StackOverflow question here:
  # http://stackoverflow.com/questions/4964255/interpolate-
  # missing-values-in-a-time-series-with-a-seasonal-cycle
  # Answer by Rob Hyndman
  x <- stats::ts(well.ts$mean_GWL, frequency = 12) #Changed from med_GWL to mean_GWL here
  
  #Check for convergence fitting the time series model
  struct <- suppressWarnings(stats::StructTS(x))
  if (struct$code != 0) {
    print(paste0("Convergence code for well ", well, " returned ", struct$code,
                 ": ", struct$message))
  }
  
  well.ts$fit <- as.vector(stats::ts(rowSums(stats::tsSmooth(struct)[, -2])))
  # Fill in missing values
  
  well.ts <- dplyr::mutate(well.ts, 
                           Date = zoo::as.Date(.data$yearmonth),
                           Year = lubridate::year(.data$yearmonth),
                           Month = lubridate::month(.data$yearmonth),
                           mean_GWL = replace(.data$mean_GWL, #Changed from med_GWL to mean_GWL here
                                             is.na(.data$mean_GWL), 
                                             .data$fit[is.na(.data$mean_GWL)]),
                           nReadings = replace(.data$nReadings, is.na(.data$nReadings), 0))
  
  for (col in c("EMS_ID", "Well_Num")) {
    well.ts[, col] <- zoo::na.locf(well.ts[, col])
  }
  
  return(well.ts)
}

# =========================================================================== #
# CHRIS CODE ADDENDUM - I think the follow 6 tables are replicates
# of the tables we generated above - they just have column names that say 
# 'median' instead of 'mean'...
# 
# wells_month_mean <- mutate(wells_prep, data = map(data, ~monthly_values_mean(.x)))
# wells_ts_mean <- mutate(wells_month_mean, data = map(data, ~make_well_ts_mean(.x)))
# 
# ## last 10 years
# wells_month_10_mean <- mutate(wells_prep_10, data = map(data, ~monthly_values_mean(.x)))
# wells_ts_10_mean <- mutate(wells_month_10_mean, data = map(data, ~make_well_ts_mean(.x)))
# 
# 
# ## last 20 years
# wells_month_20_mean <- mutate(wells_prep_20, data = map(data, ~monthly_values_mean(.x)))
# wells_ts_20_mean <- mutate(wells_month_20_mean, data = map(data, ~make_well_ts_mean(.x)))

# # Unnest data for full timeseries
# monthlywells_ts_mean <- unnest(wells_ts_mean, data)
# 
# # last 10 years
# monthlywells_ts_10_mean <- unnest(wells_ts_10_mean, data)
# 
# # last 20 years
# monthlywells_ts_20_mean <- unnest(wells_ts_20_mean, data)

# CHRIS CODE ADDENDUM END #
# =========================================================================== #

#Add on data gap checking later if desired

## Save clean data object in a temporary directory
save(monthlywells_ts, file = "./tmp/clean_well_data.RData")
save(monthlywells_ts_10, file = "./tmp/clean_well_data_10.RData")
save(monthlywells_ts_20, file = "./tmp/clean_well_data_20.RData")
# save(monthlywells_ts_mean, file = "./tmp/clean_well_data_mean.RData")
# save(monthlywells_ts_10_mean, file = "./tmp/clean_well_data_10_mean.RData")
# save(monthlywells_ts_20_mean, file = "./tmp/clean_well_data_20_mean.RData")
save(obs_wells_clean, file = "./tmp/clean_well_attr.RData")

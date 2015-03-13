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

###############################################################################
# This script works on data that has been downloaded from the GWL tool 
# (http://www.env.gov.bc.ca/wsd/data_searches/obswell/index.html) and has been 
# processed to have 1 observation per month using the 01_clean.R script. 
# For testing purposes, a copy of the cleaned, monthly data is available at: 
# http://www.data.gov.bc.ca/dbc/catalogue/detail.page?config=dbc&P110=recorduid:179324
###############################################################################

###############################################################################
# Load and prepare the data for analysis
###############################################################################

# Load required packages
library(dplyr)
library(bcgroundwater)

## Source a few handy functions we will need
source("func.R")

# Summarise as mean annual values
annualwells.ts <- monthlywells.ts %>%
  group_by(EMS_ID, Well_Num, Year) %>%
  summarize(mean_GWL = mean(med_GWL), SD = sd(med_GWL))

# Generate summary data for each well
welldata_attr <- monthlywells.ts %>%
  group_by(EMS_ID, Well_Num) %>%
  summarise(dataStart = as.Date(min(Date)), 
            dataEnd = as.Date(max(Date)), 
            dataYears = as.numeric(dataEnd - dataStart) / 365, 
            nObs = n(), 
            nMissing = length(med_GWL[nReadings == 0]), 
            percent_missing = round(nMissing/nObs*100, 1))

###############################################################################
# Analysis of mean annual groundwater levels, using a Mann-Kendall trend test
# with trend-free prewhitening, as implemented in the package 'zyp'. Methods are 
# documented here: 
# http://www.env.gov.bc.ca/soe/archive/print_ver/water/2014_GWL_Trends_methods.pdf
###############################################################################

# Only use wells with relatively current data, more than 10 years of data, and 
# less than 25% missing monthly observations
latest_date <- "2004-01-01"
wells.for.analysis <- welldata_attr$Well_Num[welldata_attr$dataYears >= 10 &
                                             welldata_attr$percent_missing < 25 & 
                                             welldata_attr$dataEnd > latest_date]

annual.data.for.analysis <- annualwells.ts[annualwells.ts$Well_Num %in% 
                                             wells.for.analysis,]

# Perform the analysis
results.annual <- gwlZypTest(dataframe=annual.data.for.analysis, by = "Well_Num", 
                             col="mean_GWL", method="both")

# Join the analysis results to the well summary data
wells_results <- merge(results.annual[results.annual$test_type=="yuepilon",], 
                       welldata_attr, by="Well_Num", all=TRUE)

# Assign each well to a trend category according to the slope and significance 
# of the trend
wells_results$state <- with(wells_results, 
                            ifelse(trend >= 0.1 & sig < 0.05, 
                                   "Large rate of decline", 
                                   ifelse(trend < 0.1 & trend >= 0.03 & sig < 0.05, 
                                          "Moderate rate of decline", 
                                          ifelse(trend <= -0.03 & sig < 0.05, 
                                                 "Increasing", "Stable"))))

## Join this to the attributes from DataBC:
results.out <- merge(obs_wells_attr, wells_results, 
                     by.x = "OBSERVATION_WELL_NUMBER", by.y = "Well_Num", 
                    all.x = FALSE, all.y = TRUE)

attr.out <- with(results.out, data.frame(
  EMS_ID = EMS_ID, 
  Well_Num = OBSERVATION_WELL_NUMBER, 
#  Well_Name = gsub("Obs Well ", "Observation Well #", simpleCap(STATION_NAME)), 
#   Aquifer_Type = ifelse(is.na(AQUIFER_LITHOLOGY_CODE), "Unknown", 
#                         ifelse(AQUIFER_LITHOLOGY_CODE == "BED", "Bedrock", 
#                                ifelse(AQUIFER_LITHOLOGY_CODE == "UNC", "Sand and Gravel", 
#                                       "Unknown"))), 
  REGION_NM = REGION_NM, 
  AREA_NAME = AREA_NAME, 
  Lat = round(LATITUDE,4), 
  Long = round(LONGITUDE,4), 
  wellDepth_m = round(DEPTH_WELL_DRILLED*0.3048), 
  waterDepth_m = round(WATER_DEPTH*0.3048), 
  start_date = dataStart, 
  last_date = dataEnd, 
  nYears = dataYears, 
  percent_missing = percent_missing, 
  trend_line_int = round(intercept,4), 
  trend_line_slope = round(trend,4), 
  sig = sig, 
  state = ifelse(trend >= 0.1 & sig < 0.05, "Large rate of decline", 
                 ifelse(trend < 0.1 & trend >= 0.03 & sig < 0.05, 
                        "Moderate rate of decline", 
                        ifelse(trend <= -0.03 & sig < 0.05, "Increasing", 
                               "Stable"))), 
stringsAsFactors = FALSE
))

attr.out$state = with(attr.out, 
                      ifelse((is.na(trend_line_int) & nYears < 10) | 
                               is.na(last_date), 
                             "Recently established well; time series too short for trend analysis.", 
                             ifelse(is.na(trend_line_int) & 
                                      (percent_missing >= 25 | last_date < latest_date), 
                                    "Too many missing observations to perform trend analysis.", state)))

attr.out$category <- ifelse(attr.out$state %in% c("Increasing", "Stable"), 
                            "Stable or Increasing", 
                            ifelse(grepl("Recently|missing", attr.out$state), 
                                   "N/A", attr.out$state))







################################################################################
# Plotting results
################################################################################

# Set the well number and subset the data 
# (this can be put in a 'for' loop to plot all wells)
well <- "329"
plotdata <- monthlywells.ts[monthlywells.ts$Well_Num == well,]
well.attr <- wells_results[wells_results$Well_Num == well,]

# Plot the groundwater time series with the calculated trend
areaplot <- gwlAreaPlot(data=plotdata, trend=well.attr$trend, 
                        intercept=well.attr$intercept, 
                        state=well.attr$state, sig=well.attr$sig, 
                        showInterpolated=TRUE, save=FALSE, 
                        mkperiod="monthly")

# Plot the seasonal variation in the time series for a well
monthplot <- gwlMonthlyPlot(dataframe=plotdata, splines=TRUE, save=FALSE)

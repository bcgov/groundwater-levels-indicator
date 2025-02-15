library(bslib)
library(tidyverse)
library(sf)
library(leaflet)
library(envreportutils)
library(shinyjs)
library(ggtext)
library(htmltools)
library(ggthemes)

## Load data
results_out <- read.csv("www/gw_well_results.csv") %>%
  mutate(state_short = case_when(state == "Recently established well; time series too short for trend analysis" ~ "Insufficient Data",
                              state == "Too many missing observations to perform trend analysis" ~ "Insufficient Data",
                              is.na(state) ~ "Insufficient Data",
                              state== "Well not active in 2013" ~ "Insufficient Data",
                              state=="Large data gaps present in time series" ~ "Insufficient Data",
                              TRUE ~ state)) %>%
  mutate(slope = -1*trend_line_slope) %>% #This is reversed due to how slope is reported (meters below ground surface)
  mutate(state_short = fct_relevel(factor(state_short), 
                                   c("Increasing",
                                     "Stable and/or Non-significant",
                                     "Moderate Rate of Decline",
                                     "Large Rate of Decline",
                                     "Insufficient Data"))) %>%
  mutate(pt_size = case_when(state_short == "Insufficient Data" ~ 2,
                             .default = 5),
         state_add = case_when(state_short == "Stable and/or Non-significant" 
                               & sig_state == "Significant" ~ "Stable",
                               state_short == "Stable and/or Non-significant" 
                               & sig_state == "Not Significant" ~ "Not Significant",
                               state_short == "Stable and/or Non-significant" 
                               & (trend_line_slope > -0.03 & trend_line_slope < 0.03) ~ "Stable and Non-significant",
                               TRUE ~ state_short
         ))

levels(results_out$state_short)

#Define unique states
state_list <- as.data.frame(unique(results_out$state_short))

#Use this to colour markers on map
results_t <- results_out %>%
  mutate(combined = paste0(period, "-", time_scale, "-", month)) %>%
  select(Well_Num, state_short, combined) %>%
  pivot_wider(., names_from = combined, values_from = state_short)

wells_sf <- read_sf("www/gw_well_attributes.gpkg") %>%
  st_transform(crs = 4326) %>%
  select(-Results_All, -Results_10yrs, -Results_20yrs) %>%
  mutate(Well_Num = as.integer(Well_Num))

wells_sf_full <- right_join(wells_sf, results_t, by=c("Well_Num"="Well_Num"))

monthlywells_ts <- read.csv("www/GWL_Monthly_Medians.csv") %>%
  mutate(stat = "median", value = med_GWL) %>%
  select(Well_Num, Year, Date, Month, stat, value, nReadings)

monthlywells_ts_mean <- read.csv("www/GWL_Monthly_Means.csv")  %>%
  mutate(stat = "mean", value = mean_GWL) %>%
  select(Well_Num, Year, Date, Month, stat, value, nReadings)

monthly_readings <- rbind(monthlywells_ts, monthlywells_ts_mean)

regions_sf <- read_sf("data/nr_polygons.gpkg") %>%
  st_transform(crs = 4326) %>%
  mutate(region_name = str_remove_all(REGION_NAME, " Natural Resource Region")) %>%
  mutate(region_name = ifelse(region_name == "Thompson-Okanagan", "Thompson / Okanagan",
                              ifelse(region_name == "Kootenay-Boundary", "Kootenay / Boundary", region_name)))

bbox_list <- lapply(st_geometry(regions_sf), st_bbox)
maxmin <- as.data.frame(matrix(unlist(bbox_list),byrow=T,nrow=nrow(regions_sf)))
names(maxmin) <- names(bbox_list[[1]])

#Add x/y bounds to spatial file
regions_sf <- bind_cols(regions_sf, maxmin)
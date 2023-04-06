## Load input data

results_out <- read.csv("data/gw_well_results.csv") %>%
  mutate(state_short = ifelse(state == "Recently established well; time series too short for trend analysis",
                              "Recently established well", ifelse(state == "Too many missing observations to perform trend analysis",
                                                                  "Too many missing observations", state))) %>%
  mutate(slope = -1*trend_line_slope) %>% #This is reversed due to how slope is reported (meters below ground surface)
  mutate(month = replace(month, is.na(month), "NA"))

#Define unique states
state_list <- as.data.frame(unique(results_out$state_short))

wells_sf <- read_sf("data/gw_well_attributes.gpkg") %>%
  st_transform(crs = 4326) %>%
  select(-Results_All, -Results_10yrs, -Results_20yrs) %>%
  mutate(Well_Num = as.integer(Well_Num))

monthlywells_ts <- read.csv("data/GWL_Monthly_Medians.csv") %>%
  mutate(stat = "median", value = med_GWL) %>%
  select(Well_Num, Year, Date, Month, stat, value, nReadings)

monthlywells_ts_mean <- read.csv("data/GWL_Monthly_Means.csv")  %>%
  mutate(stat = "mean", value = mean_GWL) %>%
  select(Well_Num, Year, Date, Month, stat, value, nReadings)

#monthly_readings <- rbind(monthlywells_ts, monthlywells_ts_mean)

regions_sf <- read_sf("data/nr_polygons.gpkg") %>%
  st_transform(crs = 4326) %>%
  mutate(region_name = str_remove_all(REGION_NAME, " Natural Resource Region")) %>%
  mutate(region_name = ifelse(region_name == "Thompson-Okanagan", "Thompson / Okanagan",
                              ifelse(region_name == "Kootenay-Boundary", "Kootenay / Boundary", region_name)))

#Create bounding boxes for each region
bbox_list <- lapply(st_geometry(regions_sf), st_bbox)
maxmin <- as.data.frame(matrix(unlist(bbox_list),byrow=T,nrow=nrow(regions_sf)))
names(maxmin) <- names(bbox_list[[1]])

#Add x/y bounds to spatial file
regions_sf <- bind_cols(regions_sf, maxmin)

#Define the input data frame for well characteristics
well_attr <- as.data.frame(wells_sf) %>%
  select(Well_Num, REGION_NAME, aquifer_id,
         Lat, Long, start_date, last_date, nYears, percent_missing) %>%
  mutate(start_date = as.character(start_date), last_date = as.character(last_date)) %>%
  mutate(aquifer_id = replace(aquifer_id, is.na(aquifer_id), "NA"))

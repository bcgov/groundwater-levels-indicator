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


# Load data -------------------------------------------------------------------

## Source package libraries
if (!exists(".header_sourced")) source("header.R")

## Load output data from 03_analysis.R if necessary
if (!exists("results_out"))  load("./tmp/analysis_data.RData")
if (!exists("monthlywells_ts")) load("./tmp/clean_well_data.RData")
if (!exists("results_sf")) load("./tmp/well_data_attributes_sf.RData")
if (!exists("results_monthly")) load("./tmp/monthly_results_all.RData")


# NR regions for mapping
regions_sf <- bcmaps::nr_regions() %>%
  st_transform(crs = 4326) %>%
  mutate(region_name = str_remove_all(REGION_NAME, " Natural Resource Region")) %>%
  mutate(region_name = ifelse(region_name == "Thompson-Okanagan", "Thompson / Okanagan",
                              ifelse(region_name == "Kootenay-Boundary", "Kootenay Boundary", region_name)))


# Set color scale
colour.scale <- c("Increasing"="#2c7bb6", "Stable and/or Non-significant"="#abd9e9",
                  "Moderate Rate of Decline"="#fdae61", "Large Rate of Decline"="#d7191c") 



# Select wells analyzed and create categories for visualization -----------------------------

results_viz <- results_out[results_out$category != "N/A",] %>%
  mutate(region_name_short = str_replace(region_name, "( / )|( )", "_"), 
         state = factor(state, levels = c("Increasing", 
                                          "Stable and/or Non-significant",
                                          "Moderate Rate of Decline",
                                          "Large Rate of Decline"),
                        ordered = TRUE),
         category = factor(category, levels = c("Stable or Increasing", 
                                                "Moderate Rate of Decline",
                                                "Large Rate of Decline"),
                           ordered = TRUE),
         col = case_when(
           state == "Stable and/or Non-significant" ~ "#abd9e9",
           state == "Moderate Rate of Decline" ~ "#fdae61",
           state == "Large Rate of Decline" ~ "#d7191c",
           state == "Increasing" ~ "#2c7bb6"
         )) %>%
  mutate(Well_Name = paste0("Observation Well #", Well_Num)) %>%
  select(c(-start_year.y, -end_year.y)) %>%
  rename(start_year = start_year.x) %>%
  rename(end_year = end_year.x)

#save results_viz df to tmp folder for use in gwl.Rmd
save(results_viz, file = "tmp/results_viz.RData")

# Provincial & Regional Summary Plots ------------------------------
#Count the number of wells in each state and calculate the respective proportions

input_summary <- results_viz %>%
  group_by(state,col) %>%
  summarize("count"=n()) %>%
  mutate(total_no_wells = nrow(results_viz)) %>%
  mutate(prop = (count/total_no_wells)*100) %>%
  mutate("no_wells_lab" = ifelse(count>1, paste0(count, " wells"), paste0(count, " well"))) %>%
  mutate(label_x = cumsum(count)) #Calculate the total count of wells for bar graph label

#summary df & provincial summary bar chart of categories
bc_bar_chart <- ggplot(data=input_summary) +
  geom_col(mapping=aes(x=prop, y=state, fill=state), width = 0.4, colour = "black") +
  scale_fill_manual(values=colour.scale) +
  geom_text(aes(x=prop, y=state, label = no_wells_lab), hjust = -0.1) +
  scale_x_continuous(expand = c(0,0)) +
  expand_limits(x=c(0,105)) +
  # labs(title = "Summary of Trends in Groundwater Levels in British Columbia") +
  xlab("Proportion of Wells (%)") + 
  ylab(NULL) +
  theme_soe() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0)) + 
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_y_discrete(breaks = unique(input_summary$state),
                   labels = c("Increasing",
                              "Stable and/or \nNon-significant",
                              "Moderate Rate \nof Decline",
                              "Large Rate \nof Decline"))

bc_bar_chart

svg_px("./out/figs/bc_bar_chart.svg", width = 800, height = 400)
plot(bc_bar_chart)
dev.off()

#Summarize results by region and count wells in each state
input_regional <- results_viz %>%
  group_by(region_name) %>%
  mutate(num_wells=n(), # calculate wells per region
         num_wells_lab = ifelse(num_wells>1, paste0(num_wells, " wells"), paste0(num_wells, " well")),
         prop_tot = (num_wells/length(unique(results_viz$Well_Num)))*100) %>%
  ungroup() %>%
  group_by(region_name, state, col, num_wells, num_wells_lab, prop_tot) %>%
  summarize(count=n()) %>%
  mutate(prop = (count/num_wells)*100) 


require(forcats)
#Create regional summary plot
regional_bar_chart <- ggplot(data=input_regional) +
  geom_col(mapping=aes(x=count, y=fct_reorder(region_name, num_wells), fill=state), width = 0.4, color = "black") +
  scale_fill_manual(values=colour.scale) +
  geom_text(aes(x=num_wells + 5, y=region_name, label = num_wells_lab)) +
  scale_x_continuous(expand = c(0,0)) +
  expand_limits(x=c(0,72)) +
  guides(fill = guide_legend(reverse = TRUE))+
  # labs(title = "Summary of Trends in Groundwater Levels across \nNatural Resource Regions") +
  xlab("Number of Wells") + ylab(NULL) +
  theme_soe() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.direction="horizontal",
        plot.title = element_text(hjust = 0)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_y_discrete(breaks = unique(fct_reorder(input_regional$region_name,
                                               input_regional$num_wells)),
                   labels = c("Cariboo","Kootenay\nBoundary",
                              "Northeast",
                              "Omineca",
                              "Skeena",
                              "South Coast",
                              "Thompson/\nOkanagan",
                              "West Coast"))+ 
  guides(fill = guide_legend(nrow = 2))

regional_bar_chart

svg_px("./out/figs/regional_bar_chart.svg", width = 800, height = 400)
plot(regional_bar_chart)
dev.off()


# Monthly Trend Summary ---------------------------------------------------

monthly_viz <- results_monthly |> 
  group_by(month, state) |> 
  summarise (count = n()) |> 
  filter(state != "Recently established well; time series too short for trend analysis") |> 
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                                          "Oct", "Nov", "Dec")))

monthly_bar_chart <- ggplot(data=monthly_viz) +
  geom_col(mapping=aes(y=count, x= month, fill=state), width = 0.4, color = "black") +
  scale_fill_manual(values=colour.scale) +
  #geom_text(aes(x=num_wells + 5, y=region_name, label = num_wells_lab)) +
  scale_y_continuous(expand = c(0,0)) +
  #expand_limits(x=c(0,72)) +
  #guides(fill = guide_legend(reverse = TRUE))+
  # labs(title = "Summary of Trends in Groundwater Levels across \nNatural Resource Regions") +
  ylab("Number of Wells") + xlab(NULL) +
  theme_soe() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.direction="horizontal",
        plot.title = element_text(hjust = 0)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) 


monthly_bar_chart

#Static map for PDF
results_sf = results_sf %>%
  mutate(significant = case_when(grepl("\\*", Results_All)|Results_All == "Insufficient Data" ~ 1,
                                 .default = 0.1),
         
         result = str_replace(Results_All, "\\*", ""),
         result = str_replace(result, "\\*", "")) %>%
  st_transform(crs = '+proj=longlat +datum=WGS84' ) %>%
  mutate(result = case_when(result %in% c("Too many missing observations","Recently established well") ~ "Insufficient Data",
                            .default = result)) %>%
  mutate(size = case_when(result == "Insufficient Data" ~ 1,
                          .default = 3)) %>%
  mutate(result = fct_relevel(factor(result), 
                              c("Increasing",
                                "Stable and/or Non-significant",
                                "Moderate Rate of Decline",
                                "Large Rate of Decline",
                                "Insufficient Data")))

# #Color scheme
# mypal = colorFactor(palette = c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c", "grey67"),
#                     domain = results_sf,
#                     levels = c("Increasing",
#                                "Stable and/or Non-significant",
#                                "Moderate Rate of Decline",
#                                "Large Rate of Decline",
#                                "Insufficient Data"
#                     ),
#                     ordered = T)
# 
# bounds = st_bbox(results_sf) %>%
#   as.vector()
# 
# leaflet(options =
#           leafletOptions(zoomControl = FALSE)) %>%
#   #setView(lat = 55, lng = -125, zoom = 5) %>%
#   #fitBounds(lng1 = bounds[1], lat1 = bounds[2], lng2 = bounds[3], lat2 = bounds[4]) %>%
#   addTiles(group = "Streets") %>%
#   addPolygons(data = regions_sf,
#               weight=1,
#               color = "black",
#               fill = NA) %>%
#   addCircleMarkers(data = results_sf,
#                    radius = ~size,
#                    color= ~mypal(result), 
#                    opacity = 1,
#                    fillColor = ~mypal(result)) %>%
#  addLegend(pal = mypal,
#            values = ~result,
#            title = "Groundwater Trend",
#            data = results_sf,
#            #className = "info legend solid circle", #Css from original leaflet script
#            opacity = 1,
#            layerId = 'legend',
#            position = 'topright') %>%
#   ## save html to png
#   saveWidget("temp.html", selfcontained = FALSE)
# webshot("temp.html", file = "tmp/static_leaflet.png",
#         cliprect = "viewport", zoom = 8)
 

# Map of Obs Well Trends --------------------------------------------------


prov_map <- ggplot() +
  geom_sf(data = bcmaps::bc_bound(), color = "black", fill= NA) +
  geom_sf(data = regions_sf, color = "black", fill= NA) +
  geom_point(data = results_sf, aes(color = result, geometry=geometry), 
             stat = "sf_coordinates", 
             size =3) +
  scale_color_manual(values = c("Stable and/or Non-significant" = "#abd9e9",
                               "Increasing" = "#2c7bb6",
                               "Moderate Rate of Decline" = "#fdae61",
                               "Large Rate of Decline" = "#d7191c",
                               "Insufficient Data" = "grey67"),
                     guide = guide_legend(theme(title = "", legend.position = "bottom", 
                                                legend.direction = "horizontal"))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal()

prov_map

svg_px("./out/figs/prov_map.svg", width = 400, height = 800)
plot(prov_map)
dev.off()

#save version for rmd
save(bc_bar_chart, regional_bar_chart, prov_map, file = "tmp/figures.RData")

# Individual Obs Well Plots (Web & PDF) ----------------------------------------
well_plots <- monthlywells_ts %>%
  mutate(Well_Num1 = Well_Num) %>% # both top level and nested data need Well_Num
  nest(-Well_Num1) %>% 
  rename(Well_Num = Well_Num1) %>%
  right_join(results_viz, by = c("Well_Num")) %>%
  mutate(colour = col,
         state_chr = as.character(state),
         month_plot = map(data, ~gwl_monthly_plot(dataframe = .x, splines = TRUE,
                                                  save = FALSE)),
         area_plot = pmap(list(data, trend_line_slope, trend_line_int, state_chr, sig),
                          ~gwl_area_plot(data = ..1, trend = ..2, intercept = ..3,
                                         trend_category = ..4, sig = ..5,
                                         showInterpolated = TRUE, save = FALSE,
                                         mkperiod = "annual", 
                                         show_stable_line = FALSE) +
                            theme(plot.title = element_text(lineheight = 1,
                                                            margin = margin(b = -10)),
                                  plot.subtitle = element_blank(),
                                  axis.title.x = element_blank(),
                                  plot.margin = unit(c(5, 1, 2, 5), units = "pt"),
                                  legend.box.spacing = unit(c(0, 0, 0, 0), units = "pt"),
                                  legend.margin = margin(0, 0, 0, 0),
                                  legend.position = "top")))


## Print Obs Well Plots
# for (i in seq_len(nrow(well_plots))) {
#   # # Month plots
#   # svg_px(file.path(status.well,
#   #                  glue("month_", well_plots$Well_Num[i], ".svg")),
#   #        width = 350, height = 220)
#   # plot(well_plots$month_plot[[i]])
#   # dev.off()
#   
#   # Area plots
#   svg_px(file.path(status.well,
#                    glue("area_", well_plots$Well_Num[i], ".svg")),
#          width = 600, height = 200)
#   plot(well_plots$area_plot[[i]])
#   dev.off()
# }


## Map Summary (for PDF print version)------------------------------------------

#get natural resource regions
bc <- bc_bound()
nrr <- nr_regions()
nrr_clip <- ms_clip(nrr, bc)
nrr_simp <-  ms_simplify(nrr_clip) %>% 
  st_transform(4326)

# Save nrr_simp for use in shiny app
write_sf(nrr_simp, "app/www/nr_polygons.gpkg")


#Provincial summary map
styles <- 'feature:all|element:all|saturation:-75' 

# Get British Columbia basemap
# You will likely need to get an API key from google and enable it for the 
# Maps Static API to get basemaps using ggmap. 
# See help in ?ggmap::register_google and/or 
# https://cloud.google.com/maps-platform/
# If you save the key in your .Renviron file as a variable called `GMAP_KEY`
# the following will work, otherwise just supply your key directly.

#ggmap::register_google(Sys.getenv("GMAP_KEY"))
BCextent <- c(-139,48,-114,60)
names(BCextent) <- c("left", "bottom", "right", "top")

fourCorners <- expand.grid(
  as.data.frame(matrix(BCextent, ncol = 2, byrow = TRUE,
                       dimnames = list(NULL, c("Long", "Lat"))))
)

BCcenter <- c(mean(BCextent[c("left","right")]), 
              mean(BCextent[c("top","bottom")]))

#if (!nzchar("GMAP_KEY")) {
  # ggMapBC <- get_stadiamap(bbox = BCextent, zoom = 5, scale = 1, 
  #                          maptype = 'stamen_terrain')
  
  
# } else {
#   ggMapBC <- get_map(location = BCextent, zoom = 5, scale = 1, maptype = "terrain",
#                      source = "stamen")
# }


#tweak df for map plot
results_map_df <- results_out %>% 
  filter(category != "N/A") %>%
  mutate(state = factor(state, 
                           levels = c("Large Rate of Decline",
                                      "Moderate Rate of Decline",
                                      "Stable and/or Non-significant", "Increasing"),
                           ordered = TRUE)) %>% 
  arrange(fct_rev(category)) %>% 
  bind_cols(st_as_sf(., crs = 4326, coords = c("Long", "Lat")) %>% 
              st_transform(3857) %>%
              st_coordinates() %>%
              as_tibble()) 




# Save Plots Objects------------------------------------------------------------

#save plot objects to tmp folder for use in gwl.Rmd


## Individual Observation Well Maps (PDF print version)-------------------------

create_site_maps = FALSE # if TRUE, all individual site maps will be created. If FALSE, code won't be run

if (!dir.exists('tmp/pngs')) dir.create('tmp/pngs')

if (create_site_maps) {

#create list of well maps
wellMaps <- vector("list", length(unique(results_viz$Well_Num)))
names(wellMaps) <- unique(as.integer(results_viz$Well_Num))

for (w in names(wellMaps)) {
  well <- filter(results_viz, as.integer(Well_Num) == as.integer(w))
  # wellMaps[[w]] <- tryCatch(get_stamenmap(center = c(well$Long[1], well$Lat[1]), 
  #                                         zoom = 8, 
  #                                         maptype = 'terrain-background',
  #                                         style = styles), 
  #                           error = function(e) NULL)
  wellMaps[[w]] <- leaflet(options =
                             leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles("OpenStreetMap") %>%
    setView(lng = well$Long[1], lat = well$Lat[1],
            zoom = 12)
}

#individual Obs Well ggmap plots 
well_plots <- well_plots %>% 
  mutate(Well_Num = as.integer(Well_Num))

leaflets = tibble(Well_Num = as.integer(names(wellMaps)), 
                  maps = wellMaps)

well_plots = well_plots %>%
  left_join(leaflets, by = "Well_Num") %>%
  mutate(map_plot = pmap(list(Long, Lat, colour, maps),
                         ~ ..4 %>%
                           addCircleMarkers(lng = ..1, lat = ..2, fillColor = ..3, 
                                            opacity = 1,
                                            fillOpacity = 1,
                                            color = "black",
                                            radius = 6,
                                            weight = 2,
                                            label = Well_Num) |> 
                           addSimpleGraticule(interval = 20)))

  for (i in 1:nrow(well_plots)) {
    library(mapview)
    well_plots$map_plot[[i]] %>%
      mapview::mapshot(file = paste0("tmp/pngs/",well_plots$Well_Num[[i]],".png"))
    print(paste0("Row ", i, " of ", nrow(well_plots)," complete"))
  }

  # 
  # mutate(map_plot = pmap(list(Long, Lat, colour, maps), 
  #                        ~ plot_point_with_inset(long = ..1, lat = ..2,
  #                                                pointColour = ..3,
  #                                                bigMap = ..4,
  #                                                overviewMap = ggMapBC,
  #                                                overviewExtent = BCextent)))
#save for use in .Rmd
save(well_plots, file = "tmp/well_plots.RData")

}


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


## Setup -------------------------------------------------------------------

## Source package libraries
if (!exists(".header_sourced")) source("header.R")

## Load saved data if necessary
if (!exists("results_out"))  load("./tmp/analysis_data.RData")
if (!exists("monthlywells_ts")) load("./tmp/clean_well_data.RData")
if (!exists("results_sf")) load("./tmp/well_data_attributes_sf.RData")

regions_sf <- read_sf("data/nr_polygons.gpkg") %>%
  st_transform(crs = 4326) %>%
  mutate(region_name = str_remove_all(REGION_NAME, " Natural Resource Region")) %>%
  mutate(region_name = ifelse(region_name == "Thompson-Okanagan", "Thompson / Okanagan",
                              ifelse(region_name == "Kootenay-Boundary", "Kootenay / Boundary", region_name)))

## Create Google maps?
create_ggmaps <- TRUE

## Bar chart theme
theme_barcharts <- theme(
  axis.text = element_text(size = 14),
  axis.title = element_blank(), 
  plot.title = element_text(size = 17, hjust = 0.5),
  plot.margin = unit(c(6,8,6,2),"mm")
)

## Plot settings 
colour.scale <- c("Increasing"="#2171b5", "Stable"="#bdd7e7",
                  "Moderate Rate of Decline"="#ff7b7b", "Large Rate of Decline"="#ff0000") 



## Paths for saving plots
# status.bc <- "out/figs/status-bc"
# status.reg <- "out/figs/status-by-reg"
# status.reg.bc <- "out/figs/status-by-reg-bc"
# status.well <- "leaflet_map/well_plots"
# status.reg.all <- "leaflet_map/regional_plots"


# #facet label function
nLabeller <- function(n, singular, sep = " ") {
  suffix <- ifelse(n == 1, singular, paste0(singular,"s"))
  label <- paste(n, suffix, sep = sep)
  label
}

## Select wells analyzed and create factors
results_viz <- results_out[results_out$category != "N/A",] %>%
  mutate(region_name_short = str_replace(region_name, "( / )|( )", "_"), 
         state = factor(state, levels = c("Increasing", 
                                          "Stable",
                                          "Moderate Rate of Decline",
                                          "Large Rate of Decline"),
                        ordered = TRUE),
         category = factor(category, levels = c("Stable or Increasing", 
                                                "Moderate Rate of Decline",
                                                "Large Rate of Decline"),
                           ordered = TRUE),
         col = case_when(
           state == "Stable" ~ "#bdd7e7",
           state == "Moderate Rate of Decline" ~ "#ff7b7b",
           state == "Large Rate of Decline" ~ "#ff0000",
           state == "Increasing" ~ "#2171b5"
         )) %>%
  mutate(Well_Name = paste0("Observation Well #", Well_Num)) %>%
  select(c(-start_year.y, -end_year.y)) %>%
  rename(start_year = start_year.x) %>%
  rename(end_year = end_year.x)

#save results_viz df to tmp folder for use in gwl.Rmd
save(results_viz, file = "tmp/results_viz.RData")

## Provincial & Regional Summary Plots (PDF)------------------------------
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
  xlab("Proportion of Wells (%)") + ylab(NULL) +
  theme_soe() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0)) + 
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_y_discrete(breaks = unique(input_summary$state),
                   labels = c("Increasing",
                              "Stable",
                              "Moderate Rate \nof Decline",
                              "Large Rate \nof Decline"))

#regional summary df
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
                   labels = c("Cariboo","Kootenay/\nBoundary",
                              "Northeast",
                              "Omineca",
                              "Skeena",
                              "South Coast",
                              "Thompson/\nOkanagan",
                              "West Coast"))+ 
  guides(fill = guide_legend(nrow = 2))

svg_px("./out/figs/bc_bar_chart_2023.svg", width = 800, height = 400)
plot(bc_bar_chart)
dev.off()

svg_px("./out/figs/regional_bar_chart_2023.svg", width = 800, height = 400)
plot(regional_bar_chart)
dev.off()

#save version for rmd
save(bc_bar_chart, regional_bar_chart, file = "tmp/figures.RData")

#Static map for PDF
results_sf = results_sf %>%
  mutate(significant = case_when(grepl("\\*", Results_All)|result == "Insufficient Data" ~ 1,
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
                                "Stable",
                                "Moderate Rate of Decline",
                                "Large Rate of Decline",
                                "Insufficient Data")))

#Color scheme
mypal = colorFactor(palette = c("#2171b5", "#bdd7e7", "#ff7b7b", "#ff0000", "grey67"),
                    domain = results_sf,
                    levels = c("Increasing",
                               "Stable",
                               "Moderate Rate of Decline",
                               "Large Rate of Decline",
                               "Insufficient Data"
                    ),
                    ordered = T)

bounds = st_bbox(regions_sf) %>%
  as.vector()

leaflet(options =
          leafletOptions(zoomControl = FALSE)) %>%
  # setView(lat = 55, lng = -125, zoom = 5) %>%
  fitBounds(lng1 = bounds[1], lat1 = bounds[2], lng2 = bounds[3], lat2 = bounds[4]) %>%
  addTiles(group = "Streets") %>%
  addPolygons(data = regions_sf,
              color = "black",
              fillColor = "white",
              weight = 1,
              fillOpacity = 0.2) %>%
  addCircleMarkers(data = results_sf,
                   radius = ~size,
                   color = "black",
                   weight = 0.8,
                   fillOpacity = ~significant,
                   fillColor = ~mypal(result)) %>%
 addLegend(pal = mypal,
           values = ~result,
           title = "Groundwater Trend",
           data = results_sf,
           #className = "info legend solid circle", #Css from original leaflet script
           opacity = 1,
           layerId = 'legend',
           position = 'topright') %>%
  ## save html to png
  saveWidget("temp.html", selfcontained = FALSE)
webshot("temp.html", file = "tmp/static_leaflet.png",
        cliprect = "viewport", zoom = 2)

#Original script for leaflet web app
# sum_data_reg <- results_viz %>%
#   group_by(region_name, region_name_short, category) %>% #Ekaterina changed REGION_NAME to lowercase
#   summarise(frequency = n()) %>%
#   mutate(proportion = frequency/sum(frequency),
#          #region_lab = paste0(gsub("(\\s)","\\\n",
#          #                    gsub("\\s/\\s*", "/\\\n", REGION_NAME)),
#          #                    "\n(", nLabeller(sum(frequency), "well"), ")")) %>%
#          region_lab = paste0(region_name,
#                              "\n(", nLabeller(sum(frequency), "well"), ")")) %>%
#   complete(nesting(region_lab), category,
#            fill = list(frequency = 0, proportion = 0)) #EKaterina took out nesting by region_name as well

# #regional bar chart plot with percentage on y and sample size labels
# regional_bar_chart <- ggplot(sum_data_reg,
#                              aes(x = fct_reorder2(region_lab, category, proportion), 
#                                  y = proportion, fill = category)) + 
#   geom_bar(stat = 'identity', alpha = 0.7) +
#   coord_flip() +
#   scale_fill_manual(name = "", values = colour.scale) +
#   scale_y_continuous(labels = percent_format(accuracy = 1),
#                      expand = c(0, 0), limits = c(0, 1.04),
#                      breaks = seq(0, 1, .2)) +
#   theme_soe() +
#   theme_barcharts +
#   theme(panel.grid.major.y = element_blank(),
#         axis.title.y = element_blank(),
#         legend.text = element_text(size = 16),
#         legend.position = "bottom",
#         legend.direction = "vertical")


# #combined bc & regional bar chart plots with one legend using cowplot + patchwork
# bc_bar_nolegend <- bc_bar_chart + theme(legend.position = 'none')
# regional_nolegend <- regional_bar_chart + theme(legend.position = 'none')
# 
# legend <- ggdraw(get_legend(regional_bar_chart + 
#                               theme(legend.direction = "horizontal")))
# 
# combined_bc_summary <- bc_bar_nolegend + 
#   regional_nolegend - 
#   legend + 
#   plot_layout(ncol = 1, heights = c(5, 0.5)) + 
#   plot_annotation(caption = "*Note that only wells with enough data for trend analysis are included in these figures.", 
#                   theme = theme(plot.caption = element_text(size = 14)))
# 
# 
# ## Save bar chart plots for Web
# #bc bar chart
# png_retina(glue(status.bc, ".png"), width = 500, height = 600)
# plot(bc_bar_chart)
# dev.off()
# 
# svg_px(glue(status.bc, ".svg"), width = 500, height = 600)
# plot(bc_bar_chart)
# dev.off()
# 
# #regional bar chart
# png_retina(glue(status.reg, ".png"), width = 500, height = 600)
# plot(regional_bar_chart)
# dev.off()
# 
# svg_px(glue(status.reg, ".svg"), width = 500, height = 600)
# plot(regional_bar_chart)
# dev.off()
# 
# #bar charts combined bc + regions
# png_retina(glue(status.reg.bc, ".png"), width = 900, height = 600)
# plot(combined_bc_summary)
# dev.off()
# 
# svg_px(glue(status.reg.bc, ".svg"), width = 900, height = 600)
# plot(combined_bc_summary)
# dev.off()


#Individual Summary Plots for Each NR region (Web only)-------------------------

# Saved to a list object
# regional_plots <- sum_data_reg %>%
#   split(.$region_name_short) %>%
#   map(~ ggplot(.) +
#         geom_col(aes(category, proportion, fill = category), alpha = 0.7) +
#         coord_flip() +
#         scale_fill_manual(name = "", values = colour.scale) +
#         scale_y_continuous(labels = percent_format(accuracy = 1),
#                            expand = c(0,0), limits = c(0, 1.04),
#                            breaks = seq(0, 1, .2)) +
#         theme_soe() +
#         theme_barcharts +
#         theme(panel.grid.major.y = element_blank(),
#               legend.position = "none",
#               legend.text = element_text(size = 16),
#               plot.margin = unit(c(6,12,6,2),"mm")))

# To look at one plot in list object:
# regional_plots[["Northeast"]]

# To look at all the plots in the list object:
# walk(regional_plots, ~ plot(.x))

## Save individual regional bar charts
# for (i in seq_along(regional_plots)) {
#   svg_px(file.path(status.reg.all, 
#                    glue("summary_", names(regional_plots)[i], ".svg")), 
#          width = 800, height = 400)
#   plot(regional_plots[[i]])
#   dev.off()
# }


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
                                      "Stable", "Increasing"),
                           ordered = TRUE)) %>% 
  arrange(fct_rev(category)) %>% 
  bind_cols(st_as_sf(., crs = 4326, coords = c("Long", "Lat")) %>% 
              st_transform(3857) %>%
              st_coordinates() %>%
              as_tibble()) 

#lines 169-172 above:
#convert full df to an sf object, transform projection, extract coordinates, 
#bind coordinates back to original df (tx Andy Teucher)

#hard-code colours


#legend_order <- names(colrs)

#source function for aligning sf object with ggmap object.  ## LH - error with this. Replaced with ggmap.
# devtools::source_gist("1467691edbc1fd1f7fbbabd05957cbb5", 
#                       filename = "ggmap_sf.R")
# 
# nrr_simp <-  ms_simplify(nrr_clip) %>% 
#   st_transform(3857)
# 
# #plot
# #original
# summary_map <- ggmap_sf(ggMapBC, extent = "device") + 
#   coord_map(xlim = c(-139, -114), ylim = c(47.8,60)) + 
#   geom_sf(data = nrr_simp, fill = NA, inherit.aes = FALSE, size = 0.15) + 
#   coord_sf(datum = NA) +
#   geom_point(data = results_map_df, aes(x = X, y = Y, fill = state),
#              shape = 21, size = 2, colour = "grey30") + 
#   scale_fill_manual(values = colrs, breaks = legend_order) + 
#   theme(legend.position = "bottom", legend.title = element_blank(),
#         legend.direction = "vertical",
#         legend.text = element_text(colour = "black", size = 11)) +
#   guides(fill = guide_legend(ncol = 2))
# plot(summary_map)

# #summary_map <- ggmap_sf(ggMapBC, extent = "device") + 
# summary_map <- ggmap(ggMapBC, extent="device") + 
#   coord_map(xlim = c(-139, -114), ylim = c(47.8,60)) + 
#   geom_sf(data = nrr_simp, fill = NA, inherit.aes = FALSE, size = 0.15) + 
#   coord_sf(crs=4326) +
#   geom_point(data = results_map_df, aes(x = Long, y = Lat, fill = state),
#              shape = 21, size = 2, colour = "grey30") +
#   scale_fill_manual(values = colrs, breaks = legend_order) + 
#   theme(legend.position = "bottom", legend.title = element_blank(),
#         legend.direction = "vertical",
#         legend.text = element_text(colour = "black", size = 11)) +
#   guides(fill = guide_legend(ncol = 2))
# plot(summary_map)



# #save list of well maps for gwl.Rmd
#   save(summary_map, file="./tmp/map_data.RData")


# Save Plots Objects------------------------------------------------------------

#save plot objects to tmp folder for use in gwl.Rmd



## Individual Observation Well Maps (PDF print version)-------------------------

if (create_ggmaps) {

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
            zoom = 9)
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
                                            label = Well_Num)))

  for (i in 1:nrow(well_plots)) {
    library(mapview)
    ifelse(!dir.exists(file.path("tmp", "pngs")), dir.create(file.path("tmp", "pngs")), FALSE)
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


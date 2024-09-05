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
if (!exists("monthlywells_ts_10")) load("./tmp/clean_well_data_10.RData")
if (!exists("results_sf")) load("./tmp/well_data_attributes_sf.RData")
if (!exists("results_monthly")) load("./tmp/monthly_results_all.RData")
if (!exists("results_annual_10")) load("./tmp/results_annual_10.RData")
if (!exists("obs_wells")) load("./tmp/well_location_data.RData")
if (!exists("obs_well_viz")) load("./tmp/obs_well_viz.RData")

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

results_viz <- results_out %>%
  filter(!is.na(trend_line_int)) |> 
  mutate(region_name_short = str_replace(region_name, "( / )|( )", "_"), 
         state = factor(state, levels = c("Increasing", 
                                          "Stable and/or Non-significant",
                                          "Moderate Rate of Decline",
                                          "Large Rate of Decline"),
                        ordered = TRUE),
         # category = factor(category, levels = c("Stable or Increasing", 
         #                                        "Moderate Rate of Decline",
         #                                        "Large Rate of Decline"),
         #                   ordered = TRUE),
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

save(results_viz, file = "tmp/results_viz.RData")

aquifer_info <- obs_well_viz |> 
  select(Well_Num, Aquifer_Type, region_name, aquifer_id)

results_viz_10 <- results_annual_10 %>%
  filter(!is.na(trend_line_int)) |> 
  mutate(region_name_short = str_replace(region_name, "( / )|( )", "_"), 
         state = factor(state, levels = c("Increasing", 
                                          "Stable and/or Non-significant",
                                          "Moderate Rate of Decline",
                                          "Large Rate of Decline"),
                        ordered = TRUE),
         # category = factor(category, levels = c("Stable or Increasing", 
         #                                        "Moderate Rate of Decline",
         #                                        "Large Rate of Decline"),
         #                   ordered = TRUE),
         col = case_when(
           state == "Stable and/or Non-significant" ~ "#abd9e9",
           state == "Moderate Rate of Decline" ~ "#fdae61",
           state == "Large Rate of Decline" ~ "#d7191c",
           state == "Increasing" ~ "#2c7bb6"
         )) %>%
  left_join(aquifer_info, by=c("Well_Num", "region_name")) |> 
  mutate(Well_Name = paste0("Observation Well #", Well_Num)) 

save(results_viz_10, file = "tmp/results_viz_10.RData")

by_aquifer <- results_viz_10 |> 
  mutate(aquifer_id = case_when(is.na(aquifer_id) ~ Well_Name,
                                TRUE ~ as.character(aquifer_id))) |> 
  group_by(aquifer_id, Aquifer_Type, state) |>
  summarise(n_wells =n()) |> 
  mutate(aquifer_id = paste0(aquifer_id, collapse = ", ")) |> 
  mutate(state = paste0(state, collapse = ", ")) |> 
  ungroup() |> 
  group_by(aquifer_id, Aquifer_Type, state) |> 
  summarise(n_state = n()) |> 
  ungroup() |> 
  mutate(state_figure = case_when(str_detect(state, ",") ~ "Mixed",
         TRUE ~ state))

write_csv(by_aquifer, "results-by-aquifer.csv")
    
#save results_viz df to tmp folder for use in gwl.Rmd
save(by_aquifer, file = "tmp/by_aquifer.RData")

# Provincial & Regional Summary Plots ------------------------------
#Count the number of wells in each state and calculate the respective proportions

input_summary <- by_aquifer %>%
  group_by(state_figure) %>%
  summarize(count=n()) %>%
  mutate(total_aquifers = sum(count)) %>%
  mutate(prop = (count/total_aquifers)*100) %>%
  mutate(aquifer_lab = ifelse(count>1, paste0(count, " aquifers"), paste0(count, " aquifers"))) %>%
  mutate(state_figure = factor(state_figure, levels = c("Increasing", 
                                          "Stable and/or Non-significant",
                                          "Moderate Rate of Decline",
                                          "Large Rate of Decline",
                                          "Mixed"),
                        ordered = TRUE)) 

colour.scale.aquifer <- c("Increasing"="#2c7bb6", "Stable and/or Non-significant"="#abd9e9",
                  "Moderate Rate of Decline"="#fdae61", "Large Rate of Decline"="#d7191c", 
                  "Mixed" = "darkgrey") 

save(input_summary, file = "tmp/input_summary.RData")

#summary df & provincial summary bar chart of categories
bc_bar_chart <- ggplot(data=input_summary) +
  geom_col(mapping=aes(x=count, y=state_figure, fill=state_figure), width = 0.4, colour = "black") +
  scale_fill_manual(values=colour.scale.aquifer,
                    labels = c("Increasing",
                               "Stable and/or \nNon-significant",
                               "Moderate Rate \nof Decline",
                               "Large Rate \nof Decline",
                               "Mixed")) +
  geom_text(aes(x=count, y=state_figure, label = aquifer_lab), hjust = -0.1) +
  scale_x_continuous(expand = c(0,0)) +
  expand_limits(x=c(0,110)) +
  # labs(title = "Summary of Trends in Groundwater Levels in British Columbia") +
  xlab("Number of Aquifers") + 
  ylab(NULL) +
  theme_soe() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0)) + 
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) #+
  # scale_fill_discrete(breaks = unique(input_summary$state),
  #                  labels = c("Increasing",
  #                             "Stable and/or \nNon-significant",
  #                             "Moderate Rate \nof Decline",
  #                             "Large Rate \nof Decline"))

bc_bar_chart

svg_px("./out/figs/bc_bar_chart.svg", width = 800, height = 400)
plot(bc_bar_chart)
dev.off()


by_aquifer_region <- results_viz_10 |> 
  mutate(aquifer_id = case_when(is.na(aquifer_id) ~ Well_Name,
                                TRUE ~ as.character(aquifer_id))) |> 
  group_by(region_name, aquifer_id, Aquifer_Type, state) |>
  summarise(n_wells =n()) |> 
  mutate(aquifer_id = paste0(aquifer_id, collapse = ", ")) |> 
  mutate(state = paste0(state, collapse = ", ")) |> 
  ungroup() |> 
  group_by(region_name, aquifer_id, Aquifer_Type, state) |> 
  summarise(n_state = n()) |> 
  ungroup() |> 
  mutate(state_figure = case_when(str_detect(state, ",") ~ "Mixed",
                                  TRUE ~ state))

# Provincial & Regional Summary Plots ------------------------------
#Count the number of wells in each state and calculate the respective proportions

input_regional <- by_aquifer_region %>%
  group_by(region_name, state_figure) %>%
  summarize(count=n()) %>%
  mutate(total_aquifers = sum(count)) %>%
  mutate(prop = (count/total_aquifers)*100) %>%
  mutate(aquifer_lab = ifelse(count>1, paste0(total_aquifers, " aquifers"), paste0(total_aquifers, " aquifers"))) %>%
  mutate(state_figure = factor(state_figure, levels = c("Increasing", 
                                                        "Stable and/or Non-significant",
                                                        "Moderate Rate of Decline",
                                                        "Large Rate of Decline",
                                                        "Mixed"),
                               ordered = TRUE)) 

colour.scale.aquifer <- c("Increasing"="#2c7bb6", "Stable and/or Non-significant"="#abd9e9",
                          "Moderate Rate of Decline"="#fdae61", "Large Rate of Decline"="#d7191c", 
                          "Mixed" = "darkgrey") 

save(input_regional, file = "tmp/input_regional.RData")

require(forcats)
#Create regional summary plot
regional_bar_chart <- ggplot(data=input_regional) +
  geom_col(mapping=aes(x=count, y=fct_reorder(region_name, total_aquifers), fill=state_figure), 
           width = 0.4, color = "black") +
  scale_fill_manual(values=colour.scale.aquifer) +
  geom_text(aes(x=total_aquifers + 5, y=region_name, label = aquifer_lab), size = 4) +
  scale_x_continuous(expand = c(0,0)) +
  expand_limits(x=c(0,55)) +
  guides(fill = guide_legend(reverse = TRUE, nrow=2))+
  # labs(title = "Summary of Trends in Groundwater Levels across \nNatural Resource Regions") +
  xlab("Number of Aquifers") + ylab(NULL) +
  theme_soe() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.direction="horizontal",
        plot.title = element_text(hjust = 0)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_y_discrete(breaks = unique(fct_reorder(input_regional$region_name,
                                               input_regional$total_aquifers)),
                   labels = c("Cariboo","Kootenay\nBoundary",
                              "Northeast",
                              "Omineca",
                              "Skeena",
                              "South Coast",
                              "Thompson/\nOkanagan",
                              "West Coast"))
regional_bar_chart

svg_px("./out/figs/regional_bar_chart.svg", width = 800, height = 600)
plot(regional_bar_chart)
dev.off()


# Monthly Trend Summary ---------------------------------------------------

monthly_viz <- results_monthly |> 
  filter(state == "Increasing" |
           state == "Stable and/or Non-significant" |
           state =="Moderate Rate of Decline" |
           state =="Large Rate of Decline") |> 
  group_by(month, state) |> 
  summarise (count = n()) |> 
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                                          "Oct", "Nov", "Dec")))

monthly_bar_chart <- ggplot(data=monthly_viz) +
  geom_col(mapping=aes(y=count, x= month, fill=state), width = 0.4, color = "black") +
  scale_fill_manual(values=colour.scale) +
  #geom_text(aes(x=num_wells + 5, y=region_name, label = num_wells_lab)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(labels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                             "Oct", "Nov", "Dec"))+
  #expand_limits(x=c(0,72)) +
  #guides(fill = guide_legend(reverse = TRUE))+
  # labs(title = "Summary of Trends in Groundwater Levels across \nNatural Resource Regions") +
  ylab("Number of Wells") + xlab("Month") +
  theme_soe() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.direction="horizontal",
        plot.title = element_text(hjust = 0)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) 


monthly_bar_chart

svg_px("./out/figs/monthly_bar_chart.svg", width = 400, height = 400)
plot(monthly_bar_chart)
dev.off()

#Static map for PDF
results_sf = results_sf %>%
  mutate(significant = case_when(grepl("\\*", Results_All)|Results_All == "Insufficient Data" ~ 1,
                                 .default = 0.1),
         
         result = str_replace(Results_All, "\\*", ""),
         result = str_replace(result, "\\*", "")) %>%
  st_transform(crs = '+proj=longlat +datum=WGS84' ) %>%
  mutate(result = case_when(result %in% c("Too many missing observations","Recently established well",
                                          "Well not active in 2013", "NA") ~ "Insufficient Data",
                            .default = result)) %>%
  mutate(size = case_when(result == "Insufficient Data" ~ 1,
                          .default = 3)) %>%
  mutate(result = fct_relevel(factor(result), 
                              c("Increasing",
                                "Stable and/or Non-significant",
                                "Moderate Rate of Decline",
                                "Large Rate of Decline",
                                "Insufficient Data"))) |> 
  filter(result != "Insufficient Data")

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
             size =2) +
  scale_color_manual(values = c("Stable and/or Non-significant" = "#abd9e9",
                               "Increasing" = "#2c7bb6",
                               "Moderate Rate of Decline" = "#fdae61",
                               "Large Rate of Decline" = "#d7191c",
                               "Insufficient Data" = "grey67"),
                     guide = guide_legend(theme(title = "", legend.position = "bottom", 
                                                legend.direction = "horizontal"))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  guides(fill = guide_legend(nrow=2))+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.direction="horizontal",
        plot.title = element_text(hjust = 0)) 

prov_map

svg_px("./out/figs/prov_map.svg", width = 500, height = 900)
plot(prov_map)
dev.off()

#save version for rmd
save(bc_bar_chart, regional_bar_chart, prov_map, monthly_bar_chart, file = "tmp/figures.RData")

# Individual Obs Well Plots (Web & PDF) ----------------------------------------
well_plots <- monthlywells_ts %>%
  #mutate(Well_Num1 = Well_Num) %>% # both top level and nested data need Well_Num
  #nest(-Well_Num1) %>% 
  #rename(Well_Num = Well_Num1) %>%
  #nest(Well_Num) |> 
  left_join(results_viz, by = c("Well_Num")) #%>%
  # mutate(colour = col,
  #        state_chr = as.character(state),
  #        month_plot = map(data, ~gwl_monthly_plot(dataframe = .x, splines = TRUE,
  #                                                 save = FALSE)),
  #        area_plot = pmap(list(data, trend_line_slope, trend_line_int, state_chr, sig),
  #                         ~gwl_area_plot(data = ..1, trend = ..2, intercept = ..3,
  #                                        trend_category = ..4, sig = ..5,
  #                                        showInterpolated = TRUE, save = FALSE,
  #                                        mkperiod = "annual", 
  #                                        show_stable_line = FALSE) +
  #                           theme(plot.title = element_text(lineheight = 1,
  #                                                           margin = margin(b = -10)),
  #                                 plot.subtitle = element_blank(),
  #                                 axis.title.x = element_blank(),
  #                                 plot.margin = unit(c(5, 1, 2, 5), units = "pt"),
  #                                 legend.box.spacing = unit(c(0, 0, 0, 0), units = "pt"),
  #                                 legend.margin = margin(0, 0, 0, 0),
  #                                 legend.position = "top")))

well_plots_10 <- monthlywells_ts_10 |> 
  #mutate(Well_Num1 = Well_Num) %>% # both top level and nested data need Well_Num
  #nest(-Well_Num1) %>% 
  #rename(Well_Num = Well_Num1) %>%
  left_join(results_viz_10, by = c("Well_Num")) 
  # mutate(colour = col,
  #        state_chr = as.character(state),
  #        area_plot = pmap(list(data, trend_line_slope, trend_line_int, state_chr, sig),
  #                         ~gwl_area_plot(data = ..1, trend = ..2, intercept = ..3,
  #                                        trend_category = ..4, sig = ..5,
  #                                        showInterpolated = TRUE, save = FALSE,
  #                                        mkperiod = "annual", 
  #                                        show_stable_line = FALSE) +
  #                           theme(plot.title = element_text(lineheight = 1,
  #                                                           margin = margin(b = -10)),
  #                                 plot.subtitle = element_blank(),
  #                                 axis.title.x = element_blank(),
  #                                 plot.margin = unit(c(5, 1, 2, 5), units = "pt"),
  #                                 legend.box.spacing = unit(c(0, 0, 0, 0), units = "pt"),
  #                                 legend.margin = margin(0, 0, 0, 0),
  #                                 legend.position = "top")))
# a <- gwl_aplot(well_plots_10, intercept = well_plots_10$trend_line_int, 
#                slope = well_plots_10$trend_line_slope, sig = well_plots_10$sig, 
#                state_short = well_plots_10$state,
#                well = well, reg = reg)

save(well_plots, file = "tmp/well_plots.RData")

save(well_plots_10, file = "tmp/well_plots_10.RData")

gwl_aplot <- function(data, intercept, slope, sig, state_short, well, reg, trend_type) {
  
  if(nrow(data) >0) {
    maxgwl = max(data$med_GWL, na.rm = TRUE)
    mingwl = min(data$med_GWL, na.rm = TRUE)
    gwlrange = maxgwl - mingwl
    midgwl = (maxgwl + mingwl) / 2
    lims  = c(midgwl + gwlrange, midgwl - gwlrange)
    data$max_lims <- max(lims[1], max(data$med_GWL, na.rm = TRUE) + 5)
    
    plot_data = data %>%
      group_by(Year) %>%
      summarize(
        annual_median = median(med_GWL),
        n_months = n(),
        missing_dat = case_when(any(nReadings == 0) ~ "missing", T ~ "complete"),
        max = quantile(med_GWL, 0.975),
        min = quantile(med_GWL, 0.025)
      ) %>%
      mutate(Date = as.Date(paste0(Year, "-01-01"))) %>%
      select(Date, annual_median, missing_dat, min, max)
  }
  
  int.well = intercept + slope * as.numeric(min(as.Date(data$Date)))
  
  trend_df = data.frame(int.well, slope)
  # print(trend_data)
  # print(trend_df)
  
  plot = ggplot(plot_data) +
    ggtitle(paste0(trend_type, "\nStation Class: ", state_short)) +
    labs(subtitle = paste0(slope, " m/year; p:value ", sig)) +
    geom_errorbar(aes(
      x = as.Date(Date),
      ymin = min,
      ymax = max,
      col = missing_dat
    ), width = 0.3) +
    geom_point(aes(x = as.Date(Date), y = annual_median, col = missing_dat)) +
    scale_x_date(expand = c(0.1, 0.1)) +
    scale_y_reverse(expand = c(0, 0)) +
    coord_cartesian(ylim = lims) +
    scale_colour_manual(
      name = "",
      labels = c(
        'Annual Median (95% Confidence Intervals)',
        'Incomplete Data (Interpolated)'
      ),
      values = c("blue", "#A9A9A9")
    ) +
    theme_minimal() +
    theme(
      text = element_text(colour = "black", size = 13),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line = element_line(colour = "grey50"),
      legend.position = "bottom",
      legend.box =  "horizontal",
      plot.title = element_text(hjust = 0),
      plot.subtitle = element_text(hjust = 0, face = "plain", size = 11)
    ) +
    theme(plot.margin = margin(10, 10, 0, 10, "points")) +
    xlab("Date") +
    ylab("Depth Below Ground (metres)") +
    theme(legend.position = "bottom")
  
  if(unique(state_short) %in% c("Increasing", "Moderate Rate of Decline", "Large Rate of Decline")) {
    plot +
      geom_abline(data = trend_df,
                  aes(intercept = -int.well, slope = slope),
                  col = "orange")
  }
  
  plot
}


status.well <- "out/figs/"

for (well in pdf_wells) {
  if (is.na(well)) next
  
  # wellname <- filter(well_plots, Well_Num == well) %>%
  #   pull(Well_Name)
  
  # aquifer_id <- filter(well_plots, Well_Num == well) %>%
  #   pull(aquifer_id)
  # 
  # cat(paste0('\\subsubsection*{',reg,": ",
  #            knitr_latex_char(wellname),
  #            '}'))
  # 
  # cat(paste0('\\subsubsection*{',"Associated aquifer number: ",
  #            knitr_latex_char(aquifer_id),
  #            '}'))
  #mapplot <- filter(well_plots, Well_Num == well) %>% pull(map_plot)
  # mapplot = readPNG(here(paste0("tmp/pngs/",well,".png")))
  #   mapplot = ggdraw() +
  #   draw_image(mapplot)
  monthplot <- filter(well_plots, Well_Num == well) 
  areaplot_10 <- filter(well_plots_10, Well_Num == well)
  areaplot <- filter(well_plots, Well_Num == well)
  #%>% unnest(col=c(gropd_df))
  # a_10 <- gwl_area_plot(areaplot_10, areaplot_10$trend_line_slope, areaplot_10$trend_line_int,
  #                       areaplot_10$state, areaplot_10$sig, showInterpolated = TRUE, save = FALSE,
  #                       mkperiod = "annual", show_stable_line = FALSE)
  # areaplot <- filter(well_plots, Well_Num == well) #%>% unnest(col=c(data))
  # a_10 <- gwl_area_plot(areaplot)
  
  m <- gwl_monthly_plot(monthplot)
  
  a_10 <- gwl_aplot(areaplot_10, intercept = areaplot_10$trend_line_int, 
                    slope = areaplot_10$trend_line_slope, sig = areaplot_10$sig, 
                    state_short = areaplot_10$state,
                    well = well, reg = reg, trend_type = "10-year Trend (2013-2023) ")
  
  a <- gwl_aplot(areaplot, intercept = areaplot$trend_line_int, 
                 slope = areaplot$trend_line_slope, sig = areaplot$sig, 
                 state_short = areaplot$state,
                 well = well, reg = reg,  trend_type = "All available data")
  
  g <- grid.arrange(#m, a_10, a,
    m + theme(text = element_text(size = 6),
              axis.title.y = element_text(size = 5,
                                          hjust = 0.5,
                                          vjust = 1),
              legend.position = ("bottom"),
              plot.title = element_text(size = 6),
              plot.margin = unit(c(0.5,0.5,0,0.5),"cm")),
    a_10 + theme(axis.text = element_text(size = 4),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 5,
                                             hjust = 0.5,
                                             vjust = 1),
                 legend.text = element_text(size = 5),
                 legend.position = ("bottom"),
                 plot.title = element_text(size = 6),
                 plot.subtitle = element_text(size = 5),
                 plot.margin = unit(c(0.5,0.5,0,0.5),"cm")),
    a + theme(axis.text = element_text(size = 4),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 5,
                                          hjust = 0.5,
                                          vjust = 1),
              legend.text = element_text(size = 5),
              legend.position = ("bottom"),
              plot.title = element_text(size = 6),
              plot.subtitle = element_text(size = 5),
              plot.margin = unit(c(0.5,0.5,0,0.5),"cm")),
    layout_matrix = matrix(c(1,2,3), nrow = 3, byrow = TRUE))
  
  g
  
  ggsave(g, file=paste0(status.well, "combined_fig_", well,".png"), width = 11, height = 15, units = "cm")
  #png(file=paste0(status.well, "combined_fig_", well,".png"), width = 700, height = 1000)
  dev.off()
  print(well)
}
  


# Print Obs Well Plots

# status.well <- "out/figs"
# app_wells_10 <- results_viz_10 |> 
#   pull(Well_Num)
# 
# app_wells <- results_viz |> 
#   pull(Well_Num)
# 
# pdf_wells <- as.integer(intersect(app_wells_10, app_wells))
# 
# well_plots <-  well_plots |> 
#   filter(Well_Num %in% pdf_wells)
# 
# for (i in seq_len(nrow(well_plots$Well_Num))) {
#   # Month plots
#   
#   
#   png_retina(file.path(status.well,
#                    glue("month_", well_plots$Well_Num[i], ".png")),
#          width = 350, height = 220)
#   plot(well_plots$month_plot[[i]])
#   dev.off()
# 
#   # Area plots
#   png_retina(file.path(status.well,
#                    glue("area_", well_plots$Well_Num[i], ".png")),
#          width = 600, height = 200)
#   plot(well_plots$area_plot[[i]])
#   dev.off()
#   
#   # Area plots, 10-yr
#   png_retina(file.path(status.well,
#                    glue("area_10_", well_plots_10$Well_Num[i], ".png")),
#          width = 600, height = 200)
#   plot(well_plots_10$area_plot[[i]])
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

create_site_maps = TRUE # if TRUE, all individual site maps will be created. If FALSE, code won't be run

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


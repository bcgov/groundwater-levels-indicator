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


# Setup -------------------------------------------------------------------

## Source package libraries
if (!exists(".header_sourced")) source("header.R")

## Load saved data if necessary
if (!exists("results_out"))  load("./tmp/analysis_data.RData")
if (!exists("monthlywells_ts")) load("./tmp/clean_well_data.RData")

## Create google maps?
create_ggmaps <- FALSE

## Provincial Summaries

## Select wells analyzed and create factors
results_viz <- results_out[results_out$category != "N/A",] %>%
  mutate(region_name_short = str_replace(REGION_NAME, "( / )|( )", "_"),
         state = factor(state, levels = c("Increasing", 
                                          "Stable",
                                          "Moderate Rate of Decline",
                                          "Large Rate of Decline"),
                        ordered = TRUE),
         category = factor(category, levels = c("Stable or Increasing", 
                                                "Moderate Rate of Decline",
                                                "Large Rate of Decline"),
                           ordered = TRUE))




# Plot Settings -----------------------------------------------------------

## Bar chart theme
theme_barcharts <- theme(
  axis.text = element_text(size = 14),
  axis.title = element_blank(), 
  plot.title = element_text(size = 17, hjust = 0.5),
  plot.margin = unit(c(6,6,6,2),"mm")
)


## Plot theme
# theme_set(theme_classic() +
#             theme(text = element_text(colour = "black"),
#                   axis.line = element_blank(),
#                   axis.ticks = element_blank(),
#                   panel.grid.major = element_line(colour = "grey85", size = 0.5,
#                                                 linetype = 1),
#                   panel.grid.minor = element_line(colour = "grey90", size = 0.5,
#                                                 linetype = 1),
#                   panel.grid.major.x = element_blank(),
#                   panel.spacing = unit(0.6, "lines"),
#                   plot.title = element_text(vjust = 2, hjust = 0.5),
#                   axis.title = element_text(vjust = 0.1),
#                   legend.position = "bottom", legend.title = element_blank(),
#                   legend.text = element_text(size = 12),
#                   axis.text.x = element_blank(),
#                   strip.background = element_blank()))

label.colour <- "black" 
colour.scale <- brewer.pal(3,"Blues")

# Overall summaries of trend categories -------------------------------------

#summary df
sum_data <- results_viz %>%
  group_by(category) %>%
  summarise(frequency = n()) %>%
  arrange(desc(category)) %>%
  mutate(percent = round(frequency/sum(frequency), digits = 2),
         position = cumsum(percent) - percent/2,
         geography = "British Columbia")

#bar chart of category summary
bc_bar_chart <- ggplot(sum_data, aes(x = geography, y = percent, fill = category)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_manual(name = "", values = colour.scale) +
  geom_text(aes(y = position, label = paste0(frequency, " wells")),
            colour = label.colour, size = 5) +
  scale_y_continuous(expand = c(0,0), labels = percent) +
  labs(y = "Percent of Groundwater Wells", x = " ") +
  theme_soe() +
  theme_barcharts +
  theme(panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 16))


#pie chart of category summary
# (pie_plot <- ggplot(results_viz, aes(x = factor(1), fill = category)) + 
#   geom_bar(width = 1) + coord_polar(start = 0, theta = "y") + 
#   scale_fill_manual(values = colour.scale) + 
#   theme(line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), plot.title = element_text(vjust = 0),
#         legend.position = c(0.5,0.01), legend.direction = "horizontal",
#         legend.title = element_blank(), 
#     #    plot.margin = unit(c(rep(0,4)), "cm"),
#         panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
#   geom_text(data = sum_data,
#             aes(x = 1.2, y = pos, label = paste0(per,"%")),
#             colour = label.colour) + 
#   ggtitle("Percentage of Groundwater Wells in Three Different\nCategories of Long-term Trends in Groundwater Levels")
# )

## Summary of categories by region

#facet label function
nLabeller <- function(n, singular, sep = " ") {
  suffix <- ifelse(n == 1, singular, paste0(singular,"s"))
  label <- paste(n, suffix, sep = sep)
  label
}

#regional summary df
sum_data_reg <- results_viz %>%
  group_by(REGION_NAME, region_name_short, category) %>%
  summarise(frequency = n()) %>%
  mutate(proportion = frequency/sum(frequency), 
         #region_lab = paste0(gsub("(\\s)","\\\n", 
         #                         gsub("\\s/\\s*", "/\\\n", REGION_NAME)), 
         #                    "\n(", nLabeller(sum(frequency), "well"), ")")) %>% 
         region_lab = paste0(REGION_NAME,
                            "\n(", nLabeller(sum(frequency), "well"), ")")) %>%
  complete(nesting(REGION_NAME, region_lab), category, fill = list(frequency = 0, proportion = 0))


#bar chart plot with percentage on y and sample size labels
regional_bar_chart <- ggplot(sum_data_reg,
                             aes(x = fct_reorder2(region_lab, category, proportion), 
                                 y = proportion, fill = category)) + 
  geom_bar(stat = 'identity', alpha = 0.7) +
  coord_flip() +
  scale_fill_manual(name = "", values = colour.scale) +
  scale_y_continuous(labels = percent, expand = c(0,0)) +
  theme_soe() +
  theme_barcharts +
  theme(panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        legend.direction = "vertical")


#combined bar chart plots with one legend using cowplot + patchwork
bc_bar_nolegend <- bc_bar_chart + theme(legend.position='none')
regional_nolegend <- regional_bar_chart + theme(legend.position='none')

legend <- ggdraw(get_legend(regional_bar_chart + theme(legend.direction = "horizontal")))

combined_bc_summary <- bc_bar_nolegend + 
  regional_nolegend - 
  legend + 
  plot_layout(ncol = 1, heights = c(5, 1))

# ## Summarize by aquifer type
# 
# #aquifer type df
# sum_data_aq <- results_viz %>% 
#   filter(AQUIFER_TYPE != "Unknown") %>%
#   group_by(AQUIFER_TYPE, category) %>%
#   summarise(Freq = n()) %>%
#   mutate(prop = Freq/sum(Freq),
#          aq_lab = paste0(AQUIFER_TYPE, "\n(",
#                          nLabeller(sum(Freq), "well"), ")"))
# 
# #bar chart plot with percentage on y and sample size labels
# (aq_plot <- ggplot(sum_data_aq, aes(x = category, y = prop, fill = category)) +
#     geom_bar(stat = 'identity') + facet_grid(~ aq_lab) +
#     labs(title = "Trends in Groundwater Levels by Aquifer Type",
#          x = element_blank(), y = "Percent of Groundwater Wells") +
#     scale_fill_manual(values = colour.scale) +
#     scale_y_continuous(labels = percent, limits = c(0,1)) +
#     theme(axis.text.y = element_text(colour = "black"),
#           strip.text = element_text(colour = "black", size = 9))
# )


# ggmap plots ------------------------------------------------------------

## Map Summary (for PDF print version)

if(create_ggmaps) {
  
  #Provincial summary map
  styles <- 'feature:all|element:all|saturation:-75'
  
  # Get British Columbia basemap
  # You will likely need to get an API key from google and enable it for the 
  # Maps Static API to get basemaps using ggmap. 
  # See help in ?ggmap::register_google and/or 
  # https://cloud.google.com/maps-platform/
  # If you save the key in your .Renviron file as a variable called `GMAP_KEY`
  # the following will work, otherwise just supply your key directly.
  ggmap::register_google(Sys.getenv("GMAP_KEY"))
  BCextent <- c(-139,48,-114,60)
  names(BCextent) <- c("left", "bottom", "right", "top")
  fourCorners <- expand.grid(as.data.frame(matrix(BCextent, ncol=2, byrow=TRUE,
                                                  dimnames=list(NULL, c("Long", "Lat")))))
  BCcenter <- c(mean(BCextent[c("left","right")]), 
                mean(BCextent[c("top","bottom")]))
  
  if(Sys.getenv("GMAP_KEY") != ""){
    ggMapBC <- get_googlemap(center=BCcenter, zoom=5, scale=1, 
                             maptype='roadmap', visible=fourCorners, style=styles)
  } else {
    ggMapBC <- get_map(location = BCcenter, zoom = 5, scale = 1, maptype = "terrain",
                       source = "stamen")
  }
  
  #tweak df for map plot
  results_map_df <- results_out %>% 
    mutate(category = recode(category, `N/A` = "Not enough data to-date for trend analysis"),
           category = factor(category, levels = c("Large Rate of Decline",
                                                  "Moderate Rate of Decline",
                                                  "Stable or Increasing",
                                                  "Not enough data to-date for trend analysis"),
                             ordered = TRUE)) %>% 
    arrange(fct_rev(category)) %>% 
    bind_cols(st_as_sf(., crs = 4326, coords = c("Long", "Lat")) %>% 
                st_transform(3857)%>%
                st_coordinates() %>%
                as_tibble()) 
  
  #lines 169-172 above:
  #convert full df to an sf object, transform projection, extract coordinates, 
  #bind coordinates back to original df (tx Andy Teucher)
  
  #hard-code colours
  colrs <- c("Stable or Increasing" = "#deebf7",
             "Moderate Rate of Decline" = "#9ecae1",
             "Large Rate of Decline" = "#3182bd",
             "Not enough data to-date for trend analysis" = "grey80")
  
  legend_order <- c("Stable or Increasing",
                    "Large Rate of Decline",
                    "Moderate Rate of Decline",
                    "Not enough data to-date for trend analysis")
  
  #get natural resource regions
  bc <- bc_bound(class = "sf")
  nrr <- nr_regions(class = "sf")
  nrr_clip <- ms_clip(nrr, bc)
  nrr_simp <-  ms_simplify(nrr_clip) %>% 
    st_transform(3857)
  
  #source function for aligning sf object with ggmap object
  devtools::source_gist("1467691edbc1fd1f7fbbabd05957cbb5", 
                        filename = "ggmap_sf.R")
  
  #plot
  summary_map <- ggmap_sf(ggMapBC, extent = "device") + 
    coord_map(xlim = c(-139, -114), ylim = c(47.8,60)) + 
    geom_sf(data = nrr_simp, fill = NA, inherit.aes = FALSE, size = 0.15) + coord_sf(datum=NA) +
    geom_point(data = results_map_df, aes(x = X, y = Y, fill = category),
               shape = 21, size = 2.5, colour = colour.scale[3]) + 
    scale_fill_manual(values = colrs, breaks = legend_order) + 
    theme(legend.position = "bottom", legend.title = element_blank(),
          legend.direction = "vertical",
          legend.text = element_text(colour = "black", size = 11)) +
    guides(fill=guide_legend(ncol=2))
  plot(summary_map)
  
  
  ## Observation Well Maps (PDF print version)
  
  #create list of well maps
  wellMaps <- list()
  for(w in unique(results_viz$Well_Num)) {
    well <- filter(results_viz, Well_Num == w)
    wellMaps[[w]] <- tryCatch(get_googlemap(center = c(well$Long[1], well$Lat[1]), 
                                            zoom = 8, scale = 1, maptype = 'roadmap', style = styles), 
                              error = function(e) NULL)
  }
  names(wellMaps) <- unique(results_viz$Well_Num)
  
  #save list of well maps for print version
  save(summary_map, file="./tmp/map_data.RData")
}



# Individual plots for each NR region -------------------------------------
# Saved to a list object
regional_plots <- sum_data_reg %>%
  split(.$region_name_short) %>%
  map(~ ggplot(.) +
          geom_col(aes(category, proportion, fill = category), alpha = 0.7) +
          labs(title = .$region_lab) +
          coord_flip() +
          scale_fill_manual(name = "", values = colour.scale) +
          scale_y_continuous(labels = percent, expand = c(0,0)) +
          theme_soe() +
          theme_barcharts +
          theme(panel.grid.major.y = element_blank(),
                legend.position = "bottom",
                legend.text = element_text(size = 16),
                plot.margin = unit(c(6,12,6,2),"mm")))

# To look at one plot in list object:
# regional_plots[["Northeast\n(2 wells)"]]

# To look at all the plots in the list object:
# walk(regional_plots, ~ plot(.x))


# Individual Obs Well Plots ---------------------------------------------------
well_plots <- monthlywells_ts %>%
  mutate(Well_Num1 = Well_Num) %>% # both top level and nested data need Well_Num
  nest(-Well_Num1, -EMS_ID) %>%
  rename(Well_Num = Well_Num1) %>%
  right_join(results_viz, by = c("Well_Num", "EMS_ID")) %>%
  mutate(colour = case_when(category == "Large Rate of Decline" ~ colour.scale[3],
                            category == "Stable or Increasing" ~ colour.scale[1],
                            TRUE ~ colour.scale[2]),
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
                                                            margin = margin(b = -5)),
                                  plot.subtitle = element_text(lineheight = 1,
                                                               margin = margin(t = 0)),
                                  plot.margin = unit(c(5, 1, 2, 5), units = "pt"),
                                  legend.box.spacing = unit(c(2, 0, 0, 0), units = "pt"))))

# +
#   theme(plot.title = element_text(hjust = 0, lineheight = 1,
#                                   margin = margin(b = -10)),
#         plot.subtitle = element_text(hjust = 0, lineheight = 1,
#                                      margin = margin(t = 0)),
#         plot.margin = unit(c(5, 1, 0, 5), units = "pt"),
#         legend.box.margin = margin(0, 0, 0, 0),
#         legend.margin = margin(0, 0, 0, 0),
#         legend.box.spacing = unit(c(0,0,0,0), units = "pt"))


# Individual Obs Well ggmap plots ------------------------------------------
if(create_ggmaps){
  well_plots <- left_join(tibble(Well_Num = names(WellMaps), maps = WellMaps)) %>%
    mutate(map_plot = pmap(Long, Lat, colour, map, 
                           ~plot_point_with_inset(long = ..1, lat = ..2,
                                                  pointColour = ..3,
                                                  bigMap = ..4, overviewMap = ggMapBC,
                                                  overviewExtent = BCextent)))
}
  
#   
#   grid.arrange(mapplot + theme(text = element_text(size = 10),
#                                plot.margin = unit(c(0,0.5,0,0.01),"cm")),
#                monthplot + theme(axis.text = element_text(size = 8),
#                                  axis.title.x = element_text(face = "bold", size = 8),
#                                  plot.title = element_text(size = 10, face = "bold"),
#                                  axis.title.y = element_text(size = 8,
#                                                              hjust = 0.5,
#                                                              vjust = -.8,
#                                                              face = "bold"),
#                                  legend.box = "vertical",
#                                  legend.position = "top",
#                                  legend.text = element_text(size = 9),
#                                  plot.margin = unit(c(1,0.01,1,0.01),"cm"),
#                                  legend.spacing.y = unit(-.4, 'cm')),
#                areaplot + theme(axis.text = element_text(size = 8),
#                                 axis.title.x = element_text(face = "bold", size = 8),
#                                 axis.title.y = element_text(size = 8,
#                                                             hjust = 0.5,
#                                                             vjust = -.8,
#                                                             face = "bold"),
#                                 legend.text = element_text(size = 9),
#                                 plot.title = element_text(size = 11, face = "bold"),
#                                 plot.subtitle = element_text(size = 9),
#                                 plot.margin = unit(c(0.5,0.5,0.5,0.01),"cm")),
#                layout_matrix = matrix(c(1,2,3,3), nrow = 2, byrow = TRUE))
# }  

# Save plots --------------------------------------------------------------


## Save plot objects to tmp folder
# save(pie_plot, regional_plot, aq_plot, file = "tmp/figures.RData")
save(bc_bar_chart, regional_bar_chart, combined_bc_summary,
     regional_plots, well_plots, file = "tmp/figures.RData")

## Save plots as high resolution PNG/SVGs for web
status.bc <- "out/figs/status-bc"
status.reg <- "out/figs/status-by-reg"
# status.aq <- "out/figs/status-by-aq"
status.reg.bc <- "out/figs/status-by-reg-bc"
status.well <- "leaflet_map/well_plots"
status.reg.all <- "leaflet_map/regional_plots"

#bc bar chart
png_retina(glue(status.bc, ".png"), width = 500, height = 600)
plot(bc_bar_chart)
dev.off()

svg_px(glue(status.bc, ".svg"), width = 500, height = 600)
plot(bc_bar_chart)
dev.off()

#regional bar chart
png_retina(glue(status.reg, ".png"), width = 500, height = 600)
plot(regional_bar_chart)
dev.off()

svg_px(glue(status.reg, ".svg"), width = 500, height = 600)
plot(regional_bar_chart)
dev.off()

# #aquifer type bar chart
# png_retina(glue(status.aq, ".png"), width = 440, height = 400)
# plot(aq_plot)
# dev.off()
# 
# svg_px(glue(status.aq, ".svg"), width = 440, height = 400)
# plot(aq_plot)
# dev.off()

#bar charts combined bc + regions
png_retina(glue(status.reg.bc, ".png"), width = 900, height = 600)
plot(combined_bc_summary)
dev.off()

svg_px(glue(status.reg.bc, ".svg"), width = 900, height = 600)
plot(combined_bc_summary)
dev.off()

# Obs Well plots chart
for(i in seq_len(nrow(well_plots))) {
  # Month plots
  # svg_px(file.path(status.well, 
  #                  glue("month_", well_plots$Well_Num[i], ".svg")), 
  #        width = 400, height = 250)
  # plot(well_plots$month_plot[[i]])
  # dev.off()
  
  # Area plots
  svg_px(file.path(status.well,
                   glue("area_", well_plots$Well_Num[i], ".svg")),
         width = 700, height = 275)
  plot(well_plots$area_plot[[i]])
  dev.off()
}

#individual regional bar charts
for(i in seq_along(regional_plots)) {
  svg_px(file.path(status.reg.all, 
                   glue("summary_", names(regional_plots)[i], ".svg")), 
         width = 800, height = 400)
  plot(regional_plots[[i]])
  dev.off()
}

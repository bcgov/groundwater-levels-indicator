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


## Source package libraries
if (!"package:bcgroundwater" %in% search()) source("header.R")


## Load saved data if necessary
if (!exists("results_out"))  load("./tmp/analysis_data.RData")


##########################
## PROVINCIAL SUMMARIES ##
##########################

## Plot theme
theme_set(theme_classic() +
            theme(text = element_text(colour = "black"),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  panel.grid.major = element_line(colour = "grey85", size = 0.5,
                                                linetype = 1),
                  panel.grid.minor = element_line(colour = "grey90", size = 0.5,
                                                linetype = 1),
                  panel.grid.major.x = element_blank(),
                  panel.spacing = unit(0.6, "lines"),
                  plot.title = element_text(vjust = 2, hjust = 0.5),
                  axis.title = element_text(vjust = 0.1),
                  legend.position = "bottom", legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  axis.text.x = element_blank(),
                  strip.background = element_blank()))

label.colour <- "black" 
colour.scale <- brewer.pal(3,"Blues")


## Select wells analyzed and create factors
results_viz <- results_out[results_out$category != "N/A",] %>%
  mutate(state = factor(state, levels = c("Increasing", 
                                          "Stable",
                                          "Moderate Rate of Decline",
                                          "Large Rate of Decline"),
                        ordered = TRUE),
         category = factor(category, levels = c("Stable or Increasing", 
                                                "Moderate Rate of Decline",
                                                "Large Rate of Decline"),
                           ordered = TRUE))


## Overall summary of trend categories

#summary df
sum_data <- results_viz %>%
  group_by(category) %>%
  summarise(Freq = n()) %>%
  arrange(desc(category)) %>%
  mutate(per = round(Freq/sum(Freq)*100),
         pos = cumsum(Freq) - Freq/2)

#pie chart of category summary
(pie_plot <- ggplot(results_viz, aes(x = factor(1), fill = category)) + 
  geom_bar(width = 1) + coord_polar(start = 0, theta = "y") + 
  scale_fill_manual(values = colour.scale) + 
  theme(line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), plot.title = element_text(vjust = 0),
        legend.position = c(0.5,0.01), legend.direction = "horizontal",
        legend.title = element_blank(), 
    #    plot.margin = unit(c(rep(0,4)), "cm"),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  geom_text(data = sum_data,
            aes(x = 1.2, y = pos, label = paste0(per,"%")),
            colour = label.colour) + 
  ggtitle("Percentage of Groundwater Wells in Three Different\nCategories of Long-term Trends in Groundwater Levels")
)

## Summary of categories by region

#facet label function
nLabeller <- function(n, singular, sep = " ") {
  suffix <- ifelse(n == 1, singular, paste0(singular,"s"))
  label <- paste(n, suffix, sep = sep)
  label
}

#regional summary df
sum_data_reg <- results_viz %>%
  group_by(REGION_NAME, category) %>%
  summarise(Freq = n()) %>%
  mutate(prop = Freq/sum(Freq), 
         region_lab = paste0(gsub("(\\s)","\\\n", 
                                  gsub("\\s/\\s*", "/\\\n", REGION_NAME)), 
                             "\n(", nLabeller(sum(Freq), "well"), ")"))

#bar chart plot with percentage on y and sample size labels
(regional_plot <- ggplot(sum_data_reg, aes(x = category, y = prop, fill = category)) + 
    geom_bar(stat = 'identity') + facet_grid(~ region_lab) + 
    labs(title = "Trends in Groundwater Levels by Region", 
         x = element_blank(), y = "Percent of Groundwater Wells") + 
    scale_fill_manual(values = colour.scale) +
    scale_y_continuous(labels = percent, limits = c(0,1)) +
    theme(axis.text.y = element_text(colour = "black"),
          strip.text = element_text(colour = "black", size = 9))
)


## Summarize by aquifer type

#aquifer type df
sum_data_aq <- results_viz %>% 
  filter(AQUIFER_TYPE != "Unknown") %>%
  group_by(AQUIFER_TYPE, category) %>%
  summarise(Freq = n()) %>%
  mutate(prop = Freq/sum(Freq),
         aq_lab = paste0(AQUIFER_TYPE, "\n(",
                         nLabeller(sum(Freq), "well"), ")"))

#bar chart plot with percentage on y and sample size labels
(aq_plot <- ggplot(sum_data_aq, aes(x = category, y = prop, fill = category)) +
    geom_bar(stat = 'identity') + facet_grid(~ aq_lab) +
    labs(title = "Trends in Groundwater Levels by Aquifer Type",
         x = element_blank(), y = "Percent of Groundwater Wells") +
    scale_fill_manual(values = colour.scale) +
    scale_y_continuous(labels = percent, limits = c(0,1)) +
    theme(axis.text.y = element_text(colour = "black"),
          strip.text = element_text(colour = "black", size = 9))
)


## Save plot objects to tmp folder
save(pie_plot, regional_plot, aq_plot, file = "tmp/figures.RData")


## Save plots as high resolution PNG/SVGs for web
pie <- "out/figs/status_pie"
status.reg <- "out/figs/status-by-reg"
status.aq <- "out/figs/status-by-aq"
status.reg.aq <- "out/figs/status-by-reg-aq"

#pie chart
png_retina(glue(pie, ".png"), width = 440, height = 400)
plot(pie_plot)
dev.off()

svg_px(glue(pie, ".svg"), width = 440, height = 400)
plot(pie_plot)
dev.off()

#regional bar chart
png_retina(glue(status.reg, ".png"), width = 800, height = 400)
plot(regional_plot)
dev.off()

svg_px(glue(status.reg, ".svg"), width = 800, height = 400)
plot(regional_plot)
dev.off()


#aquifer type bar chart
png_retina(glue(status.aq, ".png"), width = 440, height = 400)
plot(aq_plot)
dev.off()

svg_px(glue(status.aq, ".svg"), width = 440, height = 400)
plot(aq_plot)
dev.off()

#bar charts combined using grid.arrange()
png_retina(glue(status.reg.aq, ".png"), width = 930, height = 330)
grid.arrange(regional_plot + theme(legend.position = "none"),
             aq_plot + theme(legend.position = "none"),
             ncol = 2, widths = c(3,2))
dev.off()

svg_px(glue(status.reg.aq, ".svg"), width = 930, height = 330)
grid.arrange(regional_plot + theme(legend.position = "none"),
             aq_plot + theme(legend.position = "none"),
             ncol = 2, widths = c(3,2))
dev.off()



## Map Summary (for PDF print version)

#Provincial summary map
styles <- 'feature:all|element:all|saturation:-75'

# Get British Columbia basemap
BCextent <- c(-139,48,-114,60)
names(BCextent) <- c("left", "bottom", "right", "top")
fourCorners <- expand.grid(as.data.frame(matrix(BCextent, ncol=2, byrow=TRUE,
                                                dimnames=list(NULL, c("Long", "Lat")))))
BCcenter <- c(mean(BCextent[c("left","right")]), 
              mean(BCextent[c("top","bottom")]))
ggMapBC <- get_googlemap(center=BCcenter, zoom=5, scale=1, 
                         maptype='roadmap', visible=fourCorners, style=styles)


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

#save list of well maps for print version
save(BCextent, ggMapBC, summary_map, wellMaps, file="./tmp/map_data.RData")



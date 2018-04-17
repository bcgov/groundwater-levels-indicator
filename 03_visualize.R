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

library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(scales)
library(ggmap)

# Load saved data if necessary
load("./tmp/analysis_data.RData")

theme_set(theme_classic() + 
            theme(text = element_text(colour = "#666666"),
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

label.colour <- "#3d3d3d"
colour.scale <- brewer.pal(3,"Blues")

# Select wells analyzed and create factors
results_viz <- results_out[results_out$category != "N/A",] %>%
  mutate(state = factor(state, levels = c("Increasing", 
                                          "Stable",
                                          "Moderate rate of decline",
                                          "Large rate of decline"),
                        ordered = TRUE),
         category = factor(category, levels = c("Stable or Increasing", 
                                                "Moderate rate of decline",
                                                "Large rate of decline"),
                           ordered = TRUE))

# overall summary
sum_data <- results_viz %>%
  group_by(category) %>%
  summarise(Freq = n()) %>%
  arrange(desc(category)) %>%
  mutate(per = round(Freq/sum(Freq)*100),
         pos = cumsum(Freq) - Freq/2)

pie_plot <- ggplot(results_viz, aes(x = factor(1), fill = category)) + 
  geom_bar(width = 1) + coord_polar(start = 0, theta = "y") + 
  scale_fill_manual(values = colour.scale) + 
  theme(line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), plot.title = element_text(vjust = 0),
        legend.position = c(0.5,0.01), legend.direction = "horizontal",
        legend.title = element_blank(), plot.margin = unit(c(rep(0,4)), "cm"),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  geom_text(data = sum_data,
            aes(x = 1.2, y = pos, label = paste0(per,"%")),
            colour = label.colour) + 
  ggtitle("Percentage of observation wells in three different\ncategories of long-term trends in water levels")

# Summarize by region

nLabeller <- function(n, singular, sep = " ") {
  suffix <- ifelse(n == 1, singular, paste0(singular,"s"))
  label <- paste(n, suffix, sep = sep)
  label
}

sum_data_reg <- results_viz %>%
  group_by(REGION_NAME, category) %>%
  summarise(Freq = n()) %>%
  mutate(prop = Freq/sum(Freq), 
         region_lab = paste0(gsub("(\\s)","\\\n", 
                                  gsub("\\s/\\s*", "/\\\n", REGION_NAME)), 
                             "\n(", nLabeller(sum(Freq), "well"), ")"))

## Plot with percentage on y and sample size labels
regional_plot <- ggplot(sum_data_reg, aes(x = category, y = prop, fill = category)) + 
  geom_bar(stat = 'identity') + facet_grid(~ region_lab) + 
  labs(title = "Trends in groundwater levels by region", 
       x = element_blank(), y = "Percent of wells") + 
  scale_fill_manual(values = colour.scale) +
  scale_y_continuous(labels = percent, limits = c(0,1))

## Summarize by aquifer type

sum_data_aq <- results_viz %>% 
  filter(AQUIFER_TYPE != "Unknown") %>%
  group_by(AQUIFER_TYPE, category) %>%
  summarise(Freq = n()) %>%
  mutate(prop = Freq/sum(Freq),
         aq_lab = paste0(AQUIFER_TYPE, "\n(",
                         nLabeller(sum(Freq), "well"), ")"))

## Plot with percentage on y and sample size labels
aq_plot <- ggplot(sum_data_aq, aes(x = category, y = prop, fill = category)) +
  geom_bar(stat = 'identity') + facet_grid(~ aq_lab) +
  labs(title = "Trends in groundwater levels by aquifer type",
       x = element_blank(), y = "Percent of wells") +
  scale_fill_manual(values = colour.scale) +
  scale_y_continuous(labels = percent, limits = c(0,1))


# Get maps ----------------------------------------------------------------

styles <- 'feature:all|element:all|saturation:-75'

# Get BC basemap
BCextent <- c(-139,48,-114,60)
names(BCextent) <- c("left", "bottom", "right", "top")
fourCorners <- expand.grid(as.data.frame(matrix(BCextent, ncol=2, byrow=TRUE,
                                                dimnames=list(NULL, c("Long", "Lat")))))
BCcenter <- c(mean(BCextent[c("left","right")]), 
              mean(BCextent[c("top","bottom")]))
ggMapBC <- get_googlemap(center=BCcenter, zoom=5, scale=1, 
                         maptype='roadmap', visible=fourCorners, style=styles)

# Create list of well maps
wellMaps <- list()
for(w in unique(results_viz$Well_Num)) {
  well <- filter(results_viz, Well_Num == w)
  wellMaps[[w]] <- tryCatch(get_googlemap(center = c(well$Long[1], well$Lat[1]), 
                                          zoom = 8, scale = 1, maptype = 'roadmap', style = styles), 
                            error = function(e) NULL)
}

save(BCextent, ggMapBC, wellMaps, file="./tmp/map_data.RData")
save(pie_plot, regional_plot, aq_plot, file = "tmp/figs.RData")

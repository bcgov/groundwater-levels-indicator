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

library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(scales)

theme_set(theme_classic() + 
            theme(text=element_text(colour="#666666")
                  , axis.line=element_blank()
                  , axis.ticks=element_blank()
                  , panel.grid.major=element_line(colour="grey85", size=0.5
                                                  , linetype=1)
                  , panel.grid.minor=element_line(colour="grey90", size=0.5
                                                  , linetype=1)
                  , panel.grid.major.x=element_blank()
                  , panel.margin=unit(0.6, "lines")
                  , plot.title=element_text(vjust=2)
                  , axis.title=element_text(vjust=0.1)
                  , legend.position="bottom", legend.title=element_blank()
                  , legend.text=element_text(size=12)
                  , axis.text.x = element_blank()
                  , strip.background=element_blank()))

label.colour <- "#3d3d3d"
colour.scale <- brewer.pal(3,"Blues")

# Select wells analyzed and create factors
attr.viz <- attr.out[attr.out$category != "N/A",]

attr.viz$state <- factor(attr.viz$state
                         , levels=c("Increasing", "Stable"
                                    , "Moderate rate of decline"
                                    , "Large rate of decline")
                         , ordered=TRUE)

attr.viz$category <- factor(attr.viz$category, 
                            levels=c("Stable or Increasing", 
                                     "Moderate rate of decline"
                                     , "Large rate of decline")
                            , ordered=TRUE)

# overall summary
sum.data <- attr.viz %>%
  group_by(category) %>%
  summarise(Freq = n()) %>%
  mutate(per=round(Freq/sum(Freq)*100)
         , pos=cumsum(Freq)-Freq/2)

pie <- ggplot(attr.viz, aes(x=factor(1), fill=category)) + 
  geom_bar(width=1) + coord_polar(theta="y") + 
  scale_fill_manual(values=colour.scale) + 
  theme(line=element_blank(), axis.text=element_blank()
        , axis.title=element_blank(), plot.title=element_text(vjust=0)
        , legend.position=c(0.5,0.01), legend.direction="horizontal"
        , legend.title=element_blank(), plot.margin=unit(c(rep(0,4)), "cm")) + 
  geom_text(data=sum.data
            , aes(x=1.2, y=pos, label=paste0(per,"%"))
            , colour=label.colour) + 
  ggtitle("Percentage of observation wells in three different\ncategories of long-term trends in water levels")

# Summarize by region

nLabeller <- function(n, singular, sep=" ") {
  suffix <- ifelse(n == 1, singular, paste0(singular,"s"))
  label <- paste(n, suffix, sep=sep)
  label
}

sum.data.reg <- attr.viz %>%
  group_by(REGION_NM, category) %>%
  summarise(Freq = n()) %>%
  mutate(prop=Freq/sum(Freq), 
         region_lab = paste0(gsub("\\s","\\\n", 
                                  gsub("\\s/\\s*", "/\\\n", REGION_NM)), 
                             "\n(", nLabeller(sum(Freq), "well"), ")"))

## Plot with percentage on y and sample size labels
reg.plot <- ggplot(sum.data.reg, aes(x=category, y=prop, fill=category)) + 
  geom_bar(stat='identity') + facet_grid(~ region_lab) + 
  labs(title="Trends in groundwater levels by region", 
       x=element_blank(), y="Percent of wells") + 
  scale_fill_manual(values=colour.scale) +
  scale_y_continuous(labels=percent, limits=c(0,1))

################################################################################
## Below not working because aquifer type not in data

# Summarize by aquifer type

sum.data.aq <- attr.viz[attr.viz$Aquifer_Type != "Unknown",] %>%
  group_by(Aquifer_Type, category) %>%
  summarise(Freq = n()) %>%
  mutate(prop=Freq/sum(Freq), 
         aq_lab = paste0(Aquifer_Type, "\n(", 
                         nLabeller(sum(Freq), "well"), ")"))

## Plot with percentage on y and sample size labels
aq.plot <- ggplot(sum.data.aq, aes(x=category, y=prop, fill=category)) + 
  geom_bar(stat='identity') + facet_grid(~ aq_lab) + 
  labs(title="Trends in groundwater levels by aquifer type", 
       x=element_blank(), y="Percent of wells") + 
  scale_fill_manual(values=colour.scale) +
  scale_y_continuous(labels=percent, limits=c(0,1))

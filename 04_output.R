# Copyright 2024 Province of British Columbia
# 
# This work is licensed under the Creative Commons Attribution 4.0 International License.
# To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/.


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
         col = case_when(
           state == "Stable and/or Non-significant" ~ "#abd9e9",
           state == "Moderate Rate of Decline" ~ "#fdae61",
           state == "Large Rate of Decline" ~ "#d7191c",
           state == "Increasing" ~ "#2c7bb6"
         )) %>%
  mutate(Well_Name = paste0("Observation Well #", Well_Num)) %>%
  select(c(-start_year.y, -end_year.y, -EMS_ID)) %>%
  rename(start_year = start_year.x) %>%
  rename(end_year = end_year.x) |> 
  arrange(region_name, Well_Num)

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
         col = case_when(
           state == "Stable and/or Non-significant" ~ "#abd9e9",
           state == "Moderate Rate of Decline" ~ "#fdae61",
           state == "Large Rate of Decline" ~ "#d7191c",
           state == "Increasing" ~ "#2c7bb6"
         )) %>%
  left_join(aquifer_info, by=c("Well_Num", "region_name")) |> 
  mutate(Well_Name = paste0("Observation Well #", Well_Num)) |> 
  arrange(region_name, Well_Num)

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
  geom_col(mapping=aes(x=count, y=state_figure, fill=state_figure), 
           width = 0.4, colour = "black") +
  scale_fill_manual(values=colour.scale.aquifer,
                    labels = c("Increasing",
                               "Stable and/or \nNon-significant",
                               "Moderate Rate \nof Decline",
                               "Large Rate \nof Decline",
                               "Mixed")) +
  geom_text(aes(x=count, y=state_figure, label = aquifer_lab), hjust = -0.1) +
  scale_x_continuous(expand = c(0,0)) +
  expand_limits(x=c(0,110)) +
  xlab("Number of Aquifers") + 
  ylab(NULL) +
  theme_soe() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0)) + 
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) 

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
  mutate(aquifer_lab = paste0(total_aquifers, " aquifers")) %>%
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
  geom_label(aes(x=total_aquifers, y=region_name, label = aquifer_lab),
            hjust = -0.1, label.size = 0) +
  scale_x_continuous(expand = c(0,0)) +
  expand_limits(x=c(0,55)) +
  guides(fill = guide_legend(reverse = TRUE, nrow=2))+
  xlab("Number of Aquifers") + 
  ylab(NULL) +
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
                            #"Northeast",
                            #"Omineca",
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
  filter(result != "Insufficient Data") |> 
  filter(result != "Large data gaps present in time series") |> 
  mutate(result = fct_relevel(factor(result), 
                              c("Increasing",
                                "Stable and/or Non-significant",
                                "Moderate Rate of Decline",
                                "Large Rate of Decline"))) 

# Map of Obs Well Trends --------------------------------------------------

prov_map <- ggplot() +
  geom_sf(data = bcmaps::bc_bound(), color = "black", fill= NA) +
  geom_sf(data = regions_sf, color = "black", fill= NA) +
  geom_point(data = results_sf, aes(fill = result, geometry=geometry), 
             shape=21,
             color= "black",
             stat = "sf_coordinates", 
             size =3) +
  scale_fill_manual(values = c("Stable and/or Non-significant" = "#abd9e9",
                               "Increasing" = "#2c7bb6",
                               "Moderate Rate of Decline" = "#fdae61",
                               "Large Rate of Decline" = "#d7191c"),
                     breaks = c("Stable and/or Non-significant",
                                "Increasing",
                                "Moderate Rate of Decline",
                                "Large Rate of Decline")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        #legend.direction="horizontal",
        plot.title = element_text(hjust = 0)) +
  guides(fill = guide_legend(nrow=2))

prov_map


svg_px("./out/figs/prov_map.svg", width = 500, height = 900)
plot(prov_map)
dev.off()

#save version for rmd
save(bc_bar_chart, regional_bar_chart, prov_map, monthly_bar_chart, file = "tmp/figures.RData")

              # Individual Obs Well Plots (Web & PDF) ----------------------------------------
well_plots <- monthlywells_ts %>%
  left_join(results_viz, by = c("Well_Num")) 

well_plots_10 <- monthlywells_ts_10 |> 
  left_join(results_viz_10, by = c("Well_Num")) 

save(well_plots, file = "tmp/well_plots.RData")

save(well_plots_10, file = "tmp/well_plots_10.RData")

gwl_aplot <- function(data, sig, state_short, well, reg, trend_type) {
  
  if(nrow(data) >0) {
    maxgwl = max(data$med_GWL, na.rm = TRUE)
    mingwl = min(data$med_GWL, na.rm = TRUE)
    gwlrange = maxgwl - mingwl
    midgwl = (maxgwl + mingwl) / 2
    lims  = c(midgwl + gwlrange, midgwl - gwlrange)
    data$max_lims <- max(lims[1], max(data$med_GWL, na.rm = TRUE) + 5)
    slope = unique(-1 * data$trend_line_slope)

    slope.well = data %>%
      pull(trend_line_slope)
    slope.well = - as.numeric(slope.well)/365
    
    intercept.well = data %>%
      pull(trend_line_int)
    
    int.well = intercept.well + slope.well * as.numeric(min(as.Date(data$Date)))
    
    trend_df = data.frame(int.well, slope.well)
      
    plot_data = data %>%
      group_by(Year) %>%
      summarise(
        annual_median = median(med_GWL),
        missing_dat = case_when(any(nReadings == 0) ~ "missing", T ~ "complete"),
        max = quantile(med_GWL, 0.975),
        min = quantile(med_GWL, 0.025)
      ) %>%
      mutate(Date = as.Date(paste0(Year, "-01-01"))) %>%
      select(Date, Year, annual_median, missing_dat, min, max)
  }
  
  
  plot = ggplot(plot_data, aes(x = Date, y = annual_median, col = missing_dat, ymin = min,
                               ymax = max)) +
    ggtitle(paste0(trend_type, "\nStation Class: ", state_short)) +
    labs(subtitle = paste0(slope, " m/year; p value: ", sig)) +
    geom_point() +
    geom_errorbar(width = 0.3) +
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
  
  plot
  
  if(as.character(unique(state_short)) %in% c("Increasing", 
                              "Moderate Rate of Decline",
                              "Large Rate of Decline")){
    plot +
      geom_abline(data = trend_df, 
                  aes(intercept = - int.well, slope = slope.well), col = "orange")
  } else{
    plot
    }

}


status.well <- "out/figs/"

app_wells_10 <- results_viz_10 |>
  pull(Well_Num)

app_wells <- results_viz |> 
  pull(Well_Num)

pdf_wells <- intersect(app_wells_10, app_wells) 

stn_plots <- list()

for (well in pdf_wells) {
  if (is.na(well)) next
  
  monthplot <- filter(well_plots, Well_Num == well) 
  areaplot_10 <- filter(well_plots_10, Well_Num == well)
  areaplot <- filter(well_plots, Well_Num == well)

  m <- gwl_monthly_plot(monthplot)
  
  a_10 <- gwl_aplot(areaplot_10, sig = areaplot_10$sig, 
                    state_short = areaplot_10$state,
                    well = well, reg = reg, trend_type = "10-year Trend (2013-2023) ")
  
  a <- gwl_aplot(areaplot, sig = areaplot$sig, 
                 state_short = areaplot$state,
                 well = well, reg = reg,  trend_type = "All available data")
  
  stn_plots[[well]][["month"]] <- m
  
  stn_plots[[well]][["a_10"]] <- a_10
  
  stn_plots[[well]][["area"]] <- a
  
  
  print(well)
}
  
write_rds(stn_plots, "out/print_stn_plots.rds")


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


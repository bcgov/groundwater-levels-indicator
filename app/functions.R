
#This file contains functions used in the Groundwater Indicator Shiny app scripts.
#Includes:
#   1) prov_summary_plot(data) - creates provincial summary bar plot
#      based on groundwater well trend statistics
#   2) regional_summary_plot(data) - creates regional summary bar plot
#      based on groundwater well trend statistics
#   3) groundwater_level_plot(data, clicked_station,trend_results) - creates groundwater water
#      graphs with interpolated values and trend lines (for wells with significant trends)
#   4) sf_filter <- function(period, time_scale, month) - selects state column based on input filters


##########################################################
#1. Define function to create regional summary plot
prov_summary_plot <- function(data){

  #Filter out wells with no state
  data <- data %>%
    filter(., state !="Too many missing observations to perform trend analysis" &
             state !="Recently established well; time series too short for trend analysis" )

#Count the number of wells in each state and calculate the respective proportions
input_summary <- data %>%
  group_by(state) %>%
  summarize("count"=n()) %>%
  mutate(col = case_when(
    state == "Stable" ~ "gray70",
    state == "Moderate Rate of Decline" ~ "orange",
    state == "Large Rate of Decline" ~ "darkorange",
    state == "Increasing" ~ "skyblue2"
  )) %>%
  mutate(order = case_when(
    state == "Stable" ~ 2,
    state == "Moderate Rate of Decline" ~ 3,
    state == "Large Rate of Decline" ~ 4,
    state == "Increasing" ~ 1
  )) %>%
  mutate(total_no_wells = nrow(data)) %>%
  mutate(prop = (count/total_no_wells)*100) %>%
  mutate("no_wells_lab" = ifelse(count>1, paste0(count, " wells"), paste0(count, " well"))) %>%
  mutate(label_x = cumsum(count))  %>% #Calculate the total count of wells for bar graph label
  arrange(order) %>%
  mutate(col = as.factor(col), state = factor(state))

#Create field for barplot labels
# bar_labels <- input_summary %>%
#   group_by(state) %>%
#   summarize("no_wells"=sum(count)) %>%
#   mutate("no_wells_lab" = paste0(no_wells, " wells"))

# input_summary <- right_join(input_summary, bar_labels)

#Define factor levels for barplot colouring
input_summary$col <- factor(input_summary$col, levels=rev(input_summary$col))
input_summary$state <- factor(input_summary$state, levels=rev(input_summary$state))

#Define factor levels for barplot colouring and legend
barcol = levels(input_summary$col)
barlab = levels(input_summary$state)
#
#   input_summary <- input_summary %>%
#     mutate(label_x = cumsum(count))

#Create provincial summary bar graph
ggplot(data=input_summary) +
  geom_col(mapping=aes(x=prop, y=state, fill=state, width = 0.5)) +
  scale_fill_manual(label=barlab, values=barcol) +
  geom_text(aes(x=prop, y=state, label = no_wells_lab), hjust = -0.1) +
  scale_x_continuous(expand = c(0,0)) +
  expand_limits(x=c(0,115)) +
  #guides(fill = guide_legend(reverse = TRUE, nrow=2))+
  #labs(title = "Groundwater Wells by State") +
  xlab("Proportion of wells (%)") + ylab(NULL) +
  theme_classic() +
  #theme(legend.position="bottom",
  #      legend.title=element_blank())
  theme(legend.position="none")

}
##########################################################
#2. Define function to create regional summary plot
regional_summary_plot <- function(data){

  #Filter out wells with no state
  data <- data %>%
    filter(., state !="Too many missing observations to perform trend analysis" &
             state !="Recently established well; time series too short for trend analysis" )


#Summarize results by region and count wells in each state
input_regional <- data %>%
  group_by(REGION_NAME, state) %>%
  summarize("count"=n()) %>%
  mutate( col = case_when(
    state == "Stable" ~ "gray70",
    state == "Moderate Rate of Decline" ~ "orange",
    state == "Large Rate of Decline" ~ "darkorange",
    state == "Increasing" ~ "skyblue2"
  ))

#Count total number of wells in each region
bar_labels_r <- input_regional %>%
  group_by(REGION_NAME) %>%
  summarize("no_wells"=sum(count)) %>%
  mutate("no_wells_lab" = ifelse(no_wells>1, paste0(no_wells, " wells"), paste0(no_wells, " well"))) %>%
  mutate(prop_tot = (no_wells/length(unique(data$Well_Num)))*100) #Calculate total proportion for graph scale


#Add total number of wells to regional input dataset
input_regional <- right_join(input_regional, bar_labels_r) %>%
  mutate(total_no_wells = length(unique(data$Well_Num))) %>% #added
  mutate(prop = (count/total_no_wells)*100) #Calculate proportion on total number of wells

#Define factor levels for barplot colouring and ordering
input_regional$REGION_NAME <-factor(input_regional$REGION_NAME, c("West Coast", "Thompson / Okanagan", "South Coast", "Skeena", "Omineca",
                                                                  "Northeast", "Kootenay / Boundary", "Cariboo"))
input_regional$col <- factor(input_regional$col, c("darkorange", "orange", "gray70", "skyblue2"))
input_regional$state <- factor(input_regional$state, c("Large Rate of Decline", "Moderate Rate of Decline", "Stable", "Increasing"))

barcolr=levels(as.factor(input_regional$col))
barlabr=levels(as.factor(input_regional$state))

#Create regional summary plot
ggplot(data=input_regional) +
  geom_col(mapping=aes(x=prop, y=REGION_NAME, fill=state, width = 0.5)) +
  scale_fill_manual(label=barlabr, values=barcolr) +
  geom_text(aes(x=prop_tot, y=REGION_NAME, label = no_wells_lab), hjust = -0.1) +
  scale_x_continuous(expand = c(0,0)) +
  expand_limits(x=c(0,55)) +
  guides(fill = guide_legend(reverse = TRUE))+
  #labs(title = "Groundwater Wells by Region") +
  xlab("Proportion of Wells (%)") + ylab(NULL) +
  theme_classic() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.box.just ="left")

}

##########################################################
#3. Define function to create groundwater level plot and trend line
groundwater_level_plot = function(data,period_choice,var_choice,month_choice,clicked_station,trend_results,slopes){

  # trend_results <- results_out %>%
  #   filter(period == "Yearly", time_scale == "All")

#If no selection, print "No selection" - set as default when app opens
  if(clicked_station == 'No selection'){
      ggplot() +
        geom_text(aes(x=1,y=1,label='Click on a region or groundwater monitoring station on the map to see its plot.')) +
        ggthemes::theme_map()
    } else {

      #If a well is selected (clicked on Leaflet map) - select data only for that well
      well_num = trend_results %>%
        filter(Well_Num == clicked_station)

      #If a well is recently established
      if(well_num$state_short == "Recently established well"){
        ggplot() +
          geom_text(aes(x=1,y=1,label='No plot available')) +
          ggthemes::theme_map()
      }else{

      #If 10 years selected, filter years of data shown
      if(var_choice == "10 Years"){
        data <- data %>% filter(Year >= 2013)
      }

      #If 20 years selected, filter years of data shown
      if(var_choice == "20 Years"){
        data <- data %>% filter(Year >= 2003)
      }

        #If month selected, use monthly means
        if(period_choice == "Monthly"){
          data <- data %>% filter(stat == "mean")
        }else{
          data <- data %>% filter(stat == "median")
        }

      well_levels = left_join(well_num, data, by=c("Well_Num" = "Well_Num"), multiple = "all") %>%
        mutate(Date = as_date(ymd(Date))) %>%
        mutate(Year = as.integer(Year))

      #Identify interpolated values (values with zero readings)
      nZeroReadings <- filter(well_levels, nReadings==0)

      #Define water level limits for plot creation
      maxgwl = max(well_levels$value, na.rm = TRUE)
      mingwl = min(well_levels$value, na.rm = TRUE)
      gwlrange = maxgwl - mingwl
      midgwl = (maxgwl + mingwl)/2
      lims  = c(midgwl + gwlrange, midgwl - gwlrange)
      well_levels$max_lims <- max(lims[1], max(well_levels$value, na.rm = TRUE) + 5)

      #Define date limits for plot creation
      minDate = as.Date(min(well_levels$Date))
      maxDate = as.Date(max(well_levels$Date))
      nYears <- as.numeric(difftime(maxDate, minDate, units = "days"))/365

      #Create bar charts if monthly selected
      if(period_choice == "Monthly"){

        month_no <- as.numeric(match(month_choice,month.abb))

        well_levels_monthly <- well_levels %>%
          filter(Month == month_no)

        plot <- ggplot(data=well_levels_monthly, aes(x=Year, y=value, yend = max_lims, xend=Year)) +
          geom_segment(aes(color = "Groundwater Level", linewidth = 0.5)) +
          labs(x = "Year", y = "Depth Below Ground (metres)") +
          scale_y_reverse(expand = c(0,0)) + coord_cartesian(ylim = lims) +
          scale_x_continuous(expand = c(0,0)) +
          theme_classic() +
          theme(
            text = element_text(colour = "black"),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.line = element_line(colour="grey50"),
            legend.position = "bottom", legend.box =  "horizontal",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 11)) +
          theme(plot.margin = margin(10, 10, 10, 10, "points")) +
          theme(legend.position="bottom",
                legend.title=element_blank(),
                legend.box.just ="left") +
          scale_color_manual(breaks = c("Groundwater Level"),
                             values = c("Groundwater Level" = "#C6DDFD"),
                             guide=guide_legend(override.aes=list(linetype=c("solid"),
                                                                  shape=c(NA), size=c(5)))) +
          scale_fill_manual(labels = "Groundwater Level", values = c("Groundwater Level" = "#C6DDFD"))

        #Print plots for wells without significant trends (water level and interpolated points only)
        if(well_num$state == "Stable" | well_num$state == "Too many missing observations to perform trend analysis" |
           well_num$state == "Recently established well; time series too short for trend analysis"){

          plot }

        else{

          #For wells with significant trends (increasing, moderate rate of decline, or large rate of decline)
          #Add trend line information
          slope = -as.numeric(well_num$trend_line_slope)/365
          intercept = as.numeric(well_num$trend_line_int)
          int.well = intercept + slope * as.numeric(minDate)

          #If interpolated values present, add trend line to chart and both line and points to legend and print plot
          # if(nrow(nZeroReadings)>0){
          #   plot +
          #     geom_abline(aes_string(intercept = "intercept", slope = "slope", colour = "'LTT'"),
          #                 data = data.frame(intercept = -int.well, slope = slope), size = 1) +
          #     scale_colour_manual(name = '', values = c(LTT = 'orange', Interp = 'grey60'),
          #                         labels = c('Long-term Trend', 'Interpolated (Missing) Values'),
          #                         guide = guide_legend(override.aes = list(colour = c("orange", "grey60"), shape = c(NA, 16), linetype = c(1, 0))))
          # }else {
            #If no interpolated values present, only add trend line to chart and legend and print plot
            plot +
              geom_abline(aes_string(intercept = "intercept", slope = "slope", colour = "'LTT'"),
                          data = data.frame(intercept = -int.well, slope = slope), size = 1) +
              scale_colour_manual(breaks = c('Long-term Trend', 'Groundwater Level'),
                                  values = c(LTT = 'orange', GWL = '#C6DDFD'),
                                  labels = c('Long-term Trend', 'Groundwater Level'),
                                  guide=guide_legend(override.aes=list(linetype=c("solid", "solid"),
                                                                       shape=c(NA, NA), size=c(5, 5)))) +
              scale_fill_manual(labels = c('Long-term Trend', 'Groundwater Level'),
                                values = c(LTT = 'orange', GWL = '#C6DDFD'))
        }

        }else{

      #Define base plot with only water levels
        plot <- ggplot(well_levels, aes_string(x = "Date")) +
          geom_ribbon(aes_string(ymin = "value",
                                 ymax = "max_lims",
                                 fill = "'Groundwater Level'"), alpha = 0.3) +
          # labs(title = "Observed Long-term Trend in Groundwater Levels\n", x = "Date",
          #      y = "Depth Below Ground (metres)") +
          labs(x = "Date", y = "Depth Below Ground (metres)") +
          theme_minimal() +
          theme(
            text = element_text(colour = "black"),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.line = element_line(colour="grey50"),
            legend.position = "bottom", legend.box =  "horizontal",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 11)) +
          scale_y_reverse(expand = c(0,0)) + coord_cartesian(ylim = lims) +
          scale_x_date(expand = c(0,0)) +
          #scale_x_date(labels = "Year", year("Date"),
          # breaks = scale_x_date(dplyr::if_else(nYears < 10, #This is not working
          #                                   "1 year",
          #                                   "3 years")),
          #             expand = c(0,0)) +
          scale_fill_manual(name = '', values = c('Groundwater Level' = "#1E90FF"))

        #If interpolated values were identified, add point element to chart and legend
        if(nrow(nZeroReadings)>0){
          plot <- plot +
            geom_point(data = well_levels[well_levels$nReadings == 0,],
                       aes_string(y = "value", colour = "'Interp'"),
                       size = 0.5) +
            scale_colour_manual(name = '', values = c(Interp = 'grey60'),
                                labels = c('Interpolated (Missing) Values'),
                                guide = guide_legend(override.aes = list(colour = c("grey60"), shape = c(16), linetype = c(0))))
        }


        #Print plots for wells without significant trends (water level and interpolated points only)
        if(well_num$state == "Stable" | well_num$state == "Too many missing observations to perform trend analysis" |
          well_num$state == "Recently established well; time series too short for trend analysis"){

          plot }

      else{

      #For wells with significant trends (increasing, moderate rate of decline, or large rate of decline)
      #Add trend line information
      slope = -as.numeric(well_num$trend_line_slope)/365
      intercept = as.numeric(well_num$trend_line_int)
      int.well = intercept + slope * as.numeric(minDate)

      #If interpolated values present, add trend line to chart and both line and points to legend and print plot
      if(nrow(nZeroReadings)>0){
        plot +
          geom_abline(aes_string(intercept = "intercept", slope = "slope", colour = "'LTT'"),
                      data = data.frame(intercept = -int.well, slope = slope), size = 1) +
          scale_colour_manual(name = '', values = c(LTT = 'orange', Interp = 'grey60'),
                              labels = c('Long-term Trend', 'Interpolated (Missing) Values'),
                              guide = guide_legend(override.aes = list(colour = c("orange", "grey60"), shape = c(NA, 16), linetype = c(1, 0))))
      }else {
        #If no interpolated values present, only add trend line to chart and legend and print plot
        plot +
          geom_abline(aes_string(intercept = "intercept", slope = "slope", colour = "'LTT'"),
                      data = data.frame(intercept = -int.well, slope = slope), size = 1) +
          scale_colour_manual(name = '', values = c(LTT = 'orange', Interp = 'grey60'),
                              labels = c('Long-term Trend', 'Interpolated (Missing) Values'))
      }}
      }
    }
}}


#################################################
#4. Function to select state based on input filters

sf_filter <- function(period, time_scale, month){

  if(period == "Yearly"){
  col <- case_when(time_scale == "All" ~ 16,
                   time_scale == "10 Years" ~ 17,
                   time_scale == "20 Years" ~ 18)

  return(col)

  }

  if(period == "Monthly"){
    col <- case_when(time_scale == "All" & month == "Jan" ~ 19,
                       time_scale == "All" & month == "Feb" ~ 20,
                       time_scale == "All" & month == "Mar" ~ 21,
                       time_scale == "All" & month == "Apr" ~ 22,
                       time_scale == "All" & month == "May" ~ 23,
                       time_scale == "All" & month == "Jun" ~ 24,
                       time_scale == "All" & month == "Jul" ~ 25,
                       time_scale == "All" & month == "Aug" ~ 26,
                       time_scale == "All" & month == "Sep" ~ 27,
                       time_scale == "All" & month == "Oct" ~ 28,
                       time_scale == "All" & month == "Nov" ~ 29,
                       time_scale == "All" & month == "Dec" ~ 30,
                     time_scale == "10 Years" & month == "Jan" ~ 31,
                     time_scale == "10 Years" & month == "Feb" ~ 32,
                     time_scale == "10 Years" & month == "Mar" ~ 33,
                     time_scale == "10 Years" & month == "Apr" ~ 34,
                     time_scale == "10 Years" & month == "May" ~ 35,
                     time_scale == "10 Years" & month == "Jun" ~ 36,
                     time_scale == "10 Years" & month == "Jul" ~ 37,
                     time_scale == "10 Years" & month == "Aug" ~ 38,
                     time_scale == "10 Years" & month == "Sep" ~ 39,
                     time_scale == "10 Years" & month == "Oct" ~ 40,
                     time_scale == "10 Years" & month == "Nov" ~ 41,
                     time_scale == "10 Years" & month == "Dec" ~ 42,
                    time_scale == "20 Years" & month == "Jan" ~ 43,
                    time_scale == "20 Years" & month == "Feb" ~ 44,
                    time_scale == "20 Years" & month == "Mar" ~ 45,
                    time_scale == "20 Years" & month == "Apr" ~ 46,
                    time_scale == "20 Years" & month == "May" ~ 47,
                    time_scale == "20 Years" & month == "Jun" ~ 48,
                    time_scale == "20 Years" & month == "Jul" ~ 49,
                    time_scale == "20 Years" & month == "Aug" ~ 50,
                    time_scale == "20 Years" & month == "Sep" ~ 51,
                    time_scale == "20 Years" & month == "Oct" ~ 52,
                    time_scale == "20 Years" & month == "Nov" ~ 53,
                    time_scale == "20 Years" & month == "Dec" ~ 54)

    return(col)

}


}





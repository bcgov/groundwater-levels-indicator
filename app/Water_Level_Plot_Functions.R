##########################################################
#Define function to create groundwater level plot and trend line
groundwater_level_plot = function(data, period_choice, var_choice, month_choice, clicked_station, trend_results){

  # #Code for testing
  # trend_results <- results_out %>%
  #   filter(period == "Monthly", time_scale == "All", month == "Dec")

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
        data <- data %>% filter(Year >= 2012)
      }

      #If 20 years selected, filter years of data shown
      if(var_choice == "20 Years"){
        data <- data %>% filter(Year >= 2002)
      }

      # #If month selected, use monthly means
      # if(period_choice == "Monthly"){
      #   data <- data %>% filter(stat == "mean")
      # }else{
      #   data <- data %>% filter(stat == "median")
      # }

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

      #Monthly plots  #########################################################
      if(period_choice == "Monthly"){

        month_no <- as.numeric(match(month_choice,month.abb))

        well_levels_monthly <- well_levels %>%
          filter(Month == month_no)

        nZeroReadings_month <- nZeroReadings %>%
          filter(Month == month_no)

        #Water level plot (top plot)
        well_plot_df <- well_levels %>%
          group_by(Month) %>%
          summarize(dev_med = mean(value, na.rm = TRUE),
                    dev_Q5 = stats::quantile(value, prob = 0.05,
                                             na.rm = TRUE),
                    dev_Q95 = stats::quantile(value, prob = 0.95,
                                              na.rm = TRUE))

        data.last.12 <- tail(well_levels[, c("Date","value")], 12)

        splines.df <- as.data.frame(stats::spline(as.numeric(well_plot_df$Month), well_plot_df$dev_med,
                                                  method = "fmm"))
        splines.df$y_Q5 <- stats::spline(as.numeric(well_plot_df$Month), well_plot_df$dev_Q5,
                                         method = "fmm")$y
        splines.df$y_Q95 <- stats::spline(as.numeric(well_plot_df$Month), well_plot_df$dev_Q95,
                                          method = "fmm")$y
        names(splines.df) <- names(well_plot_df)
        well_plot_df <- splines.df

        plot.levels <- ggplot(data = well_plot_df, aes_string(x = "Month", y = "dev_med")) +
          geom_ribbon(aes_string(ymin = "dev_Q5", ymax = "dev_Q95", fill = "''"), alpha = 0.2) +
          geom_line(aes_string(colour = "''"), alpha = 0.4, size = 1) +
          labs(title = "Monthly Mean Groundwater Level Patterns", x = "Month",
               y = "Water Levels (mbgs)") +
          theme_minimal() +
          theme(
            text = element_text(colour = "black"),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.line = element_line(colour="grey50"),
            legend.position = "bottom", legend.box = "horizontal",
            plot.title = element_text(hjust = 0.5)
            #axis.text.x = element_text(angle = 45) # May need if using full month names
          ) +
          scale_y_reverse() +
          scale_x_continuous(breaks = 1:12, labels = month.abb) +
          scale_colour_manual(name = '', values = "#1E90FF",
                              labels = c("Mean Deviation from Yearly Average"),
                              guide = "legend") +
          scale_fill_manual(name = '', values = "#1E90FF", guide = 'legend',
                            labels = c('Range of 90% of Water Levels')) +
          scale_alpha_identity(name = '', labels = NA)

        #Monthly trend plot (bottom plot)
        plot.trend <- ggplot(well_levels_monthly, aes_string(x = "Date")) +
          geom_point(data = well_levels_monthly,
                     aes_string(y = "value", colour = "'Monthly'"), size = 2) +
          labs(title = paste0(month.name[well_levels_monthly$Month], " Mean Groundwater Level"),
               x = "Year", y = "Mean Groundwater Level (mbgs)") +
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
          scale_y_reverse(expand = c(0,0)) + coord_cartesian(ylim = lims) +
          #scale_x_continuous(expand = c(0,0)) +
          theme(legend.position="bottom",
                legend.title=element_blank(),
                legend.box.just ="left") +
          scale_colour_manual(name = '', values = c(Monthly = 'darkblue'),
                              labels = c('Monthly Mean'),
                              guide = guide_legend(override.aes = list(colour = c("darkblue"),
                                                                       shape = c(16), linetype = c(0), size = c(2))))

        #If interpolated values were identified, add point element to chart and legend
        if(nrow(nZeroReadings_month)>0){
          plot.trend <- plot.trend +
            geom_point(data = nZeroReadings_month,
                       aes_string(y = "value", colour = "'Interp'"), size = 2) +
            scale_colour_manual(name = '', values = c(Monthly = 'darkblue', Interp = 'grey60'),
                                labels = c('Monthly Mean', 'Interpolated (Missing) Value'),
                                guide = guide_legend(override.aes = list(colour = c("darkblue", "grey60"),
                                                                         shape = c(16, 16), linetype = c(0, 0), size = c(2, 2))))

        }

        #Print plots for wells without significant trends (water level and interpolated points only)
        if(well_num$state == "Stable" | well_num$state == "Too many missing observations to perform trend analysis" |
           well_num$state == "Recently established well; time series too short for trend analysis"){

          plot.trend <- plot.trend }

        else{

          #For wells with significant trends (increasing, moderate rate of decline, or large rate of decline)
          #Add trend line information
          slope = -as.numeric(well_num$trend_line_slope)/365
          intercept = as.numeric(well_num$trend_line_int)
          int.well = intercept + slope * as.numeric(minDate)

          #If interpolated values present, add trend line to chart and both line and points to legend and print plot
          if(nrow(nZeroReadings_month)>0){
            plot.trend <- plot.trend +
              geom_abline(aes_string(intercept = "intercept", slope = "slope", colour = "'LTT'"),
                          data = data.frame(intercept = -int.well, slope = slope), size = 1) +
              scale_colour_manual(name = '', values = c(LTT = 'orange', Monthly = 'darkblue', Interp = 'grey60'),
                                  labels = c('Long-term Trend', 'Monthly Mean', 'Interpolated (Missing) Value'),
                                  guide = guide_legend(override.aes = list(colour = c("orange", "darkblue", "grey60"),
                                                                           shape = c(NA, 16, 16), linetype = c(1, 0, 0), size = c(1, 2, 2))))

          }else {
            #If no interpolated values present, only add trend line to chart and legend and print plot
            plot.trend <- plot.trend +
              geom_abline(aes_string(intercept = "intercept", slope = "slope", colour = "'LTT'"),
                          data = data.frame(intercept = -int.well, slope = slope), size = 1) +
              scale_colour_manual(name = '', values = c(LTT = 'orange', Monthly = 'darkblue'),
                                  labels = c('Long-term Trend', 'Monthly Mean'),
                                  guide = guide_legend(override.aes = list(colour = c("orange", "darkblue"),
                                                                           shape = c(NA, 16), linetype = c(1, 0), size = c(1, 2))))
          }

        }}else{

          #Yearly plots  ##################################################

          #Create annual time series
          annualwells_ts <- left_join(well_num, data, by=c("Well_Num" = "Well_Num"), multiple = "all") %>%
            group_by(Well_Num, Year) %>%
            summarize(annual_mean = mean(value), n_months = n()) %>%
            mutate(Date = as.Date(paste0(Year, "-01-01"))) %>%
            select(Date, annual_mean)

          well_levels <- left_join(well_levels, annualwells_ts, by=c("Date" = "Date"))

          #Water level plot (top plot)

          #Define base plot with only water levels
          plot.levels <- ggplot(well_levels, aes_string(x = "Date")) +
            geom_ribbon(aes_string(ymin = "value",
                                   ymax = "max_lims",
                                   fill = "'Groundwater Level'"), alpha = 0.3) +
            labs(title = "Monthly Median Groundwater Levels",
                 x = "Date", y = "Water Level (mbgs)") +
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
            scale_fill_manual(name = '', labels = c('Monthly Median Groundwater Level'), values = c('Groundwater Level' = "#1E90FF"))

          #If interpolated values were identified, add point element to chart and legend
          if(nrow(nZeroReadings)>0){
            plot.levels <- plot.levels +
              geom_point(data = well_levels[well_levels$nReadings == 0,],
                         aes_string(y = "value", colour = "'Interp'"),
                         size = 0.5) +
              scale_colour_manual(name = '', values = c(Interp = 'grey60'),
                                  labels = c('Interpolated (Missing) Values'),
                                  guide = guide_legend(override.aes = list(colour = c("grey60"), shape = c(16), linetype = c(0))))
          }

          #Trend plot (bottom plot) with annual means of monthly medians

          plot.trend <- ggplot(well_levels, aes_string(x = "Date")) +
            geom_point(data = well_levels,
                       aes_string(y = "annual_mean", colour = "'Annual'"),
                       size = 2) +
            labs(title = "Observed Long-term Trend in Groundwater Levels",
                 x = "Date", y = "Metres Below Ground Surface") +
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
            scale_colour_manual(name = '', values = c(Annual = "darkblue"),
                                labels = c('Annual Mean Groundwater Level'),
                                guide = guide_legend(override.aes = list(colour = c("darkblue"),
                                                                         shape = c(16), linetype = c(0), size = c(2))))

          #Print plots for wells without significant trends (water level and interpolated points only)
          if(well_num$state == "Stable" | well_num$state == "Too many missing observations to perform trend analysis" |
             well_num$state == "Recently established well; time series too short for trend analysis"){

            plot.trend <- plot.trend }

          else{

            #For wells with significant trends (increasing, moderate rate of decline, or large rate of decline)
            #Add trend line information
            slope = -as.numeric(well_num$trend_line_slope)/365
            intercept = as.numeric(well_num$trend_line_int)
            int.well = intercept + slope * as.numeric(minDate)

            plot.trend <- plot.trend +
              geom_abline(aes_string(intercept = "intercept", slope = "slope", colour = "'LTT'"),
                          data = data.frame(intercept = -int.well, slope = slope), size = 1) +
              scale_colour_manual(name = '', values = c(LTT = 'orange', Annual = "darkblue"),
                                  labels = c('Long-term Trend', 'Annual Mean Groundwater Level'),
                                  guide = guide_legend(override.aes = list(colour = c("orange", "darkblue"),
                                                                           shape = c(NA, 16), linetype = c(1, 0), size = c(1, 2))))

          }

        }

      ggarrange(plot.levels, plot.trend, nrow = 2)

    }
  }
}

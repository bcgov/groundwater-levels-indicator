server <- function(input, output, session) {
  
  #Add month drop down when period_rv == "Monthly"
  output$month_selector_UI = renderUI({
    if(period_rv() == 'Yearly') return(NULL)
    selectizeInput(inputId = 'month_selector',
                   label = 'Month',
                   multiple = F,
                   choices = month.abb,
                   selected = month.abb[1])
  })
  
  
  # Reactive start points - for some reason doesnt want to recognise month
  # menu selections
  var_rv = reactiveVal('All')
  period_rv = reactiveVal('Yearly')
  month_rv = reactiveVal('January')
  region_rv = reactiveVal('All')
  my_params_rv = reactiveValues()
  
  #clicked selections
  station_click = reactiveVal('No Selection')
  
  #BC button
  observeEvent(input$abs_button, {
  zoom = input$leafmap_zoom
  lat = input$leafmap_center[2]
  lon = input$leafmap_center[1]

    # Change region_rv() to 'All'
    if(region_rv() != 'All'| zoom != 5 | lat != 52 | lon != -133){
      region_rv('All')
      
      # Update map to BC zoom (customizable)
      leafletProxy('leafmap') |>
        setView(lat = 52, lng = -133, zoom = 5)
      
      updateSelectInput(session = session,
                        'All',
                        selected = 'All')
    }
  })
  
  observe({
   
    var_rv(input$var_choice)
    period_rv(input$time_scale)
    
    #set month to January
    req(input$month_selector)
    if(is.null(month_rv())){
      month_rv('Jan')
    }
    month_rv(input$month_selector)
  })
  
  # Region reactives
  region_drop_rv = reactive({
    input$region_choice
  })
  
  region_click_rv = reactive({
    input$leafmap_shape_click$id
  })
  
  # update drop down so matches map click region
  observeEvent(input$leafmap_shape_click$id,{
    updateSelectInput(session = session,
                      'region_choice',
                      selected = input$leafmap_shape_click$id)
    region_rv(region_click_rv())
  })
  
  observeEvent(input$region_choice,{
    
    region_rv(region_drop_rv())
    
  })
  
  #Filter data based on time span and yearly/monthly
  filtered_data = reactive({
    if(period_rv() == "Yearly"){
      results_out %>%
        filter(time_scale == var_rv() & period == period_rv())
    }
    else{
      results_out %>%
        filter(time_scale == var_rv() & period == period_rv() & month == month_rv())
    }
  })
  
  #station click reactive
  observeEvent(input$leafmap_marker_click, {
    if(is.null(station_click())){
      station_click('No Selection')
    }
    station_click(input$leafmap_marker_click$id)
    # if(period_rv()=="Yearly"){
      shiny::updateTabsetPanel(
        inputId = 'tabset',
        selected = 'Trend Plot')
    # }
  })
  
  #filter based on region - wells
  map_data = reactive({
    print(region_rv())
    if(region_rv()== "All") {
      wells_sf_full %>%
        right_join(filtered_data(), by = "Well_Num")
    }
    else{
      wells_sf_full %>%
        right_join(filtered_data(), by = "Well_Num")
        #filter(REGION_NAME == region_rv()) %>%
        #left_join(filtered_data())
    }
  })
  
  # zoom  
  boundary_data = reactive({
    if(region_rv()== "All") {
      regions_sf
    }
    else{
      wells_sf_full %>%
        filter(REGION_NAME == region_rv()) %>%
        st_bbox()
    }
  })
  
  # polygon
  region_data = reactive({
    if(region_rv()== "All") {
      regions_sf
    }
    else{
      regions_sf %>%
        filter(region_name == region_rv())
    }
  })
  
  #Color scheme
  mypal = colorFactor(palette = c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c", "black"),
                      domain = map_data(),
                      levels = c("Increasing",
                                 "Stable and/or Non-significant",
                                 "Moderate Rate of Decline",
                                 "Large Rate of Decline",
                                 "Insufficient Data"
                      ),
                      ordered = T)
  
  # Map
  output$leafmap <- renderLeaflet({
    map = leaflet(options = leafletOptions(zoomSnap = 0.25,
                                           zoomDelta = 0.25)) %>%
      addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
      addTiles(group = 'Streets') |> 
      add_bc_home_button()
    
    if(region_rv() == "All"){
      map %>%
        # set_bc_view()
        setView(lat = 52, lng = -133, zoom = 5)
    }
    else{
      map %>%
        fitBounds(as.numeric(boundary_data()$xmin)-1, as.numeric(boundary_data()$ymin)-1, 
                  as.numeric(boundary_data()$xmax)+1, as.numeric(boundary_data()$ymax)+1)
    }
  })
  
  observe({
    leafletProxy('leafmap') %>%
      clearMarkers() %>%
      addPolygons(layerId = ~region_name,
                  data = region_data(),
                  label = ~paste0(REGION_NAME), color = "black", fillColor = "white",
                  weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.2,
                  highlightOptions = highlightOptions(color = "#979B9D", weight = 2,
                                                      bringToFront = FALSE)) %>%
      addCircleMarkers(layerId = ~Well_Num,
                       color = 'black',
                       fillColor = ~mypal(state_short),
                       radius = ~pt_size,
                       weight = 1,
                       group="selected",
                       fillOpacity = 1,
                       label = ~paste0("Well No. ", Well_Num, " - ",state),
                       data = map_data(),
                       clusterOptions = markerClusterOptions(maxClusterRadius = 0.000001,
                                                             spiderfyDistanceMultiplier = 2)) %>%
      removeControl("legend") %>%
      addLegend(pal = mypal,
                values = ~state_short,
                title = "Groundwater Trend",
                data = map_data(),
                #className = "info legend solid circle", #Css from original leaflet script
                opacity = 1,
                layerId = 'legend',
                position = 'topright') %>%
      addLayersControl(baseGroups = c("CartoDB","Streets"),
                       options = layersControlOptions(collapsed = F),
                       position = 'topright')
  })
  
  output$summaryPlot = renderPlot({
    
    if(station_click() == 'No Selection'){
      ggplot() +
        geom_text(aes(x=1,y=1,label='Click on a region or groundwater monitoring station on the map to see its plot.')) +
        ggthemes::theme_map()
    }
    else{
      if(period_rv() == "Yearly") {
        data = monthly_readings %>%
          filter(case_when(var_rv() == "10 Years" ~ Well_Num == station_click() & Year >=2013,
                           var_rv() == "20 Years" ~ Well_Num == station_click() & Year >=2003,
                           T ~ Well_Num == station_click())) %>%
          filter(stat == "median")
        
        if(nrow(data) >0){
          
          maxgwl = max(data$value, na.rm = TRUE)
          mingwl = min(data$value, na.rm = TRUE)
          gwlrange = maxgwl - mingwl
          midgwl = (maxgwl + mingwl)/2
          lims  = c(midgwl + gwlrange, midgwl - gwlrange)
          data$max_lims <- max(lims[1], max(data$value, na.rm = TRUE) + 5)


          ggplot(data, aes(x = as.Date(Date))) +
            labs(title = paste0("Well Number: ", station_click()))+
            geom_ribbon(aes_string(ymin = "value",
                                   ymax = "max_lims",
                                   fill = "'Groundwater Level'"), alpha = 0.6) +
            geom_point(data = data %>% filter(nReadings==0), aes_string(y = "value", col = "'interp'")) +
            scale_x_date(expand = c(0,0)) +
            scale_y_reverse() +
            scale_fill_manual(name = '', labels = 'Monthly Range in Groundwater Levels',
                              values = c('Groundwater Level' = "#1E90FF")) +
            xlab("Date") +
            ylab ("Depth Below Ground (metres)")+
            theme_minimal()+
            theme(
              text = element_text(colour = "black", size = 13),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.line = element_line(colour="grey50"),
              legend.position = "bottom", legend.box =  "horizontal",
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 11)) +
            theme(plot.margin = margin(10, 10, 10, 10, "points")) +
            theme(legend.position = "bottom") +
            scale_colour_manual(name = '', values = c(interp = "#A9A9A9"),
                                label = "Interpolated Values",
                                guide = guide_legend(override.aes = list(colour = c("#A9A9A9"), shape = c(16), linetype = c(0))))
        }
        else {
          ggplot() +
            geom_text(aes(x=1,y=1,label='Insufficient Data')) +
            ggthemes::theme_map()
        }
      }
      else{
        data = monthly_readings %>%
          filter(case_when(var_rv() == "10 Years" ~ Well_Num == station_click() & Year >=2013,
                           var_rv() == "20 Years" ~ Well_Num == station_click() & Year >=2003,
                           T ~ Well_Num == station_click())) %>%
          filter(stat == "median") %>%
          group_by(Month) %>%
          summarise(median = mean(value),
                    lowerCI = quantile(value, prob = 0.05),
                    upperCI = quantile(value, prob = 0.95))
        
        if(nrow(data) >0){

            # print(data)
            ggplot(data) +
            ggtitle(paste0("Well Number: ", station_click()))+
            geom_ribbon(aes(x = Month, ymax = upperCI, ymin = lowerCI, fill = "''"),
                        alpha = 0.4)+
            geom_line(aes(x = Month, y = median, col = "''"),
                      linewidth = 1)+
            scale_y_reverse() +
            scale_x_continuous(breaks = 1:12, labels = month.abb) +
            theme_minimal() +
              theme(
                text = element_text(colour = "black", size = 13),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                axis.line = element_line(colour="grey50"),
                legend.position = "bottom", legend.box =  "horizontal",
                plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 11)) +
            scale_fill_manual(name = '', values = "#1E90FF", guide = 'legend',
                              labels = c('Range of 90% of Water Levels')) +
            scale_colour_manual(name = '', values = "black",
                                labels = c("Median"),
                                guide = "legend") +
            xlab("Month") +
            ylab("Depth Below Ground (metres)")
        }
        else {
          ggplot() +
            geom_text(aes(x=1,y=1,label='Insufficient Data')) +
            ggthemes::theme_map()
        }
      }
      
    }
  })
  
  output$trendPlot = renderPlot({
    
    if(station_click() == 'No Selection'){
      ggplot() +
        geom_text(aes(x=1,y=1,label='Click on a region or groundwater monitoring station on the map to see its plot.')) +
        ggthemes::theme_map()
    }
    else{
      if(period_rv() == "Yearly") {
        data = monthly_readings %>%
          filter(case_when(var_rv() == "10 Years" ~ Well_Num == station_click() & Year >=2013,
                           var_rv() == "20 Years" ~ Well_Num == station_click() & Year >=2003,
                           T ~ Well_Num == station_click())) %>%
          filter(stat == "median")
        
        if(nrow(data) >0){
          
          maxgwl = max(data$value, na.rm = TRUE)
          mingwl = min(data$value, na.rm = TRUE)
          gwlrange = maxgwl - mingwl
          midgwl = (maxgwl + mingwl)/2
          lims  = c(midgwl + gwlrange, midgwl - gwlrange)
          data$max_lims <- max(lims[1], max(data$value, na.rm = TRUE) + 5)
          
          plot_data = data %>%
            group_by(Year) %>%
            summarize(annual_median = median(value), 
                      n_months = n(),
                      missing_dat = case_when(any(nReadings == 0) ~ "missing",
                                              T~ "complete"),
                      max = quantile(value, 0.975),
                      min = quantile(value, 0.025)) %>%
            mutate(Date = as.Date(paste0(Year, "-01-01"))) %>%
            select(Date, annual_median, missing_dat, min, max)
          
          #extract slope and intercept to draw trendline
          trend_data = filtered_data() %>%
            filter(Well_Num == station_click())
          slope = trend_data %>%
            pull(trend_line_slope)
          slope = - as.numeric(slope)/365
            
          intercept = filtered_data() %>%
            filter(Well_Num == station_click()) %>%
            pull(trend_line_int)
          
          int.well = intercept + slope * as.numeric(min(as.Date(data$Date)))
          
          trend_df = data.frame(int.well, slope)
          
          # extract sig value for trend
          sig_state = filtered_data() %>%
            filter(Well_Num == station_click()) |> 
            pull(sig_state)
          
          plot = ggplot(plot_data) +
            ggtitle(paste0(
              "Well Number: ",
              station_click(),
              "\nStation Class: ",
              trend_data$state_add
            )) +
            labs(
              subtitle = paste0(
                "Slope: ",
                round(trend_data$slope, 2),
                " m/year; p-value: ",
                round(trend_data$sig, 2),
                "; Trend: ",
                trend_data$sig_state
              )
            ) +
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
              plot.margin = margin(10, 10, 10, 10, "points"),
              text = element_text(colour = "black", size = 13),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.line = element_line(colour = "grey50"),
              legend.position = "bottom",
              legend.box =  "horizontal",
              plot.title = element_text(hjust = 0),
              plot.subtitle = element_text(hjust = 0, face = "plain", size = 11, 
                                           color = ifelse(trend_data$sig > 0.05, 'black', 'red'))
            ) +
            xlab("Date") +
            ylab("Depth Below Ground (metres)") 
          
          if(filtered_data() %>%
             filter(Well_Num == station_click()) %>%
             pull(state_short) %in% c("Increasing", 
                                      "Moderate Rate of Decline",
                                      "Large Rate of Decline")){
            plot +
              geom_abline(data = trend_df, 
                          aes(intercept = - int.well, slope = slope), col = "orange")
          }
          else{
            plot
          }
          
          
        }
        else {
          ggplot() +
            geom_text(aes(x=1,y=1,label='Insufficient Data')) +
            ggthemes::theme_map()
        }
      }
      else{
        data = monthly_readings %>%
          filter(case_when(var_rv() == "10 Years" ~ Well_Num == station_click() & Year >=2013,
                           var_rv() == "20 Years" ~ Well_Num == station_click() & Year >=2003,
                           T ~ Well_Num == station_click())) %>%
          # filter(Month == match(month_rv(),month.abb)) %>%
          filter(stat == "mean") %>%
          mutate(missing_dat = case_when(nReadings == 0 ~ "missing",
                                         T~ "complete"))
        
        monthly_data = data %>% 
          filter(Month == match(month_rv(),month.abb))
        
        if(nrow(data) >0){
          #extract slope and intercept to draw trendline
          trend_data = filtered_data() %>%
            filter(Well_Num == station_click())
          slope = trend_data %>%
            pull(trend_line_slope)
          slope = - as.numeric(slope)/365
          
          intercept = filtered_data() %>%
            filter(Well_Num == station_click()) %>%
            pull(trend_line_int)
          
          int.well = intercept + slope * as.numeric(min(as.Date(data$Date)))
          
          trend_df = data.frame(intercept = -int.well, slope = slope)
        maxgwl = max(data$value, na.rm = TRUE)
        mingwl = min(data$value, na.rm = TRUE)
        gwlrange = maxgwl - mingwl
        midgwl = (maxgwl + mingwl)/2
        lims  = c(midgwl + gwlrange, midgwl - gwlrange)
        data$max_lims <- max(lims[1], max(data$value, na.rm = TRUE) + 5)
        
        plot = ggplot(monthly_data) + 
          ggtitle(paste0("Well Number: ", station_click(),"; Month: ", month(match(month_rv(),month.abb), label = T, abbr = F),
                         " \nStation Class: ",trend_data$state_add)) +
          labs(subtitle = paste0("Slope: ", round(trend_data$slope, 2), " m/year; p-value: ", 
                                 round(trend_data$sig,2), "; Trend: ", trend_data$sig_state))+
          #geom_errorbar(aes(x = as.Date(Date), ymin = min, ymax = max, col = missing_dat), width = 0.3) +
          geom_point(aes(x = as.Date(Date), y = value, col = missing_dat)) +
          # ggtitle(paste0(month(match(month_rv(),month.abb), label = T, abbr = F), " Mean Water Level"))  +
          theme_minimal() +
          theme(text = element_text(colour = "black", size = 13),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                axis.line = element_line(colour="grey50"),
                legend.position = "bottom", legend.box =  "horizontal",
                plot.title = element_text(hjust = 0),
                plot.margin = margin(10, 10, 10, 10, "points"),
                plot.subtitle = element_text(hjust = 0, face = "plain", size = 11, 
                                             color = ifelse(trend_data$sig > 0.05, 'black', 'red'))) +
          scale_y_reverse() +
          scale_colour_manual(name = "",
                              labels = c('Monthly Mean', 'Incomplete Data (Interpolated)'), 
                              values = c("blue", "#A9A9A9")) +
          coord_cartesian(ylim = lims) +
          xlab("Date") +
          ylab("Depth Below Ground (metres)")
        
        
        if(filtered_data() %>%
           filter(Well_Num == station_click()) %>%
           pull(state_short) %in% c("Increasing", 
                                    "Moderate Rate of Decline",
                                    "Large Rate of Decline")){
          plot +
            geom_abline(data = trend_df, aes_string(intercept = "intercept", slope = "slope"),
                        col = "#d95f02")
        }
        else{
          plot
        }
        }
        else{
          ggplot() +
            geom_text(aes(x=1,y=1,label='Insufficient Data')) +
            ggthemes::theme_map()
        }
      }
    }
  })
  
  output$aquifer = renderPlot({
    if(station_click() == 'No Selection'){
      ggplot() +
        geom_text(aes(x=1,y=1,label='Click on a groundwater monitoring station on the map to see its Aquifer Information.')) +
        ggthemes::theme_map()
    }
  })
  
  output$aquifer_id = renderText({
    aquifer_id = wells_sf_full %>%
            filter(Well_Num == station_click())%>%
            pull(aquifer_id)


          paste0("Aquifer ID: ", aquifer_id)
  })
  
  output$aquifer_type = renderText({
    
    aquifer_type = wells_sf_full %>%
      filter(Well_Num == station_click())%>%
      pull(Aquifer_Type)
    
    paste0("Aquifer Type: ", aquifer_type)
  })
  
  output$aquifer_url = renderText({
    aquifer_id = wells_sf_full %>%
      filter(Well_Num == station_click())%>%
      pull(aquifer_id)
    
    if(is.na(aquifer_id)) {
      
      print("Aquifer information unavailable") 
      
    }else{
      
      HTML(paste0("<a href=", "https://apps.nrs.gov.bc.ca/gwells/aquifers/",
           aquifer_id, ">View available aquifer information</a"))
    }
  })
  
  output$aquifer_url2 = renderText({
    
    HTML(paste0("<a href=", 
                "https://governmentofbc.maps.arcgis.com/apps/webappviewer/index.html?id=b53cb0bf3f6848e79d66ffd09b74f00d&find=OBS%20WELL%20",
                station_click(),
                ">View this well on the Provincial Groundwater Observation Well Network</a"))
  }) 
  
  output$disclaimer = renderText({
    
    print("Note: The external links above must be opened in a new browser tab or window ")
    
  }) 
  
  # create a condition you use in the ui
  output$cond <- reactive({
    station_click() != 'No Selection' 
  })
  
  outputOptions(output, "cond", suspendWhenHidden = FALSE)
}
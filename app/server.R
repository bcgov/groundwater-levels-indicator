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
  
  #clicked selections
  station_click = reactiveVal('No Selection')
  
  #BC button
  observeEvent(input$abs_button, {
    
    # Change region_rv() to 'All'
    if(region_rv() != 'All'){
      region_rv('All')
      
      # Update map to BC zoom (customizable)
      leafletProxy('my_leaf') |>
        setView(lat = 40, lng = -130, zoom = 5)
      
      updateSelectInput(session = session,
                        'region_choice',
                        selected = 'All')
    }
    # if(region_rv() == "All") {
    #   #   # Update map to BC zoom (customizable)
    #   #   leafletProxy('my_leaf') |>
    #   #     setView(lat = 55, lng = -125, zoom = 5) 
    # }
    
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
    if(period_rv()=="Yearly"){
      shiny::updateTabsetPanel(
        inputId = 'tabset',
        selected = 'Summary Plot')
    }
    else{
      shiny::updateTabsetPanel(
        inputId = 'tabset',
        selected = 'Trend Plot')
    }
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
        filter(REGION_NAME == region_rv()) %>%
        left_join(filtered_data())
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
  mypal = colorFactor(palette = c("#2171b5", "#bdd7e7", "#ff7b7b", "#ff0000", "grey67"),
                      domain = map_data(),
                      levels = c("Increasing",
                                 "Stable",
                                 "Moderate Rate of Decline",
                                 "Large Rate of Decline",
                                 "Insufficient Data"
                      ),
                      ordered = T)
  
  # Map
  output$leafmap <- renderLeaflet({
    map = leaflet() %>%
      addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
      addTiles(group = 'Streets') %>%
      # add_bc_home_button() %>%
      # set_bc_view() %>%
      addLayersControl(baseGroups = c("CartoDB","Streets"),
                       options = layersControlOptions(collapsed = F),
                       position = 'topright') %>%
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
                       fillOpacity = 0.8,
                       label = ~paste0("Well No. ", Well_Num, " - ",state),
                       data = map_data()) %>%
      removeControl("legend") %>%
      addLegend(pal = mypal,
                values = ~state_short,
                title = "Groundwater Trend",
                data = map_data(),
                #className = "info legend solid circle", #Css from original leaflet script
                opacity = 1,
                layerId = 'legend',
                position = 'topright')
    
    if(region_rv() == "All"){
      map %>%
        # set_bc_view()
        setView(lat = 50, lng = -130, zoom = 5)
    }
    else{
      map %>%
        fitBounds(as.numeric(boundary_data()$xmin)-1, as.numeric(boundary_data()$ymin)-1, as.numeric(boundary_data()$xmax)+1, as.numeric(boundary_data()$ymax)+1)
    }
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
            geom_ribbon(aes_string(ymin = "value",
                                   ymax = "max_lims",
                                   fill = "'Groundwater Level'"), alpha = 0.6) +
            geom_point(data = data %>% filter(nReadings==0), aes_string(y = "value", col = "'interp'")) + 
            scale_x_date(expand = c(0,0)) +
            scale_y_reverse() +
            scale_fill_manual(name = '', labels = 'Monthly Median Groundwater Levels', values = c('Groundwater Level' = "#1E90FF")) +
            xlab("Date") +
            ylab ("Water Level (Meters Below Ground Level)")+
            theme_minimal()+
            theme(
              text = element_text(colour = "black", size = 11),
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
          summarise(median = median(value),
                    upperCI = quantile(value, 0.975),
                    lowerCI = quantile(value, 0.025))
        if(nrow(data) >0){

            # print(data)
            ggplot(data) +
            geom_ribbon(aes(x = Month, ymax = upperCI, ymin = lowerCI),
                        fill = "#1E90FF", alpha = 0.4)+
            geom_line(aes(x = Month, y = median),
                      col = "black",
                      linewidth = 1)+
              scale_y_reverse() +
            scale_x_continuous(breaks = 1:12, labels = month.abb) +
            theme_minimal() +
              theme(
                text = element_text(colour = "black"),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                axis.line = element_line(colour="grey50"),
                legend.position = "bottom", legend.box =  "horizontal",
                plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 11)) +
              theme(plot.margin = margin(10, 10, 10, 10, "points")) +
            xlab("Month") +
            ylab("Median Water Level \n(Meters Below Ground Level)")
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
          # print(trend_data)
          # print(trend_df)
          
          plot = ggplot(plot_data) +
            ggtitle(paste0("Station Class: ",trend_data$state_short,"\n",trend_data$slope," m/year")) +
            geom_errorbar(aes(x = as.Date(Date), ymin = min, ymax = max, col = missing_dat), width = 0.3) +
            geom_point(aes(x = as.Date(Date), y = annual_median, col = missing_dat)) +
            scale_x_date(expand = c(0.1,0.1)) +
            scale_y_reverse(expand = c(0,0)) + 
            coord_cartesian(ylim = lims) +
            scale_colour_manual(name = "",
                                labels = c('Annual Median (95% Confidence Intervals)', 'Incomplete Data (Interpolated)'), 
                                values = c("blue", "#A9A9A9")) +
            theme_minimal() +
            theme(
              text = element_text(colour = "black"),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.line = element_line(colour="grey50"),
              legend.position = "bottom", legend.box =  "horizontal",
              plot.title = element_text(hjust = 0),
              plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 11)) +
            theme(plot.margin = margin(10, 10, 10, 10, "points")) +
            xlab("Date") +
            ylab("Mean Water Level \n(Meters Below Ground Level)") +
            theme(legend.position = "bottom")
          
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
          filter(stat == "median") %>%
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
          ggtitle(paste0(month(match(month_rv(),month.abb), label = T, abbr = F), " Mean Water Level","\nStation Class: ",trend_data$state_short,"\n",trend_data$slope," m/year")) +
          geom_point(aes(x = as.Date(Date), y = value, col = missing_dat)) +
          # ggtitle(paste0(month(match(month_rv(),month.abb), label = T, abbr = F), " Mean Water Level"))  +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5),
                legend.position = "none") +
          theme(
            text = element_text(colour = "black"),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.line = element_line(colour="grey50"),
            legend.position = "bottom", legend.box =  "horizontal",
            plot.title = element_text(hjust = 0),
            plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 11)) +
          theme(plot.margin = margin(10, 10, 10, 10, "points")) +
          scale_y_reverse() +
          scale_colour_manual(name = "",
                              labels = c('Annual Median (95% Confidence Intervals)', 'Incomplete Data (Interpolated)'), 
                              values = c("blue", "#A9A9A9")) +
          coord_cartesian(ylim = lims) +
          xlab("Date") +
          ylab("Mean Water Level \n(Meters Below Ground Level)")
        
        # print(trend_data)
        # print(trend_df)
        # print(min(as.Date(data$Date)))
        # print(as.numeric(min(as.Date(data$Date))))
        # print(lims)
        # print(monthly_data$Date)
        
        if(filtered_data() %>%
           filter(Well_Num == station_click()) %>%
           pull(state_short) %in% c("Increasing", 
                                    "Moderate Rate of Decline",
                                    "Large Rate of Decline")){
          plot +
            geom_abline(data = trend_df, aes_string(intercept = "intercept", slope = "slope"),
                        col = "orange")
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
  
  output$aquifer = renderUI({
    if(station_click() == 'No Selection'){
      ggplot() +
        geom_text(aes(x=1,y=1,label='Click on a groundwater monitoring station on the map to see its Aquifer Information.')) +
        ggthemes::theme_map()
    }
    else{
      aquifer_id = wells_sf_full %>%
        filter(Well_Num == station_click())%>%
        pull(aquifer_id)
      
      aquifer_type = wells_sf_full %>%
        filter(Well_Num == station_click())%>%
        pull(Aquifer_Type)
      
      paste0("Aquifer ID: ", aquifer_id)
    }
  })
    
    # output$aquiferData = renderText({
    #   if(station_click() == 'No Selection'){
    #     paste("")
    #   }
    #   else{
    # 
    #   }
    # })

    
    # ggplot() +
    #   geom_text(aes(x=0.5,y=5,label=paste0("Aquifer ID: ", aquifer_id))) +
    #   geom_text(aes(x=0.5,y=4,label=paste0("Aquifer Type: ", aquifer_type))) +
    #   labs(title = tags$a("View available aquifer information", href = paste0("https://apps.nrs.gov.bc.ca/gwells/aquifers/",
    #                                      aquifer_id))) +
    #   ggthemes::theme_map()
    
    
    # l1 = paste0("Aquifer ID: ", aquifer_id)
    # 
    # 
    # 
    # l2 = paste0("Aquifer Type: ", aquifer_type)
    # 
    # l3 <- paste0("<a href=", "https://apps.nrs.gov.bc.ca/gwells/aquifers/",
    #                          aquifer_id, ">View available aquifer information</a","<br/>")
    # 
    # l4 = paste0("<a href=", "https://governmentofbc.maps.arcgis.com/apps/webappviewer/index.html?id=b53cb0bf3f6848e79d66ffd09b74f00d&find=OBS%20WELL%20",
    #             aquifer_id, ">View this well on the Provincial Groundwater Observation Well Network</a")
    
    # HTML(paste(l1, l2, l3, l4, sep = '<br/>'))
}
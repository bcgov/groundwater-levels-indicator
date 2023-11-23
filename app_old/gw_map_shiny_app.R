#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source('UI.R')
source('Result_Summary_Plot_Functions.R')
source('Water_Level_Plot_Functions.R')
source('Load_Data.R')

# Define UI for application
ui <- fluidPage(
  dataset_selection_bar, results_box, url_links
)


#################################################
# Define server logic
server <- function(input, output, session) {


################################################################
#Define reactive user interface (UI) objects

  # 1. Define reactive monthly drop down user interface element
  #If "Monthly" selected, show month drop down menu
  output$month_selector_UI = renderUI({
    if(period_rv() == 'Yearly') return(NULL)
    selectizeInput(inputId = 'month_selector',
                   label = 'Month',
                   multiple = F,
                   choices = month.abb,
                   selected = month.abb[1])
  })

  # 2. Define reactive reset button
  #Show only when the map element is selected
  output$reset <- renderUI({
  if (click_region() == 'No selection' & click_station() == 'No selection'){}
    else{

      reset_button

      }

  })


##############################################################
# Set up reactive values for user filter selection
# Prepare filtered data sets

source('Define_Reactive_Values.R')
source(file.path('Filter_Data.R'), local = T)$value
source(file.path('Update_Reactive_Values.R'), local = T)$value
source(file.path('Output_Text_Elements.R'), local = T)$value

###########################################################
#Event observations based on UI filter menus

  # 1. Region selector
  observeEvent(input$user_region_choice, {
    if(input$user_region_choice == 'All'){

      reset_elements(map = wells_map_start())

    }
    else{

      #Filter region by click selection
      newRegion <- filter(regions_sf, region_name == input$user_region_choice)
      region_selected(newRegion)
      click_region('Selection')

      region_update(region = input$user_region_choice)

    }

  })

  # 2. Apply drop down filters
  observe({

    req(input$user_period_choice)

    var_rv(input$user_var_choice)
    period_rv(input$user_period_choice)
    month_rv(input$month_selector)


    #If region selected, update region plot with selection
          if(click_region() != 'No selection'){

            region_update(region = input$user_region_choice)


          }

    #If well selected, update well plot elements with selection
          if(click_station() != 'No selection'){

            well_update(well = click_station())

          }

          else{

            leafletProxy("leafmap", session, data = wells_map_start()) %>%
              clearMarkers() %>%
              addCircleMarkers(layerId = ~Well_Num,
                               color = 'black',
                               fillColor = ~mypal(state_short),
                               radius = 5,
                               weight = 1,
                               fillOpacity = 0.8,
                               label = ~paste0("Well No. ", Well_Num, " - ",state_short),
                               data = wells_map_start())}
  })


  # 3. Selection reset button
  observeEvent(input$reset, {

    #Reset region selection
    updateSelectInput(inputId = "user_region_choice", selected = "All")

    #Reset table element
    reset_elements(map = wells_map_start())



  })



###########################################################
#Event observations based on clicks on the Leaflet map.

  # 1. If well clicked, define reactive values for well plot and info boxes
  observeEvent(input$leafmap_marker_click, {

    well_update(well = input$leafmap_marker_click$id,
                lat = input$leafmap_marker_click$lat,
                long = input$leafmap_marker_click$lng)


  })

  #2. If region clicked, update UI filter and show summary plot
  observeEvent(input$leafmap_shape_click, {

    click_region(input$leafmap_shape_click$id)

    #Reset well plot element
    click_station('No selection')

    #Filter region by click selection
    newRegion <- filter(regions_sf, OBJECTID == click_region())
    region_selected(newRegion)

    updateSelectInput(inputId = "user_region_choice", selected = paste0(newRegion$region_name))


  })


  ###################################################
  #Output element definitions

  #1. Define output header text element
  output$selected_station = renderText({

    text_header(click_station = click_station(),
                click_region = click_region())

  })


  #2. Define aquifer URL text element
  #Text element with observation well and aquifer information URLs
  output$AquiferURLs <- renderText({

    if(click_station() != 'No selection' ){

      HTML(paste0("<div style='background-color:white; padding: 10px'>",
                  "<strong>Learn more about this well:</strong>", "<br>"),
           well_url(), "<br>",
           aquifer_url(),"</div>")

      }
  })

  #3. Define trend result text element
  #Define colours for results text box
  colour_box <- data.frame(state=c("Large Rate of Decline", "Moderate Rate of Decline", "Stable", "Increasing", "Too many missing observations to perform trend analysis",
                                   "Recently established well; time series too short for trend analysis"),
                           color=c("darkorange", "#FFC300", "#999999", "#BCDEFF", "#FFC7D1", "white"))


  #Define trend result text element
  output$trendResult <- renderText({

    if(click_station() != 'No selection'){
  #For wells with stable or no trend information, do not include trend information in the results box
  if(state() == "Stable" | state() == "Too many missing observations to perform trend analysis"
     | state() == "Recently established well; time series too short for trend analysis"){

      HTML(paste0("<div style='background-color:",background_color(),"; padding: 8px'>",
                  paste0("Trend Category: <br>", state()),
                  "</div>")) }

  #For wells with trend information, include in the results box
  else{

      HTML(paste0("<div style='background-color:",background_color(),"'>",
                  paste0("Trend Category: <br>", state(), "<br> ", trendpre(), " ",
                         #format(slope() * 365, digits = 2, nsmall = 2,
                         format(slope(), digits = 2, nsmall = 2,
                                scientific = FALSE), " m/year"),
                  "</div>"))}
    }
      })

  #4. Define plot feature
  output$plot <- renderPlot({

    if(click_region() == 'No selection'){

    #Function to create well water level plot
    groundwater_level_plot(data = monthly_readings(),
                      period_choice = period_rv(),
                      var_choice = var_rv(),
                      month_choice = month_rv(),
                      clicked_station = click_station(),
                      trend_results = filtered_data())
    }

    else{
      req(regional_subset())
      prov_summary_plot(regional_subset())
    }


  })

  #5. Define leaflet map element

  #Define well trend state as factors
  results_out$state_short <- factor(results_out$state_short, c("Increasing",
                                             "Stable",
                                             "Moderate Rate of Decline",
                                             "Large Rate of Decline",
                                             "Too many missing observations",
                                             "Recently established well"
                                             ))

  mypal = colorFactor(palette = c("skyblue2", "gray60", "orange", "darkorange", "pink", "white"),
                      domain = wells_map(),
                      levels = c("Increasing",
                                 "Stable",
                                 "Moderate Rate of Decline",
                                 "Large Rate of Decline",
                                 "Too many missing observations",
                                 "Recently established well"
                                 ),
                      ordered = T)




  output$leafmap <- renderLeaflet({

    leaflet() %>%
      addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
      addTiles(group = 'Streets') %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      add_bc_home_button() %>%
      set_bc_view() %>%
      addLayersControl(baseGroups = c("CartoDB","Streets","Terrain"),
                       options = layersControlOptions(collapsed = F),
                       position = 'topright') %>%
      addPolygons(layerId = ~OBJECTID,
                  data = region_selected(),
                  label = ~paste0(REGION_NAME), color = "white", fillColor = "#C8D7E5",
                  weight = 1.5, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "#979B9D", weight = 2,
                                                      bringToFront = FALSE)) %>%
      addCircleMarkers(layerId = ~Well_Num,
                     color = 'black',
                     fillColor = ~mypal(state_short),
                     radius = 5,
                     weight = 1,
                     fillOpacity = 0.8,
                     label = ~paste0("Well No. ", Well_Num, " - ",state_short),
                     data = isolate(wells_map())) %>%
      removeControl("legend") %>%
      addLegend(pal = mypal,
                values = ~state_short,
                title = "Groundwater Trend",
                data = wells_map(),
                #className = "info legend solid circle", #Css from original leaflet script
                opacity = 1,
                layerId = 'legend',
                position = 'bottomleft')
  })


}
# Run the application
shinyApp(ui = ui, server = server)



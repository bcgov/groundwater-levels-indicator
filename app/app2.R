#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(RColorBrewer)
library(bcgroundwater)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(leaflet)
library(sf)
library(envreportutils)
library(DT)
#source('UI.R')
source('functions.R')

## Load data
results_out <- read.csv("out/gw_well_results.csv") %>%
  mutate(state_short = ifelse(state == "Recently established well; time series too short for trend analysis",
                              "Recently established well", ifelse(state == "Too many missing observations to perform trend analysis",
                                                                  "Too many missing observations", state))) %>%
  mutate(slope = -1*trend_line_slope) #This is reversed due to how slope is reported (meters below ground surface)

#Define unique states
state_list <- as.data.frame(unique(results_out$state_short))

#Use this to colour markers on map
results_t <- results_out %>%
  mutate(combined = paste0(period, "-", time_scale, "-", month)) %>%
  select(Well_Num, state_short, combined) %>%
  pivot_wider(., names_from = combined, values_from = state_short)

wells_sf <- read_sf("out/gw_well_attributes.gpkg") %>%
  st_transform(crs = 4326) %>%
  select(-Results_All, -Results_10yrs, -Results_20yrs) %>%
  mutate(Well_Num = as.integer(Well_Num))

wells_sf_full <- right_join(wells_sf, results_t, by=c("Well_Num"="Well_Num"))

monthlywells_ts <- read.csv("out/GWL_Monthly_Medians.csv") %>%
  mutate(stat = "median", value = med_GWL) %>%
  select(Well_Num, Year, Date, Month, stat, value, nReadings)

monthlywells_ts_mean <- read.csv("out/GWL_Monthly_Means.csv")  %>%
  mutate(stat = "mean", value = mean_GWL) %>%
  select(Well_Num, Year, Date, Month, stat, value, nReadings)

monthly_readings <- rbind(monthlywells_ts, monthlywells_ts_mean)

regions_sf <- read_sf("out/nr_polygons.gpkg") %>%
  st_transform(crs = 4326) %>%
  mutate(region_name = str_remove_all(REGION_NAME, " Natural Resource Region")) %>%
  mutate(region_name = ifelse(region_name == "Thompson-Okanagan", "Thompson / Okanagan",
                              ifelse(region_name == "Kootenay-Boundary", "Kootenay / Boundary", region_name)))

#Create bounding boxes for each region
bbox_list <- lapply(st_geometry(regions_sf), st_bbox)
maxmin <- as.data.frame(matrix(unlist(bbox_list),byrow=T,nrow=nrow(regions_sf)))
names(maxmin) <- names(bbox_list[[1]])

#Add x/y bounds to spatial file
regions_sf <- bind_cols(regions_sf, maxmin)

# Define UI for application
ui <- fluidPage(

  fluidRow(

    column(1, ),

    column(3,
           fluidRow(
           selectizeInput(inputId = "user_region_choice", label = "Natural Resource Region",
                          choices = c("All", "Cariboo", "Kootenay / Boundary", "Northeast", "Omineca",
                         "Skeena", "South Coast", "Thompson / Okanagan", "West Coast"), selected = "All")),

            fluidRow(column(3, offset=1,
             uiOutput("reset"),
             br()
      ))),

        column(2,
           radioButtons(inputId = "user_var_choice", label = "Time Range",
                        choices = c("All Data" = "All", "10 Years (2012-2022)" = "10 Years",
                                    "20 Years (2002-2022)" = "20 Years"), selected = "All")),
    column(3,
           selectizeInput(inputId = "user_period_choice", label = "Metric to Display",
                          choices = c("Mean Annual" = "Yearly", "Mean Monthly" = "Monthly"), selected = "Yearly"),

    ),

    column(3,
           uiOutput("month_selector_UI")),

),

  fluidRow(
    div(
      style="padding: 8px; border-bottom: 1px solid #CCC; background: #EEEEEE;",
    column(6,
           leafletOutput("leafmap", height = 600)),

    fluidRow(
      column(6, htmlOutput("selected_station")),
      column(6, htmlOutput("trendResult")),
      column(6,
             plotOutput("plot", height = 400))),


  )),

  fluidRow(
    column(6, htmlOutput("AquiferURLs"))

  )
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

    fluidRow(id = "reset", top = 100, left = 50,
                 right = "auto", bottom = "auto", width = "auto", height = "auto",
                 actionButton(inputId = "reset", label = "Clear selection", class = "btn-primary"))}

  })


##############################################################
# Set up reactive values for user filter selection

  var_rv <- reactiveVal('All') #User period choice (All, 10 Years, or 20 Years)
  period_rv <- reactiveVal('Yearly') #User variable choice (Yearly or Monthly)
  month_rv <- reactiveVal('Jan') #User month choice, available when Monthly variable selected

  #Define input data frames
  #Filter results data set by input variables
  filtered_data <- reactive({

       if(period_rv() == "Yearly"){
      filter(results_out,
             time_scale == var_rv(),
             period == period_rv())}

    else{

      #req()

      filter(results_out,
             time_scale == var_rv(),
             period == period_rv(),
             month == month_rv())}

  })


  wells_map_start = reactive({

    #sf_filter function identifies specified results column on sf object
    col <- sf_filter(time_scale = var_rv(),
                     period = period_rv(),
                     month = month_rv())

    if(input$user_region_choice != 'All'){

      well_markers <- filter(wells_sf_full, REGION_NAME == input$user_region_choice)

      } else{ well_markers <- wells_sf_full }


    well_markers %>%
      mutate(state_short = .[[col]]) %>%
      mutate(state_short = factor(state_short, c("Increasing",
                                        "Stable",
                                        "Moderate Rate of Decline",
                                        "Large Rate of Decline",
                                        "Too many missing observations",
                                        "Recently established well"
                                        )))

  })

  #Count missing wells
  well_state_count = reactive({

    filter(filtered_data(), region_name == region_name()) %>%
    group_by(state_short) %>%
    summarize("count" = n()) %>%
    right_join(state_list, by=c("state_short"="unique(results_out$state_short)")) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    arrange(state_short)

})

#Define the input data frame for the output table element
well_attr <- as.data.frame(wells_sf) %>%
  select(Well_Num, Well_Name, REGION_NAME, EMS_ID, Aquifer_Type, aquifer_id,
         Lat, Long, wellDepth_m, waterDepth_m, start_date, last_date, nYears, percent_missing) %>%
  mutate(start_date = as.character(start_date), last_date = as.character(last_date)) %>%
  mutate(aquifer_id = replace(aquifer_id, is.na(aquifer_id), "NA"))

#############################################################
# Set up reactive values for user's click response

  #Map selection values
  click_region <- reactiveVal('No selection') #well selection
  click_station <- reactiveVal('No selection') #region selection
  click_lat <- reactiveVal() #latitude of click selection
  click_long <- reactiveVal() #longitude of click selection
  wells_map <- reactiveVal() #well spatial file selection
  region_selected <- reactiveVal(regions_sf) #region spatial file selection

  #Region attributes
  regional_subset <- reactiveVal() #subset results data by region
  region_name <- reactiveVal() #save selected region name
  #well_state_count <- reactiveVal() #save count of well states
  recent_cnt <- reactiveVal() #count of wells too recent for trend analysis
  missing_cnt <- reactiveVal() #count of wells with too many missing values for trend analysis

  #Well attributes (Well Number, state, and trend slope)
  well_num = reactiveVal(well_attr) #well attribute table selection
  state = reactiveVal('No selection') #selected well state
  slope = reactiveVal() #selected well slope
  trendpre = reactiveVal() #positive/negative slope
  background_color = reactiveVal() #background colour of state
  aquifer_id = reactiveVal() #corresponding aquifer for selected well
  aquifer_url = reactiveVal() #url of corresponding aquifer
  well_url = reactiveVal() #url of corresponding well

###########################################################
#Event observations based on UI filter menus

  # 1. Region selector
  observeEvent(input$user_region_choice, {
    if(input$user_region_choice == 'All'){

      #Reset all elements
      region_selected(regions_sf)
      wells_map(wells_map_start())
      well_num(well_attr)
      click_station('No selection')
      click_region('No selection')

    }
    else{
      #Filter region highlighted
      newRegion <- filter(regions_sf, region_name == input$user_region_choice)
      region_selected(newRegion)

      #Set region name
      newRegionName <- as.character(newRegion$region_name)
      region_name(newRegionName)

      #Filter wells shown on map
      # wells_selected_reg <- filter(wells_map_start(), REGION_NAME == input$user_region_choice)
      # wells_map(wells_selected_reg)

      #Filter output table data frame
      well <- filter(well_attr, REGION_NAME == input$user_region_choice)
      well_num(well)

      #Filter results data frame by selected region
      region_data <- filter(filtered_data(), region_name == region_name())
      regional_subset(region_data)

      newRecent <- as.character(well_state_count()[4,2])
      recent_cnt(newRecent)
      newMissing <- as.character(well_state_count()[6,2])
      missing_cnt(newMissing)

      #Zoom map to selected region
      leafletProxy("leafmap", session) %>%
        fitBounds(region_selected()$xmin, region_selected()$ymin, region_selected()$xmax, region_selected()$ymax) %>%
        clearMarkers() %>%
        addCircleMarkers(layerId = ~Well_Num,
                         color = 'black',
                         fillColor = ~mypal(state_short),
                         radius = 5,
                         weight = 1,
                         fillOpacity = 0.8,
                         label = ~paste0("Well No. ", Well_Num, " - ",state_short),
                         data = wells_map_start())

    }

  })

  # 2. Apply filters button
  observe({



    var_rv(input$user_var_choice)
    period_rv(input$user_period_choice)

    req(input$month_selector)

    month_rv(input$month_selector)
    if(is.null(month_rv())){month_rv('Jan')}

    #If region selected, update region plot with selection
          if(click_region() != 'No selection'){

            var_rv(input$user_var_choice)
            period_rv(input$user_period_choice)
            month_rv(input$month_selector)
            if(is.null(month_rv())){month_rv('Jan')}

            newRecent <- as.character(well_state_count()[4,2])
            recent_cnt(newRecent)
            newMissing <- as.character(well_state_count()[6,2])
            missing_cnt(newMissing)

            #newRegion <- filter(regions_sf, OBJECTID == click_region())
            #region_selected(newRegion)

            #Filter results data frame by selected region
            region_data <- filter(filtered_data(), region_name == region_name())
            regional_subset(region_data)

            #Keep regional well subset but update input data set
            wells_selected_reg <- filter(wells_map_start(), REGION_NAME == region_name())
            wells_map(wells_selected_reg)

            leafletProxy("leafmap", session) %>%
              fitBounds(region_selected()$xmin, region_selected()$ymin, region_selected()$xmax, region_selected()$ymax, options = leafletOptions(animate = FALSE)) %>%
              clearMarkers() %>%
              addCircleMarkers(layerId = ~Well_Num,
                               color = 'black',
                               fillColor = ~mypal(state_short),
                               radius = 5,
                               weight = 1,
                               fillOpacity = 0.8,
                               label = ~paste0("Well No. ", Well_Num, " - ",state_short),
                               data = wells_map_start())

          }

    #If well selected, update well plot elements with selection
          if(click_station() != 'No selection'){

            var_rv(input$user_var_choice)
            period_rv(input$user_period_choice)
            month_rv(input$month_selector)
            if(is.null(month_rv())){month_rv('Jan')}

            #Update state of selected well
            newState <- filter(filtered_data(), Well_Num==click_station()) %>%
              select(state)
            state(newState)
            #Update slope of selected well
            newSlope <- filter(filtered_data(), Well_Num==click_station()) %>%
              select(slope)
            slope(newSlope)
            #Re-define trend prefix based on trend slope
            newTrendpre <- ifelse(slope() > 0, "+", " ")
            trendpre(newTrendpre)
            #Update trend box background colour based on state
            newBackground_color = colour_box[colour_box$state == as.character(state()), "color"]
            background_color(newBackground_color)

            #Keep selected well selected on map
            leafletProxy("leafmap", session, data = wells_map_start()) %>%
              clearMarkers() %>%
              addCircleMarkers(layerId = ~Well_Num,
                               color = 'black',
                               fillColor = ~mypal(state_short),
                               radius = 5,
                               weight = 1,
                               fillOpacity = 0.8,
                               label = ~paste0("Well No. ", Well_Num, " - ",state_short),
                               data = wells_map_start()) %>%
              addCircleMarkers(lat = click_lat(), click_long(),
                               group = "selected",
                               fillColor = "yellow",
                               fillOpacity = 1)

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
    well_num(well_attr)

    #Reset map zoom
    leafletProxy("leafmap", session, data = wells_map_start()) %>%
      set_bc_view() %>%
      clearGroup("selected")

    #Reset well plot and info boxes
    click_station('No selection')
    click_region('No selection')
    click_long(NULL)

  })



###########################################################
#Event observations based on clicks on the Leaflet map.

  # 1. If well clicked, define reactive values for well plot and info boxes
  observeEvent(input$leafmap_marker_click, {

    #Reset regional selection
    click_region('No selection')

    #Define input variables
    # Capture the info of the clicked point and use this for filtering.
    click_station(input$leafmap_marker_click$id)
    click_lat(input$leafmap_marker_click$lat)
    click_long(input$leafmap_marker_click$lng)

    # #Filter output table data frame
    well <- filter(well_attr, Well_Num==click_station())
    well_num(well)

    #Return aquifer number of selected well if available
    newAquifer <- as.character(well$aquifer_id)
    aquifer_id(newAquifer)
    if(newAquifer!="NA"){
    newAquifer_url <- paste0("<a href=", "https://apps.nrs.gov.bc.ca/gwells/aquifers/",
                             newAquifer, ">View available aquifer information</a")}
    else{
      newAquifer_url <- ""
    }

    aquifer_url(newAquifer_url)

    #Return well URL

     newWell_url <- paste0("<a href=", "https://governmentofbc.maps.arcgis.com/apps/webappviewer/index.html?id=b53cb0bf3f6848e79d66ffd09b74f00d&find=OBS%20WELL%20",
                               click_station(), ">View this well on the Provincial Groundwater Observation Well Network</a")
     well_url(newWell_url)

    #Return state of selected well
    newState <- filter(filtered_data(), Well_Num==click_station()) %>%
      select(state)
    state(newState)
    #Return slope of selected well
    newSlope <- filter(filtered_data(), Well_Num==click_station()) %>%
      select(slope)
    slope(newSlope)
    #Define trend prefix based on trend slope
    newTrendpre <- ifelse(slope() > 0, "+", "") #This is reversed due to how slope is reported (meters below ground surface)
    trendpre(newTrendpre)
    #Define background colour based on state
    newBackground_color = colour_box[colour_box$state == as.character(state()), "color"]
    background_color(newBackground_color)

    #Highlight selected well on map
    leafletProxy("leafmap", session, data = wells_map()) %>%
      clearGroup("selected") %>%
      addCircleMarkers(lat = click_lat(), lng = click_long(),
                       group = "selected",
                  fillColor = "yellow",
                  fillOpacity = 1)


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

  if(click_station() == 'No selection' & click_region() == 'No selection'){
    return(NULL)
  }

  if(click_station() != 'No selection' & click_region() == 'No selection'){


      HTML(paste0("<div style='background-color:white; padding: 8px'>",
                  "<strong>Observation Well: ", click_station()), "</strong> <br>",
           "Aquifer ID:", aquifer_id(), "</div")

  }else{

    #Report selected region

    HTML(paste0("<div style='background-color:white; padding: 8px'>",
                "<strong>Natural Resource Region: ", region_name(),
                "</strong> <br>",
                "Count of recent wells: ", recent_cnt(), "<br>",
                "Count of wells with missing data: ", missing_cnt(), "</div"))
    }
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
    #Variable choices commented out for now
    groundwater_level_plot(data = monthly_readings,
                      period_choice = period_rv(),
                      var_choice = var_rv(),
                      month_choice = month_rv(),
                      clicked_station = click_station(),
                      trend_results = filtered_data())
    }

    else{
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


  col_rv <- reactive({

  col <- sf_filter(time_scale = var_rv(),
                   period = period_rv(),
                   month = month_rv())

  as.character(colnames(wells_sf_full[,col])[1])

  })

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



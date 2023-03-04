#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RColorBrewer)
library(bcgroundwater)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(leaflet)
library(sf)
source('UI.R')
source('functions.R')

## Load data
results_out <- read.csv("data/gw_well_attributes.csv") %>%
  rename("region_name" = REGION_NAME)
monthlywells_ts <- read.csv("data/gwl_monthly.csv")
wells_sf <- read_sf("data/gw_well_attributes.gpkg") %>%
  st_transform(crs = 4326) %>%
  mutate(state_short = ifelse(category == "N/A", "No trend analysis", state))
regions_sf <- read_sf("data/ADM_NR_DST_polygon_dissolved.gpkg") %>%
  st_transform(crs = 4326) %>%
  mutate(region_name = str_remove_all(RGN_RG_NTM, " Natural Resource Region"))

#if (!exists("results_out"))    load("./data/analysis_data.RData")
#if (!exists("monthlywells_ts")) load("./data/clean_well_data.RData")

# Define UI for application
ui <- fluidPage(

  fluidRow(

    column(3,
           radioButtons(inputId = "user_period_choice", label = "Time Range",
                        choices = c("All Data" = "All", "Last 10 Years (2012-2022)" = "2012+"), selected = "All")),

    column(3,
           selectizeInput(inputId = "user_var_choice", label = "Metric to Display",
                          choices = c("Mean Annual" = "Mean", "Minimum Annual" = "Minimum"), selected = "Mean")),

    column(3,
           radioButtons(inputId = "time_scale", label = "Yearly or Monthly Data",
                        choices = c("Yearly", "Monthly"), selected = "Yearly")),
    column(3,
           uiOutput("month_selector_UI")),

  ),

  fluidRow(
    div(
      style="padding: 8px; border-bottom: 1px solid #CCC; background: #EEEEEE;",
    column(5,
           leafletOutput("leafmap", height = 600)),

    fluidRow(
      column(6, h3(textOutput("selected_station"))),
      column(3, style="padding-left:8px;padding-right:8px;padding-top:8px;padding-bottom:8px", htmlOutput("trendResult")),
      column(3, htmlOutput("AquiferURLs")),
      column(6,
             plotOutput("plot", height = 400))),


  )),

  fluidRow(tableOutput("table")

  )
)



# Define server logic
server <- function(input, output) {

  #Based on Chris's streamflow script
  # Update month selector to show months, if user picks month time-scale
  observeEvent(input$time_scale, {
    if(input$time_scale == 'Monthly'){
      updateSelectizeInput(inputId = 'user_var_choice',
                           choices = c("Monthly Mean" = "Mean", "Monthly Minimum" = "Minimum")
      )
    }
    if(input$time_scale == 'Annual'){
      updateSelectizeInput(inputId = 'user_var_choice',
                           choices = c("Mean Annual" = "Mean", "Minimum Annual" = "Minimum")
      )
    }
  })


  output$month_selector_UI = renderUI({
    if(input$time_scale == 'Yearly') return(NULL)
    selectizeInput(inputId = 'month_selector',
                   label = 'Month',
                   multiple = F,
                   choices = month.abb,
                   selected = month.abb[1])
  })


well_attr <- as.data.frame(wells_sf) %>%
  select(Well_Num, Well_Name, REGION_NAME, EMS_ID, Aquifer_Type, aquifer_id,
         Lat, Long, wellDepth_m, waterDepth_m, start_date, last_date, nYears, percent_missing)

  # Set up reactive values for user's click response
  click_station <- reactiveVal('No selection')
  well_num = reactiveVal(well_attr)
  state = reactiveVal('No selection')
  slope = reactiveVal()
  #trendpre = reactiveVal("")



  output$trendResult <- renderUI({
    return(NULL)})


  # Watch for a click on the leaflet map. Once clicked...
  # 1. Update selection.
  observeEvent(input$leafmap_marker_click, {
    # Capture the info of the clicked point and use this for filtering.
    click_station(input$leafmap_marker_click$id)
    well <- filter(well_attr, Well_Num==click_station())
    well_num(well)
    newState <- filter(results_out, Well_Num==click_station()) %>%
      select(state)
    state(newState)
    newSlope <- filter(results_out, Well_Num==click_station()) %>%
      select(trend_line_slope)
    slope(newSlope)
    trendpre <- ifelse(slope() > 0, "+", "")

    #Text with well id
    output$selected_station = renderText({
      paste0("Observation Well: ",click_station())})

    output$AquiferURLs <- renderText({
      HTML(paste0("<div>", "<strong>Learn more about this well:</strong>", "<br>",
      "<a href='https://www.env.gov.bc.ca/soe/indicators/water/groundwater-levels.html'",
      ">Groundwater Level Data & Information</a"), "<br>",
      "<a href='https://www.env.gov.bc.ca/soe/indicators/water/groundwater-levels.html'",
      ">Aquifer Summary</a", "</div>")
    })

  if(newState == "Too many missing observations to perform trend analysis"
     | newState == "Recently established well; time series too short for trend analysis"){

    output$trendResult <- renderText({paste0(state())})}


    if(newState == "Increasing" | newState == "Stable" | newState == "Moderate Rate of Decline" |
       newState == "Large Rate of Decline"){

      output$trendResult <- renderText({
        background_color = colour_box[colour_box$state == as.character(state()), "color"]
        HTML(paste0("<div style='background-color:",background_color,"'>",
                    paste0("Trend Category: <br>", state(), "<br> ", trendpre, " ",
                           #format(slope() * 365, digits = 2, nsmall = 2,
                           format(slope(), digits = 2, nsmall = 2,
                                  scientific = FALSE), " m/year"),
                    "</div>")) })}



  })



  #Colour selector for results text box
  colour_box <- data.frame(state=c("Large Rate of Decline", "Moderate Rate of Decline", "Stable", "Increasing", "Too many missing observations to perform trend analysis",
                                   "Recently established well; time series too short for trend analysis"),
                           color=c("darkorange", "#FFC300", "#999999", "#BCDEFF", "#EEEEEE", "white"))


  #Plot
  output$plot <- renderPlot({

    #Variable choices commented out for now
    groundwater_level_plot(data = monthlywells_ts,
                      variable_choice = "input$user_var_choice",
                      clicked_station = click_station(),
                      trend_results = results_out)#,
                      #slopes = "senslope_dat()")#,
                      #caption_label = "date_choice_label()")


  })

  mypal = colorFactor(palette = c("skyblue2", "gray60", "orange", "darkorange", "gray90", "white"),
                  domain = wells_sf$state,
                  levels = c("Increasing",
                             "Stable",
                             "Moderate Rate of Decline",
                             "Large Rate of Decline",
                             "Too many missing observations to perform trend analysis",
                             "Recently established well; time series too short for trend analysis"),
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
                       position = 'bottomleft') %>%
      addPolygons(data = regions_sf, label = paste(regions_sf$RGN_RG_NTM), color = "white", fillColor = "#C8D7E5", weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "#979B9D", weight = 2,
                                                      bringToFront = FALSE)) %>%
      addCircleMarkers(layerId = ~Well_Num,
                     color = 'black',
                     fillColor = ~mypal(state),
                     radius = 5,
                     weight = 1,
                     fillOpacity = 0.8,
                     label = ~paste0("Well No. ",Well_Num, " - ",state_short),
                     data = wells_sf) #%>%
      # addLegend(pal = mypal(), #This doesn't work
      #           values = ~state,
      #           title = 'Groundwater Trend',
      #           data = wells_sf,
      #           layerId = 'legend')
  })



  output$table <- renderTable({well_num()})

}
# Run the application
shinyApp(ui = ui, server = server)

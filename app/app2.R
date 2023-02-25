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
wells_sf <- read_sf("data/gw_well_attributes.gpkg") %>% #This can be the same object as results_out
  st_transform(crs = 4326)

#if (!exists("results_out"))    load("./data/analysis_data.RData")
#if (!exists("monthlywells_ts")) load("./data/clean_well_data.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(

  fluidRow(

    column(3,
           radioButtons("radio1", h3("Time Range"),
                        choices = list("All data" = 1, "Last 10 years" = 2), selected = 1)),

    column(3,
           radioButtons("radio2", h3("Metric"),
                        choices = list("Mean" = 1, "Minimum" = 2), selected = 1)),

    column(3,
           radioButtons("radio3", h3("Calculated on"),
                        choices = list("Annual" = 1, "Monthly" = 2), selected = 1)),

    column(3,
           selectInput("select", h3("Month Choice"),
                       choices = list("Jan" = 1, "Feb" = 2,
                                      "Mar" = 3), selected = 1)),
  ),



  fluidRow(
    div(
      style="padding: 8px; border-bottom: 1px solid #CCC; background: #EEEEEE;",
    column(5,
           leafletOutput("leafmap", height = 600)),

    fluidRow(
      column(6, h3(textOutput("selected_station"))),
      column(3, style="padding-left:8px;padding-right:8px;padding-top:8px;padding-bottom:8px", htmlOutput("trendResult")),
      column(3, h6("Learn more about this well"),
             a(href="https://www.env.gov.bc.ca/soe/indicators/water/groundwater-levels.html", "Groundwater Level Data & Information"),
             br(),
              a(href="https://www.env.gov.bc.ca/soe/indicators/water/groundwater-levels.html", "Aquifer Summary")),
      column(6,
             plotOutput("plot", height = 400))),


  )),

  fluidRow(tableOutput("table")

  )
)



# Define server logic
server <- function(input, output) {

  #Based on Chris's streamflow script
  # Set up reactive values for user's click response
  click_station <- reactiveVal('No selection')
  well_num = reactiveVal(results_out)
  state = reactiveVal('No selection')
  slope = reactiveVal()


  # Watch for a click on the leaflet map. Once clicked...
  # 1. Update selection.
  observeEvent(input$leafmap_marker_click, {
    # Capture the info of the clicked point and use this for filtering.
    click_station(input$leafmap_marker_click$id)
    well <- filter(results_out, Well_Num==click_station())
    well_num(well)
    newState <- filter(results_out, Well_Num==click_station()) %>%
      select(state)
    state(newState)
    newSlope <- filter(results_out, Well_Num==click_station()) %>%
      select(trend_line_slope)
    slope(newSlope)
  })

  #Text with well id
  output$selected_station = renderText({
    paste0("Observation Well: ",click_station())})

  #Plot
  output$plot <- renderPlot({

    #Variable choices commented out for now
    groundwater_level_plot(data = monthlywells_ts,
                      variable_choice = "input$user_var_choice",
                      clicked_station = click_station(),
                      stations_shapefile = wells_sf,
                      slopes = "senslope_dat()")#,
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
      addCircleMarkers(layerId = ~Well_Num,
                     color = 'black',
                     fillColor = ~mypal(state),
                     radius = 5,
                     weight = 1,
                     fillOpacity = 0.8,
                     label = ~paste0("Well No. ",Well_Num, " - ",state),
                     data = wells_sf) #%>%
      # addLegend(pal = mypal(), #This doesn't work
      #           values = ~state,
      #           title = 'Groundwater Trend',
      #           data = wells_sf,
      #           layerId = 'legend')
  })


  colour_box <- data.frame(state=c("Large Rate of Decline", "Moderate Rate of Decline", "Stable", "Increasing", "Too many missing observations to perform trend analysis",
                                   "Recently established well; time series too short for trend analysis"),
                           color=c("darkorange", "#FFC300", "#999999", "#BCDEFF", "#EEEEEE", "white"))

  output$trendResult <- renderText({
    background_color = colour_box[colour_box$state == as.character(state()), "color"]
    HTML(paste0("<div style='background-color:",background_color,"'>",
                paste0("Trend Category: ", state(),
                       " ",
                       slope()),
                "</div>"))
  })

  output$table <- renderTable({well_num()})

}
# Run the application
shinyApp(ui = ui, server = server)

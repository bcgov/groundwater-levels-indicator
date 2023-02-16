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
source('UI.R')
source('functions.R')

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

    column(6,
           leafletOutput("leafmap", height = 500)),
      column(6,
                    "Fluid 6"),


  )

)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$leafmap <- renderLeaflet({

    leaflet() %>%
      addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
      addTiles(group = 'Streets') %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      add_bc_home_button() %>%
      set_bc_view() %>%
      addLayersControl(baseGroups = c("CartoDB","Streets","Terrain"),
                       options = layersControlOptions(collapsed = F),
                       position = 'bottomright')
  })

}

# Run the application
shinyApp(ui = ui, server = server)

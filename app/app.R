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
library(tidyverse)
library(dplyr)
library(sf)
source('functions.R')

#Read in data, filter out uncategorized wells
input_dataframe <- read.csv("data/gw_well_results.csv") %>%
  filter(., state !="Too many missing observations to perform trend analysis" & state !="Recently established well; time series too short for trend analysis" )

# Define UI for application
ui <- fluidPage(

  fluidRow(

    column(3,
           radioButtons(inputId = "user_period_choice", label = "Time Range",
                        choices = c("All Data" = "All", "Last 10 Years (2012-2022)" = "10 Years"), selected = "All")),

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

    column(5,
           plotOutput("summary_plot", height = 400)),
      column(7,
             plotOutput("regional_plot", height = 400)),


  )

)




# Define server logic
server <- function(input, output) {

  # Update month selector to show months, if user picks month time-scale
  observeEvent(input$time_scale, {
    if(input$time_scale == 'Monthly'){
      updateSelectizeInput(inputId = 'user_var_choice',
                           choices = c("Monthly Mean" = "Mean", "Monthly Minimum" = "Minimum")
      )
    }
    if(input$time_scale == 'Yearly'){
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

 #Filter dataset by input variables
  filtered_data <- reactive({

    filter(input_dataframe,
           period == input$user_period_choice,
           metric == input$user_var_choice,
           time_scale == input$time_scale)

  })


  output$summary_plot <- renderPlot({

    prov_summary_plot(filtered_data())

  })

  output$regional_plot <- renderPlot({

    regional_summary_plot(filtered_data())

})}

# Run the application
shinyApp(ui = ui, server = server)

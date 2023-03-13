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
input_dataframe <- read.csv("data/gw_well_results.csv")

# Define UI for application
ui <- fluidPage(

  fluidRow(

    column(3,
           radioButtons(inputId = "user_period_choice", label = "Time Range",
                        choices = c("All Data" = "All", "Last 10 Years (2012-2022)" = "10 Years",
                                    "Last 20 Years (2002-2022)" = "20 Years"), selected = "All")),

    column(3,
           selectizeInput(inputId = "user_var_choice", label = "Metric to Display",
                          choices = c("Mean Annual" = "Yearly", "Mean Monthly" = "Monthly"), selected = "Yearly")),

    column(3,
           uiOutput("month_selector_UI")),


  ),

  fluidRow(

    column(5,
           plotOutput("summary_plot", height = 400)),
      column(7,
             plotOutput("regional_plot", height = 440)),


  )


)




# Define server logic
server <- function(input, output) {

  #Define reactive UI for months based on monthly mean selection
  output$month_selector_UI = renderUI({
    if(input$user_var_choice == 'Yearly') return(NULL)
    selectizeInput(inputId = 'month_selector',
                   label = 'Month',
                   multiple = F,
                   choices = month.abb,
                   selected = month.abb[1])
  })

  #Split input data set to speed up processing?
  # monthly_data <- input_dataframe %>%
  #   filter(time_scale == "Monthly")
  # yearly_data <- input_dataframe %>%
  #   filter(time_scale == "Yearly")

 #Filter dataset by input variables
  filtered_data <- reactive({

    if(input$user_var_choice == 'Yearly'){
    filter(input_dataframe,
           time_scale == input$user_var_choice,
           period == input$user_period_choice)}

    else{
     filter(input_dataframe,
            time_scale == input$user_var_choice,
             period == input$user_period_choice,
            month == input$month_selector)}

  })

  month <- reactiveVal('Yearly')

  observeEvent(input$month_selector, {

  if(input$user_var_choice == 'Monthly'){
    filter(input_dataframe,
           period == input$user_period_choice,
           time_scale == input$user_var_choice,
           month == input$month_selector)

    month_in <- paste0(input$month_selector)
    month(month_in)
    }

    #output$text <- renderText({paste0(month())})

  })



  output$summary_plot <- renderPlot({

    prov_summary_plot(filtered_data())

  })

  output$regional_plot <- renderPlot({

    regional_summary_plot(filtered_data())

})}

# Run the application
shinyApp(ui = ui, server = server)

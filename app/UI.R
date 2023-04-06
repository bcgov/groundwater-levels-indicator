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

# Data subset options
dataset_selection_bar = fluidRow(

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

    )

#Well or region selection results display
results_box = fluidRow(
  div(
    style="padding: 8px; border-bottom: 1px solid #CCC; background: #EEEEEE;",
    column(6,
           leafletOutput("leafmap", height = 600)),

    fluidRow(
      column(6, htmlOutput("selected_station")),
      column(6, htmlOutput("trendResult")),
      column(6,
             plotOutput("plot", height = 400))),


  ))


# Aquifer and groundwater well URL links
url_links = fluidRow(
  column(6, htmlOutput("AquiferURLs"))

)

#Reset button

reset_button =  fluidRow(
  id = "reset", top = 100, left = 50,
                             right = "auto", bottom = "auto", width = "auto", height = "auto",
                             actionButton(inputId = "reset", label = "Clear selection", class = "btn-primary"))

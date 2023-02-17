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

input <- read.csv("data/gw_well_attributes.csv") %>%
  filter(., state !="Too many missing observations to perform trend analysis" & state !="Recently established well; time series too short for trend analysis" )


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
           plotOutput("summary_plot", height = 500)),
      column(6,
             plotOutput("regional_plot", height = 500)),


  )

)

#All summary
input_summary <- input %>%
  group_by(state) %>%
  summarize("count"=n()) %>%
  mutate( col = case_when(
    state == "Stable" ~ "gray70",
    state == "Moderate Rate of Decline" ~ "orange",
    state == "Large Rate of Decline" ~ "darkorange",
    state == "Increasing" ~ "skyblue2"
  )) %>%
  mutate(total_no_wells = nrow(input)) %>%
  mutate(prop = (count/total_no_wells)*100)

bar_labels <- input_summary %>%
  group_by(state) %>%
  summarize("no_wells"=sum(count)) %>%
  mutate("no_wells_lab" = paste0(no_wells, " wells"))

input_summary <- right_join(input_summary, bar_labels)

input_summary$col <- factor(input_summary$col, c("darkorange", "orange", "gray70", "skyblue2"))
input_summary$state <- factor(input_summary$state, c("Large Rate of Decline", "Moderate Rate of Decline", "Stable", "Increasing"))

barcol=levels(as.factor(input_summary$col))
barlab=levels(as.factor(input_summary$state))

input_summary <- input_summary %>%
  mutate(label_x = cumsum(count))

#Regional summary
input_regional <- input %>%
  group_by(REGION_NAME, state) %>%
  summarize("count"=n()) %>%
  mutate( col = case_when(
    state == "Stable" ~ "gray70",
    state == "Moderate Rate of Decline" ~ "orange",
    state == "Large Rate of Decline" ~ "darkorange",
    state == "Increasing" ~ "skyblue2"
  ))

bar_labels_r <- input_regional %>%
  group_by(REGION_NAME) %>%
  summarize("no_wells"=sum(count)) %>%
  mutate("no_wells_lab" = paste0(no_wells, " wells"))

input_regional <- right_join(input_regional, bar_labels_r)

input_regional$REGION_NAME <-factor(input_regional$REGION_NAME, c("West Coast", "Thompson / Okanagan", "South Coast", "Skeena", "Omineca",
                                                                "Northeast", "Kootenay / Boundary", "Cariboo"))

input_regional$col <- factor(input_regional$col, c("darkorange", "orange", "gray70", "skyblue2"))
input_regional$state <- factor(input_regional$state, c("Large Rate of Decline", "Moderate Rate of Decline", "Stable", "Increasing"))

barcolr=levels(as.factor(input_regional$col))
barlabr=levels(as.factor(input_regional$state))

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$summary_plot <- renderPlot({

    ggplot(data=input_summary) +
      geom_col(mapping=aes(x=prop, y=state, fill=state, width = 0.5)) +
      scale_fill_manual(label=barlab, values=barcol) +
      geom_text(aes(x=prop, y=state, label = no_wells_lab), hjust = -0.1) +
      scale_x_continuous(expand = c(0,0)) +
      expand_limits(x=c(0,110)) +
      guides(fill = guide_legend(reverse = TRUE))+
      xlab("Proportion of wells (%)") + ylab(NULL) +
      theme_classic() +
      theme(legend.position="bottom",
            legend.title=element_blank())
  })

  output$regional_plot <- renderPlot({

    ggplot(data=input_regional) +
      geom_col(mapping=aes(x=count, y=REGION_NAME, fill=state, width = 0.5)) +
      scale_fill_manual(label=barlabr, values=barcolr) +
      geom_text(aes(x=no_wells, y=REGION_NAME, label = no_wells_lab), hjust = -0.1) +
      scale_x_continuous(expand = c(0,0)) +
      expand_limits(x=c(0,60)) +
      guides(fill = guide_legend(reverse = TRUE))+
      xlab("Number of wells") + ylab(NULL) +
      theme_classic() +
      theme(legend.position="bottom",
      legend.title=element_blank())
    })

}

# Run the application
shinyApp(ui = ui, server = server)

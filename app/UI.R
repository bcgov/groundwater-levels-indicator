

# Trend selection options
trend_select_options_tab = wellPanel(
  selectizeInput(inputId = 'region_choice',
                 label = 'Natural Resource Region',
                 choices = c("All", "Cariboo", "Kootenay / Boundary", "Northeast", "Omineca",
                             "Skeena", "South Coast", "Thompson / Okanagan", "West Coast"),
                 selected = 'Mean',
                 width = '100%'),
  fluidRow(
    column(width = 6,
           radioButtons(inputId = 'time_scale',
                        label = 'Yearly or Monthly Data',
                        choices = c('Annual' = "Yearly",
                                    'Monthly'),
                        selected = 'Yearly'
           )
    ),
    column(width = 6,
           uiOutput('month_selector_UI')
    )
  ),
  radioButtons(inputId = 'var_choice',
               label = 'Timespan Range',
               choices = c('All Data' = "All",
                           '20 Years (2002 - 2022)' = "20 Years",
                           '10 Years (2012 - 2022)' = "10 Years"
                           ),
               selected = 'All',
               inline = F)
)

map_abs_panel = absolutePanel(
  top = 0, left = 0, right = 0,
  fixed = TRUE,
  div(
    style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFFF;",
    fluidRow(
      leafletOutput('leafmap',
                    height = '1000px')
    )
  )
)

water_level_plot_tab = card(
  card_body(
    plotOutput('summaryPlot', height = 300)
  )
)

aquifer_info_tab = card(
  div(
    style ="background:white",
    conditionalPanel(
      condition = "output.cond == false",
      plotOutput("aquifer", height = 300)
    ),
    conditionalPanel(
      style = "height:300px",
      condition = "output.cond == true",
      
      textOutput("aquifer_id"),
      br(),
      br(),
      textOutput("aquifer_type"),
      br(),
      br(),
      htmlOutput("aquifer_url"),
      br(),
      br(),
      htmlOutput("aquifer_url2"),
      
      tags$head(tags$style("#aquifer_id{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
      ))
      
    )
  )
)

trend_plot_tab = card(
  card_body(
    plotOutput('trendPlot', height = 300)
  )
)

# Absolute Panel with trend selection.
trend_select_abs_panel = absolutePanel(
  id = 'trend_selector',
  top = 400, left = 10, width = 450, #height = 800,
  draggable = F,
  tabsetPanel(
    id = 'tabset',
    tabPanel('Trend Options',trend_select_options_tab),
    tabPanel('Trend Plot', trend_plot_tab),
    tabPanel('Summary Plot',water_level_plot_tab),
    tabPanel('Aquifer Info', aquifer_info_tab)
  )
)

ui = shiny::fluidPage(
  # Enables us to do some fancy things in javascript...
  useShinyjs(),
  # Include our own styling sheet; defined class 'my_home_button'
  includeCSS('www/bc_button.css'),

  tags$head(tags$style(
    HTML('#trend_selector {opacity:0.9;}
         #trend_selector:hover{opacity:0.9;}'))),
  titlePanel("Flow Indicator"),
  # Throw in our own action button, setting class to 'my_home_button'
  actionButton(
    'abs_button',
    '',
    class = 'my_bc_button'
  ),
  map_abs_panel,
  trend_select_abs_panel
)
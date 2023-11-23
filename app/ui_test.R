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
                        choices = c('Annual','Monthly'),
                        selected = 'Annual'
           )
    ),
    column(width = 6,
           uiOutput('month_selector_UI')
    )
  ),
  radioButtons(inputId = 'period_choice',
               label = 'Timespan Range',
               choices = c('All Data',
                           '10 Years (2012 - 2022)',
                           '20 Years (2002 - 2022)'),
               selected = 'all',
               inline = F)
)

station_plot_tab = wellPanel(
  plotOutput('myplot', height = 300)
)

median_level_plot_tab = card(
  card_body(
    plotOutput('medianPlot', height = 300)
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
  top = 240, left = 10, width = 450, #height = 800,
  draggable = F,
  tabsetPanel(
    id = 'tabset',
    tabPanel('Trend Options',trend_select_options_tab),
    tabPanel('Median Plot',median_level_plot_tab),
    tabPanel('Trend Plot', trend_plot_tab)
  )
)

# Absolute panel with map as background.
map_abs_panel = absolutePanel(
  top = 0, left = 0, right = 0,
  fixed = TRUE,
  div(
    style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;",
    fluidRow(
      leafletOutput('leafmap',
                    height = '600px')
    )
  )
)
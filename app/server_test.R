server <- function(input, output, session) {
  # monthly drop-down menu (if monthly is selected)
  output$month_selector_UI = renderUI({
    if(period_rv() == 'Yearly') return(NULL)
    selectizeInput(inputId = 'month_selector',
                   label = 'Month',
                   multiple = F,
                   choices = month.abb,
                   selected = month.abb[1])
  })
  
  # Reset button - only active if a region or station have been selected
  output$reset <- renderUI({
    
    if (click_region() == 'No selection' & click_station() == 'No selection'){}
    else{
      
      fluidRow(id = "reset", top = 100, left = 50,
               right = "auto", bottom = "auto", width = "auto", height = "auto",
               actionButton(inputId = "reset", label = "Clear selection", class = "btn-primary"))}
    
  })
  
  # Set up reactive values for other components of interface
  var_rv <- reactiveVal('All') #User period choice (All, 10 Years, or 20 Years)
  period_rv <- reactiveVal('Yearly') #User variable choice (Yearly or Monthly)
  month_rv <- reactiveVal('Jan') #User month choice, available when Monthly variable selected
  
  #Filter results data set by input variables - annual or monthly, and all, 10 or 20 years
  filtered_data <- reactive({
    
    if(period_rv() == "Yearly"){
      filter(results_out,
             time_scale == var_rv(),
             period == period_rv())}
    
    else{
      
      filter(results_out,
             time_scale == var_rv(),
             period == period_rv(),
             month == month_rv())}
    
  })
  
  
  # Stations on map at start (based on whether a region has been selected, or all etc)
  wells_map_start = reactive({
    
    #sf_filter function identifies specified results column on sf object
    col <- sf_filter(time_scale = var_rv(),
                     period = period_rv(),
                     month = month_rv())
    
    if(input$user_region_choice != 'All'){
      
      well_markers <- filter(wells_sf_full, REGION_NAME == input$region_choice)
      
    } else{ well_markers <- wells_sf_full }
    
    
    well_markers %>%
      mutate(state_short = .[[col]]) %>%
      mutate(state_short = factor(state_short, c("Increasing",
                                                 "Stable",
                                                 "Moderate Rate of Decline",
                                                 "Large Rate of Decline",
                                                 "Insufficient Data"
      )))
    
  })
  
  # Click reactives
  # Region
  click_region <- reactiveVal('No selection')
  
  # Station
  click_station <- reactiveVal('No selection')
  
  
}
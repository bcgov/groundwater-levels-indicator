#Reactive values for dropdown menus

var_rv <- reactiveVal('All') #User period choice (All, 10 Years, or 20 Years)
period_rv <- reactiveVal('Yearly') #User variable choice (Yearly or Monthly)
month_rv <- reactiveVal('Jan') #User month choice, available when Monthly variable selected

# Set up reactive values for user's click response

#Map selection values
click_region <- reactiveVal('No selection') #well selection
click_station <- reactiveVal('No selection') #region selection
click_lat <- reactiveVal() #latitude of click selection
click_long <- reactiveVal() #longitude of click selection
wells_map <- reactiveVal() #well spatial file selection
region_selected <- reactiveVal(regions_sf) #region spatial file selection

#Region attributes
regional_subset <- reactiveVal() #subset results data by region
region_name <- reactiveVal() #save selected region name
recent_cnt <- reactiveVal() #count of wells too recent for trend analysis
missing_cnt <- reactiveVal() #count of wells with too many missing values for trend analysis

#Well attributes (Well Number, state, and trend slope)
well_num = reactiveVal() #well attribute table selection
state = reactiveVal('No selection') #selected well state
slope = reactiveVal() #selected well slope
trendpre = reactiveVal() #positive/negative slope
background_color = reactiveVal() #background colour of state
aquifer_id = reactiveVal() #corresponding aquifer for selected well
aquifer_url = reactiveVal() #url of corresponding aquifer
well_url = reactiveVal() #url of corresponding well

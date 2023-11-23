
reset_elements <- function(map){
  #Reset all elements
  region_selected(regions_sf)
  wells_map(map)
  well_num(well_attr)
  click_station('No selection')
  click_region('No selection')
  click_long(NULL)

  #Reset map zoom
  leafletProxy("leafmap", session, data = wells_map_start()) %>%
    set_bc_view() %>%
    clearGroup("selected")

}

region_update <- function(region){

  #Reset well selection
  click_station('No selection')

  req(input$user_region_choice)

  region_name(region)

  #Filter results data frame by selected region
  region_data <- filter(filtered_data(), region_name == region_name())
  regional_subset(region_data)

  newRecent <- as.character(well_state_count()[4,2])
  recent_cnt(newRecent)
  newMissing <- as.character(well_state_count()[6,2])
  missing_cnt(newMissing)

  #Zoom map to selected region
  leafletProxy("leafmap", session) %>%
    fitBounds(region_selected()$xmin, region_selected()$ymin, region_selected()$xmax, region_selected()$ymax) %>%
    clearMarkers() %>%
    addCircleMarkers(layerId = ~Well_Num,
                     color = 'black',
                     fillColor = ~mypal(state_short),
                     radius = 5,
                     weight = 1,
                     fillOpacity = 0.8,
                     label = ~paste0("Well No. ", Well_Num, " - ",state_short),
                     data = wells_map_start())

}

well_update <- function(well, lat, long){

  #Reset regional selection
  click_region('No selection')

  #Define input variables
  # Capture the info of the clicked point and use this for filtering.
  click_station(well)
  click_lat(lat)
  click_long(long)

  #Filter well characteristics data frame
  well <- filter(well_attr, Well_Num==click_station())
  well_num(well)

  #Return aquifer number of selected well if available
  newAquifer <- as.character(well$aquifer_id)
  aquifer_id(newAquifer)
  if(newAquifer!="NA"){
    newAquifer_url <- paste0("<a href=", "https://apps.nrs.gov.bc.ca/gwells/aquifers/",
                             newAquifer, ">View available aquifer information</a")}
  else{
    newAquifer_url <- ""
  }

  aquifer_url(newAquifer_url)

  #Return well URL

  newWell_url <- paste0("<a href=", "https://governmentofbc.maps.arcgis.com/apps/webappviewer/index.html?id=b53cb0bf3f6848e79d66ffd09b74f00d&find=OBS%20WELL%20",
                        click_station(), ">View this well on the Provincial Groundwater Observation Well Network</a")
  well_url(newWell_url)

  #Return state of selected well
  newState <- filter(filtered_data(), Well_Num==click_station()) %>%
    select(state)
  state(newState)
  #Return slope of selected well
  newSlope <- filter(filtered_data(), Well_Num==click_station()) %>%
    select(slope)
  slope(newSlope)
  #Define trend prefix based on trend slope
  newTrendpre <- ifelse(slope() > 0, "+", "") #This is reversed due to how slope is reported (meters below ground surface)
  trendpre(newTrendpre)
  #Define background colour based on state
  newBackground_color = colour_box[colour_box$state == as.character(state()), "color"]
  background_color(newBackground_color)

  #Highlight selected well on map
  leafletProxy("leafmap", session, data = wells_map()) %>%
    clearGroup("selected") %>%
    addCircleMarkers(lat = click_lat(), lng = click_long(),
                     group = "selected",
                     fillColor = "yellow",
                     fillOpacity = 1)




}

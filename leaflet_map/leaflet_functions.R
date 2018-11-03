popup_groundwater <- function(data, type = "well") {
  if("sf" %in% class(data)) data <- as.data.frame(data)
  data <- popup_content_groundwater(data, type) 
  
  if(type == "well") {
    data <- dplyr::mutate(data,
                          popup_row1 = envreportutils:::popup_create_row(.data$title),
                          popup_row2 = envreportutils:::popup_create_row(.data$info, .data$info2),
                          popup_row3 = envreportutils:::popup_create_row(.data$svg_wide))
  } else if (type == "region") {
    data <- dplyr::mutate(data,
                          popup_row1 = envreportutils:::popup_create_row(.data$title),
                          popup_row2 = envreportutils:::popup_create_row(.data$svg_wide))
  }
  
  envreportutils:::popup_combine_rows(data)
}

popup_content_groundwater <- function(data, type) {
  data <- dplyr::mutate(data, region_name = paste0("Region: ", .data$region_name))
  if(type == "well") {
    data <- data %>%
      dplyr::mutate(svg_wide = paste0("<img src = './well_plots/area_", 
                                      .data$well_num, ".svg'>"),
                    gw_map = paste0("https://governmentofbc.maps.arcgis.com/apps/",
                                    "webappviewer/index.html?id=b53cb0bf3f6848e79",
                                    "d66ffd09b74f00d&find=OBS%20WELL%20", 
                                    sprintf("%03d", .data$well_num)),
                    well_name = paste0("Observation Well: ", .data$well_num),
                    title = .data$well_name,
                    subtitle = .data$region_name,
                    trend_line_slope = .data$trend_line_slope * -1, 
                    trend_plus = if_else(.data$trend_line_slope > 0, "+", ""),
                    # Trend needs to be formatted individually, or formatting changes
                    trend = map2(.data$trend_line_slope, .data$trend_plus,
                                ~paste0(.y, format(.x, digits = 2, nsmall = 2), 
                                       " m/year")),
                    trend = replace(trend, state == "Stable", ""),
                    title = paste0("      <div class = 'popup-title'>\n", 
                                   "        <h2>", .data$title, "</h2>\n", 
                                   "        <h4>", .data$subtitle, "</h4>\n",
                                   "      </div>\n"),
                    info = paste0("  <div class = 'section-info'>\n", 
                                  "      <div class = 'popup-badge' ",
                                  "style = 'background-color: ", col,";
                                                       color: ", col_text, "'>\n",
                                  "        <h4>Trend Category:</h4>\n", 
                                  "        <h2>", .data$state, "</h2>\n",
                                           .data$trend,
                                  "      </div>\n",
                                  "  </div>\n"),
                    info2 = paste0("  <div class = 'section-info'>\n", 
                                   "      <div class = 'popup-badge'>\n",
                                   "        <h4><strong>Learn More About this Well</strong></h4>\n",
                                   "        <a href = '", gw_map, "' target='_blank'>", 
                                            "GW Network Map<small></a> (well info/data)</small>\n",
                                   "        <br><a href = '' target = '_blank'>",
                                            "Aquifer Factsheets</a><small> (aquifer health)</small>\n",
                                   "      </div>\n",
                                   "  </div>\n"))
  } else {
    data <- dplyr::mutate(data, 
                          svg_wide = paste0("<img src = './regional_plots/summary_", 
                                            .data$region_name_short, ".svg'>\n"),
                          title = paste0("  <div class = 'popup-title'>\n", 
                                         "        <h2>", .data$region_name, "</h2>\n", 
                                         "        <h4>Number of Observation Wells with Trend Data: ", 
                                                      .data$frequency, "</h4>\n",
                                         "  </div>\n"))
  }
  data
}

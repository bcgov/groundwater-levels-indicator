popup_groundwater <- function(data, type = "well") {
  if("sf" %in% class(data)) data <- as.data.frame(data)
  data <- popup_content_groundwater(data, type) 
  
  if(type == "well") {
    data <- dplyr::mutate(data,
                          popup_row1 = popup_create_row(.data$info, .data$svg_month),
                          popup_row2 = popup_create_row(.data$svg_wide))
  } else if (type == "region") {
    data <- dplyr::mutate(data,
                          popup_row1 = popup_create_row(.data$title),
                          popup_row2 = popup_create_row(.data$svg_wide))
  }
  
  popup_combine_rows(data)
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
                    info = paste0("  <div class = 'section-info'>\n", 
                                  "      <div class = 'popup-title'>\n", 
                                  "        <h2>", .data$title, "</h2>\n", 
                                  "        <h4>", .data$subtitle, "</h4>\n",
                                  "      </div>\n",
                                  "      <div class = 'popup-badge' ",
                                  "style = 'background-color: ", col,";
                                                       color: ", col_text, "'>\n",
                                  "        <h4>Trend Category:</h4>\n", 
                                  "        <h2>", .data$state, "</h2>\n",
                                  "      </div>\n",
                                  "      <div style = 'text-align: center'>\n",
                                  "        <h4><strong>More info: </strong><a href = '", gw_map, 
                                  "' target='_blank'>GW interactive map</a></h4>\n",
                                  "      </div>\n",
                                  "  </div>\n"),
                    svg_month = paste0("./well_plots/month_", .data$well_num, ".svg"),
                    svg_month = paste0("  <div class = 'section-column-plot'>\n",
                                       "    <img src = ", .data$svg_month, ">\n",
                                       "  </div>\n"))
  } else {
    data <- dplyr::mutate(data, 
                          svg_wide = paste0("<img src = './regional_plots/summary_", 
                                            .data$region_name_short, ".svg'>\n"),
                          title = paste0("  <div class = 'popup-title'>\n", 
                                         "        <h2>", .data$region_name, "</h2>\n", 
                                         "  </div>\n"))
  }
  data
}

# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

insufficient_statement <- function(n) {
  well_tense <- ifelse(n > 1, " wells do ", " well does ")
  ifelse(n, 
         paste0("<p>", n, well_tense, 
                "not currently have enough data for trend analysis</p>"), "")
}

popup_groundwater <- function(data, type = "well") {
  if("sf" %in% class(data)) data <- as.data.frame(data)
  
  data <- popup_content_groundwater(data, type) 
  
  if (type == "well") {
    data <- dplyr::mutate(data,
                          popup_row1 = popup_create_row(.data$title),
                          popup_row2 = popup_create_row(.data$info, .data$info2),
                          popup_row3 = popup_create_row(.data$svg_wide))
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
      dplyr::mutate(svg_wide = ifelse(!is.na(.data$trend_line_slope), 
                                      paste0("<img src = './well_plots/area_", 
                                      .data$well_num, ".svg'>"), 
                                      ""),
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
                                  "        <h4><strong>Trend Category:</strong></h4>\n", 
                                  "        <h2>", .data$state, "</h2>\n",
                                           ifelse(!is.na(.data$trend_line_slope), 
                                                  .data$trend, ""),
                                  "      </div>\n",
                                  "  </div>\n"),
                    info2 = paste0("  <div class = 'section-info'>\n", 
                                   "      <div class = 'popup-badge'>\n",
                                   "        <h4><strong>Learn More About this Well:</strong></h4>\n",
                                   "        <h4><a href = '", gw_map, "' target='_blank'>", 
                                            "Groundwater Level Data & Information</a></h4>\n",
                                   "        <h4>", 
                                   # Only show link to aquifer page if aquifer is known
                                   ifelse(!is.na(.data$aquifer_id), 
                                          paste0("<a href = 'https://apps.nrs.gov.bc.ca/gwells/aquifers/", 
                                                 .data$aquifer_id, 
                                                 "' target = '_blank'>Aquifer Summary</a>"), 
                                          ""), 
                                   "</h4>\n",
                                   "      </div>\n",
                                   "  </div>\n"))
  } else {
    data <- dplyr::mutate(data, 
                          svg_wide = paste0("<img src = './regional_plots/summary_", 
                                            .data$region_name_short, ".svg'>\n"),
                          title = paste0("  <div class = 'popup-title'>\n", 
                                         "        <h2>", .data$region_name, "</h2>\n", 
                                         # "        <h4>Number of Observation Wells in the Region: ", 
                                         #              .data$frequency_all, "</h4>\n",
                                         "        <h4>Number of Wells Analyzed: ", 
                                         .data$frequency, "</h4>\n",
                                         insufficient_statement(.data$n_insufficient),
                                         "  </div>\n"))
  }
  data
}

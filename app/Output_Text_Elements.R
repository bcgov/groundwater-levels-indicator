
text_header <- function(click_station, click_region){

  if(click_station == 'No selection' & click_region == 'No selection'){
    return(NULL)
  }

  if(click_station != 'No selection' & click_region == 'No selection'){


    HTML(paste0("<div style='background-color:white; padding: 8px'>",
                "<strong>Observation Well: ", click_station), "</strong> <br>",
         "Aquifer ID:", aquifer_id(), "</div")

  }else{

    #Report selected region

    HTML(paste0("<div style='background-color:white; padding: 8px'>",
                "<strong>Natural Resource Region: ", region_name(),
                "</strong> <br>",
                "Count of recent wells: ", recent_cnt(), "<br>",
                "Count of wells with missing data: ", missing_cnt(), "</div"))
  }

}

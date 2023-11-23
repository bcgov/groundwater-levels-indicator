#Define input data frames
#Filter results data set by input variables

#Filter trend results data
filtered_data <- reactive({

  if(period_rv() == "Yearly"){
    filter(results_out,
           time_scale == var_rv(),
           period == period_rv())

    }

  else{

    req(input$month_selector)

    filter(results_out,
           time_scale == var_rv(),
           period == period_rv(),
           month == month_rv())}

})

monthly_readings <- reactive({

  if(period_rv() == "Yearly"){

    monthlywells_ts

  }

  else{

    req(input$user_region_choice)

    monthlywells_ts_mean
  }

})


#Use filtered trend results to define well states for map element
wells_map_start = reactive({

  wells_sf_full <- right_join(wells_sf, filtered_data(), by=c("Well_Num"="Well_Num"))

  if(input$user_region_choice != 'All'){

    well_markers <- filter(wells_sf_full, REGION_NAME == input$user_region_choice)

  } else{ well_markers <- wells_sf_full }

  well_markers %>%
    mutate(state_short = factor(state_short, c("Increasing",
                                               "Stable",
                                               "Moderate Rate of Decline",
                                               "Large Rate of Decline",
                                               "Too many missing observations",
                                               "Recently established well"
    )))

})

#Count missing wells for summary plot
well_state_count = reactive({

  filter(filtered_data(), region_name == region_name()) %>%
    group_by(state_short) %>%
    summarize("count" = n()) %>%
    right_join(state_list, by=c("state_short"="unique(results_out$state_short)")) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    arrange(state_short)

})



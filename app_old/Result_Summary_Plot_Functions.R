
#This file contains functions used in the Groundwater Indicator Shiny app scripts.
#Includes:
#   1) prov_summary_plot(data) - creates provincial summary bar plot
#      based on groundwater well trend statistics
#   2) regional_summary_plot(data) - creates regional summary bar plot
#      based on groundwater well trend statistics



##########################################################
#1. Define function to create regional summary plot
prov_summary_plot <- function(data){

  #Filter out wells with no state
  data <- data %>%
    filter(., state !="Too many missing observations to perform trend analysis" &
             state !="Recently established well; time series too short for trend analysis" )

#Count the number of wells in each state and calculate the respective proportions
input_summary <- data %>%
  group_by(state) %>%
  summarize("count"=n()) %>%
  mutate(col = case_when(
    state == "Stable" ~ "gray70",
    state == "Moderate Rate of Decline" ~ "orange",
    state == "Large Rate of Decline" ~ "darkorange",
    state == "Increasing" ~ "skyblue2"
  )) %>%
  mutate(order = case_when(
    state == "Stable" ~ 2,
    state == "Moderate Rate of Decline" ~ 3,
    state == "Large Rate of Decline" ~ 4,
    state == "Increasing" ~ 1
  )) %>%
  mutate(total_no_wells = nrow(data)) %>%
  mutate(prop = (count/total_no_wells)*100) %>%
  mutate("no_wells_lab" = ifelse(count>1, paste0(count, " wells"), paste0(count, " well"))) %>%
  mutate(label_x = cumsum(count))  %>% #Calculate the total count of wells for bar graph label
  arrange(order) %>%
  mutate(col = as.factor(col), state = factor(state))

#Define factor levels for barplot colouring
input_summary$col <- factor(input_summary$col, levels=rev(input_summary$col))
input_summary$state <- factor(input_summary$state, levels=rev(input_summary$state))

#Define factor levels for barplot colouring and legend
barcol = levels(input_summary$col)
barlab = levels(input_summary$state)


#Create provincial summary bar graph
ggplot(data=input_summary) +
  geom_col(mapping=aes(x=prop, y=state, fill=state, width = 0.5)) +
  scale_fill_manual(label=barlab, values=barcol) +
  geom_text(aes(x=prop, y=state, label = no_wells_lab), hjust = -0.1) +
  scale_x_continuous(expand = c(0,0)) +
  expand_limits(x=c(0,115)) +
  #guides(fill = guide_legend(reverse = TRUE, nrow=2))+
  #labs(title = "Groundwater Wells by State") +
  xlab("Proportion of wells (%)") + ylab(NULL) +
  theme_classic() +
  #theme(legend.position="bottom",
  #      legend.title=element_blank())
  theme(legend.position="none")

}
##########################################################
#2. Define function to create regional summary plot
regional_summary_plot <- function(data){

  # data <- input_dataframe %>%
  #   filter(time_scale == "All", period == "Yearly")

  #Filter out wells with no state
  data <- data %>%
    filter(., state !="Too many missing observations to perform trend analysis" &
             state !="Recently established well; time series too short for trend analysis" )


#Summarize results by region and count wells in each state
input_regional <- data %>%
  group_by(region_name, state) %>%
  summarize("count"=n()) %>%
  mutate( col = case_when(
    state == "Stable" ~ "gray70",
    state == "Moderate Rate of Decline" ~ "orange",
    state == "Large Rate of Decline" ~ "darkorange",
    state == "Increasing" ~ "skyblue2"
  ))

states_unique <- input_regional %>%
  group_by(state, col) %>%
  summarize() %>%
  mutate(order = case_when(
    state == "Stable" ~ 2,
    state == "Moderate Rate of Decline" ~ 3,
    state == "Large Rate of Decline" ~ 4,
    state == "Increasing" ~ 1)) %>%
  arrange(order) %>%
  mutate(col = as.factor(col), state = factor(state))

#Count total number of wells in each region
bar_labels_r <- input_regional %>%
  group_by(region_name) %>%
  summarize("no_wells"=sum(count)) %>%
  mutate("no_wells_lab" = ifelse(no_wells>1, paste0(no_wells, " wells"), paste0(no_wells, " well"))) %>%
  mutate(prop_tot = (no_wells/length(unique(data$Well_Num)))*100) #Calculate total proportion for graph scale


#Add total number of wells to regional input dataset
input_regional <- right_join(input_regional, bar_labels_r) %>%
  mutate(total_no_wells = length(unique(data$Well_Num))) %>% #added
  mutate(prop = (count/total_no_wells)*100) #Calculate proportion on total number of wells

#Define factor levels for barplot colouring and ordering
input_regional$region_name <-factor(input_regional$region_name, c("West Coast", "Thompson / Okanagan", "South Coast", "Skeena", "Omineca",
                                                                  "Northeast", "Kootenay / Boundary", "Cariboo"))
input_regional$col <- factor(input_regional$col, levels=rev(states_unique$col))
input_regional$state <- factor(input_regional$state, levels=rev(states_unique$state))

barcolr=levels(input_regional$col)
barlabr=levels(input_regional$state)

#Create regional summary plot
ggplot(data=input_regional) +
  geom_col(mapping=aes(x=prop, y=region_name, fill=state, width = 0.5)) +
  scale_fill_manual(label=barlabr, values=barcolr) +
  geom_text(aes(x=prop_tot, y=region_name, label = no_wells_lab), hjust = -0.1) +
  scale_x_continuous(expand = c(0,0)) +
  expand_limits(x=c(0,55)) +
  guides(fill = guide_legend(reverse = TRUE))+
  #labs(title = "Groundwater Wells by Region") +
  xlab("Proportion of Wells (%)") + ylab(NULL) +
  theme_classic() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.box.just ="left")

}


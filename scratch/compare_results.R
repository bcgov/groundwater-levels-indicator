# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.


## Source package libraries
if (!"package:bcgroundwater" %in% search()) source("header.R")


## Load saved data if necessary
if (!exists("results_out"))  load("./tmp/analysis_data.RData")



## Quick summary
results_out %>%
  group_by(state) %>%
  summarise(Number = n())


## Missing aquifer types?
missing_aquifer_types <- results_out %>%
  group_by(state) %>%
  filter(AQUIFER_TYPE == "Unknown") %>% 
  select(Well_Num, REGION_NAME, AQUIFER_TYPE, wellDepth_m, state)

write.csv(missing_aquifer_types, "tmp/ow_missing_aquifer_type.csv", row.names = FALSE)


## Compare 2014 results with 2018

#2018
sum_results2018 <- results_out %>% 
  select(Well_Num, category) %>% 
  group_by(category) %>% 
  summarise(totals2018 = n()) %>% 
  adorn_totals("row")

well_results2018 <- results_out %>% 
  select(Well_Num, category2018 = category) %>% 
  filter(category2018 != "N/A")


#2014
#get data from the B.C. Data Catalogue released under the OGl-BC licence
results2014 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/a74f1b97-17f7-499b-84e7-6455e169e425/resource/a8933793-eadb-4a9c-992c-da4f6ac8ca51/download/gwwellattributes.csv") 


sum_results2014 <- results2014 %>% 
  select(Well_Num, category) %>% 
  mutate(category = recode(category, 
                           `Large rate of decline` = "Large Rate of Decline", 
                           `Moderate rate of decline` = "Moderate Rate of Decline")) %>%
  group_by(category) %>% 
  summarise(totals2014 = n()) %>% 
  adorn_totals("row")

well_results2014 <- results2014 %>% 
  mutate(category = recode(category, 
                           `Large rate of decline` = "Large Rate of Decline", 
                           `Moderate rate of decline` = "Moderate Rate of Decline"),
         Well_Num = as.numeric(Well_Num)) %>% 
  select(Well_Num, category2014 = category) %>% 
  filter(category2014 != "N/A")


#compare
sum_totals <- sum_results2018 %>% 
  left_join(sum_results2014)

well_compare <- well_results2014 %>% 
  full_join(well_results2018) %>% 
  left_join(welldata_attr) %>% 
  select(-EMS_ID)

write.csv(well_compare, "tmp/well_compare_2014_2018.csv", row.names = FALSE)
write.csv(sum_totals, "tmp/summary_compare_2014_2018.csv", row.names = FALSE)



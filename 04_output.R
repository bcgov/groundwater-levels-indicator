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

# Load required packages
library(ggplot2)
library(gridExtra)
library(envreportutils)
library(janitor)
library(readr)
library(dplyr)

# Export data files and charts

attr.out.file <- "out/GW_Well_Attributes.csv"
gwl.out.file <- "out/GWL_Monthly.csv"
pie.png <- "out/figs/status_pie.png"
status.reg.png <- "out/figs/status-by-reg.png"
status.aq.png <- "out/figs/status-by-aq.png"
status.reg.aq.png <- "out/figs/status-by-reg-aq.png"

load("./tmp/raw_data.RData")
load("./tmp/analysis_data.RData")
load("./tmp/well_data_attributes.RData")

dir.create("out/figs/", recursive = TRUE, showWarnings = FALSE)

png_retina(pie.png, width = 440, height = 400)
plot(pie_plot)
dev.off()

png_retina(status.reg.png, width = 800, height = 400)
plot(regional_plot)
dev.off()

png_retina(status.aq.png, width = 440, height = 400)
plot(aq_plot)
dev.off()

png_retina(status.reg.aq.png, width = 930, height = 330)
grid.arrange(regional_plot + theme(legend.position = "none"),
             aq_plot + theme(legend.position = "none"),
             ncol = 2, widths = c(3,2))
dev.off()

# Write data files (remove interpolated values and NA values and make sure only
# keep wells which match results_out dataset)
monthly_out <- monthlywells_ts %>%
  filter(nReadings > 0, 
         Well_Num %in% results_out$Well_Num) %>%
  select(EMS_ID, Well_Num, Date, Year, Month, med_GWL, dev_med_GWL, nReadings)

write.csv(results_out, attr.out.file, row.names = FALSE)
write.table(monthly_out, file = gwl.out.file, sep = ",", row.names = FALSE)



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
# get data from the B.C. Data Catalogue released under the OGl-BC licence
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

# compare
sum_totals <- sum_results2018 %>% 
  left_join(sum_results2014)

well_compare <- well_results2014 %>% 
  full_join(well_results2018) %>% 
  left_join(welldata_attr) %>% 
  select(-EMS_ID)

write.csv(well_compare, "out/well_compare_2014_2018.csv", row.names = FALSE)
write.csv(sum_totals, "out/summary_compare_2014_2018.csv", row.names = FALSE)

## Reduce PDF print_ver file size from ~37MB to ~13MB
file.copy("print_ver/gwl.pdf", "print_ver/envreportbc_gwl_June2018.pdf")
tools::compactPDF("print_ver/envreportbc_gwl_June2018.pdf", gs_quality = "ebook")
  
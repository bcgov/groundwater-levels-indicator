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

library(envreportutils) 

source("01_load.R")
source("02_clean.R")
source("03_analysis.R")
source("04_output.R")

# mon_year <- format(Sys.Date(), "%b%Y")
# outfile <- paste0("envreportbc_groundwater_trends_", mon_year, ".pdf")
# 
# rmarkdown::render("print_ver/gwl.Rmd",
#                   output_file = outfile)
# extrafont::embed_fonts(file.path("print_ver/", outfile))


## Reduce PDF print_ver file size
print_ver_final <- "print_ver/envreportbc_gwl_March2019.pdf"
file.remove(print_ver_final)
file.copy("print_ver/gwl.pdf", print_ver_final) #delete existing file, does not overwrite
tools::compactPDF(print_ver_final, gs_quality = "ebook")


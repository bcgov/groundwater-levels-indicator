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


## Install the packages we will need from CRAN:
package_list <- c("dplyr", "rgdal", "sp", "ggplot2", "stringr", #"lubridate", "zoo"
                  "grid", "scales", "ggmap", "devtools", "rvest", "RColorBrewer",
                  "purrr", "sf", "gridExtra", "bcmaps", "scales", "forcats",
                  "rmapshaper", "janitor", "readr")
package_new <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(package_new)) install.packages(package_new)


## Install the packages we will need from GitHub:
package_github <- c("bcgroundwater", "envreportutils", "bcmaps.rdata")
package_new <- package_github[!(package_github %in% installed.packages()[,"Package"])]
if(length(package_new)) devtools::install_github(paste0("bcgov/", package_new))


## Load required packages
library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(bcgroundwater)
library(sp)
library(rgdal)
library(rvest)
library(stringr)
library(bcmaps)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(scales)
library(ggmap)
library(forcats)
library(rmapshaper)
library(gridExtra)
library(envreportutils)
library(janitor)
library(readr)
library(glue)

## Create project directories
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
if (!exists("out")) dir.create("out", showWarnings = FALSE)
if (!exists("out/figs")) dir.create("out/figs", showWarnings = FALSE)



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
package_list <- c("dplyr", "rgdal", "sp", "ggplot2", "stringr", #"zoo"
                  "grid", "scales", #ggmap", 
                  "devtools", "rvest", "RColorBrewer",
                  "purrr", "sf", "gridExtra", "bcmaps", "scales", "forcats",
                  "rmapshaper", "janitor", "readr", "cowplot", "glue", "lubridate")
package_new <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(package_new)) install.packages(package_new)


## Install the packages we will need from GitHub:
package_github <- c(bcgov = "bcgroundwater", bcgov = "envreportutils", 
                    bcgov = "bcmapsdata", thomasp85 = "patchwork")
package_new <- package_github[!(package_github %in% installed.packages()[,"Package"])]
if(length(package_new)) {
  devtools::install_github(paste(names(package_new), package_new, sep = "/"))
}


## ggmap needs to be installed from GitHub, using the `tidyup` branch 
# from a known working commit https://github.com/dkahle/ggmap/tree/3ffc51b965709162dbaa62b3baa6106f59eba27b.
# See https://github.com/dkahle/ggmap/issues/51
if (!"ggmap" %in% installed.packages()[, "Package"] || 
    utils::packageVersion("ggmap") < package_version("2.7.902")) {
  devtools::install_github("dkahle/ggmap", ref = "3ffc51b965709162")
}

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
library(patchwork)
library(cowplot)

## Create project directories
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
if (!exists("out")) dir.create("out", showWarnings = FALSE)
if (!exists("out/figs")) dir.create("out/figs", showWarnings = FALSE)
if (!exists("leaflet_map/well_plots")) dir.create("leaflet_map/well_plots", showWarnings = FALSE)
if (!exists("leaflet_map/regional_plots")) dir.create("leaflet_map/regional_plots", showWarnings = FALSE)

## Invisible header object
.header_sourced <- TRUE


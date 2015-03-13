# Copyright 2015 Province of British Columbia
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

################################################################################
#
# Download maps for use in generating the print version
#
################################################################################

require(ggmap)
require(plyr)

styles <- 'feature:all|element:all|saturation:-75'

# Get BC basemap
BCextent <- c(-139,48,-114,60)
names(BCextent) <- c("left", "bottom", "right", "top")
fourCorners <- expand.grid(as.data.frame(matrix(BCextent, ncol=2, byrow=TRUE
                                                , dimnames=list(NULL, c("Long", "Lat")))))
BCcenter <- c(mean(BCextent[c("left","right")]), 
              mean(BCextent[c("top","bottom")]))
ggMapBC <- get_googlemap(center=BCcenter, zoom=5, scale=1
                         , maptype='roadmap', visible=fourCorners, style=styles)

# Create list of well maps (the while loop is not great)
wellMaps <- list()

while (length(wellMaps) < nrow(attr.out)) {
  temp <- dlply(attr.out, .(Well_Num), function(x) {
    if (!x$Well_Num %in% names(wellMaps)) { 
      tryCatch(
        get_googlemap(center=c(x$LONGITUDE, x$LATITUDE), zoom=8, scale=1
                      , maptype='roadmap', style=styles)
        , error=function(e) NULL)
    }
  })
  wellMaps <- compact(c(wellMaps, temp))
}

save(BCextent, ggMapBC, wellMaps, file="./tempdata/mapData.RData")

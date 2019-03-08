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


construct_url <- function(obj, epsg, query) {
  baseurl <- "https://openmaps.gov.bc.ca/geo/pub/{obj}/ows?service=WFS&version=2.0.0&request=GetFeature&typeName={obj}&SRSNAME=epsg:{epsg}&outputFormat=json{query}"
  
  if (!is.null(query)) {
    query <- paste0("&CQL_FILTER=", query)
  } else {
    query <- ""
  }
  URLencode(glue::glue(baseurl, obj = obj, epsg = epsg, query = query))
}


bcdc_map <- function(id, epsg = 3005, query = NULL) {
  url <- construct_url(id, epsg, query)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  res <- httr::GET(url, httr::write_disk(tmp))
  httr::stop_for_status(res)
  sf::st_read(tmp)
}

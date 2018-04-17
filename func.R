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

#' Recreate compact from plyr (remove NULL values from a list)
compact <- function (l) {
  Filter(Negate(is.null), l)
}

#' Find out which well was downloaded laast
#'
#' Find the last well that was downloaded and find the row that represents that 
#' well in the dataframe of wells.
#' @param dir the directory where well files are being saved
#' @param df the dataframe from which well IDs are taken
#' @export
#' @return (integer) the row of the dataframe representing the last well downloaded
#' @examples \dontrun{
#'
#'}
getLastRow <- function(dir="./data/welldata", df=wells.df) {
  files <- list.files(dir, pattern="well[A-Za-z0-9]+\\.csv", full.names=TRUE)
  if (length(files)==0) {
    last.row <- 1
  } else {
    fileinfo <- do.call("rbind", lapply(files, file.info))
    last.file <- rownames(fileinfo[fileinfo$ctime==max(fileinfo$ctime),])
    last.emsID <- strsplit(last.file,paste0(dir,"/well|.csv"))[[1]][2]
    last.row <- as.numeric(rownames(wells.df[wells.df$EMS_ID == last.emsID,]))
  }
  return(last.row)
}

#' Replace characters with LaTeX compatible characters/code
#'
#' Replaces or escapes characters (\,#,$,%,&,_,^,~) that can't be used 
#' directly in LaTeX. Does not deal with { and }
#' @param x String containing invalid LaTeX characters
#' @export
#' @return String with LaTeX compatbible characters
#' @examples \dontrun{
#'
#'}
knitr_latex_char <- function(x) {
  y <- gsub("\\\\", "\\\\textbackslash{}", x) # backslash has to be first!
  y <- gsub("([#$%&_])", "\\\\\\1", y) # Doesdn't deal with { or } because of function{}
  y <- gsub("\\^", "\\\\textasciicircum{}", y)
  y <- gsub("~", "\\\\textasciitilde{}", y)
  return(y)
}

#' converts a string to a string with capitalized first letters
#'
#'<full description>
#' @param x string
#' @export
#' @return string
#' @examples \dontrun{
#' simpleCap(c("HELLO MY NAME IS", "the quick brown fox"))
#'}
simpleCap <- function(x) {
  s <- strsplit(gsub("\\s+"," ",x), "\\s|\\(")
  
  s <- sapply(s, function(t) {
    paste(toupper(substring(t, 1, 1)), tolower(substring(t, 2))
          , sep = "", collapse = " ")
  })
  
  gsub("\\s\\s"," \\(",s)
}

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

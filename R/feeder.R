#' @import digest
#' @import readr
#' @import stringr
#' @import dplyr
#' @import tibble
#' @import xml2
#' @importFrom lubridate parse_date_time
#'
NULL

# Avoid some "no visible binding" notes with devtools::check().
#
globalVariables(c("download.file", "encoding"))
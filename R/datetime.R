#' @importFrom lubridate parse_date_time
parse.date <- function(date) {
  if (is.null(date)) return(NA)
  
  FORMATS = c("a, d b Y H:M:S z", "a, d b Y H:M z", "Y-m-d H:M:S z", "d b Y H:M:S", "d b Y H:M:S z", "a b d H:M:S z Y", "Y-m-d", "d b! Y")
  #
  # Transform time zone codes.
  #
  date = sub("\\b(UTC|GMT)\\b", "+0000", date)
  #
  # Strip out "T" between day and hour (as in "2016-07-23T06:16:08-07:00").
  #
  date = sub("([[:digit:]]{2})T([[:digit:]]{2})", "\\1 \\2", date)
  #
  # Fix time zone offset: insert space before and remove colon (as in "2016-07-23T06:16:08-07:00")
  #
  date = sub("(?<=[^[:blank:]])([+-])([[:digit:]]{2}):?([[:digit:]]{2})$", " \\1\\2\\3", date, perl = TRUE)
  
  # Replace month names from other languages.
  #
  month_map <- list(
    "jan" = "jan", "feb" = "fev", "mar" = "mar", "apr" = "abr", "may" = "mai", "jun" = "jun",
    "jul" = "jul", "aug" = "ago", "sep" = "set", "oct" = "out", "nov" = "nov", "dec" = "dez"
  )
  #
  for (month in names(month_map)) {
    date = sub(month_map[[month]], month, date, ignore.case = TRUE)
  }
  
  parsed = parse_date_time(date, orders = FORMATS, locale = "C")
  if (is.na(parsed)) stop("Unable to parse date.", call. = FALSE)
  parsed
}
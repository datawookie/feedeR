# Ensure that all feed URLs begin with http:// or https://.
#
clean.url <- function(url) {
  # paste0("http://", sub("(https?://)?(.*)", "\\2", url))
  ifelse(!grepl("^https?://", url), paste("http://", url, sep = "/"), url)
}

parse.date <- function(date) {
  FORMATS = c(
    "%a, %d %b %Y %H:%M:%S %z", # Fri, 05 Aug 2016 13:28:00 +0000
    "%Y-%m-%dT%H:%M:%S %z"      # 2016-07-23T06:16:08-07:00
  )
  #
  # Transform time zone codes.
  #
  date = sub("GMT$", "+0000", date)
  #
  # Fix time zone offset: insert space before and remove colon.
  #
  date = sub("(?<=[^[:blank:]])([+-])([[:digit:]]{2}):?([[:digit:]]{2})$", " \\1\\2\\3", date, perl = TRUE)
  #
  for (fmt in FORMATS) {
    parsed <- strptime(date, fmt, tz = "UTC")
    #
    if (!is.na(parsed)) return(as.POSIXct(parsed))
  }
  stop("Unable to parse date.", call. = FALSE)
}

# ATOM ----------------------------------------------------------------------------------------------------------------

#' @references
#' \url{https://en.wikipedia.org/wiki/Atom_(standard)}
#' @import dplyr
parse.atom <- function(feed) {
  feed <- xmlToList(feed$feed)
  #
  list(
    title = feed$title,
    link  = feed[names(feed) == "link"][[2]] %>% unname,
    updated = parse.date(feed$updated),
    items = bind_rows(lapply(feed[names(feed) == "entry"], function(item) {
      data.frame(
        title = item$title,
        date  = if(is.null(item$published)) NA else parse.date(item$published),
        link  = item$link,
        stringsAsFactors = FALSE
      )
    }))
  )
}

# RSS -----------------------------------------------------------------------------------------------------------------

# Entry fields to include:
#
# - category
# - description

#' @import dplyr
parse.rss <- function(feed) {
  feed <- xmlToList(feed$rss[["channel"]])
  #
  list(
    title = feed$title,
    link  = feed$link,
    updated = if(is.null(feed$lastBuildDate)) NA else parse.date(feed$lastBuildDate),
    items = bind_rows(lapply(feed[names(feed) == "item"], function(item) {
      data.frame(
        title = item$title,
        date  = if(is.null(item$pubDate)) NA else parse.date(item$pubDate),
        link  = if(is.null(item$origLink)) item$link else item$origLink,
        stringsAsFactors = FALSE
      )
    }))
  )
}

# ---------------------------------------------------------------------------------------------------------------------

#' @import XML
parse.xml <- function(xml) {
  xmlTreeParse(xml)$doc$children
}

feed.type <- function(feed) {
  if("rss" %in% names(feed)) {
    return("RSS")
  } else {
    return("Atom")
  }
}

#' @import RCurl
#' @import dplyr
feed.read <- function(url) {
  url %>% clean.url %>% getURL %>% parse.xml
}

#' Extract RSS/Atom feed
#' @description
#' Read feed metadata and entries.
#' @param url URL for the feed.
#' @return A list containing the following elements:
#'
#' - title: Title of the original site.
#'
#' - link: A link to the original site.
#'
#' - updated: When the feed was last updated.
#'
#' - items: A data frame with records for each entry in the feed.
#' @examples
#' feed.extract("https://feeds.feedburner.com/RBloggers")
#' feed.extract("http://journal.r-project.org/rss.atom")
#' @export
feed.extract <- function(url) {
  feed <-feed.read(url)

  # Decide on type of feed and parse appropriately.
  #
  type = feed.type(feed)
  #
  if (type == "RSS") {
    feed <- parse.rss(feed)
  } else if (type == "Atom") {
    feed <- parse.atom(feed)
  } else {
    stop("Unknown feed type!", .call = FALSE)
  }

  feed
}

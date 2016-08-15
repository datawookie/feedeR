# Ensure that all feed URLs begin with http:// or https://.
#
clean.url <- function(url) {
  ifelse(!grepl("^https?://", url), paste("http://", url, sep = ""), url)
}

#' @importFrom lubridate parse_date_time
parse.date <- function(date) {
  FORMATS = c("a, d b Y H:M:S z", "Y-m-d H:M:S z", "d b Y H:M:S", "d b Y H:M:S z")
  #
  # Transform time zone codes.
  #
  date = sub("GMT$", "+0000", date)
  #
  # Strip out "T" between day and hour (as in "2016-07-23T06:16:08-07:00").
  #
  date = sub("([[:digit:]]{2})T([[:digit:]]{2})", "\\1 \\2", date)
  #
  # Fix time zone offset: insert space before and remove colon (as in "2016-07-23T06:16:08-07:00")
  #
  date = sub("(?<=[^[:blank:]])([+-])([[:digit:]]{2}):?([[:digit:]]{2})$", " \\1\\2\\3", date, perl = TRUE)
  #
  parsed = parse_date_time(date, orders = FORMATS, locale = "C")
  if (is.na(parsed)) stop("Unable to parse date.", call. = FALSE)
  parsed
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
feed.read <- function(url, encoding) {
  url %>% clean.url %>% getURL(.encoding = encoding) %>% parse.xml
}

#' Extract RSS/Atom feed
#' @description
#' Read feed metadata and entries.
#' @param url URL for the feed.
#' @param encoding Explicitly identify the encoding of the content.
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
#' feed.extract("http://www.valor.com.br/financas/mercados/rss", "ISO-8859-2")
#' @export
feed.extract <- function(url, encoding = integer()) {
  feed <-feed.read(url, encoding)

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

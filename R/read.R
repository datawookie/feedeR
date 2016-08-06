# Ensure that all feed URLs begin with http://.
#
clean.url <- function(url) {
  paste0("http://", sub("(https?://)?(.*)", "\\2", url))
}

parse.date <- function(date) {
  FORMATS = c(
    "%a, %d %b %Y %H:%M:%S %z", # Fri, 05 Aug 2016 13:28:00 +0000
    "%Y-%m-%dT%H:%M:%S %z"      # 2016-07-23T06:16:08-07:00
  )
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

#' @import dplyr
parse.rss <- function(feed) {
  feed <- xmlToList(feed$rss[["channel"]])
  #
  list(
    title = feed$title,
    items = bind_rows(lapply(feed[names(feed) == "item"], function(item) {
      # Notes:
      #
      # - There might also be a "link" field in original item.
      #
      data.frame(
        title = item$title,
        date  = if(is.null(item$pubDate)) NA else parse.date(item$pubDate),
        link  = item$origLink,
        stringsAsFactors = FALSE
      )
    }))
  )
}

# ---------------------------------------------------------------------------------------------------------------------

#' @import XML
parse.xml <- function(xml) {
  xmlTreeParse(xml)$doc
}

feed.type <- function(feed) {
  if("rss" %in% names(feed)) {
    return("RSS")
  } else {
    return("Atom")
  }
}

#' @import RCurl
feed.read <- function(url) {
  parse.xml(getURL(clean.url(url)))$children
}

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
xxx = feed.extract("https://feeds.feedburner.com/RBloggers")
yyy = feed.extract("http://journal.r-project.org/rss.atom")

# http://fastml.com/atom.xml

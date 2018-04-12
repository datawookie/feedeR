USERAGENT = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.167 Safari/537.36"

# Ensure that all feed URLs begin with http:// or https://.
#
clean.url <- function(url) {
  ifelse(!grepl("^https?://", url), paste("http://", url, sep = ""), url)
}

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

locate.link <- function(text) {
  grepl("^https?://", text)
}

filter.link <- function(text) {
  text[locate.link(text)]
}

# RDF -----------------------------------------------------------------------------------------------------------------

#' Parse feeds encoded in RDF/XML.
#' @param feed The URL of the feed.
#' @references
#' https://en.wikipedia.org/wiki/RDF/XML
#' @examples
#' \dontrun{
#' parse.rdf(feed.read("http://feeds.feedburner.com/oatmealfeed"))
#' }
#' @import dplyr
parse.rdf <- function(feed) {
  feed <- xmlToList(feed$RDF)
  #
  list(
    title = feed$channel$title,
    link = feed$channel$link,
    updated = parse.date(feed$channel$date),
    items = bind_rows(lapply(feed[names(feed) == "item"], function(item) {
      data.frame(
        title = item$title,
        date  = parse.date(item$date),
        link  = item$link,
        stringsAsFactors = FALSE
      )
    }))
  )
}

# ATOM ----------------------------------------------------------------------------------------------------------------

find.link <- function(links) {
  if (length(links) == 1) {
    return(links$link["href"])
  } else {
    index = sapply(links, function(n) "title" %in% names(n))
    if (any(index)) {
      return(links[index]$link["href"])
    }
    index = sapply(links, function(n) n["rel"] == "alternate")
    if (any(index)) {
      return(links[index]$link["href"])
    }
  }
}

#' @references
#' \url{https://en.wikipedia.org/wiki/Atom_(standard)}
#' @import dplyr
parse.atom <- function(feed) {
  feed <- xmlToList(feed$feed)
  #
  list(
    title = feed$title,
    link  = feed[names(feed) == "link"][[2]] %>% unname %>% filter.link,
    updated = parse.date(feed$updated),
    items = bind_rows(lapply(feed[names(feed) == "entry"], function(item) {
      data.frame(
        title = item$title,
        date  = if(!is.null(item$published)) parse.date(item$published) else
          if(!is.null(item$updated)) parse.date(item$updated) else NA,
        link  = if(!is.null(item$origLink)) item$origLink else find.link(item[names(item) == "link"]),
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
    updated = parse.date(feed$lastBuildDate),
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
  if (nchar(xml) == 0) stop("XML document is empty!")
  #
  xmlTreeParse(xml, options = NOCDATA)$doc$children
}

feed.type <- function(feed) {
  if("rss" %in% names(feed)) {
    return("RSS")
  } else if("RDF" %in% names(feed)) {
    return("RDF")
  } else {
    return("Atom")
  }
}

#' @import RCurl
#' @import dplyr
feed.read <- function(url, encoding = integer()) {
  url %>% clean.url %>% getURL(.encoding = encoding, httpheader = c('User-Agent' = USERAGENT)) %>% parse.xml
}

#' Extract data from feeds
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
#'
#' - hash: A hash key constructed from the post link. This is intended for easy indexing.
#' @examples
#' \dontrun{
#' feed.extract("https://feeds.feedburner.com/RBloggers")
#' feed.extract("http://journal.r-project.org/rss.atom")
#' feed.extract("http://www.valor.com.br/financas/mercados/rss", "ISO-8859-2")
#' }
#' @import digest
#' @export
feed.extract <- function(url, encoding = integer()) {
  feed <- feed.read(url, encoding)

  # Decide on type of feed and parse appropriately.
  #
  type = feed.type(feed)
  #
  if (type == "RSS") {
    feed <- parse.rss(feed)
  } else if (type == "RDF") {
    feed <- parse.rdf(feed)
  } else if (type == "Atom") {
    feed <- parse.atom(feed)
  } else {
    stop("Unknown feed type!", .call = FALSE)
  }

  feed$items$hash = as.character(sapply(feed$items$link, function(n) {digest(n, algo = "xxhash64")}))

  feed
}

# Ensure that all feed URLs begin with http:// or https://.
#
clean.url <- function(url) {
  ifelse(!grepl("^https?://", url), paste("http://", url, sep = ""), url)
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

get_title <- function(title) {
  if (class(title) == "list" && "text" %in% names(title)) {
    title$text
  } else {
    title
  }
}

#' @references
#' \url{https://en.wikipedia.org/wiki/Atom_(standard)}
#' @import dplyr
parse.atom <- function(feed) {
  feed <- xmlToList(feed$feed)
  #
  list(
    title = get_title(feed$title),
    link  = feed[names(feed) == "link"][[2]] %>% unname %>% filter.link,
    updated = parse.date(feed$updated),
    items = bind_rows(lapply(feed[names(feed) == "entry"], function(item) {
      data.frame(
        title = get_title(item$title),
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
      if (is.null(item$title)) return(NULL)
      #
      date = if(is.null(item$pubDate)) NA else parse.date(item$pubDate)
      if (is.na(suppressWarnings(as.integer(date)))) return(NULL)
      #
      data.frame(
        title = item$title,
        date  = date,
        link  = if(is.null(item$origLink)) item$link else item$origLink,
        stringsAsFactors = FALSE
      )
    }))
  )
}

# ---------------------------------------------------------------------------------------------------------------------

feed.type <- function(feed) {
  if("rss" %in% names(feed)) {
    return("RSS")
  } else if("RDF" %in% names(feed)) {
    return("RDF")
  } else {
    return("Atom")
  }
}

#' @import dplyr
#' @import XML
feed.read <- function(xml) {
  xmlTreeParse(xml, options = NOCDATA, asText = TRUE)$doc$children
}

#' Extract data from feeds
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
#'
#' - hash: A hash key constructed from the post link. This is intended for easy indexing.
#' @examples
#' \dontrun{
#' feed.extract("https://feeds.feedburner.com/RBloggers")
#' feed.extract("http://journal.r-project.org/rss.atom")
#' feed.extract("http://www.valor.com.br/financas/mercados/rss", "ISO-8859-2")
#' }
#' @import digest
#' @import readr
#' @import stringr
#' @export
feed.extract <- function(url) {
  XMLFILE = tempfile(fileext = "-index.xml")

  options(HTTPUserAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.157 Safari/537.36")
  #
  download.file(
    url = clean.url(url),
    XMLFILE,
    quiet = TRUE
  )

  XML = read_file(XMLFILE)

  # Replace raw "&" in text (which was causing "xmlParseEntityRef: no name").
  #
  XML = str_replace_all(XML, "(?<= )&(?= )", "&amp;")

  feed <- feed.read(XML)

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

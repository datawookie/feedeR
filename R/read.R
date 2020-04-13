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

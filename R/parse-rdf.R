#' Parse feeds encoded in RDF/XML.
#' @param feed The URL of the feed.
#' @references
#' https://en.wikipedia.org/wiki/RDF/XML
#' @examples
#' \dontrun{
#' parse.rdf(feed.read("http://feeds.feedburner.com/oatmealfeed"))
#' }
parse.rdf <- function(feed) {
  feed <- xmlToList(feed$RDF)
  #
  list(
    title = feed$channel$title,
    link = feed$channel$link,
    updated = parse.date(feed$channel$date),
    items = bind_rows(lapply(feed[names(feed) == "item"], function(item) {
      tibble(
        title = item$title,
        date  = parse.date(item$date),
        link  = item$link,
        description = item$encoded
      )
    }))
  )
}
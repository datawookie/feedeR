#' Parse feeds encoded in RDF/XML.
#' @param feed The URL of the feed.
#' @references
#' https://en.wikipedia.org/wiki/RDF/XML
#' @examples
#' \dontrun{
#' parse.rdf(feed.read("http://feeds.feedburner.com/oatmealfeed"))
#' }
parse.rdf <- function(feed) {
  feed <- feed$RDF
  #
  list(
    title = feed$channel$title[[1]],
    link = feed$channel$link[[1]],
    updated = parse.date(feed$channel$date[[1]]),
    items = bind_rows(lapply(feed[names(feed) == "item"], function(item) {
      tibble(
        title = item$title[[1]],
        date  = parse.date(item$date[[1]]),
        link  = item$link[[1]],
        description = item$encoded[[1]] %>% str_trim()
      )
    }))
  )
}
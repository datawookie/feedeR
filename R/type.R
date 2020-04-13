feed.type <- function(feed) {
  if("rss" %in% names(feed)) {
    return("RSS")
  } else if("RDF" %in% names(feed)) {
    return("RDF")
  } else {
    return("Atom")
  }
}

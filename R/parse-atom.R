find.link <- function(links) {
  if (length(links) == 1) {
    return(attributes(links$link)$href)
  } else {
    index = sapply(links, function(n) "title" %in% names(n))
    if (any(index)) {
      return(links[index]$link["href"])
    }
    index = sapply(links, function(n) attributes(n)["rel"] == "alternate")
    if (any(index)) {
      return(attributes(links[index]$link)$href)
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
parse.atom <- function(feed) {
  feed <- feed$feed
  #
  # feed <- unlist(feed, recursive = FALSE)
  #
  for (link in feed[names(feed) == "link"]) {
    link = attributes(link)
    if (link$rel == "self") break
  }
  #
  list(
    title = get_title(feed$title[[1]]),
    link  = link$href,
    updated = parse.date(feed$updated[[1]]),
    items = bind_rows(lapply(feed[names(feed) == "entry"], function(item) {
      description <- item$content[[1]]
      description <- ifelse(is.null(description), NA, description)
      tibble(
        title = get_title(item$title[[1]]),
        date  = if(!is.null(item$published[[1]])) parse.date(item$published[[1]]) else
          if(!is.null(item$updated[[1]])) parse.date(item$updated[[1]]) else NA,
        link  = if(!is.null(item$origLink[[1]])) item$origLink[[1]] else find.link(item[names(item) == "link"]),
        description = description
      )
    }))
  )
}

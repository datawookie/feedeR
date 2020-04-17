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
parse.atom <- function(feed) {
  feed <- xmlToList(feed$feed)
  #
  list(
    title = get_title(feed$title),
    link  = feed[names(feed) == "link"][[2]] %>% unname %>% filter.link,
    updated = parse.date(feed$updated),
    items = bind_rows(lapply(feed[names(feed) == "entry"], function(item) {
      tibble(
        title = get_title(item$title),
        date  = if(!is.null(item$published)) parse.date(item$published) else
          if(!is.null(item$updated)) parse.date(item$updated) else NA,
        link  = if(!is.null(item$origLink)) item$origLink else find.link(item[names(item) == "link"]),
        description = NA
      )
    }))
  )
}
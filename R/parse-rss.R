# Entry fields to include:
#
# - category
# - description
#
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
      tibble(
        title = item$title,
        date  = date,
        link  = if(is.null(item$origLink)) item$link else item$origLink,
        description = item$description,
        stringsAsFactors = FALSE
      )
    }))
  )
}
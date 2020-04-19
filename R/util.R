# Ensure that all feed URLs begin with http:// or https://.
#
clean.url <- function(url) {
  ifelse(!grepl("^https?://", url), paste("http://", url, sep = ""), url)
}

# locate.link <- function(text) {
#   grepl("^https?://", text)
# }
# 
# filter.link <- function(text) {
#   text[locate.link(text)]
# }
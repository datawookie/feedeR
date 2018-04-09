context("RDF")

URL = "http://feeds.nature.com/nplants/rss/current"

document = paste(readLines(URL), collapse = "\n")

test_that("parse RDF feed", {
  feed <<- parse.xml(document)
  expect_is(feed, "list")
})

test_that("identify RDF feed", {
  expect_equal(feed.type(feed), "RDF")
})

test_that("read RDF feed", {
  expect_type(feed.extract(URL), "list")
})

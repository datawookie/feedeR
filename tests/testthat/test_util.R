context("URL")

test_that("replace https:// with http://", {
  expect_equal(clean.url("https://feeds.feedburner.com/RBloggers"), "http://feeds.feedburner.com/RBloggers")
})

test_that("insert missing http://", {
  expect_equal(clean.url("feeds.feedburner.com/RBloggers"), "http://feeds.feedburner.com/RBloggers")
})

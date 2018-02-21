context("Atom")

# A sample Atom feed from \url{https://validator.w3.org/feed/docs/atom.html}:
#
document = '<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>Example Feed</title>
  <link href="http://example.org/"/>
  <updated>2003-12-13T18:30:02Z</updated>
  <author>
    <name>John Doe</name>
  </author>
  <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>
  <entry>
    <title>Atom-Powered Robots Run Amok</title>
    <link href="http://example.org/2003/12/13/atom03"/>
    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
    <updated>2003-12-13T18:30:02Z</updated>
    <summary>Some text.</summary>
  </entry>
</feed>'

test_that("parse Atom feed", {
  feed <<- parse.xml(document)
  expect_is(feed, "list")
})

test_that("identify Atom feed", {
  expect_equal(feed.type(feed), "Atom")
})

test_that("parse feed with origLink", {
  expect_is(feed.extract("http://feeds.feedburner.com/GeekingWithGreg"), "list")
})

test_that("parse feed without origLink", {
  expect_is(feed.extract("http://fastml.com/atom.xml"), "list")
  expect_is(feed.extract("http://feeds.feedburner.com/FeaturedPosts-Dataviz?format=xml"), "list")
})

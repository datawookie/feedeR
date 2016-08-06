context("RSS")

# A sample RSS feed:
#
document = '<?xml version="1.0" encoding="windows-1252"?>
<rss version="2.0">
<channel>
<title>Sample Feed - Favorite RSS Related Software &amp; Resources</title>
<description>Take a look at some of FeedForAll&apos;s favorite software and resources for learning more about RSS.</description>
<link>http://www.feedforall.com</link>
<category domain="www.dmoz.com">Computers/Software/Internet/Site Management/Content Management</category>
<copyright>Copyright 2004 NotePage, Inc.</copyright>
<docs>http://blogs.law.harvard.edu/tech/rss</docs>
<language>en-us</language>
<lastBuildDate>Mon, 1 Nov 2004 13:17:17 -0500</lastBuildDate>
<managingEditor>marketing@feedforall.com</managingEditor>
<pubDate>Tue, 26 Oct 2004 14:06:44 -0500</pubDate>
<webMaster>webmaster@feedforall.com</webMaster>
<generator>FeedForAll Beta1 (0.0.1.8)</generator>
<image>
<url>http://www.feedforall.com/feedforall-temp.gif</url>
<title>FeedForAll Sample Feed</title>
<link>http://www.feedforall.com/industry-solutions.htm</link>
<description>FeedForAll Sample Feed</description>
<width>144</width>
<height>117</height>
</image>
<item>
<title>RSS Resources</title>
<description>Our favorite RSS Resources</description>
<link>http://www.feedforall.com</link>
<pubDate>Tue, 26 Oct 2004 14:01:01 -0500</pubDate>
</item>
</channel>
</rss>'

test_that("parse RSS feed", {
  feed <<- parse.xml(document)
  expect_is(feed, "XMLDocumentContent")
})

test_that("identify RSS feed", {
  expect_equal(feed.type(feed), "RSS")
})

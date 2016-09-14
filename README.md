![Feed icon](https://upload.wikimedia.org/wikipedia/en/4/43/Feed-icon.svg)

[![Build Status](https://travis-ci.org/DataWookie/feedeR.svg?branch=master)](https://travis-ci.org/DataWookie/feedeR)

# Feed Reader Package for R

A package for reading [RSS](https://en.wikipedia.org/wiki/RSS) and [Atom](https://en.wikipedia.org/wiki/Atom_(standard)) feeds.

## Installation

Easy to install.
```
devtools::install_github("DataWookie/feedeR")
```

## Usage

For a RSS feed:
```
feed.extract("https://feeds.feedburner.com/RBloggers")
```
For an Atom feed:
```
feed.extract("http://journal.r-project.org/rss.atom")
```

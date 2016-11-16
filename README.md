![Feed icon](https://upload.wikimedia.org/wikipedia/en/4/43/Feed-icon.svg)

<br>

[![Build Status](https://travis-ci.org/DataWookie/feedeR.svg?branch=master)](https://travis-ci.org/DataWookie/feedeR)
[![GitHub version](https://badge.fury.io/gh/DataWookie%2FfeedeR.svg)](https://badge.fury.io/gh/DataWookie%2FfeedeR)
[![codecov.io](https://codecov.io/github/DataWookie/feedeR/coverage.svg?branch=master)](https://codecov.io/github/DataWookie/feedeR?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/feedeR)](https://cran.r-project.org/package=feedeR)
![CRAN version](http://www.r-pkg.org/badges/version/feedeR)

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

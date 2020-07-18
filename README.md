
<!-- README.md is generated from README.Rmd. Please edit that file -->

# feedeR <img src="man/figures/feedeR-hex.png" align="right" alt="" width="120" />

[![Build
Status](https://travis-ci.org/DataWookie/feedeR.svg?branch=master)](https://travis-ci.org/DataWookie/feedeR)
[![GitHub
version](https://badge.fury.io/gh/DataWookie%2FfeedeR.svg)](https://badge.fury.io/gh/DataWookie%2FfeedeR)
[![codecov.io](https://codecov.io/github/DataWookie/feedeR/coverage.svg?branch=master)](https://codecov.io/github/DataWookie/feedeR?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/feedeR)](https://cran.r-project.org/package=feedeR)

# Feed Reader Package for R

A package for reading [RSS](https://en.wikipedia.org/wiki/RSS) and
[Atom](https://en.wikipedia.org/wiki/Atom_\(standard\)) feeds.

## Installation

Easy to install.

    devtools::install_github("datawookie/feedeR")

## Usage

For a RSS feed:

    feed.extract("https://feeds.feedburner.com/RBloggers")

For an Atom feed:

    feed.extract("http://journal.r-project.org/rss.atom")

## Similar Projects

  - The [{scifetch}](https://github.com/yufree/scifetch) package has
    `getrss()`.
  - The
    [{tidyRSS}](https://cran.r-project.org/web/packages/tidyRSS/index.html)
    package.

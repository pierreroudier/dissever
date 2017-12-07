[![Travis-CI Build Status](https://travis-ci.org/pierreroudier/dissever.svg?branch=master)](https://travis-ci.org/pierreroudier/dissever)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dissever)](https://CRAN.R-project.org/package=dissever)

# `dissever`: a method for spatial downscaling

## What is `dissever`?

`dissever` is a general method for spatial downscaling introduced by Malone *et al.* in their [2012 *Computers & Geosciences* paper](http://www.sciencedirect.com/science/article/pii/S0098300411002895). This method has been extended and generalised [by Roudier *et al.*](https://www.sciencedirect.com/science/article/pii/S0168169917304192) to use and leverage a wide range of regression methods.

This package implements the latter in R, and modifies it so numerous regression methods can be tested. It leverages the power of [the `caret` package](https://topepo.github.io/caret) to do so.

# Installing `dissever`

To install the stable version of `dissever`:

```
install.packages('dissever')
```

To install the development version of `dissever`:

```
# install.packages('devtools')
devtools::install_github('pierreroudier/dissever')
```

[![Travis-CI Build Status](https://travis-ci.org/pierreroudier/dissever.svg?branch=master)](https://travis-ci.org/pierreroudier/dissever)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dissever)](http://cran.r-project.org/web/packages/dissever)

# `dissever`: a method for spatial downscaling

`dissever` is a general method for spatial downscaling introduced by Malone *et al.* in their [2012 *Computers & Geosciences* paper](http://www.sciencedirect.com/science/article/pii/S0098300411002895).

This package implements this method in R, and modifies it so numerous regression methods can be tested. It leverages the power of [the `caret` package](https://topepo.github.io/caret) to do so.

The package is not on CRAN yet, but can be installed easily using the `devtools` package. If you don't have `devtools` installed:

```
install.packages('devtools')
```

Then to install `dissever`:

```
devtools::install_github('pierreroudier/dissever')
```


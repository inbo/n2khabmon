
<!-- README.md is generated from README.Rmd. Please edit that file -->

# n2khabmon

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10246316.svg)](https://doi.org/10.5281/zenodo.10246316)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/inbo/n2khabmon/workflows/R-CMD-check/badge.svg)](https://github.com/inbo/n2khabmon/actions?query=workflow%3AR-CMD-check)
[![inbo
r-universe-name](https://inbo.r-universe.dev/badges/:name?color=c04384)](https://inbo.r-universe.dev)
[![inbo r-universe package
status](https://inbo.r-universe.dev/badges/n2khabmon)](https://inbo.r-universe.dev)
<!-- badges: end -->

The goal of `n2khabmon` is to prepare and manage Flemish monitoring
schemes regarding Natura 2000 habitats and regionally important biotopes
(RIBs).

## Installation

To install the current package version from the `main` branch (latest
stable release), run:

``` r
install.packages("n2khabmon", repos = c(inbo = "https://inbo.r-universe.dev", 
                                        CRAN = "https://cloud.r-project.org"))
```

The above provides a pre-compiled package for Windows and macOS, which
should be faster than below approach. INBO staff should have the INBO
repository enabled already (check with `getOption("repos")`), in which
case **`install.packages("n2khabmon")`** is all you need!

If you want to install from the source repository, run:

``` r
remotes::install_github("inbo/n2khabmon",
                        build_vignettes = TRUE,
                        upgrade = TRUE)
```

Note that this will install the package from the `main` branch. If you
need a version from another branch, add the `ref` argument in the above
function to provide the branch name.

Repeat the installation when you wish to upgrade.

## Code of Conduct

Please note that the `n2khabmon` package is released with a [Contributor
Code of Conduct](https://inbo.github.io/n2khabmon/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

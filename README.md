# prefixer:: <img src="man/figures/logo_prefixer.png" width=200 align="right" />

> Prefix function with their namespace and other tools for writing functions / packages

<!-- badges: start -->
[![Travis-CI Build Status](https://travis-ci.org/dreamRs/prefixer.svg?branch=master)](https://travis-ci.org/dreamRs/prefixer)
[![Project Status: Active-The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![R build status](https://github.com/dreamRs/prefixer/workflows/R-CMD-check/badge.svg)](https://github.com/dreamRs/prefixer/actions)
<!-- badges: end -->


## Overview

It can be useful to prefix function in a script to prevent use of the wrong one, e.g. `stats::filter` vs `dplyr::filter` or `plyr::summarise` vs `dplyr::summarise`.
This package provide a Shiny gadget to interactively add prefix to function in a script, if a function exist in several packages, you can choose the one you want use.

If you're in a package, you can generate `@importFrom` tag from function definition and after remove prefix if needed.


Installation :

```r
# with remotes
remotes::install_github("dreamRs/prefixer")
```


:warning: functions of this package will modify your scripts



## prefixer:: gadget

You can launch the addin via RStudio's Addins menu. Interface looks like this :

![](man/figures/prefixerUI.png)


```r
fread_dir <- function(path, pattern = "\\.csv$") {
  paths <- list.files(path = path, pattern = pattern, full.names = TRUE)
  files <- lapply(paths, fread)
  files <- setNames(files, paths)
  rbindlist(l = files, idcol = "path")
}
```

Becomes => 

```r
fread_dir <- function(path, pattern = "\\.csv$") {
  paths <- list.files(path = path, pattern = pattern, full.names = TRUE)
  files <- lapply(paths, data.table::fread)
  files <- stats::setNames(files, paths)
  data.table::rbindlist(l = files, idcol = "path")
}
```



## @importFrom

From prefixed functions, you can generate roxygen `@importFrom` tag via addin *@importFrom*.

Or manually with `import_from(fun = fread_dir)`:

```r
#' @importFrom data.table fread rbindlist
#' @importFrom stats setNames
fread_dir <- function(path, pattern = "\\.csv$") {
  paths <- list.files(path = path, pattern = pattern, full.names = TRUE)
  files <- lapply(paths, data.table::fread)
  files <- stats::setNames(files, paths)
  data.table::rbindlist(l = files, idcol = "path")
}
```



## Unprefix

After generated `@importFrom` tags, you can if you want remove prefix before your functions via addin *Unprefix* : 

```r
#' @importFrom data.table fread rbindlist
#' @importFrom stats setNames
fread_dir <- function(path, pattern = "\\.csv$") {
  paths <- list.files(path = path, pattern = pattern, full.names = TRUE)
  files <- lapply(paths, fread)
  files <- setNames(files, paths)
  rbindlist(l = files, idcol = "path")
}
```



## Roxygen comments

Addin *Roxygen comment* allow to comment selected line with `#'`.



## Not-ASCII

Addin *Not-ASCII* escape all not-ASCII characters between quotes to Unicode, to avoid warning in R CMD check :

```r
frenchFileInput <- function(inputId) {
  fileInput(
    inputId = inputId,
    label = "Sélectionner un fichier :",
    buttonLabel = "Parcourir...",
    placeholder = "Aucun fichier sélectionné"
  )
}
```
becomes =>

```r
frenchFileInput <- function(inputId) {
  fileInput(
    inputId = inputId,
    label = "S\u00e9lectionner un fichier :",
    buttonLabel = "Parcourir...",
    placeholder = "Aucun fichier s\u00e9lectionn\u00e9"
  )
}
```



## Show Non-ASCII files

Show files containing Non-ASCII characters in RStudio markers tabs. This function is similar to `tools::showNonASCIIfile`.

```r
show_nonascii_file()
```
![](man/figures/show_nonascii_file.png)



## Check Rd files

Check if your Rd files (documentation) contains examples and return value:

```r
check_Rd_examples()
# 1 Rd file(s) with no examples. 
#  - unprefix.Rd 

check_Rd_value()
# 2 Rd file(s) with no value. 
#  - prefixer.Rd 
#  - unprefix.Rd 
```


## Count calls

Get functions used in scripts and where they come from, for example functions used in this package:

```r
count_calls("R/")
#                      call  n                  package
# 1                    tags 36                htmltools
# 2              rstudioapi 15                     <NA>
# 3         str_replace_all 15                  stringr
# 4                     div  9                htmltools
# 5                       b  8                     <NA>
# 6            observeEvent  8                    shiny
# 7                     pre  6                htmltools
# 8           set_selection  6                 prefixer
# 9  getSourceEditorContext  5               rstudioapi
# 10             insertText  5               rstudioapi
# ...
```



# prefixer::

> Prefix function with their namespace

[![Travis-CI Build Status](https://travis-ci.org/dreamRs/prefixer.svg?branch=master)](https://travis-ci.org/dreamRs/prefixer)


## Overview

It can be useful to prefix function in a script to prevent use of the wrong one, e.g. `stats::filter` vs `dplyr::filter` or `plyr::summarise` vs `dplyr::summarise`.
This package provide a Shiny gadget to interactively add prefix to function in a script, if a function exist in several packages, you can choose the one you want use.

If you're in a package, you can generate `@importFrom` tag from function definition and after remove prefix if needeed.


Installation :

```r
# From Github only for now
source("https://install-github.me/dreamRs/prefixer")

# or traditionnal :
devtools::install_github("dreamRs/prefixer")
```


## prefixer:: gadget

You can launch the addin via RStudio's Addins menu. Interface looks like this :

![prefixer](inst/img/prefixerUI.png)


## @importFrom

To generate `@importFrom` tag, you can use `import_from` manually (for the moment) :

```r
import_from(fun = prefixer)
```


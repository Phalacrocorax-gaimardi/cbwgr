
# cbwgr

<!-- badges: start -->
<!-- badges: end -->

cbwgr calculates past and future global warming contributions for Ireland using the FaiR Simple Climate Model.


## Prerequisites

Python must be installed on your system along with the numpy, xarray and fair python packages.

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```


## Installation

You can install the development version of cbwgr like so:

``` r
library(devtools)
install_github("Phalacrocorax-gaimardi/cbwgr")
```

## Example 1

Run global scenarios (SSPs)
``` r
library(cbwgr)
global_scenarios <- c('ssp119','ssp126')
#import some python modules
import_python_modules()
f <- gen_fair()
f$run
```

## Example 2

"# cbwg" 

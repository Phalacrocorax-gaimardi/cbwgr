
# cbwgr

<!-- badges: start -->
<!-- badges: end -->

cbwgr calculates past and future global warming contributions for Ireland using the FaiR Simple Climate Model.


## Prerequisites

Python must be installed on your system. To install FaIR
``` 
pip install fair
```


## Installation

To install cbwgr from github you need a personal access token:

``` r
library(remotes)
install_github("Phalacrocorax-gaimardi/cbwgr",auth_token = "personal-access-token")
```

## Example 1

Select global and 
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

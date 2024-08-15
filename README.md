
# cbwgr

<!-- badges: start -->
<!-- badges: end -->

cbwgr calculates past and future global warming contributions for Ireland using the FaiR Simple Climate Model.


## Prerequisites

Python must be installed on your system. To install FaIR
``` 
pip install fair
```
If there are multiple versions of you need to ensure that fair is installed to the version of Python used by the R package reticulate. 

## Installation

To install cbwgr from github you might need a personal access token:

``` r
library(remotes)
install_github("Phalacrocorax-gaimardi/cbwgr",auth_token = "personal-access-token")
```

## Example 1

Run some global scenarios in FAIR. 
``` r
library(tidyverse)
library(cbwgr)
global_scenarios <- c('ssp119','ssp126')

#import python modules
xr <- reticulate::import("xarray")
np <- reticulate::import("numpy")
ff <- reticulate::import("fair.fair")
io <- reticulate::import("fair.io")
interface <- reticulate::import("fair.interface")

f <- gen_fair()
f$run
```
By default FaIR runs with 66 parameter configurations (ensemble members or "configs" based on ESM calibrations). 

To find GSAT in 'ssp126' and for all configs
```r
gsat <- get_warming(f,'ssp126)
```

## Example 2
Calculate the warming contribution of Ireland in a national emissions scenario combination  "300mt-led" and "Sc3e".

Create a dataframe of "leave-one-out" global emissions:
```r
rcmip_loo <- cbwg_to_loo_global_emissions(global_scenarios,tim_scenarios,goblin_scenarios)
```
Create a new model instance based on LOO emissions in global_scenarios and national scenarios, and find global warming gsat_loo in this scenario
```r
g <- gen_fair_loo(rcmip_loo,"300mt-led","Sc3e")
g$run()
gsat_loo <- get_warming(g,'ssp126')
```
The difference in global warming between gsat and gsat_loo is the national marginal warming contribution. To find this
```r
gsat <- gsat %>% rename()

##Example 3

The national ffi scenarios are "tim_scenarios" and the national AFOLU scenarios are "goblin_scenarios".

``` r
tim_scenarios <- c("250mt-led","300mt-led","300mt-bau","350mt-led","350mt-bau","400mt-bau","450mt-bau")
goblin_scenarios <- c("Sc1a","Sc2a","Sc3a","Sc1b","Sc2b","Sc3b","Sc1c","Sc2c","Sc3c","Sc1d","Sc2d","Sc3d","Sc1e","Sc2e","Sc3e")
```


"# cbwg" 

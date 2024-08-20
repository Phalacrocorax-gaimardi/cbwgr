
# cbwgr

<!-- badges: start -->
<!-- badges: end -->

cbwgr calculates past and future global warming contributions for Ireland using the FaiR Simple Climate Model.


## Prerequisites

Python must be installed on your system. To install FaIR
``` 
pip install fair
```
If there are multiple versions you need to ensure that fair is installed to the version of Python used by the R package reticulate. 

## Installation

At the moment, to install cbwgr from github you need a personal access token:

``` r
library(remotes)
install_github("Phalacrocorax-gaimardi/cbwgr",auth_token = "personal-access-token")
```

## Example 1

Run some global scenarios in FAIR. 
``` r
library(tidyverse)
library(cbwgr)

#import python modules
xr <- reticulate::import("xarray")
np <- reticulate::import("numpy")
ff <- reticulate::import("fair.fair")
io <- reticulate::import("fair.io")
interface <- reticulate::import("fair.interface")
#select some global emissions scenarios
global_scenarios <- c('ssp119','ssp126','ssp434','ssp245')

f <- gen_fair()
f$run
```
By default cbwgr runs FaIR with 66 parameter configurations (ensemble members or "configs" based on CMIP6 ESM calibrations). The model parameters for these configs are in cbwgr::fair_configs_cmip6.

For example, to find warming in 'ssp126' for all configs (relative to 1851-1900)
```r
gsat <- get_warming(f,'ssp126')
```
gsat contains global warming for each scenario/config combination. The median warming is
in 'ssp126' over all configs is (relative to 1851-1900)
```r
gsat_m <- gsat %>% group_by(year) %>% summarise(value=median(value))
```
## Example 2
Calculate the warming contribution of Ireland in a national emissions scenario combination  "300mt-led" and "Sc3e".

Create a dataframe of "leave-one-out" global emissions:
```r
rcmip_loo <- cbwg_to_loo_global_emissions(global_scenarios,tim_scenarios,goblin_scenarios)
```
Create a new model instance based on leave-one-outb (LOO) emissions for global_scenarios and national scenarios, and the find global warming gsat_loo in ssp126.
```r
g <- gen_fair_loo(rcmip_loo,"300mt-led","Sc3e")
g$run()
gsat_loo <- get_warming(g,'ssp126')
```
The difference in global warming between gsat and gsat_loo is the national marginal warming contribution. To find this
```r
gsat <- gsat %>% rename("global"=value)
gsat_loo <- gsat %>% rename("global_loo"=value)

gsat_ie <- gsat %>% inner_join(gsat_loo) %>% mutate(ireland=global-global_loo)
```
This contains the Irish marginal contribution to warming in all 66 configs. The median Irish contribution is:
```r
gsat_ie_m <- gsat_ie %>% group_by(year) %>% summarise(ireland=median(ireland))
```

"# cbwg" 

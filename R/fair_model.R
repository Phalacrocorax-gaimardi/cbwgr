#Sys.setenv(RETICULATE_PYTHON = "/Users/Joe/AppData/Local/Programs/Python/Python38/")

#' import_python_modules
#'
#' Imports fair, Xarray, Numpy modules which need to be installed as python libraries in the python version used by
#' reticulate
#'
#' @return Null
#' @export
#'
#' @examples
import_python_modules <- function(){

  #reticulate::import("rpytools")
  Sys.setenv(RETICULATE_PYTHON = "/Users/Joe/AppData/Local/Programs/Python/Python38/")
  xr <- reticulate::import("xarray")
  np <- reticulate::import("numpy")
  ff <- reticulate::import("fair.fair")
  io <- reticulate::import("fair.io")
  interface <- reticulate::import("fair.interface")
}


#FaiR functions

#' cbwg_scen_to_fair
#'
#' re-formats CBWG emissions input file scenario to FaIR format and units
#'
#' @param cbwg_emissions input emissions in kt gas
#' @param tim_scen ffi co2 scenario
#' @param goblin_scen aflolu scenario
#'
#' @return
#' @export
#'
#' @examples
cbwg_scen_to_fair <- function(cbwg_emissions, tim_scen,goblin_scen){

  ie_gases <- cbwg_emissions$gas %>% unique()
  ie_scenario_gases <- c("FFI-CO2","LULUCF-CO2","AG-CH4","N2O")
  ie_non_scenario_gases <- ie_gases[!(ie_gases %in% ie_scenario_gases)]
  #cbwg <- cbwg_emissions %>% filter(ga <= end_2021)
  cbwg <- cbwg_emissions %>% dplyr::mutate(scenario=ifelse(gas %in% ie_non_scenario_gases | year < 2022,"CBWG",scenario))
  cbwg <- cbwg %>% dplyr::mutate(scenario = stringr::str_replace(scenario,"_EXTRAP",""))

  cbwg <- cbwg %>% dplyr::filter(scenario %in% c("CBWG",tim_scen,goblin_scen))

  #preliminaries: collapse CH4 AG, WASTE and LULUCF emissions to AFOLU emissions
  ch4 <- cbwg %>% dplyr::filter(stringr::str_detect(gas,"CH4")) %>% dplyr::group_by(year) %>% dplyr::summarise(gas="CH4",value=sum(value),units=units[1])
  #ch4 <- ch4 %>% group_by(year) %>% summarise(gas="AFOLU-CH4",units=units[1],scenario=scenario[1], value=sum(value)) %>% ungroup()
  cbwg <- cbwg %>% dplyr::filter(!stringr::str_detect(gas,"CH4")) %>% dplyr::bind_rows(ch4)

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="CH4"~value/1000,gas!="CH4"~value))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="CH4","Mt CH4/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="FFI-CO2"~value/1e+6,gas!="FFI-CO2"~value))
  cbwg <- cbwg %>% dplyr::mutate(gas=dplyr::case_when(gas=="FFI-CO2"~"CO2 FFI",gas!="FFI-CO2"~gas))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="CO2 FFI","Gt CO2/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="LULUCF-CO2"~value/1e+6,gas!="LULUCF-CO2"~value))
  cbwg <- cbwg %>% dplyr::mutate(gas=dplyr::case_when(gas=="LULUCF-CO2"~"CO2 AFOLU",gas!="LULUCF-CO2"~gas))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="CO2 AFOLU","Gt CO2/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="N2O"~value/1e+3,gas!="N2O"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="N2O"~"N2OB",gas!="N2O"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="N2O","Mt N2O/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="SO2"~value/1000,gas!="SO2"~value))
  cbwg <- cbwg %>% dplyr::mutate(gas=dplyr::case_when(gas=="SO2"~"Sulfur",gas!="SO2"~gas))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units, gas =="Sulfur","Mt SO2/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="NH3"~value/1000,gas!="NH3"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="NH3"~"NH3B",gas!="NH3"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="NH3","Mt NH3/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="CO"~value/1000,gas!="CO"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="CO"~"COI",gas!="CO"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="CO","Mt CO/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="NOx"~value/1000,gas!="NOx"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="NOx"~"NOXI",gas!="NOx"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="NOx","Mt NO2/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="NMVOC"~value/1000,gas!="NMVOC"~value))
  cbwg <- cbwg %>% dplyr::mutate(gas=dplyr::case_when(gas=="NMVOC"~"VOC",gas!="NMVOC"~gas))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="VOC","Mt VOC/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="BC"~value/1000,gas!="BC"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="BC"~"BCI",gas!="BC"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="BC","Mt BC/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="OC"~value/1000,gas!="OC"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="OC"~"OCI",gas!="OC"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="OC","Mt OC/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="HFC-125"~value/1000,gas!="HFC-125"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="HFC-125"~"HFC125",gas!="HFC-125"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="HFC-125","kt HFC125/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="HFC-134a"~value/1000,gas!="HFC-134a"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="HFC-134a"~"HFC134A",gas!="HFC-134a"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="HFC-134a","kt HFC134a/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="HFC-143a"~value/1000,gas!="HFC-143a"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="HFC-143a"~"HFC143A",gas!="HFC-143a"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="HFC-143a","kt HFC143a/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="HFC-227ea"~value/1000,gas!="HFC-227ea"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="HFC-227ea"~"HFC227EA",gas!="HFC-227ea"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="HFC-227ea","kt HFC227ea/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="HFC-23"~value/1000,gas!="HFC-23"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="HFC-23"~"HFC23",gas!="HFC-23"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="HFC-23","kt HFC23/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="HFC-152a"~value/1000,gas!="HFC-152a"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="HFC-152a"~"HFC152A",gas!="HFC-152a"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="HFC-152a","kt HFC152a/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="HFC-32"~value/1000,gas!="HFC-32"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="HFC-32"~"HFC32",gas!="HFC-32"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="HFC-32","kt HFC32/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="C2F6"~value/1000,gas!="C2F6"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="C2F6"~"C2F6",gas!="C2F6"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="C2F6","kt C2F6/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="CF4"~value/1000,gas!="CF4"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="CF4"~"CF4",gas!="CF4"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="CF4","kt CF4/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="SF6"~value/1000,gas!="SF6"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="SF6"~"SF6",gas!="SF6"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="SF6","kt SF6/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="NF3"~value/1000,gas!="NF3"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="NF3"~"NF3",gas!="NF3"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="NF3","kt NF3/yr"))

  cbwg <- cbwg %>% dplyr::mutate(value=dplyr::case_when(gas=="c-C4F8"~value/1000,gas!="c-C4F8"~value))
  #cbwg <- cbwg %>% mutate(variable=case_when(gas=="c-C4F8"~"CC4F8",gas!="c-C4F8"~variable))
  cbwg <- cbwg %>% dplyr::mutate(units=replace(units,gas=="c-C4F8","kt CC4F8/yr"))

  cbwg <- cbwg %>% dplyr::rename("unit"=units)
  cbwg <- cbwg %>% dplyr::rename("specie"=gas)
  cbwg <- cbwg %>% dplyr::mutate(tim = tim_scen,goblin = goblin_scen) %>% dplyr::select(-scenario)
  return(cbwg)
}

#' cbwg_to_loo_global_emissions
#'
#' Creates a global emissions dataset for specified global and Irish emissions scenarios in FaIR format. Contains 21 climate forcers.
#'
#' @param global_scenarios a vector SSP scenarios
#' @param tim_scenarios vector of Energy system scenarios
#' @param goblin_scenarios vector AFOLU scenarios
#'
#' @return emissions
#' @export
#'
#' @examples
cbwg_to_loo_global_emissions <- function(global_scenarios,tim_scenarios,goblin_scenarios){

  rcmip0 <- rcmip_fair %>% dplyr::rename("global"=value) %>% dplyr::filter(scenario %in% global_scenarios)
  rcmip_loo <- tibble::tibble()
  for(tim_scen in tim_scenarios)
    for(goblin_scen in goblin_scenarios){
      scen_ie <- cbwg_scen_to_fair(emissions_cbwg_2,tim_scen,goblin_scen)
      scen_ie$scenario <- NULL
      scen_ie$year <- scen_ie$year + 0.5
      #combine with rcmip
      comb <- rcmip0 %>% dplyr::inner_join(scen_ie,by=c("specie","year","unit"))
      comb <- comb %>% dplyr::rename("ireland"=value)
      comb <- comb %>% dplyr::mutate(global_loo = global-ireland)
      rcmip_loo <- dplyr::bind_rows(rcmip_loo,comb)
      rcmip_loo <- rcmip_loo %>% dplyr::select(scenario,tim,goblin,year,specie, global,ireland,global_loo,unit)
    }
  return(rcmip_loo %>% dplyr::select(scenario,tim,goblin,specie,year,global,ireland,global_loo,unit))
}


#global_scenarios <- c('ssp119','ssp126','ssp434','ssp245')
#tim_scenarios <- c("250mt-bau","250mt-led","300mt-bau","300mt-led","350mt-bau","350mt-led","400mt-bau","450mt-bau")
#goblin_scenarios <-outer(c("Sc1","Sc2","Sc3"), c("a","b","c","d","e"),FUN = "paste",sep="") %>% as.vector() %>% sort()

#' gen_fair
#'
#' initialises a FaIRv2.1 model with configs from cbwgr::fair_configs_cmip and global scenario emissions from cbwgr::rcmip_fair
#'
#' @param scenarios the global scenarios to be used
#' @param endyear final year of calculation
#' @param ch4_meth CH4 method ('leach2021' or 'thornhill2021')
#' @param ghg_meth GHG forcings from concentration ('myhre1998','meinshausen2020','leach2021ghg')
#'
#' @return initialised FaiR model
#' @export
#'
#' @examples
gen_fair <- function(scenarios=global_scenarios, endyear=2100L, ch4_meth = 'thornhill2021',ghg_meth='meinshausen2020'){

  #instantiate
  print("instantiating FaIR model")
  f <- ff$FAIR(ch4_method=ch4_meth,ghg_method=ghg_meth) #NB brackets (instance)
  #define time
  f$define_time(start=1750L, end=2100L,step= 1L)
  #define scenario names
  f$define_scenarios(scenarios)
  #configs (i.e. climate sensitivity and other parameters)
  #df <- read_csv("~/Policy/CCACBudgets/4xCO2_cummins_ebm3.csv") #
  #df <- df %>% filter(run=="r1i1p1f1")
  configs <- fair_configs_cmip6 %>% dplyr::mutate(model_run = paste(model,run,sep="_"))
  config_names <- configs$model_run #%>% unique() #configs names = ESM model_runs or ensemble labels

  #names configs
  f$define_configs(config_names)
  #define species (climate forcers)
  #all species known to fair
  species <- io$read_properties()[[1]]
  #species properties
  properties <- io$read_properties()[[2]]
  #define species and properties
  f$define_species(species,properties)
  print("allocating")
  f$allocate()

  ################################
  #initialise
  ##############################
  print("initial conditions")
  f$fill_species_configs()
  interface$initialise(f$concentration, f$species_configs['baseline_concentration'])
  interface$initialise(f$emissions, f$species_configs['baseline_emissions'])
  interface$initialise(f$forcing, 0)
  interface$initialise(f$temperature, 0)
  interface$initialise(f$airborne_emissions,0)
  interface$initialise(f$cumulative_emissions,0)

  print("filling global emissions")
  f$fill_from_rcmip() #doi:10.5281/zenodo.4589756/rcmip-emissions-annual-means-v5-1-0.csv
  #print("filling emissions")


  ######################
  #climate configs based on esms
  #######################
  print("filling configurations (FaIR ensemble)")
  for(conf in config_names){
    df <- configs %>% dplyr::filter(model_run==conf)
    heat_capacities <-   df %>% dplyr::select(C1,C2,C3) %>% purrr::as_vector()
    heat_transfers <-  df %>% dplyr::select(kappa1,kappa2,kappa3) %>% purrr::as_vector()
    deep_ocean_eff <- df %>% dplyr::pull(epsilon) %>% purrr::as_vector()
    gamma <- df %>% dplyr::pull(gamma)
    sigma_eta <- df %>% dplyr::pull(sigma_eta)
    sigma_xi <- df %>% dplyr::pull(sigma_xi)
    interface$fill(f$climate_configs['ocean_heat_capacity'],  heat_capacities, config=conf)
    interface$fill(f$climate_configs['ocean_heat_transfer'], heat_transfers, config=conf)
    interface$fill(f$climate_configs['deep_ocean_efficacy'], deep_ocean_eff, config=conf)
    interface$fill(f$climate_configs['gamma_autocorrelation'], gamma, config=conf)
    interface$fill(f$climate_configs['sigma_eta'], sigma_eta, config=conf)
    interface$fill(f$climate_configs['sigma_xi'], sigma_xi, config=conf)
    interface$fill(f$climate_configs['stochastic_run'], FALSE, config=conf)
    #fill(f.climate_configs['use_seed'], True, config=config)
    #fill(f.climate_configs['seed'], seed, config=config)
  }
  print("configs good")
  #f$run()
  f %>% return()
}

#f$configs

#' get_conc
#'
#' @param fmod FaIR model run
#' @param scen a global ssp scenario
#'
#' @return a dataframe with scenario, config, specie, value and unit columns
#' @export
#'
#' @examples
get_conc <- function(fmod,scen){

  concentration_units <- ff$desired_concentration_units %>% tibble::as_tibble() %>% dplyr::bind_cols(id=1)
  concentration_units <- concentration_units %>% tidyr::pivot_longer(-id,names_to="specie",values_to="unit") %>% dplyr::select(-id)


  dat_numpy <- fmod$concentration$as_numpy()
  dat_r <- reticulate::np_array(dat_numpy$data) %>% reticulate::py_to_r()
  dimnames(dat_r) <- list(dat_numpy$timebounds$data,dat_numpy$scenario$data,dat_numpy$config$data,dat_numpy$specie$data)

  configs <- fair_configs_cmip6 %>% dplyr::mutate(model_run = paste(model,run,sep="_"))
  config_names <- configs$model_run #%>% unique() #configs names = ESM model_runs or ensemble labels

  conc <- tibble::tibble()
  for(i in seq_along(config_names)){

    conc0 <- dat_r[,which(scen==global_scenarios),i,1:64] %>% tibble::as_tibble() %>% dplyr::bind_cols(year=dat_numpy$timebounds$data)
    conc0$config <- config_names[i]
    conc <- conc %>% dplyr::bind_rows(conc0)
  }

  conc <- conc %>% tidyr::pivot_longer(c(-year,-config),names_to="specie", values_to="value") #%>% filter(esm != "EC-Earth3-Veg")
  conc$scenario <- scen
  conc <- conc %>% dplyr::right_join(concentration_units)
  conc <- conc %>% dplyr::select(scenario,config,specie,year,value,unit)
  conc %>% tidyr::drop_na() %>% return()

}

#' get_forcing
#'
#' get forcing by config and species from model run
#'
#' @param fmod FaIR model run
#' @param scen global scenario
#'
#' @return a dataframe with scenario, config, specie, value and unit columns
#' @export
#'
#' @examples
get_forcing <- function(fmod,scen){


  dat_numpy <- fmod$forcing$as_numpy()
  dat_r <- reticulate::np_array(dat_numpy$data) %>% reticulate::py_to_r()
  dimnames(dat_r) <- list(dat_numpy$timebounds$data,dat_numpy$scenario$data,dat_numpy$config$data,dat_numpy$specie$data)

  configs <- fair_configs_cmip6 %>% dplyr::mutate(model_run = paste(model,run,sep="_"))
  config_names <- configs$model_run #%>% unique() #configs names = ESM model_runs or ensemble labels

  force <-tibble::tibble()
  for(conf in config_names){
    force0 <- dat_r[,which(scen==scenarios),conf,1:64] %>% tibble::as_tibble() %>% dplyr::bind_cols(year=dat_numpy$timebounds$data)
    force0$config <- conf
    force <- dplyr::bind_rows(force,force0)
  }

  force <- force %>% tidyr::pivot_longer(c(-year,-config),names_to="specie", values_to="value") #%>% filter(esm != "EC-Earth3-Veg")
  force <- force %>% tidyr::drop_na()
  force$scenario <- scen
  force$unit <- "W/m2"
  force %>% dplyr::select(scenario,config,specie,year,value,unit) %>% return()

}


#' get_warming
#'
#' @param fmod Fair model run
#' @param scen global scenario
#' @param use_1851_1900_baseline If TRUE, use the 1851-1900 GSAT baseline
#'
#' @return a dataframe with scenario, config, value and unit columns
#' @export
#'
#' @examples
get_warming <- function(fmod,scen,use_1851_1900_baseline=TRUE){
  #
  dat_numpy <- fmod$temperature$as_numpy()
  #dat_r <- reticulate::np_array(dat_numpy$data) %>% reticulate::py_to_r()
  dat_r <- fmod$temperature$data
  dimnames(dat_r) <- list(dat_numpy$timebounds$data,dat_numpy$scenario$data,dat_numpy$config$data,dat_numpy$layer$data)
  #dim(dat_r)
  n_config <- fmod$configs %>% length()
  ssp <- dat_r[,which(scen==global_scenarios),1:n_config,1] %>% tibble::as_tibble() %>% dplyr::bind_cols(year=dat_numpy$timebounds$data)
  ssp <- ssp %>% tidyr::pivot_longer(-year,names_to="config", values_to="gsat") #%>% filter(esm != "EC-Earth3-Veg")
  ssp$scenario <- scen

  ssp0 <- ssp %>% dplyr::group_by(config) %>% dplyr::filter(year %in% 1851:1900) %>% dplyr::summarise(gsat0 = mean(gsat))
  if(!use_1851_1900_baseline) ssp0$gsat0 <- 0
  ssp <- ssp %>% dplyr::inner_join(ssp0) %>% dplyr::mutate(gsat=gsat-gsat0) %>% dplyr::select(-gsat0)
  ssp <- ssp %>% dplyr::rename("value"=gsat)
  ssp$unit <- "degreeC"
  ssp %>% dplyr::select(scenario,config,year,value,unit) %>% return()

}


#' get_ohc
#'
#' @param fmod FaIR model
#' @param scen global scenario
#'
#' @return a dataframe with scenario, config, value and unit columns
#' @export
#'
#' @examples
get_ohc <- function(fmod,scen){

  dat_numpy <- fmod$ocean_heat_content_change$as_numpy()
  dat_r <- reticulate::np_array(dat_numpy$data) %>% reticulate::py_to_r()
  dimnames(dat_r) <- list(dat_numpy$timebounds$data,dat_numpy$scenario$data,dat_numpy$config$data)

  ohc <-tibble::tibble()
  for(conf in config_names){
    ohc0 <- dat_r[,which(scen==scenarios),conf] %>% tibble::as_tibble() %>% dplyr::bind_cols(year=dat_numpy$timebounds$data)
    ohc0$config <- conf
    ohc <- dplyr::bind_rows(ohc,ohc0)
  }

  ohc <- ohc %>% tidyr::pivot_longer(c(-year,-config),names_to="specie", values_to="value") #%>% filter(esm != "EC-Earth3-Veg")
  ohc <- ohc %>% tidyr::drop_na()
  ohc$scenario <- scen
  ohc$unit <- "ZJ"
  ohc$value <- ohc$value/1e+21
  ohc %>% dplyr::select(scenario,config,year,value,unit) %>% return()

}

#' get_emissions
#'
#' @param fmod FaIR model
#' @param scen global scenario
#'
#' @return a dataframe with scenario, config, specie, value and unit columns
#' @export
#'
#' @examples
get_emissions <- function(fmod,scen){

  emissions_units <- ff$desired_emissions_units %>% tibble::as_tibble() %>% dplyr::bind_cols(id=1)
  emissions_units <- emissions_units %>% tidyr::pivot_longer(-id,names_to="specie",values_to="unit") %>% dplyr::select(-id)


  dat_numpy <- fmod$emissions$as_numpy()
  #dat_r <- reticulate::np_array(dat_numpy$data) %>% reticulate::py_to_r()
  dat_r <- fmod$emissions$data
  dimnames(dat_r) <- list(dat_numpy$timepoints$data,dat_numpy$scenario$data,dat_numpy$config$data,dat_numpy$specie$data)

  configs <- fair_configs_cmip6 %>% dplyr::mutate(model_run = paste(model,run,sep="_"))
  config_names <- configs$model_run #%>% unique() #configs names = ESM model_runs or ensemble labels

  emis <- tibble::tibble()
  for(i in seq_along(config_names)){

    emis0 <- dat_r[,which(scen==global_scenarios),i,1:64] %>% tibble::as_tibble() %>% dplyr::bind_cols(year=dat_numpy$timepoints$data)
    emis0$config <- config_names[i]
    emis <- emis %>% dplyr::bind_rows(emis0)
  }

  emis <- emis %>% tidyr::pivot_longer(c(-year,-config),names_to="specie", values_to="value") #%>% filter(esm != "EC-Earth3-Veg")
  emis$scenario <- scen
  emis <- emis %>% dplyr::inner_join(emissions_units)
  emis %>% dplyr::select(scenario,config,specie,year,value,unit) %>% tidyr::drop_na() %>% return()

}

#
#
#
#rcmip_exie <- cbwg_to_loo_global_emissions(global_scenarios,tim_scenarios,goblin_scenarios)


#' gen_fair_loo
#'
#' initialise a FaIR instance with global emissions minus a cbwg ireland emission scenario
#'
#' @param rcmip_loo global emissions dataset for global and cbwg scenarios produced by cbwg_to_loo_global_emissions
#' @param tim_scen a co2 ffi scenario name
#' @param goblin_scen afolu scenario name
#' @param endyear end year of run
#' @param ch4_meth ch4_method
#' @param ghg_meth ghg_method
#'
#' @return FaIR model initialised with global - ireland emissions
#' @export
#'
#' @examples
gen_fair_loo <- function(rcmip_loo,tim_scen="250mt-led", goblin_scen="Sc3e",endyear=2100L, ch4_meth = 'thornhill2021',ghg_meth='meinshausen2020'){
  #
  #instantiate
  fmod <- gen_fair(endyear=endyear,ch4_meth=ch4_meth,ghg_meth=ghg_meth)
  print("filling emissions")
  if(!(tim_scen %in% tim_scenarios)) stop("bad tim scenario")
  if(!(goblin_scen %in% goblin_scenarios)) stop("bad goblin scenario")
      rcmip_loo_0 <- rcmip_loo %>% dplyr::filter(tim == tim_scen,goblin==goblin_scen)
      species0 <- rcmip_loo_0$specie %>% unique()
      timepoints0 <- rcmip_loo_0$year %>% unique()
      configs <- fair_configs_cmip6 %>% dplyr::mutate(model_run = paste(model,run,sep="_"))
      config_names <- configs$model_run #%>% unique() #configs names = ESM model_runs or ensemble labels

  for(scen in global_scenarios)
    for(spec in species0)
      for(conf in config_names){
        #print(paste(scen,spec,conf,sep=" "))
        #dat <- rcmip_exie0 %>% dplyr::filter(scenario==scen,ie_scenario_ie,specie==spec) %>% dplyr::pull(global_exie)
        dat <- rcmip_loo_0 %>% dplyr::filter(scenario==scen,specie==spec) %>% dplyr::pull(global_loo)
        interface$fill(fmod$emissions, data=dat,specie=spec, scenario=scen,config=conf, timepoints = timepoints0)
      }

  #fmod$run()
  fmod %>% return()
}




#' find_neutral_probabilities
#'
#' The probability of temperature neutrality occurring before a given year neutral year, currently fixed at 2050. A specified probability
#' threshold is use to classify neutrality in 5-yearly increment e.g. 67% probability of neutrality before 2046 etc.
#'
#' @param gsat1 a data frame of warming temperature for global, national ffi and afolu emissions
#' @param p_threshold the probability threshold of interest used to classify neutrality combinations
#'
#' @return a dataframe giving probabilities of neutrality before neutral_year, neutral_year-5 and neutral_year -10.
#' @export
#'
#' @examples
find_neutral_probabilities <-function(gsat1, p_threshold=0.67){
  #
  nconfig <- dplyr::n_distinct(gsat1$config)
  neutrality <- gsat1 %>% dplyr::filter(year > 2020 & year <= 2061) %>% dplyr::group_by(config,scenario,tim,goblin) %>% dplyr::slice_max(order_by=gsat_ie,n=1,with_ties = FALSE) %>% dplyr::filter(scenario %in%  global_scenarios) %>% dplyr::arrange(year)
  neutrality <- neutrality %>% dplyr::rename("ireland"=gsat_ie)
  neutrality <- neutrality %>% dplyr::rename("max_year"=year) %>% dplyr::select(config,scenario,tim,goblin,max_year,ireland)

  neutral_probs <- neutrality  %>% dplyr::group_by(scenario,tim,goblin) %>% summarise(p_50=sum(max_year <= 2050)/nconfig,
                                                                                      p_45=sum(max_year <= 2045)/nconfig,
                                                                                      p_40=sum(max_year <= 2040)/nconfig)

  neutral_probs <- neutral_probs %>% dplyr::mutate(case = dplyr::case_when((p_40 > p_threshold)~paste(2036,2040,sep="-"),
                                                                           ((p_40 <= p_threshold) & (p_45 > p_threshold))~paste(2041,2045,sep="-"),
                                                                           ((p_50 >= p_threshold) & (p_45 < p_threshold))~paste(2046,2050,sep="-"),
                                                                           (p_50 < p_threshold)~"After 2050"))

  neutral_probs <- neutral_probs %>% dplyr::mutate(case = tidyr::replace_na(case,"After 2050"))
  neutral_probs <- neutral_probs %>% dplyr::filter(tim != "250mt-bau")
  neutral_probs$tim<- factor(neutral_probs$tim,levels=c("250mt-led","300mt-led","350mt-led","300mt-bau","350mt-bau","400mt-bau","450mt-bau"))
  neutral_probs$goblin<- factor(neutral_probs$goblin,levels=c("Sc1a","Sc2a","Sc3a","Sc1b","Sc2b","Sc3b","Sc1c","Sc2c","Sc3c","Sc1d","Sc2d","Sc3d","Sc1e","Sc2e","Sc3e"))
  neutral_probs$scenario <- factor(neutral_probs$scenario,levels=c("ssp119","ssp126","ssp434","ssp245"))
  neutral_probs %>% return()
}

#
#global_scenarios <- c("ssp119","ssp126")
#f <- gen_fair()
#f$run()
#tim_scenarios <- c("250mt-led","300mt-led","350mt-bau")
#goblin_scenarios <- c("Sc1a","Sc2c","Sc3e")
#rcmip_loo <- cbwg_to_loo_global_emissions(global_scenarios,tim_scenarios,goblin_scenarios)
#g <- gen_fair_loo(rcmip_loo,"300mt-led","Sc3e")
#g$run()
#gsat <- get_warming(g,'ssp126')
#f$run()
#gsat_global
#w1 <- get_warming(g,'ssp126')
#%>% ggplot(aes(year,value,col)) + geom_line()

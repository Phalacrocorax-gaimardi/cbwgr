#' rcmip_fair
#'
#' global emissions dataset used by default in FaiR and other SCMs.
#'
#' Parameters of the 3 timescale temperature-forcing response (Greens) functions used to create the FaIR ensemble. These are
#' found from calibration to CMIP6 Earth System Models as described in Cummins et al (2020).
#'
#' @format
#' A data frame with 66 rows and 13 columns:
#' \describe{
#'   \item{scenario}{global scenario}
#'   \item{year}{year (mid decimal year or timepoint)}
#'   \item{specie}{forcing agent}
#'   \item{value}{annual emissions quanity}
#'   \item{unit}{units}
#' }
#' @source <doi:10.5281/zenodo.4589756/rcmip-emissions-annual-means-v5-1-0.csv>
"rcmip_fair"

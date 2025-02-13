#' Configuration parameters for the FaIR Ensemble
#'
#' Parameters of the 3 timescale temperature-forcing response (Greens) functions used to create the FaIR ensemble. These are
#' found from calibration to CMIP6 Earth System Models as described in Cummins et al (2020).
#'
#' @format
#' A data frame with 66 rows and 13 columns:
#' \describe{
#'   \item{model}{ESM model name}
#'   \item{run}{run label}
#'   \item{gamma}{Auto correlation parameter for stochastic runs}
#'   \item{C1}{Heat capacity of Box 1 W y m-2 K-1}
#'   \item{C2}{Heat capacity of Box 2 W y m-2 K-1}
#'   \item{C3}{Heat capacity of Box 3 W y m-2 K-1}
#'   \item{kappa1}{Heat transfer coefficient of Box 1 W m-2 K-1}
#'   \item{kappa2}{Heat transfer coefficient of Box 2 W m-2 K-1}
#'   \item{kappa3}{Heat transfer coefficient of Box 3 W m-2 K-1}
#'   \item{epsilon}{Deep ocean heat uptake efficiency factor}
#'   \item{sigma_eta}{standard deviation of TOA forcing component W m-2}
#'   \item{sigma_xi}{standard deviation of disturbance applied to surface box W m-2}
#'   \item{F_4xCO2}{Equilibrium temperature after quadrupling CO2 K}
#' }
#' @source <https://github.com/OMS-NetZero/FAIR/blob/master/tests/test_data/4xCO2_cummins_ebm3.csv>
"fair_configs_cmip6"

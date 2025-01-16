#' Irish climate forcing emissions 1745-2100
#'
#' Historical estimates and future scenario data for 23 climate forcing gases relevant to Ireland. Historical data for 1990-2022 are form the National Inventory Report.
#' Data prior to 1990 are etsimates from a range of sources. Future scenario data for CO2, N2O and CH4 are from third iteration CBWG (105 scenarions)
#' Scenario labels prior to 2022 indicate the source of the data.
#'
#'
#' @format
#' A data frame with 15,490 rows and 5 columns:
#' \describe{
#'   \item{scenario}{data source prior to 2022, cbwg scenario or other data source after 2022}
#'   \item{year}{year}
#'   \item{value}{annual emissions amount}
#'   \item{gas}{forcing agent}
#'   \item{units}{units}
#' }
#' @source <https://zenodo.org/records/10904361, CBWG, EPA,.....>
"emissions_cbwg_3"

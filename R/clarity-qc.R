#' List secchi depth measurements that are greater than the recorded lake depth
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, IsLakeDry, SurfaceCalm, OnBottom, DepthToBottom_m, SecchiDepth_m, VisitType, DPL.
#' @export
#'
#' @examples
#' \dontrun{
#'     qcSecchiGTDepth()
#'     qcSecchiGTDepth(site = "GRBA_L_BAKR0", field.season = "2019")
#' }
qcSecchiGTDepth <- function(park, site, field.season) {

  error.list <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Clarity")

  error.list <- error.list |>
    dplyr::filter(SecchiDepth_m > DepthToBottom_m)

  return(error.list)
}

#' List clarity records where lake is dry but clarity measurements exist
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcLakeDryMeasurementsExist()
#'     qcLakeDryMeasurementsExist(site = "GRBA_L_BAKR0", field.season = "2019")
#' }
qcLakeDryMeasurementsExist <- function(park, site, field.season) {

  error.list <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Clarity")

  error.list <- error.list |>
    dplyr::filter(IsLakeDry == TRUE) |>
    dplyr::filter(!is.na(SurfaceCalm) | !is.na(OnBottom) |  !is.na(DepthToBottom_m) | !is.na(SecchiDepth_m))

  return(error.list)
}

#' List clarity records where lake is not dry but measurements are missing
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, IsLakeDry, SurfaceCalm, OnBottom, DepthToBottom_m, SecchiDepth_m, VisitType, DPL.
#' @export
#'
#' @examples
#' \dontrun{
#'     qcLakeNotDryMeasurementsMissing()
#'     qcLakeNotDryMeasurementsMissing(site = "GRBA_L_BAKR0", field.season = "2019")
#' }
qcLakeNotDryMeasurementsMissing <- function(park, site, field.season) {

  error.list <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Clarity")

  error.list <- error.list |>
    dplyr::filter(IsLakeDry == 0) |>
    dplyr::filter(is.na(SurfaceCalm) | is.na(OnBottom) |  is.na(DepthToBottom_m))

  return(error.list)
}

#' List clarity records where secchi disk is not on bottom but secchi depth measurement is missing
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcDepthMissing()
#'     qcDepthMissing(site = "GRBA_L_BAKR0", field.season = "2019")
#' }
qcSecchiDepthMissing <- function(park, site, field.season) {

  error.list <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Clarity")

  error.list <- error.list |>
    dplyr::filter(OnBottom == "N") |>
    dplyr::filter(is.na(SecchiDepth_m))

  return(error.list)
}

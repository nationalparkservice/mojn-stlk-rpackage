#' List occurrences of "TBD" unknown plant species in tree count data
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_BLUE0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the spring veg database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SpringCode, SpringName, VisitType, FieldSeason, StartDate, UnknownPlantCode, TransectNumber.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     TreeCtQcTBDSpecies(conn, park = "LAKE")
#'     TreeCtQcTBDSpecies(conn, spring = "LAKE_P_BLUE0", field.season = "2019")
#'     TreeCtQcTBDSpecies(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
qcSecchiGTDepth <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  error.list <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, "clarity")

  error.list %<>%
    dplyr::filter(SecchiDepth_m > DepthToBottom_m) %>%

  return(error.list)
}



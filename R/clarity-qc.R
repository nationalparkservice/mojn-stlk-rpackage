#' List secchi depth measurements that are greater than the recorded lake depth
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, IsLakeDry, SurfaceCalm, OnBottom, DepthToBottom_m, SecchiDepth_m, VisitType, DPL.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcSecchiGTDepth(conn)
#'     qcSecchiGTDepth(conn, site = "GRBA_L_BAKR0", field.season = "2019")
#'     qcSecchiGTDepth(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
qcSecchiGTDepth <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  error.list <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, "clarity")

  error.list %<>%
    dplyr::filter(SecchiDepth_m > DepthToBottom_m)

  return(error.list)
}

#' List clarity records where lake is dry but clarity measurements exist
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, IsLakeDry, SurfaceCalm, OnBottom, DepthToBottom_m, SecchiDepth_m, VisitType, DPL.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcLakeDryMeasurementsExist(conn)
#'     qcLakeDryMeasurementsExist(conn, site = "GRBA_L_BAKR0", field.season = "2019")
#'     qcLakeDryMeasurementsExist(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
qcLakeDryMeasurementsExist <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  error.list <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, "clarity")

  error.list %<>%
    dplyr::filter(IsLakeDry == 1) %>%
    dplyr::filter(!is.na(SurfaceCalm) | !is.na(OnBottom) |  !is.na(DepthToBottom_m) | !is.na(SecchiDepth_m))

  return(error.list)
}

#' List clarity records where lake is not dry but measurements are missing
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, IsLakeDry, SurfaceCalm, OnBottom, DepthToBottom_m, SecchiDepth_m, VisitType, DPL.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcLakeNotDryMeasurementsMissing(conn)
#'     qcLakeNotDryMeasurementsMissing(conn, site = "GRBA_L_BAKR0", field.season = "2019")
#'     qcLakeNotDryMeasurementsMissing(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
qcLakeNotDryMeasurementsMissing <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  error.list <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, "clarity")

  error.list %<>%
    dplyr::filter(IsLakeDry == 0) %>%
    dplyr::filter(is.na(SurfaceCalm) | is.na(OnBottom) |  is.na(DepthToBottom_m))

  return(error.list)
}

#' List clarity records where secchi disk is not on bottom but secchi depth is missing
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, IsLakeDry, SurfaceCalm, OnBottom, DepthToBottom_m, SecchiDepth_m, VisitType, DPL.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcDepthMissing(conn)
#'     qcDepthMissing(conn, site = "GRBA_L_BAKR0", field.season = "2019")
#'     qcDepthMissing(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
qcSecchiDepthMissing <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  error.list <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, "clarity")

  error.list %<>%
    dplyr::filter(OnBottom == "N") %>%
    dplyr::filter(is.na(SecchiDepth_m))

  return(error.list)
}


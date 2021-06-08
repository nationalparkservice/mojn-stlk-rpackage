#' Calculates mean elevations for each survey point type in a survey
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason,VisitType, DPL.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     SurveyPointElevation(conn)
#'     SurveyPointElevation(conn, site = "GRBA_L_BAKR0", field.season = "2019")
#'     SurveyPointElevation(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
SurveyPointElevation <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    levels <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, "LakeLevelSurvey")
  # levels %<>%


    return(levels)
}

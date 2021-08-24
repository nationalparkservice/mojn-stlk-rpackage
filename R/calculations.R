#' "Maximum" data quality flag
#'
#' @description Helper function for summarizing data with data quality flags. Given a vector of data quality flag codes, returns the "maximum," i.e. the most severe flag.
#'
#' @param flags A character vector of data quality flag codes (C, W, I, NF)
#'
#' @return The most severe code present
#'
MaxDQF <- function(flags) {

  max_dqf <- NA

  if ("C" %in% flags) {
    max_dqf <- "C"
  } else if ("W" %in% flags) {
    max_dqf <- "W"
  } else if ("I" %in% flags) {
    max_dqf <- "I"
  } else if ("NF" %in% flags) {
    max_dqf <- "NF"
  }

  return(max_dqf)
}

#' Calculate median values for each water quality parameter for each lake visit.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for park, field season, site code, visit date, and the median values, flags, and counts for temperature, specific conductance, pH, and dissolved oxygen.
#' @export
#'
LakeWqMedian <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  temp <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualityTemperature")
  spcond <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualitySpCond")
  ph <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualitypH")
  do <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WaterQualityDO")

  wq.visits <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit")

  temp.med <- temp %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, Park, FieldSeason, SiteCode, VisitDate), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, MeasurementDepth_m, Flag, FlagNote, DPL) %>%
    dplyr::summarise(TemperatureMedian_C = median(WaterTemperature_C, na.rm = TRUE),
                     TemperatureCount = sum(!is.na(WaterTemperature_C))) %>%
    dplyr::rename(TemperatureFlag = Flag) %>%
    dplyr::arrange(SiteCode, VisitDate)

  spcond.med <- spcond %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, Park, FieldSeason, SiteCode, VisitDate), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, MeasurementDepth_m, Flag, FlagNote, DPL) %>%
    dplyr::summarise(SpCondMedian_microS_per_cm = median(SpecificConductance_microS_per_cm, na.rm = TRUE),
                     SpCondCount = sum(!is.na(SpecificConductance_microS_per_cm))) %>%
    dplyr::rename(SpCondFlag = Flag) %>%
    dplyr::arrange(SiteCode, VisitDate)

  ph.med <- ph %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, Park, FieldSeason, SiteCode, VisitDate), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, MeasurementDepth_m, Flag, FlagNote, DPL) %>%
    dplyr::summarise(pHMedian = median(pH, na.rm = TRUE),
                     pHCount = sum(!is.na(pH))) %>%
    dplyr::rename(pHFlag = Flag) %>%
    dplyr::arrange(SiteCode, VisitDate)

  do.med <- do %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, Park, FieldSeason, SiteCode, VisitDate), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, MeasurementDepth_m, Flag, FlagNote, DPL) %>%
    dplyr::summarise(DOMedian_percent = median(DissolvedOxygen_percent), DOMedian_mg_per_L = median(DissolvedOxygen_mg_per_L),
                     DOPercentCount = sum(!is.na(DissolvedOxygen_percent)),
                     DOmgLCount = sum(!is.na(DissolvedOxygen_mg_per_L))) %>%
    dplyr::rename(DOFlag = Flag) %>%
    dplyr::arrange(SiteCode, VisitDate)

  wq.med <- temp.med %>%
    dplyr::full_join(spcond.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame", "MeasurementDepth_m", "FlagNote", "DPL")) %>%
    dplyr::full_join(ph.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame", "MeasurementDepth_m", "FlagNote", "DPL")) %>%
    dplyr::full_join(do.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame", "MeasurementDepth_m", "FlagNote", "DPL")) %>%
    dplyr::ungroup()

  return(wq.med)
}

#' Calculate median values for each water quality parameter for each stream visit.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for park, field season, site code, visit date, and the median values, flags, and counts for temperature, specific conductance, pH, and dissolved oxygen.
#' @export
#'
StreamWqMedian <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  stream_wq <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "WQStreamXSection")
  wq.visits <- ReadAndFilterData(conn = conn, path.to.data = path.to.data, park = park, site = site, field.season = field.season, data.source = data.source, data.name = "Visit")

  stream_wq_med <- stream_wq %>%
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, Park, FieldSeason, SiteCode, VisitDate, MonitoringStatus), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) %>%
    dplyr::filter(MonitoringStatus == "Sampled") %>%
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, pHFlag, DOFlag, SpCondFlag, TemperatureFlag, FlagNote, DPL) %>%
    dplyr::summarise(TemperatureMedian_C = median(WaterTemperature_C),
                     TemperatureCount = sum(!is.na(WaterTemperature_C)),
                     pHMedian = median(pH),
                     pHCount = sum(!is.na(pH)),
                     DOMedian_mg_per_L = median(DissolvedOxygen_mg_per_L),
                     DOmgLCount = sum(!is.na(DissolvedOxygen_mg_per_L)),
                     SpCondMedian_microS_per_cm = median(SpecificConductance_microS_per_cm),
                     SpCondCount = sum(!is.na(SpecificConductance_microS_per_cm))) %>%
    dplyr::arrange(SiteCode, VisitDate) %>%
    dplyr::ungroup()


  return(stream_wq_med)
}

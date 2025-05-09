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
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
LakeWqMedian <- function(park, site, field.season) {
  temp <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualityTemperature")
  spcond <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualitySpCond")
  ph <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualitypH")
  do <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualityDO")

  wq.visits <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit")

  temp.med <- temp |>
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, Park, FieldSeason, SiteCode, VisitDate), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) |>
    # dplyr::filter(MonitoringStatus == "Sampled") |>
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, MeasurementDepth_m, Flag, FlagNote, DPL) |>
    dplyr::summarise(TemperatureMedian_C = median(WaterTemperature_C, na.rm = TRUE),
                     TemperatureCount = sum(!is.na(WaterTemperature_C))) |>
    dplyr::rename(TemperatureFlag = Flag) |>
    dplyr::arrange(SiteCode, VisitDate)

  spcond.med <- spcond |>
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, Park, FieldSeason, SiteCode, VisitDate), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) |>
    # dplyr::filter(MonitoringStatus == "Sampled") |>
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, MeasurementDepth_m, Flag, FlagNote, DPL) |>
    dplyr::summarise(SpCondMedian_microS_per_cm = median(SpecificConductance_microS_per_cm, na.rm = TRUE),
                     SpCondCount = sum(!is.na(SpecificConductance_microS_per_cm))) |>
    dplyr::rename(SpCondFlag = Flag) |>
    dplyr::arrange(SiteCode, VisitDate)

  ph.med <- ph |>
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, Park, FieldSeason, SiteCode, VisitDate), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) |>
    # dplyr::filter(MonitoringStatus == "Sampled") |>
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, MeasurementDepth_m, Flag, FlagNote, DPL) |>
    dplyr::summarise(pHMedian = median(pH, na.rm = TRUE),
                     pHCount = sum(!is.na(pH))) |>
    dplyr::rename(pHFlag = Flag) |>
    dplyr::arrange(SiteCode, VisitDate)

  do.med <- do |>
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, Park, FieldSeason, SiteCode, VisitDate), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) |>
    # dplyr::filter(MonitoringStatus == "Sampled") |>
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, MeasurementDepth_m, Flag, FlagNote, DPL) |>
    dplyr::summarise(DOMedian_percent = median(DissolvedOxygen_percent), DOMedian_mg_per_L = median(DissolvedOxygen_mg_per_L),
                     DOPercentCount = sum(!is.na(DissolvedOxygen_percent)),
                     DOmgLCount = sum(!is.na(DissolvedOxygen_mg_per_L))) |>
    dplyr::rename(DOFlag = Flag) |>
    dplyr::arrange(SiteCode, VisitDate)

  wq.med <- temp.med |>
    dplyr::full_join(spcond.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame", "MeasurementDepth_m", "FlagNote", "DPL")) |>
    dplyr::full_join(ph.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame", "MeasurementDepth_m", "FlagNote", "DPL")) |>
    dplyr::full_join(do.med, by = c("Park", "FieldSeason", "SiteCode", "VisitDate", "VisitType", "SampleFrame", "MeasurementDepth_m", "FlagNote", "DPL")) |>
    dplyr::ungroup()

  return(wq.med)
}

#' Calculate median values for each water quality parameter for each stream visit.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
StreamWqMedian <- function(park, site, field.season) {
  stream_wq <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WQStreamXSection")
  wq.visits <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit")

  stream_wq_med <- stream_wq |>
    dplyr::left_join(dplyr::select(wq.visits, SampleFrame, Park, FieldSeason, SiteCode, VisitDate, MonitoringStatus), by = c("Park", "FieldSeason", "SiteCode", "VisitDate")) |>
    # dplyr::filter(MonitoringStatus == "Sampled") |>
    dplyr::group_by(Park, FieldSeason, SiteCode, VisitDate, VisitType, SampleFrame, pHFlag, DOFlag, SpCondFlag, TemperatureFlag, FlagNote, DPL) |>
    dplyr::summarise(TemperatureMedian_C = median(WaterTemperature_C),
                     TemperatureCount = sum(!is.na(WaterTemperature_C)),
                     pHMedian = median(pH),
                     pHCount = sum(!is.na(pH)),
                     DOMedian_mg_per_L = median(DissolvedOxygen_mg_per_L),
                     DOmgLCount = sum(!is.na(DissolvedOxygen_mg_per_L)),
                     SpCondMedian_microS_per_cm = median(SpecificConductance_microS_per_cm),
                     SpCondCount = sum(!is.na(SpecificConductance_microS_per_cm))) |>
    dplyr::arrange(SiteCode, VisitDate) |>
    dplyr::ungroup()

  return(stream_wq_med)
}

#' Lake water quality sanity check
#' @description Perform sanity check and compile list of potentially incorrect or outlier water quality values.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for Park, FieldSeason, SiteCode, VisitDate, MeasurementDepth_m, Parameter, Units, Median, Flag, and FlagNote.
#' @export
#'
LakeQcWqSanity <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  lake.sanity <- QcWqSanity(conn, path.to.data, park, site, field.season, data.source, "lake")
  return(lake.sanity)
}

#' Stream water quality sanity check
#' @description Perform sanity check and compile list of potentially incorrect or outlier water quality values.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for Park, FieldSeason, SiteCode, VisitDate, Parameter, Units, Median, Flag, and FlagNote.
#' @export
#'
StreamQcWqSanity <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  stream.sanity <- QcWqSanity(conn, path.to.data, park, site, field.season, data.source, "stream")
  return(stream.sanity)
}


#' Water quality sanity check
#' @description Perform sanity check and compile list of potentially incorrect or outlier water quality values. This function is not exported; instead it is called by StreamQcWqSanity and LakeQcWqSanity
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for Park, FieldSeason, SiteCode, VisitDate, MeasurementDepth_m (lake only), Parameter, Units, Median, Flag, and FlagNote.
#'
QcWqSanity <- function(conn, path.to.data, park, site, field.season, data.source = "database", wq.type) {
  if (wq.type == "stream") {
    wq.sanity.predata <- StreamWqMedian(conn, path.to.data, park, site, field.season, data.source)
  } else if (wq.type == "lake") {
    wq.sanity.predata <- LakeWqMedian(conn, path.to.data, park, site, field.season, data.source)
  } else {
    stop("Invalid wq.type")
  }

  temp.sanity <- wq.sanity.predata %>%
    dplyr::filter(TemperatureMedian_C > 20) %>%
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SiteType", "VisitDate", "VisitType", "TemperatureMedian_C", "TemperatureFlag", "FlagNote")), any_of("MeasurementDepth_m")) %>%
    tibble::add_column(Parameter = "Temperature", Units = "C", .after = "VisitType") %>%
    dplyr::rename(Median = TemperatureMedian_C, Flag = TemperatureFlag)

  spcond.sanity <- wq.sanity.predata %>%
    dplyr::filter(SpCondMedian_microS_per_cm > 1000) %>%
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SiteType", "VisitDate", "VisitType", "SpCondMedian_microS_per_cm", "SpCondFlag", "FlagNote")), any_of("MeasurementDepth_m")) %>%
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "VisitType") %>%
    dplyr::rename(Median = SpCondMedian_microS_per_cm, Flag = SpCondFlag)

  ph.sanity <- wq.sanity.predata %>%
    dplyr::filter(pHMedian > 10 | pHMedian < 6) %>%
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SiteType", "VisitDate", "VisitType", "pHMedian", "pHFlag", "FlagNote")), any_of("MeasurementDepth_m")) %>%
    tibble::add_column(Parameter = "pH", Units = "units", .after = "VisitType") %>%
    dplyr::rename(Median = pHMedian, Flag = pHFlag)

  do.mgl.sanity <- wq.sanity.predata %>%
    dplyr::filter(DOMedian_mg_per_L > 12) %>%
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SiteType", "VisitDate", "VisitType", "DOMedian_mg_per_L", "DOFlag", "FlagNote")), any_of("MeasurementDepth_m")) %>%
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "VisitType") %>%
    dplyr::rename(Median = DOMedian_mg_per_L, Flag = DOFlag)

  if (wq.type == "lake") {
    do.percent.sanity <- wq.sanity.predata %>%
      dplyr::filter(DOMedian_percent > 110 | DOMedian_percent < 2) %>%
      dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SiteType", "VisitDate", "VisitType", "DOMedian_percent", "DOFlag", "FlagNote")), any_of("MeasurementDepth_m")) %>%
      tibble::add_column(Parameter = "DO", Units = "%", .after = "VisitType") %>%
      dplyr::rename(Median = DOMedian_percent, Flag = DOFlag)
    wq.sanity <- rbind(temp.sanity, spcond.sanity, ph.sanity, do.percent.sanity, do.mgl.sanity)
  } else {
    wq.sanity <- rbind(temp.sanity, spcond.sanity, ph.sanity, do.mgl.sanity)
  }

  return(wq.sanity)
}

#' Compile list of lake water quality values that have data quality flags.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for Park, FieldSeason, SiteCode, VisitDate, MeasurementDepth_m (lake only), Parameter, Units, Median, Flag, and FlagNote.
#' @export
#'
LakeQcWqFlags <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  lake.flags <- QcWqFlags(conn, path.to.data, park, site, field.season, data.source, wq.type = "lake")
  return(lake.flags)
}

#' Compile list of stream water quality values that have data quality flags.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for Park, FieldSeason, SiteCode, VisitDate, MeasurementDepth_m (lake only), Parameter, Units, Median, Flag, and FlagNote.
#' @export
#'
StreamQcWqFlags <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  stream.flags <- QcWqFlags(conn, path.to.data, park, site, field.season, data.source, wq.type = "stream")
  return(stream.flags)
}

#' Compile list of water quality values that have data quality flags.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "MOJA".
#' @param site Optional. Site code to filter on, e.g. "LAKE_P_HOR0042".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns for Park, FieldSeason, SiteCode, VisitDate, MeasurementDepth_m (lake only), Parameter, Units, Median, Flag, and FlagNote.
#'
QcWqFlags <- function(conn, path.to.data, park, site, field.season, data.source = "database", wq.type) {
  if (wq.type == "stream") {
    wq.flags.predata <- StreamWqMedian(conn, path.to.data, park, site, field.season, data.source)
  } else if (wq.type == "lake") {
    wq.flags.predata <- LakeWqMedian(conn, path.to.data, park, site, field.season, data.source)
  } else {
    stop("Invalid wq.type")
  }

  temp.flags <- wq.flags.predata %>%
    dplyr::filter(TemperatureFlag %in% c("I", "W", "C")) %>%
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SiteType", "VisitDate", "VisitType", "TemperatureMedian_C", "TemperatureFlag", "FlagNote")), any_of("MeasurementDepth_m")) %>%
    tibble::add_column(Parameter = "Temperature", Units = "C", .after = "VisitType") %>%
    dplyr::rename(Median = TemperatureMedian_C, Flag = TemperatureFlag)

  spcond.flags <- wq.flags.predata %>%
    dplyr::filter(SpCondFlag %in% c("I", "W", "C")) %>%
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SiteType", "VisitDate", "VisitType", "SpCondMedian_microS_per_cm", "SpCondFlag", "FlagNote")), any_of("MeasurementDepth_m")) %>%
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "VisitType") %>%
    dplyr::rename(Median = SpCondMedian_microS_per_cm, Flag = SpCondFlag)

  ph.flags <- wq.flags.predata %>%
    dplyr::filter(pHFlag %in% c("I", "W", "C")) %>%
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SiteType", "VisitDate", "VisitType", "pHMedian", "pHFlag", "FlagNote")), any_of("MeasurementDepth_m")) %>%
    tibble::add_column(Parameter = "pH", Units = "units", .after = "VisitType") %>%
    dplyr::rename(Median = pHMedian, Flag = pHFlag)

  do.mgl.flags <- wq.flags.predata %>%
    dplyr::filter(DOFlag %in% c("I", "W", "C")) %>%
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SiteType", "VisitDate", "VisitType", "DOMedian_mg_per_L", "DOFlag", "FlagNote")), any_of("MeasurementDepth_m")) %>%
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "VisitType") %>%
    dplyr::rename(Median = DOMedian_mg_per_L, Flag = DOFlag)

  if (wq.type == "lake") {
    do.percent.flags <- wq.flags.predata %>%
      dplyr::filter(DOFlag %in% c("I", "W", "C")) %>%
      dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SiteType", "VisitDate", "VisitType", "DOMedian_percent", "DOFlag", "FlagNote")), any_of("MeasurementDepth_m")) %>%
      tibble::add_column(Parameter = "DO", Units = "%", .after = "VisitType") %>%
      dplyr::rename(Median = DOMedian_percent, Flag = DOFlag)
    wq.flags <- rbind(temp.flags, spcond.flags, ph.flags, do.percent.flags, do.mgl.flags)
  } else {
    wq.flags <- rbind(temp.flags, spcond.flags, ph.flags, do.mgl.flags)
  }

  return(wq.flags)
}
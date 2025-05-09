#' Lake water quality sanity check
#' @description Perform sanity check and compile list of potentially incorrect or outlier water quality values.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
qcLakeWqSanity <- function(park, site, field.season) {
  lake.sanity <- qcWqSanity(park = park, site = site, field.season = field.season, wq.type = "lake")
  return(lake.sanity)
}

#' Stream water quality sanity check
#' @description Perform sanity check and compile list of potentially incorrect or outlier water quality values.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
qcStreamWqSanity <- function(park, site, field.season) {
  stream.sanity <- qcWqSanity(park = park, site = site, field.season = field.season, wq.type = "stream")
  return(stream.sanity)
}


#' Water quality sanity check
#' @description Perform sanity check and compile list of potentially incorrect or outlier water quality values. This function is not exported; instead it is called by StreamQcWqSanity and LakeQcWqSanity
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param wq.type Either "stream" or "lake". Indicates whether to use stream or lake water quality data.
#'
#' @return A tibble
#'
qcWqSanity <- function(park, site, field.season, wq.type) {
  if (wq.type == "stream") {
    wq.sanity.predata <- StreamWqMedian(park = park, site = site, field.season = field.season)
  } else if (wq.type == "lake") {
    wq.sanity.predata <- LakeWqMedian(park = park, site = site, field.season = field.season)
  } else {
    stop("Invalid wq.type")
  }

  temp.sanity <- wq.sanity.predata |>
    dplyr::filter(TemperatureMedian_C > 20) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "TemperatureMedian_C", "TemperatureFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "Temperature", Units = "C", .after = "VisitType") |>
    dplyr::rename(Median = TemperatureMedian_C, Flag = TemperatureFlag)

  spcond.sanity <- wq.sanity.predata |>
    dplyr::filter(SpCondMedian_microS_per_cm > 1000) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "SpCondMedian_microS_per_cm", "SpCondFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "VisitType") |>
    dplyr::rename(Median = SpCondMedian_microS_per_cm, Flag = SpCondFlag)

  ph.sanity <- wq.sanity.predata |>
    dplyr::filter(pHMedian > 10 | pHMedian < 6) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "pHMedian", "pHFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "pH", Units = "units", .after = "VisitType") |>
    dplyr::rename(Median = pHMedian, Flag = pHFlag)

  do.mgl.sanity <- wq.sanity.predata |>
    dplyr::filter(DOMedian_mg_per_L > 12) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "DOMedian_mg_per_L", "DOFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "VisitType") |>
    dplyr::rename(Median = DOMedian_mg_per_L, Flag = DOFlag)

  if (wq.type == "lake") {
    do.percent.sanity <- wq.sanity.predata |>
      dplyr::filter(DOMedian_percent > 110 | DOMedian_percent < 2) |>
      dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "DOMedian_percent", "DOFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
      tibble::add_column(Parameter = "DO", Units = "%", .after = "VisitType") |>
      dplyr::rename(Median = DOMedian_percent, Flag = DOFlag)
    wq.sanity <- rbind(temp.sanity, spcond.sanity, ph.sanity, do.percent.sanity, do.mgl.sanity)
  } else {
    wq.sanity <- rbind(temp.sanity, spcond.sanity, ph.sanity, do.mgl.sanity)
  }

  return(wq.sanity)
}

#' Compile list of lake water quality values that have data quality flags.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
qcLakeWqFlags <- function(park, site, field.season) {
  lake.flags <- qcWqFlags(park = park, site = site, field.season = field.season, wq.type = "lake")

  lake.flags <- lake.flags |>
    dplyr::filter(!is.na(Median))

  return(lake.flags)
}

#' Compile list of stream water quality values that have data quality flags.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
qcStreamWqFlags <- function(park, site, field.season) {
  stream.flags <- qcWqFlags(park = park, site = site, field.season = field.season, wq.type = "stream")

  stream.flags <- stream.flags |>
    dplyr::filter(!is.na(Median))

  return(stream.flags)
}

#' Compile list of water quality values that have data quality flags.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param wq.type Either "stream" or "lake". Indicates whether to use stream or lake water quality data.
#'
#' @return A tibble
#'
qcWqFlags <- function(park, site, field.season, wq.type) {
  if (wq.type == "stream") {
    wq.flags.predata <- StreamWqMedian(park = park, site = site, field.season = field.season)
  } else if (wq.type == "lake") {
    wq.flags.predata <- LakeWqMedian(park = park, site = site, field.season = field.season)
  } else {
    stop("Invalid wq.type")
  }

  temp.flags <- wq.flags.predata |>
    dplyr::filter(TemperatureFlag %in% c("I", "W", "C")) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "TemperatureMedian_C", "TemperatureFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "Temperature", Units = "C", .after = "VisitType") |>
    dplyr::rename(Median = TemperatureMedian_C, Flag = TemperatureFlag)

  spcond.flags <- wq.flags.predata |>
    dplyr::filter(SpCondFlag %in% c("I", "W", "C")) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "SpCondMedian_microS_per_cm", "SpCondFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "VisitType") |>
    dplyr::rename(Median = SpCondMedian_microS_per_cm, Flag = SpCondFlag)

  ph.flags <- wq.flags.predata |>
    dplyr::filter(pHFlag %in% c("I", "W", "C")) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "pHMedian", "pHFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "pH", Units = "units", .after = "VisitType") |>
    dplyr::rename(Median = pHMedian, Flag = pHFlag)

  do.mgl.flags <- wq.flags.predata |>
    dplyr::filter(DOFlag %in% c("I", "W", "C")) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "DOMedian_mg_per_L", "DOFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "VisitType") |>
    dplyr::rename(Median = DOMedian_mg_per_L, Flag = DOFlag)

  if (wq.type == "lake") {
    do.percent.flags <- wq.flags.predata |>
      dplyr::filter(DOFlag %in% c("I", "W", "C")) |>
      dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "DOMedian_percent", "DOFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
      tibble::add_column(Parameter = "DO", Units = "%", .after = "VisitType") |>
      dplyr::rename(Median = DOMedian_percent, Flag = DOFlag)
    wq.flags <- rbind(temp.flags, spcond.flags, ph.flags, do.percent.flags, do.mgl.flags)
  } else {
    wq.flags <- rbind(temp.flags, spcond.flags, ph.flags, do.mgl.flags)
  }

  return(wq.flags)
}

#' Intermediate step used to clean lake water quality data for stats and plotting functions.
#' @description Limit data to primary visits and exclude data with "W" and "C" flags. Omit DO <110% or <12 mg/L.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
qcLakeWqCleaned <- function(park, site, field.season) {
  lake_cleaned <- qcWqCleaned(park = park, site = site, field.season = field.season, wq.type = "lake")
  return(lake_cleaned)
}

#' Intermediate step used to clean stream water quality data for stats and plotting functions.
#' @description Limit data to primary visits and exclude data with "W" and "C" flags. Omit DO <110% or <12 mg/L.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
qcStreamWqCleaned <- function(park, site, field.season) {
  stream_cleaned <- qcWqCleaned(park = park, site = site, field.season = field.season, wq.type = "stream")
  return(stream_cleaned)
}

#' Intermediate step used to clean water quality data for stats and plotting functions. Limit data to primary visits, and exclude data with "W" and "C" flags.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param wq.type Either "stream" or "lake". Indicates whether to use stream or lake water quality data.
#'
#' @return A tibble
#' @export
#'
qcWqCleaned <- function(park, site, field.season, wq.type) {
  if (wq.type == "stream") {
    wq.sanity.predata <- StreamWqMedian(park = park, site = site, field.season = field.season)
  } else if (wq.type == "lake") {
    wq.sanity.predata <- LakeWqMedian(park = park, site = site, field.season = field.season)
  } else {
    stop("Invalid wq.type")
  }

  temp.sanity <- wq.sanity.predata |>
    dplyr::filter(VisitType == "Primary", !(TemperatureFlag %in% c("W", "C"))) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "TemperatureMedian_C", "TemperatureFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "Temperature", Units = "C", .after = "VisitType") |>
    dplyr::rename(Median = TemperatureMedian_C, Flag = TemperatureFlag)

  spcond.sanity <- wq.sanity.predata |>
    dplyr::filter(VisitType == "Primary", !(SpCondFlag %in% c("W", "C"))) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "SpCondMedian_microS_per_cm", "SpCondFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "SpCond", Units = "uS/cm", .after = "VisitType") |>
    dplyr::rename(Median = SpCondMedian_microS_per_cm, Flag = SpCondFlag)

  ph.sanity <- wq.sanity.predata |>
    dplyr::filter(VisitType == "Primary", !(pHFlag %in% c("W", "C"))) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "pHMedian", "pHFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "pH", Units = "units", .after = "VisitType") |>
    dplyr::rename(Median = pHMedian, Flag = pHFlag)

  do.mgl.sanity <- wq.sanity.predata |>
    dplyr::filter(VisitType == "Primary", !(DOFlag %in% c("W", "C")), DOMedian_mg_per_L < 12 | is.na(DOMedian_mg_per_L)) |>
    dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "DOMedian_mg_per_L", "DOFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
    tibble::add_column(Parameter = "DO", Units = "mg/L", .after = "VisitType") |>
    dplyr::rename(Median = DOMedian_mg_per_L, Flag = DOFlag)

  if (wq.type == "lake") {
    do.percent.sanity <- wq.sanity.predata |>
      dplyr::filter(VisitType == "Primary", !(DOFlag %in% c("W", "C")), DOMedian_percent < 110 | is.na(DOMedian_percent)) |>
      dplyr::select(all_of(c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "DOMedian_percent", "DOFlag", "FlagNote")), any_of("MeasurementDepth_m")) |>
      tibble::add_column(Parameter = "DO", Units = "%", .after = "VisitType") |>
      dplyr::rename(Median = DOMedian_percent, Flag = DOFlag)
    wq.sanity <- rbind(temp.sanity, spcond.sanity, ph.sanity, do.percent.sanity, do.mgl.sanity)
  } else {
    wq.sanity <- rbind(temp.sanity, spcond.sanity, ph.sanity, do.mgl.sanity)
  }
}

#' Generate lake pH depth profile plots.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param include.title Include plot title? Defaults to true.
#' @param plotly Return an interactive plotly object instead of a ggplot object? Defaults to false.
#'
#' @return ggplot or plotly object
#' @export
#'
WqPlotPHDepthProfile <- function(park, site, field.season, include.title = TRUE, plotly = FALSE) {

  plot_ph <- WqPlotDepthProfile(param = "pH", park = park, site = site, field.season = field.season, include.title = include.title, plotly = plotly)

  return(plot_ph)
}

#' Generate lake DO depth profile plots.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param units Dissolved oxygen units. One of "mg/L" (default) or "%".
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param include.title Include plot title? Defaults to true.
#' @param plotly Return an interactive plotly object instead of a ggplot object? Defaults to false.
#' @param data.source Character string indicating whether to access data in the live database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return ggplot or plotly object
#' @export
#'
WqPlotDODepthProfile <- function(units = "mg/L", park, site, field.season, include.title = TRUE, plotly = FALSE) {

  plot_do <- WqPlotDepthProfile(param = "DO", units = units, park = park, site = site, field.season = field.season, include.title = include.title, plotly = plotly)

  return(plot_do)
}

#' Generate lake specific conductance depth profile plots.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param include.title Include plot title? Defaults to true.
#' @param plotly Return an interactive plotly object instead of a ggplot object? Defaults to false.
#'
#' @return ggplot or plotly object
#' @export
#'
WqPlotSpCondDepthProfile <- function(park, site, field.season, include.title = TRUE, plotly = FALSE) {

  plot_spcond <- WqPlotDepthProfile(param = "SpCond", park = park, site = site, field.season = field.season, include.title = include.title, plotly = plotly)

  return(plot_spcond)
}

#' Generate lake temperature depth profile plots.
#'
#' @inheritParams WqPlotDepthProfile
#'
#' @return Depth profile plot for lake water quality.
#' @export
#'
WqPlotTemperatureDepthProfile <- function(park, site, field.season, include.title = TRUE, plotly = FALSE) {

  plot_temp <- WqPlotDepthProfile(param = "Temperature", park = park, site = site, field.season = field.season, include.title = include.title, plotly = plotly)

  return(plot_temp)
}

#' Generate lake depth profile plots.
#'
#' @inheritParams ReadAndFilterData
#' @param include.title Include plot title? Defaults to true.
#' @param plotly Return an interactive plotly object instead of a ggplot object? Defaults to false.
#' @param param The water quality parameter to plot. One of "pH", "DO", "SpCond", or "Temperature".
#' @param units Units of dissolved oxygen. Either "mg/L" or "%". Ignored if `param != "DO"`.
#'
#' @return ggplot or plotly object
#'
WqPlotDepthProfile <- function(param, units, park, site, field.season, include.title = TRUE, plotly = FALSE) {

  wq <- qcLakeWqCleaned(park = park, site = site, field.season = field.season) |>
    dplyr::filter(tolower(Parameter) == tolower(param), !is.na(Median)) |>
    dplyr::rename(Depth_m = MeasurementDepth_m)

  # Filter on unit if looking at DO. If not DO, set units
  if (tolower(param) == "do") {
    if (missing(units) | !(units %in% c("mg/L", "%"))) {
      stop("Please specify correct units for DO. Must be mg/L or %.")
    }
    wq <- wq |>
      dplyr::filter(tolower(Units) == tolower(units))
  } else {
    units <- unique(wq$Units)
  }

  plot_wq <- FormatPlot(
    data = wq,
    x.col = FieldSeason,
    y.col = Depth_m,
    facet.col = SiteCode,
    plot.title = dplyr::if_else(include.title, paste(param, "Depth Profile"), ""),
    x.lab = "Field Season",
    y.lab = "Measurement Depth (m)",
    n.col.facet = 2,
    transform.y = "reverse"
  ) +
    ggplot2::aes(color = Median) +
    # ggplot2::aes(text = paste0("Field Season: ", FieldSeason, "<br>",
    #                            "Value: ", Median, "<br>",
    #                            "Measurement Depth (m): ", Depth_m, "<br>")) +
    ggplot2::labs(color = paste0("Median ", param, ifelse(tolower(param) == "ph", "", paste0(" (", units, ")")))) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) +
    ggplot2::geom_point(size = 3, pch = 19, stroke = 2, color = "black") +
    ggplot2::geom_point(size = 3, pch = 19) # color = "#3b3b3b", pch = 21

  if (tolower(param) == "ph") {
    plot_wq <- plot_wq +
      khroma::scale_color_vik(midpoint = 7.0, discrete = FALSE, reverse = TRUE)
  } else if (tolower(param) == "temperature") {
    plot_wq <- plot_wq +
      khroma::scale_color_sunset(discrete = FALSE, midpoint = mean(wq$Median))
      # khroma::scale_color_smoothrainbow(range = c(0.25, 0.8))
  } else if (tolower(param) == "spcond") {
    plot_wq <- plot_wq +
      # khroma::scale_color_lapaz(discrete = FALSE, reverse = FALSE)
      ggplot2::scale_color_distiller(palette = "YlGnBu", direction = -1)
  } else {
    plot_wq <- plot_wq +
      ggplot2::scale_color_distiller(palette = "PuBu", direction = 1)
      # khroma::scale_color_oslo(discrete = FALSE, reverse = TRUE, range = c(0, 0.8))
  }

  if (plotly) {
    plot_wq <- plotly::ggplotly(plot_wq
#                                , tooltip = "text"
                                )
  }

  return(plot_wq)
}

#' Generate stream water quality plots.
#'
#' @inheritParams ReadAndFilterData
#' @param include.title Include plot title? Defaults to true.
#' @param plotly Return an interactive plotly object instead of a ggplot object? Defaults to false.
#' @param param The water quality parameter to plot. One of "pH", "DO", "SpCond", or "Temperature".
#' @param units Units of dissolved oxygen. Either "mg/L" or "%". Ignored if `param != "DO"`.
#'
#' @return ggplot or plotly object
#'
WqPlotStream <- function(param, units, park, site, field.season, include.title = TRUE, plotly = FALSE) {
  wq <- qcStreamWqCleaned(park = park, site = site, field.season = field.season) |>
    dplyr::filter(tolower(Parameter) == tolower(param), !is.na(Median)) |>
    dplyr::filter(SiteCode != "GRBA_S_BAKR2")

  # Filter on unit if looking at DO. If not DO, set units
  if (tolower(param) == "do") {
    if (missing(units) | !(units %in% c("mg/L", "%"))) {
      stop("Please specify correct units for DO. Must be mg/L or %.")
    }
    wq <- wq |>
      dplyr::filter(tolower(Units) == tolower(units))
  } else {
    units <- unique(wq$Units)
  }

  plot_wq <- FormatPlot(
    data = wq,
    x.col = FieldSeason,
    y.col = Median,
    facet.col = SiteCode,
    plot.title = dplyr::if_else(include.title, paste("Stream Water Quality:", param), ""),
    x.lab = "Field Season",
    y.lab = paste(param),
    n.col.facet = 3
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line(group = 1,
                       linewidth = 1) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

  if (plotly) {
    plot_wq <- plotly::ggplotly(plot_wq
                              # , tooltip = "text"
    )
  }

  return(plot_wq)
}

#' Generate stream water temperature plots.
#'
#' @inheritParams ReadAndFilterData
#' @param include.title Include plot title? Defaults to true.
#' @param plotly Return an interactive plotly object instead of a ggplot object? Defaults to false.
#' @param param The water quality parameter to plot. One of "pH", "DO", "SpCond", or "Temperature".
#' @param units Units of dissolved oxygen. Either "mg/L" or "%". Ignored if `param != "DO"`.
#'
#' @return ggplot or plotly object
#'
WqPlotStreamTemperature <- function(park, site, field.season, include.title = TRUE, plotly = FALSE) {
    plot_temp <- WqPlotStream(param = "Temperature", park = park, site = site, field.season = field.season, include.title = include.title, plotly = plotly) +
      ggplot2::ylab("Water Temperature (C)")

  return(plot_temp)
}

#' Generate stream pH plots.
#'
#' @inheritParams ReadAndFilterData
#' @param include.title Include plot title? Defaults to true.
#' @param plotly Return an interactive plotly object instead of a ggplot object? Defaults to false.
#' @param param The water quality parameter to plot. One of "pH", "DO", "SpCond", or "Temperature".
#' @param units Units of dissolved oxygen. Either "mg/L" or "%". Ignored if `param != "DO"`.
#'
#' @return ggplot or plotly object
#'
WqPlotStreamPH <- function(park, site, field.season, include.title = TRUE, plotly = FALSE) {
  plot_temp <- WqPlotStream(param = "pH", park = park, site = site, field.season = field.season, include.title = include.title, plotly = plotly) +
    ggplot2::ylab("pH")

  return(plot_temp)
}

#' Generate stream specific conductance plots.
#'
#' @inheritParams ReadAndFilterData
#' @param include.title Include plot title? Defaults to true.
#' @param plotly Return an interactive plotly object instead of a ggplot object? Defaults to false.
#' @param param The water quality parameter to plot. One of "pH", "DO", "SpCond", or "Temperature".
#' @param units Units of dissolved oxygen. Either "mg/L" or "%". Ignored if `param != "DO"`.
#'
#' @return ggplot or plotly object
#'
WqPlotStreamSpCond <- function(park, site, field.season, include.title = TRUE, plotly = FALSE) {
  plot_temp <- WqPlotStream(param = "SpCond", park = park, site = site, field.season = field.season, include.title = include.title, plotly = plotly) +
    ggplot2::ylab("Specific Conductance (mg/L)")

  return(plot_temp)
}

#' Generate stream dissolved oxygen plots.
#'
#' @inheritParams ReadAndFilterData
#' @param include.title Include plot title? Defaults to true.
#' @param plotly Return an interactive plotly object instead of a ggplot object? Defaults to false.
#' @param param The water quality parameter to plot. One of "pH", "DO", "SpCond", or "Temperature".
#' @param units Units of dissolved oxygen. Either "mg/L" or "%". Ignored if `param != "DO"`.
#'
#' @return ggplot or plotly object
#'
WqPlotStreamDO <- function(park, site, field.season, include.title = TRUE, plotly = FALSE) {
  plot_temp <- WqPlotStream(param = "DO", units = "mg/L", park = park, site = site, field.season = field.season, include.title = include.title, plotly = plotly) +
    ggplot2::ylab("Dissolved Oxygen (mg/L)")

  return(plot_temp)
}

#' Calculates mean elevations for each survey point type in a survey
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason,VisitType, DPL, SurveyPoint, Benchmark, FinalCorrectedElevation_ft.
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
    StandardTemperature_F <- 68  # Standard temperature to be used for temperature corrections

    # Parse out survey point names, types, and setup number. Consolidate rod temperature into one column
    levels %<>%
      tidyr::separate(SurveyPointType, into = c("SurveyPoint", "ReadingType", "SetupNumber"), sep = "-", remove = TRUE) %>%
      dplyr::mutate(SetupNumber = readr::parse_number(SetupNumber),
                    RodTemperature_F = ifelse(SetupNumber == 1, RodTemperatureSetup1_F,
                                              ifelse(SetupNumber == 2, RodTemperatureSetup2_F,
                                                     ifelse(SetupNumber == 3, RodTemperatureSetup3_F, NA))),
                    Benchmark = ifelse(SurveyPoint == "RM1", RM1,  # We may eventually want to create this Benchmark column in the SQL Server view instead of doing it here
                                       ifelse(SurveyPoint == "RM2", RM2,
                                              ifelse(SurveyPoint == "RM3", RM3,
                                                     ifelse(SurveyPoint == "RM4", RM4,
                                                            ifelse(SurveyPoint == "RM5", RM5,
                                                                   ifelse(SurveyPoint == "RM6", RM6,
                                                                          ifelse(SurveyPoint == "WS", "Water Surface", NA)))))))) %>%
      dplyr::mutate(TempCorrectedHeight_ft = Height_ft + (CTE * Height_ft * (RodTemperature_F - StandardTemperature_F)))

    setups <- unique(levels$SetupNumber) %>% sort()
    temp_corrected_lvls <- tibble::tibble()

    # Get known elevations and calculate instrument height. Does this need to be a loop? Probably not
    for (setup in setups) {
      #TODO: Need to verify that there is only one backsight per survey per setup. We are assuming that RM1_GivenElevation_m applies to the BS of the first setup
      bs <- dplyr::filter(levels, ReadingType  == "BS", SetupNumber == setup) %>%
        dplyr::select(SiteCode, VisitDate, FieldSeason, VisitType, SetupNumber, SurveyPoint, TempCorrectedHeight_ft, RM1_GivenElevation_m)

      # Get known elevation used to calculate instrument height
      if (setup == 1) {
        bs %<>% dplyr::mutate(InstrumentHeight_ft = TempCorrectedHeight_ft + measurements::conv_unit(RM1_GivenElevation_m, "m", "ft"))
      } else {
        # Get prev. setup elevations for whatever we're taking the backsight to
        known_elev <- dplyr::filter(temp_corrected_lvls, SetupNumber == setup - 1) %>%
          dplyr::select(SiteCode, VisitDate, FieldSeason, VisitType, SurveyPoint, TempCorrectedElevation_ft)
        # Join prev. setup elevations to get known elevation, calc. instrument height
        bs %<>% dplyr::left_join(known_elev, by = c("SiteCode", "VisitDate", "FieldSeason", "VisitType", "SurveyPoint")) %>%
          dplyr::mutate(InstrumentHeight_ft = TempCorrectedHeight_ft + TempCorrectedElevation_ft) %>%
          dplyr::select(-TempCorrectedElevation_ft)
      }

      # Calc elevations for current setup
      bs %<>% dplyr::select(-RM1_GivenElevation_m, -TempCorrectedHeight_ft, -SurveyPoint)
      temp_lvls <- levels %>% dplyr::filter(SetupNumber == setup) %>%
        dplyr::left_join(bs, by = c("SiteCode", "VisitDate", "FieldSeason", "VisitType", "SetupNumber")) %>%
        dplyr::mutate(TempCorrectedElevation_ft = InstrumentHeight_ft - TempCorrectedHeight_ft)
      temp_corrected_lvls <- rbind(temp_corrected_lvls, temp_lvls)
    }

    # Get given origin elevation
    given_origin_elev <- dplyr::filter(temp_corrected_lvls, SetupNumber == 1, ReadingType == "BS") %>%
      dplyr::select(SiteCode, VisitDate, FieldSeason, VisitType, SurveyPoint, TempCorrectedElevation_ft) %>%
      dplyr::rename(GivenOriginElevation_ft = TempCorrectedElevation_ft)

    # Get final origin elevation
    final_origin_elev <- dplyr::select(temp_corrected_lvls, SiteCode, VisitDate, FieldSeason, VisitType, SurveyPoint, SetupNumber, NumberOfInstrumentSetups, TempCorrectedElevation_ft) %>%
      dplyr::filter(SetupNumber == NumberOfInstrumentSetups) %>%
      dplyr::select(-SetupNumber, NumberOfInstrumentSetups) %>%
      dplyr::rename(FinalOriginElevation_ft = TempCorrectedElevation_ft)

    # Calculate closure error from given and final origin elevations
    closure_error <- dplyr::left_join(given_origin_elev, final_origin_elev, by = c("SiteCode", "VisitDate", "FieldSeason", "VisitType", "SurveyPoint")) %>%
      dplyr::mutate(ClosureError_ft = GivenOriginElevation_ft - FinalOriginElevation_ft) # Removed absolute value, since it will affect direction of corrections

    # Calculate final corrected elevation
    final_lvls <- dplyr::arrange(temp_corrected_lvls, FieldSeason, SiteCode, VisitType, SetupNumber) %>%
      dplyr::left_join(closure_error, by = c("SiteCode", "VisitDate", "FieldSeason", "VisitType", "NumberOfInstrumentSetups", "SurveyPoint")) %>%
      tidyr::fill(ClosureError_ft, .direction = "down") %>%
      dplyr::mutate(FinalCorrectedElevation_ft = ifelse(SetupNumber == 1 & ReadingType == "BS", # Added if-else statement, since closure error correction should not be applied to the backsight toward RM-1 during the first instrument setup, since this is the given origin elevation and is fixed.
                                                        TempCorrectedElevation_ft,
                                                        SetupNumber * (ClosureError_ft / NumberOfInstrumentSetups) + TempCorrectedElevation_ft)) %>%
      dplyr::mutate(ClosureError_ft = abs(ClosureError_ft)) %>% # Re-added the absolute value calculation applied to closure error. Keep or remove?
      dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, SurveyPoint) %>%
      dplyr::mutate(FinalCorrectedElevation_ft = mean(FinalCorrectedElevation_ft)) %>%
      dplyr::select(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL, SurveyPoint, Benchmark, ClosureError_ft, FinalCorrectedElevation_ft) %>%
      unique() %>%
      dplyr::filter(!grepl("TP", SurveyPoint)) %>%
      dplyr::ungroup()

    return(final_lvls)
}

#' Calculates lake level elevations
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason,VisitType, DPL, SurveyType, BenchmarkUsed, ClosureError_ft, FinalElevation_ft.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     LakeSurfaceElevation(conn)
#'     LakeSurfaceElevation(conn, site = "GRBA_L_BAKR0", field.season = "2019")
#'     LakeSurfaceElevation(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
LakeSurfaceElevation <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  string <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, "LakeLevelString")
  survey <- SurveyPointElevation(conn, path.to.data, park, site, field.season, data.source) %>%
    dplyr::filter(Benchmark == "Water Surface") %>%
    dplyr::mutate(SurveyType = "Theodolite", BenchmarkUsed = NA) %>%
    dplyr::rename(FinalElevation_ft = FinalCorrectedElevation_ft) %>%
    dplyr::select(-SurveyPoint, -Benchmark)

  string %<>%
    dplyr::mutate(BenchmarkNumber = substring(Benchmark, nchar(Benchmark))) %>%
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType) %>%
    dplyr::mutate(MinBenchmark = min(BenchmarkNumber),
                  BenchmarkElevation_ft = measurements::conv_unit(RM1_GivenElevation_m, "m", "ft")) %>%
    dplyr::filter(BenchmarkNumber == MinBenchmark) %>%
    dplyr::mutate(FinalElevation_ft = mean(BenchmarkElevation_ft - Height_ft)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(BenchmarkUsed = Benchmark,
                  ClosureError_ft = NA,
                  SurveyType = "String") %>%
    dplyr::select(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL, SurveyType, BenchmarkUsed, ClosureError_ft, FinalElevation_ft) %>%
    unique()

  lake_elevation <- rbind(string, survey)

  return(lake_elevation)
}

#' Calculates mean and standard deviation of final corrected elevations for each benchmark across all field seasons
#'
#' @inheritParams ReadAndFilterData
#' @param sd_cutoff Optional. If specified, only return benchmarks where standard deviation of final corrected elevations is greater than or equal to `sd_cutoff`.
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, Benchmark, AverageElevation_ft, StDevElevation_ft.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     QcBenchmarkElevation(conn)
#'     QcBenchmarkElevation(conn, site = "GRBA_L_BAKR0", field.season = c("2016", "2017", "2018", "2019"))
#'     QcBenchmarkElevation(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
qcBenchmarkElevation <- function(conn, path.to.data, park, site, field.season, data.source = "database", sd_cutoff = NA) {
  lvls <- SurveyPointElevation(conn, path.to.data, park, site, field.season, data.source)

  lvls %<>%
    dplyr::select(Park, SiteShort, SiteCode, SiteName, Benchmark, FinalCorrectedElevation_ft) %>%
    dplyr::filter(Benchmark != "Water Surface") %>%
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, Benchmark) %>%
    dplyr::summarize(AverageElevation_ft = mean(FinalCorrectedElevation_ft),
                  StDevElevation_ft = sd(FinalCorrectedElevation_ft)) %>%
    dplyr::ungroup()

  if (!is.na(sd_cutoff)) {
    lvls %<>% dplyr::filter(StDevElevation_ft >= sd_cutoff)
  }

  return(lvls)
}


#' Calculates mean and standard deviation of string survey heights for each benchmark
#'
#' @inheritParams qcBenchmarkElevation
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, MeanHeight_ft, StDevHeight_ft
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     QcStringSurveyHeights(conn)
#'     QcStringSurveyHeights(conn, site = "GRBA_L_BAKR0", field.season = c("2016", "2017"))
#'     QcStringSurveyHeights(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
qcStringSurveyHeights <- function(conn, path.to.data, park, site, field.season, data.source = "database", sd_cutoff = NA) {
  str_survey <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, "LakeLevelString") %>%
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, Benchmark) %>%
    dplyr::summarise(MeanHeight_ft = mean(Height_ft),
                     StDevHeight_ft = sd(Height_ft)) %>%
    dplyr::ungroup()

  if (!is.na(sd_cutoff)) {
    str_survey %<>%
      dplyr::filter(StDevHeight_ft >= sd_cutoff)
  }

  return(str_survey)
}


#' Calculates mean and standard deviation of string survey lake level elevations for each year
#'
#' @inheritParams qcBenchmarkElevation
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, MeanFinalElevation_ft, StDevFinalElevation_ft
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     QcStringSurveyElevations(conn)
#'     QcStringSurveyElevations(conn, site = "GRBA_L_BAKR0", field.season = c("2016", "2017"))
#'     QcStringSurveyElevations(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
qcStringSurveyElevations <- function(conn, path.to.data, park, site, field.season, data.source = "database", sd_cutoff = NA) {
  str_survey <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, "LakeLevelString") %>%
    dplyr::mutate(BenchmarkElevation_ft = measurements::conv_unit(RM1_GivenElevation_m, "m", "ft")) %>%
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, Benchmark) %>%
    dplyr::summarise(FinalElevation_ft = mean(BenchmarkElevation_ft - Height_ft)) %>%
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType) %>%
    dplyr::summarise(MeanFinalElevation_ft = mean(FinalElevation_ft),
                     StDevFinalElevation_ft = sd(FinalElevation_ft)) %>%
    dplyr::ungroup()

  if (!is.na(sd_cutoff)) {
    str_survey %<>%
      dplyr::filter(StDevFinalElevation_ft >= sd_cutoff)
  }

  return(str_survey)
}

#' Plot benchmark elevations over time
#'
#' @inheritParams WqPlotDepthProfile
#'
#' @return A ggplot or plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- OpenDatabaseConnection()
#' PlotBenchmarkElevation(conn)
#' PlotBenchmarkElevation(conn, site = "GRBA_L_DEAD0", plotly = TRUE)
#' CloseDatabaseConnection(conn)
#' }
#'
PlotBenchmarkElevation <- function(conn, path.to.data, park, site, field.season, data.source = "database", include.title = TRUE, plotly = FALSE) {
  lvls <- SurveyPointElevation(conn, path.to.data, park, site, field.season, data.source) %>%
    dplyr::filter(Benchmark != "Water Surface") %>%
    tidyr::separate(Benchmark, c(NA, "Benchmark"), sep = "-", fill = "left")


  plt <- FormatPlot(lvls,
                    FieldSeason,
                    FinalCorrectedElevation_ft,
                    SiteName,
                    plot.title = ifelse(include.title, "Benchmark elevation over time", ""),
                    x.lab = "Field Season",
                    y.lab = "Elevation (ft)") +
    ggplot2::geom_point(ggplot2::aes(color = Benchmark, group = Benchmark)) +
    ggplot2::geom_line(ggplot2::aes(color = Benchmark, group = Benchmark))

  if (plotly) {
    plt <- plotly::ggplotly(plt)
  }

  return(plt)
}

#' Plot lake surface elevations over time
#'
#' @inheritParams WqPlotDepthProfile
#'
#' @return A ggplot or plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- OpenDatabaseConnection()
#' PlotLakeSurfaceElevation(conn)
#' PlotLakeSurfaceElevation(conn, site = "GRBA_L_DEAD0", plotly = TRUE)
#' CloseDatabaseConnection(conn)
#' }
#'
PlotLakeSurfaceElevation <- function(conn, path.to.data, park, site, field.season, data.source = "database", include.title = TRUE, plotly = FALSE) {
  elev <- LakeSurfaceElevation(conn, path.to.data, park, site, field.season, data.source)

  plt <- FormatPlot(data = elev,
                    x.col = FieldSeason,
                    y.col = FinalElevation_ft,
                    plot.title = ifelse(include.title, "Lake surface elevation over time", ""),
                    x.lab = "Field Season",
                    y.lab = "Elevation (ft)") +
    ggplot2::geom_point(ggplot2::aes(color = SiteName, group = SiteName, shape = SurveyType)) +
    ggplot2::geom_line(ggplot2::aes(color = SiteName, group = SiteName)) +
    ggplot2::scale_shape_discrete()

  if (plotly) {
    plt <- plotly::ggplotly(plt, tooltip = c("SurveyType", "FinalElevation_ft"))
  }

  return(plt)
}

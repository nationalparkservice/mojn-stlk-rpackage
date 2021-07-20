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
                                                                          ifelse(SurveyPoint == "WS", "Water Surface", NA))))))))

    # Calculate L, the maximum elevation difference between the origin reference mark and any point in the level circuit
    l <- dplyr::arrange(levels, FieldSeason, SiteCode, VisitType, SetupNumber) %>%
      dplyr::mutate(Backsight_ft = ifelse(ReadingType == "BS", Height_ft, NA)) %>%
      tidyr::fill(Backsight_ft, .direction = "down") %>%
      dplyr::mutate(L = abs(Backsight_ft - Height_ft)) %>%
      dplyr::group_by(FieldSeason, SiteCode, VisitType, SetupNumber) %>%
      dplyr::summarize(L = max(L)) %>%
      dplyr::ungroup()

    # Correct for rod temperature (if needed)
    levels %<>% dplyr::left_join(l, by = c("SiteCode", "FieldSeason", "VisitType", "SetupNumber")) %>%
      dplyr::mutate(TemperatureCorrection = CTE * L * (RodTemperature_F - StandardTemperature_F),
                    TempCorrectedHeight_ft = ifelse(abs(TemperatureCorrection) > 0.003,
                                                    Height_ft + (CTE * Height_ft * (RodTemperature_F - StandardTemperature_F)),
                                                    Height_ft))

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
      dplyr::mutate(ClosureError_ft = abs(GivenOriginElevation_ft - FinalOriginElevation_ft))

    # Calculate final corrected elevation
    final_lvls <- dplyr::arrange(temp_corrected_lvls, FieldSeason, SiteCode, VisitType, SetupNumber) %>%
      dplyr::left_join(closure_error, by = c("SiteCode", "VisitDate", "FieldSeason", "VisitType", "NumberOfInstrumentSetups", "SurveyPoint")) %>%
      tidyr::fill(ClosureError_ft, .direction = "down") %>%
      dplyr::mutate(FinalCorrectedElevation_ft = SetupNumber * (ClosureError_ft / NumberOfInstrumentSetups) + TempCorrectedElevation_ft) %>%
      dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, SurveyPoint) %>%
      dplyr::mutate(FinalCorrectedElevation_ft = mean(FinalCorrectedElevation_ft)) %>%
      dplyr::select(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL, SurveyPoint, Benchmark, ClosureError_ft, FinalCorrectedElevation_ft) %>%
      unique() %>%
      dplyr::filter(!grepl("TP", SurveyPoint)) %>%
      dplyr::ungroup()

    return(final_lvls)
}

#' Calculates mean and standard deviation of final corrected elevations for each benchmark across all field seasons
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, Benchmark, AverageElevation_ft, StDevElevation_ft.
#' @export
#'
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     QCBenchmarkElevation(conn)
#'     QCBenchmarkElevation(conn, site = "GRBA_L_BAKR0", field.season = c("2016", "2017", "2018", "2019"))
#'     QCBenchmarkElevation(path.to.data = "path/to/data", data.source = "local")
#'     CloseDatabaseConnection(conn)
#' }
QCBenchmarkElevation <- function(conn, path.to.data, park, site, field.season, data.source = "database", sd_cutoff = NA) {
  lvls <- SurveyPointElevation(conn, path.to.data, park, site, field.season, data.source)

  lvls %<>%
    dplyr::select(Park, SiteShort, SiteCode, SiteName, Benchmark, FinalCorrectedElevation_ft) %>%
    dplyr::filter(Benchmark != "Water Surface") %>%
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, Benchmark) %>%
    dplyr::summarize(AverageElevation_ft = mean(FinalCorrectedElevation_ft),
                  StDevElevation_ft = sd(FinalCorrectedElevation_ft)) %>%
    dplyr::ungroup()

  return(lvls)
}

PlotBenchmarkElevation <- function() {

}

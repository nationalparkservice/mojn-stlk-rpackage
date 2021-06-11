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
    StandardTemperature_F <- 68  # Standard temperature to be used for temperature corrections

    # Parse out survey point names, types, and setup number. Consolidate rod temperature into one column
    levels %<>%
      tidyr::separate(SurveyPointType, into = c("SurveyPoint", "ReadingType", "SetupNumber"), sep = "-", remove = TRUE) %>%
      dplyr::mutate(SetupNumber = readr::parse_number(SetupNumber),
                    RodTemperature_F = ifelse(SetupNumber == 1, RodTemperatureSetup1_F,
                                              ifelse(SetupNumber == 2, RodTemperatureSetup2_F,
                                                     ifelse(SetupNumber == 3, RodTemperatureSetup3_F, NA))))

    # Calculate L, the maximum elevation difference between the origin reference mark and any point in the level circuit
    l <- dplyr::arrange(levels, FieldSeason, SiteCode, VisitType, SetupNumber) %>%
      dplyr::mutate(Backsight_ft = ifelse(ReadingType == "BS", Height_ft, NA)) %>%
      tidyr::fill(Backsight_ft, .direction = "down") %>%
      dplyr::mutate(L = abs(Backsight_ft - Height_ft)) %>%
      dplyr::group_by(FieldSeason, SiteCode, VisitType, SetupNumber) %>%
      dplyr::summarize(L = max(L)) %>%
      dplyr::ungroup()

    # Correct for rod temperature (if needed)
    levels %<>% dplyr::left_join(l) %>%
      dplyr::mutate(TemperatureCorrection = CTE * L * (RodTemperature_F - StandardTemperature_F),
                    TempCorrectedHeight_ft = ifelse(abs(TemperatureCorrection) > 0.003,
                                                    Height_ft + (CTE * Height_ft * (RodTemperature_F - StandardTemperature_F)),
                                                    Height_ft))

    setups <- unique(levels$SetupNumber) %>% sort()
    final_lvls <- tibble::tibble()

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
        known_elev <- dplyr::filter(final_lvls, SetupNumber == setup - 1) %>%
          dplyr::select(SiteCode, VisitDate, FieldSeason, VisitType, SurveyPoint, FinalElevation_ft)
        # Join prev. setup elevations to get known elevation, calc. instrument height
        bs %<>% dplyr::left_join(known_elev, by = c("SiteCode", "VisitDate", "FieldSeason", "VisitType", "SurveyPoint")) %>%
          dplyr::mutate(InstrumentHeight_ft = TempCorrectedHeight_ft + FinalElevation_ft) %>%
          dplyr::select(-FinalElevation_ft)
      }

      # Calc elevations for current setup
      bs %<>% dplyr::select(-RM1_GivenElevation_m, -TempCorrectedHeight_ft, -SurveyPoint)
      temp_lvls <- levels %>% dplyr::filter(SetupNumber == setup) %>%
        dplyr::left_join(bs, by = c("SiteCode", "VisitDate", "FieldSeason", "VisitType", "SetupNumber")) %>%
        dplyr::mutate(FinalElevation_ft = InstrumentHeight_ft - TempCorrectedHeight_ft)
      final_lvls <- rbind(final_lvls, temp_lvls)
    }


    final_lvls <- dplyr::arrange(final_lvls, FieldSeason, SiteCode, VisitType, SetupNumber) %>%
      dplyr::mutate(FinalElevation_m = measurements::conv_unit(FinalElevation_ft, "ft", "m"))

    return(final_lvls)
}

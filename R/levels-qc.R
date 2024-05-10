#' Calculates mean elevations for each survey point type in a survey (digital level)
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     SurveyPointElevation()
#'     SurveyPointElevation(site = "GRBA_L_BAKR0", field.season = "2019")
#' }
SurveyPointElevation <- function(park, site, field.season) {
    levels.import <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "LakeLevelSurvey")
    dry <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit") # Data to filter out dry lakes
    StandardTemperature_F <- 68  # Standard temperature to be used for temperature corrections

    dry <- dry |>
      dplyr::select(SiteCode, VisitDate, FieldSeason, IsLakeDry)

    # Parse out survey point names, types, and setup number. Consolidate rod temperature into one column
    levels <- levels.import |>
      dplyr::inner_join(dry, by = c("SiteCode", "VisitDate", "FieldSeason")) |>
      dplyr::filter(IsLakeDry != TRUE) |>
      tidyr::separate(SurveyPointType, into = c("SurveyPoint", "ReadingType", "SetupNumber"), sep = "-", remove = TRUE) |>
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
                                                                          ifelse(SurveyPoint == "WS", "Water Surface", NA)))))))) |>
      dplyr::mutate(TempCorrectedHeight_ft = Height_ft + (CTE * Height_ft * (RodTemperature_F - StandardTemperature_F)))

    setups <- unique(levels$SetupNumber) |> sort()
    temp_corrected_lvls <- tibble::tibble()

    # Get known elevations and calculate instrument height. Does this need to be a loop? Probably not
    for (setup in setups) {
      #TODO: Need to verify that there is only one backsight per survey per setup. We are assuming that RM1_GivenElevation_m applies to the BS of the first setup
      bs <- dplyr::filter(levels, ReadingType  == "BS", SetupNumber == setup) |>
        dplyr::select(SiteCode, VisitDate, FieldSeason, VisitType, SetupNumber, SurveyPoint, TempCorrectedHeight_ft, RM1_GivenElevation_m)

      # Get known elevation used to calculate instrument height
      if (setup == 1) {
        bs <- bs |>
          dplyr::mutate(InstrumentHeight_ft = TempCorrectedHeight_ft + measurements::conv_unit(RM1_GivenElevation_m, "m", "ft"))
      } else {
        # Get prev. setup elevations for whatever we're taking the backsight to
        known_elev <- dplyr::filter(temp_corrected_lvls, SetupNumber == setup - 1) |>
          dplyr::select(SiteCode, VisitDate, FieldSeason, VisitType, SurveyPoint, TempCorrectedElevation_ft)
        # Join prev. setup elevations to get known elevation, calc. instrument height
        bs <- bs |>
          dplyr::left_join(known_elev, by = c("SiteCode", "VisitDate", "FieldSeason", "VisitType", "SurveyPoint")) |>
          dplyr::mutate(InstrumentHeight_ft = TempCorrectedHeight_ft + TempCorrectedElevation_ft) |>
          dplyr::select(-TempCorrectedElevation_ft)
      }

      # Calc elevations for current setup
      bs <- bs |>
        dplyr::select(-RM1_GivenElevation_m, -TempCorrectedHeight_ft, -SurveyPoint)
      temp_lvls <- levels |>
        dplyr::filter(SetupNumber == setup) |>
        dplyr::left_join(bs, by = c("SiteCode", "VisitDate", "FieldSeason", "VisitType", "SetupNumber")) |>
        dplyr::mutate(TempCorrectedElevation_ft = InstrumentHeight_ft - TempCorrectedHeight_ft)
      temp_corrected_lvls <- rbind(temp_corrected_lvls, temp_lvls)
    }

    # Get given origin elevation
    given_origin_elev <- dplyr::filter(temp_corrected_lvls, SetupNumber == 1, ReadingType == "BS") |>
      dplyr::select(SiteCode, VisitDate, FieldSeason, VisitType, SurveyPoint, TempCorrectedElevation_ft) |>
      dplyr::rename(GivenOriginElevation_ft = TempCorrectedElevation_ft)

    # Get final origin elevation
    final_origin_elev <- dplyr::select(temp_corrected_lvls, SiteCode, VisitDate, FieldSeason, VisitType, SurveyPoint, SetupNumber, NumberOfInstrumentSetups, TempCorrectedElevation_ft) |>
      dplyr::filter(SetupNumber == NumberOfInstrumentSetups) |>
      dplyr::select(-SetupNumber, NumberOfInstrumentSetups) |>
      dplyr::rename(FinalOriginElevation_ft = TempCorrectedElevation_ft)

    # Calculate closure error from given and final origin elevations
    closure_error <- dplyr::left_join(given_origin_elev, final_origin_elev, by = c("SiteCode", "VisitDate", "FieldSeason", "VisitType", "SurveyPoint")) |>
      dplyr::mutate(ClosureError_ft = GivenOriginElevation_ft - FinalOriginElevation_ft) # Removed absolute value, since it will affect direction of corrections

    # Calculate final corrected elevation
    final_lvls <- dplyr::arrange(temp_corrected_lvls, FieldSeason, SiteCode, VisitType, SetupNumber) |>
      dplyr::left_join(closure_error, by = c("SiteCode", "VisitDate", "FieldSeason", "VisitType", "NumberOfInstrumentSetups", "SurveyPoint")) |>
      tidyr::fill(ClosureError_ft, .direction = "down") |>
      dplyr::mutate(FinalCorrectedElevation_ft = ifelse(SetupNumber == 1 & ReadingType == "BS", # Added if-else statement, since closure error correction should not be applied to the backsight toward RM-1 during the first instrument setup, since this is the given origin elevation and is fixed.
                                                        TempCorrectedElevation_ft,
                                                        SetupNumber * (ClosureError_ft / NumberOfInstrumentSetups) + TempCorrectedElevation_ft)) |>
      dplyr::mutate(ClosureError_ft = abs(ClosureError_ft)) |> # Re-added the absolute value calculation applied to closure error. Keep or remove?
      dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, SurveyPoint) |>
      dplyr::mutate(FinalCorrectedElevation_ft = mean(FinalCorrectedElevation_ft)) |>
      dplyr::select(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL, SurveyPoint, Benchmark, ClosureError_ft, FinalCorrectedElevation_ft) |>
      unique() |>
      dplyr::filter(!grepl("TP", SurveyPoint)) |>
      dplyr::ungroup() |>
      dplyr::filter(!(SiteShort == "DEAD0" & FieldSeason == "2021" & SurveyPoint == "WS")) |>
      dplyr::filter(VisitType == "Primary") |>
      dplyr::select(-c(VisitType, DPL, SurveyPoint)) |>
      dplyr::rename(FinalElevation_ft = FinalCorrectedElevation_ft) |>
      dplyr::relocate(ClosureError_ft, .after = "FinalElevation_ft") |>
      tidyr::separate(Benchmark, c(NA, "Benchmark"), sep = "-", fill = "left")

    return(final_lvls)
}

#' Calculates mean elevations for each survey point type in a survey (string)
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     StringSurveyElevation()
#'     StringSurveyElevation(site = "GRBA_L_BAKR0", field.season = "2016")
#' }

StringSurveyElevation <- function(park, site, field.season) {
  str <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "LakeLevelString")

  elevs <- str |>
    dplyr::filter(VisitType == "Primary") |>
    dplyr::select(-c(VisitType, DPL, RM1_GivenElevation_m)) |>
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, Benchmark) |>
    dplyr::summarise(MeanHeight_ft = mean(Height_ft),
                     StDev_ft = sd(Height_ft),
                     Count = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(Offset_ft = dplyr::case_when(grepl("BM1", Benchmark)  ~ (measurements::conv_unit(100, "m", "ft") - MeanHeight_ft),
                                               (SiteCode %in% c("GRBA_L_DEAD0") & grepl("BM4", Benchmark))  ~ (measurements::conv_unit(102.105, "m", "ft") - MeanHeight_ft),
                                               TRUE ~ NA)) |>
    dplyr::group_by(SiteCode, FieldSeason) |>
    tidyr::fill(Offset_ft) |>
    dplyr::ungroup() |>
    dplyr::mutate(FinalElevation_ft = dplyr::case_when(grepl("BM1", Benchmark) ~ (measurements::conv_unit(100, "m", "ft")),
                                                       TRUE ~ MeanHeight_ft + Offset_ft)) |>
    dplyr::select(-c(MeanHeight_ft, Offset_ft)) |>
    tidyr::separate(Benchmark, c(NA, "Benchmark"), sep = "-", fill = "left") |>
    dplyr::relocate(FinalElevation_ft, .after = "Benchmark")

  return(elevs)

}

#' Calculates lake level elevations
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason,VisitType, DPL, SurveyType, BenchmarkUsed, ClosureError_ft, FinalElevation_ft.
#' @export
#'
#' @examples
#' \dontrun{
#'     LakeSurfaceElevation()
#'     LakeSurfaceElevation(site = "GRBA_L_BAKR0", field.season = "2019")
#' }
LakeSurfaceElevation <- function(park, site, field.season) {

  t1 <- try(ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "LakeLevelString"))

  if("try-error" %in% class(t1)) {
    string <- tibble::tibble(
      Park = character(),
      SiteShort = character(),
      SiteCode = character(),
      SiteName = character(),
      VisitDate = date(),
      FieldSeason = character(),
      VisitType = character(),
      DPL = character(),
      Benchmark = character(),
      RM1_GivenElevation_m = double(),
      IsLakeDry = logical(),
      Height_ft = double()
    )
  } else {
    string <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "LakeLevelString")
  }

  t2 <- try(SurveyPointElevation(park = park, site = site, field.season = field.season))

  if("try-error" %in% class(t2)) {
    survey <- tibble::tibble(
      Park = character(),
      SiteShort = character(),
      SiteCode = character(),
      SiteName = character(),
      VisitDate = date(),
      FieldSeason = character(),
      VisitType = character(),
      SurveyType = character(),
      FinalElevation_ft = double(),
      ClosureError_ft = double(),
      BenchmarkUsed = logical()
    )
  } else {
  survey <- SurveyPointElevation(park = park, site = site, field.season = field.season) |>
    dplyr::filter(Benchmark == "Water Surface") |>
    dplyr::mutate(SurveyType = "Digital Level",
                  BenchmarkUsed = NA_character_) |>
    dplyr::select(-Benchmark) |>
    dplyr::relocate(FinalElevation_ft, .after = "SurveyType") |>
    dplyr::relocate(ClosureError_ft, .after = "FinalElevation_ft")
  }

  string <- string |>
    dplyr::filter(VisitType == "Primary") |>
    dplyr::select(-c(DPL, VisitType)) |>
    dplyr::mutate(BenchmarkNumber = substring(Benchmark, nchar(Benchmark))) |>
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason) |>
    dplyr::mutate(MinBenchmark = min(BenchmarkNumber),
                  BenchmarkElevation_ft = measurements::conv_unit(RM1_GivenElevation_m, "m", "ft")) |>
    dplyr::filter(BenchmarkNumber == MinBenchmark) |>
    dplyr::mutate(FinalElevation_ft = mean(BenchmarkElevation_ft - Height_ft)) |>
    dplyr::ungroup() |>
    dplyr::mutate(BenchmarkUsed = Benchmark,
                  ClosureError_ft = as.double(NA),
                  SurveyType = "String") |>
    dplyr::select(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, SurveyType, FinalElevation_ft, ClosureError_ft, BenchmarkUsed) |>
    unique()

  not_all_na <- function(x) any(!is.na(x))

  lake_elevation <- rbind(string, survey) |>
    dplyr::filter((FieldSeason == "2018" & SurveyType == "Digital Level") | FieldSeason != "2018") |>
    dplyr::select(where(not_all_na)) |>
    dplyr::mutate(FinalElevation_m = measurements::conv_unit(FinalElevation_ft, "ft", "m"),
                  ClosureError_m = measurements::conv_unit(ClosureError_ft, "ft", "m")) |>
    dplyr::relocate(any_of("BenchmarkUsed"), .after = "ClosureError_m") |>
    dplyr::select(-c(ClosureError_ft, ClosureError_m, BenchmarkUsed))

  return(lake_elevation)
}

#' Calculates mean and standard deviation of final corrected elevations for each benchmark across all field seasons
#'
#' @inheritParams ReadAndFilterData
#' @param sd_cutoff Optional. If specified, only return benchmarks where standard deviation of final corrected elevations is greater than or equal to `sd_cutoff`.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcBenchmarkConsistency()
#'     qcBenchmarkConsistency(site = "GRBA_L_BAKR0", field.season = c("2016", "2017", "2018", "2019"))
#' }
qcBenchmarkConsistency <- function(park, site, field.season, sd_cutoff = NA) {
  lvls.dl <- SurveyPointElevation(park = park, site = site, field.season = field.season) |>
    dplyr::mutate(Method = "Digital Level") |>
    dplyr::select(-c(ClosureError_ft))

  lvls.str <- StringSurveyElevation(park = park, site = site, field.season = field.season) |>
    dplyr::mutate(Method = "String") |>
    dplyr::select(-c(StDev_ft, Count))

  lvls.all <- rbind(lvls.dl, lvls.str) |>
    dplyr::filter(Benchmark != "Water Surface") |>
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, Benchmark, Method) |>
    dplyr::summarize(MeanElevation_ft = mean(FinalElevation_ft),
                     StDev_ft = sd(FinalElevation_ft),
                     Count = dplyr::n()) |>
    dplyr::ungroup() |>
    tidyr::separate(Benchmark, c(NA, "Benchmark"), sep = "-", fill = "left")

  if (!is.na(sd_cutoff)) {
    lvls <- lvls |>
      dplyr::filter(StDev_ft >= sd_cutoff)
  }

  return(lvls.all)
}

#' Plot median, quartile, and outlier elevations for each benchmark that is not assigned a given elevation (e.g., Benchmark 1)
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#'     BenchmarkConsistencyPlot()
#'     BenchmarkConsistencyPlot(site = "GRBA_L_BAKR0")
#' }
BenchmarkConsistencyPlot <- function(park, site, field.season) {
  lvls.dl <- SurveyPointElevation(park = park, site = site, field.season = field.season) |>
    dplyr::mutate(Method = "Digital Level") |>
    dplyr::select(-c(ClosureError_ft))

  lvls.str <- StringSurveyElevation(park = park, site = site, field.season = field.season) |>
    dplyr::mutate(Method = "String") |>
    dplyr::select(-c(StDev_ft, Count))

  lvls.all <- rbind(lvls.dl, lvls.str) |>
    tidyr::separate(Benchmark, c(NA, "Benchmark"), sep = "-", fill = "left") |>
    dplyr::filter(!(Benchmark %in% c("Water Surface", "BM1", "BM4")))

  plt <- ggplot2::ggplot(data = lvls.all,
                         ggplot2::aes(x = Benchmark,
                                      y = FinalElevation_ft,
                                      group = interaction(Benchmark, Method),
                                      fill = Method)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(SiteName~.,
                        ncol = 3,
                        scales = "free") +
    khroma::scale_fill_bright() +
    ggplot2::theme(legend.position="bottom")

  return(plt)

}


#' Calculates mean and standard deviation of string survey heights for each benchmark
#'
#' @inheritParams qcBenchmarkConsistency
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcStringSurveyHeights()
#'     qcStringSurveyHeights(site = "GRBA_L_BAKR0", field.season = c("2016", "2017"))
#' }
qcStringSurveyHeights <- function(park, site, field.season, sd_cutoff = NA) {
  str_survey <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "LakeLevelString") |>
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, Benchmark) |>
    dplyr::summarise(MeanHeight_ft = mean(Height_ft),
                     StDevHeight_ft = sd(Height_ft),
                     Count = dplyr::n()) |>
    dplyr::ungroup()

  if (!is.na(sd_cutoff)) {
    str_survey <- str_survey
      dplyr::filter(StDevHeight_ft >= sd_cutoff)
  }

  return(str_survey)
}


#' Calculates mean and standard deviation of string survey lake level elevations for each year
#'
#' @inheritParams qcBenchmarkConsistency
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcStringSurveyElevations()
#'     qQcStringSurveyElevations(site = "GRBA_L_BAKR0", field.season = c("2016", "2017"))
#' }
qcStringSurveyElevations <- function(park, site, field.season, sd_cutoff = NA) {
  str_survey <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "LakeLevelString") |>
    dplyr::mutate(BenchmarkElevation_ft = measurements::conv_unit(RM1_GivenElevation_m, "m", "ft")) |>
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, Benchmark) |>
    dplyr::summarise(FinalElevation_ft = mean(BenchmarkElevation_ft - Height_ft)) |>
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType) |>
    dplyr::summarise(MeanFinalElevation_ft = mean(FinalElevation_ft),
                     StDevFinalElevation_ft = sd(FinalElevation_ft)) |>
    dplyr::ungroup()

  if (!is.na(sd_cutoff)) {
    str_survey <- str_survey
      dplyr::filter(StDevFinalElevation_ft >= sd_cutoff)
  }

  return(str_survey)
}


#' Check the difference between benchmark and water surface elevations calculated in R with those calculated in the Survey123 app
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcStringSurveyElevations()
#'     qQcStringSurveyElevations(site = "GRBA_L_BAKR0", field.season = c("2019", "2021"))
#' }
qcElevationDiscrepancies <- function(park, site, field.season) {
 r_elevs <- SurveyPointElevation(park = park, site = site, field.season = field.season)
 survey_elevs <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "LakeLevelSurvey")

 r_elevs_data <- r_elevs |>
   dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, SurveyPoint, Benchmark, FinalCorrectedElevation_ft) |>
   dplyr::rename(R_Elev_ft = FinalCorrectedElevation_ft)

 survey_elevs_data <- survey_elevs |>
   dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, FieldCalculatedWaterSurfaceElevation_m, FieldCalculatedRM2Elevation_m, FieldCalculatedRM3Elevation_m) |>
   tidyr::pivot_longer(cols = -c(SiteCode, SiteName, VisitDate, FieldSeason),
                       names_to = "SurveyPoint",
                       values_to = "Survey_Elev_m") |>
   unique() |>
   dplyr::mutate(SurveyPoint = dplyr::case_when(SurveyPoint == "FieldCalculatedWaterSurfaceElevation_m" ~ "WS",
                                                SurveyPoint == "FieldCalculatedRM2Elevation_m" ~ "RM2",
                                                SurveyPoint == "FieldCalculatedRM3Elevation_m" ~ "RM3",
                                                TRUE ~ SurveyPoint)) |>
   dplyr::filter(!is.na(Survey_Elev_m)) |>
   dplyr::mutate(Survey_Elev_ft = Survey_Elev_m * 3.28084) |>
   dplyr::select(-c("Survey_Elev_m"))

 elevs_data_joined <- r_elevs_data |>
   dplyr::inner_join(survey_elevs_data, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "SurveyPoint")) |>
   dplyr::mutate(Elev_diff = round(as.numeric(format(R_Elev_ft - Survey_Elev_ft, scientific = FALSE)), 5)) |>
   dplyr::arrange(desc(abs(Elev_diff)))

 return(elevs_data_joined)
}


#' Check the difference between closure errors calculated in R with those calculated in the Survey123 app
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcStringSurveyElevations()
#'     qQcStringSurveyElevations(site = c("GRBA_L_BAKR0", "GRBA_L_JHNS0"), field.season = "2021")
#' }
qcClosureErrorDiscrepancies <- function(park, site, field.season) {
  r_elevs <- SurveyPointElevation(park = park, site = site, field.season = field.season)
  survey_elevs <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "LakeLevelSurvey")

  r_ce_data <- r_elevs |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, ClosureError_ft) |>
    dplyr::rename(R_CE_ft = ClosureError_ft) |>
    dplyr::mutate(R_CE_ft = round(as.numeric(format(R_CE_ft, scientific = FALSE)), 4)) |>
    unique()

  survey_ce_data <- survey_elevs |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, FieldCalculatedClosureError) |>
    dplyr::rename(Survey_CE_ft = FieldCalculatedClosureError) |>
    unique() |>
    dplyr::mutate(Survey_CE_ft = round(as.numeric(format(Survey_CE_ft, scientific = FALSE)), 4)) |>
    dplyr::filter(!is.na(Survey_CE_ft))


  ce_data_joined <- r_ce_data |>
    dplyr::inner_join(survey_ce_data, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason")) |>
    dplyr::mutate(CE_diff = round(as.numeric(format(R_CE_ft - Survey_CE_ft, scientific = FALSE)), 5)) |>
    dplyr::arrange(desc(abs(CE_diff)))

  return(ce_data_joined)
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
#' PlotBenchmarkElevation()
#' PlotBenchmarkElevation(site = "GRBA_L_DEAD0", plotly = TRUE)
#' }
PlotBenchmarkElevation <- function(park, site, field.season, include.title = TRUE, plotly = FALSE) {
  str <- StringSurveyElevation(park = park, site = site, field.season = field.season) |>
    dplyr::select(-c(StDev_ft, Count)) |>
    dplyr::mutate(Method = "String")

  dl <- SurveyPointElevation(park = park, site = site, field.season = field.season) |>
    dplyr::select(-c(ClosureError_ft)) |>
    dplyr::filter(Benchmark != "Water Surface") |>
    dplyr::mutate(Method = "Digital Level")

  lvls <- rbind(str, dl) |>
    tidyr::separate(Benchmark, c(NA, "Benchmark"), sep = "-", fill = "left") |>
    tidyr::complete(FieldSeason, tidyr::nesting(Park, SiteShort, SiteCode, SiteName, Benchmark, Method))

  ptol_muted_6 <- c("#CC6677", "#332288", "#DDCC77", "#117733", "#88CCEE", "#882255")

  plt <- ggplot2::ggplot(data = lvls,
                         ggplot2::aes(x = FieldSeason,
                                      y = FinalElevation_ft,
                                      color = Benchmark,
                                      group = interaction(Benchmark, Method, SiteName),
                                      text = paste0("Field Season: ", FieldSeason, "<br>",
                                                    "Benchmark: ", Benchmark, "<br>",
                                                    "Elevation (ft): ", round(FinalElevation_ft, 2), "<br>",
                                                    "Method: ", Method))) +
    ggplot2::geom_point(size = 2.2,
                        ggplot2::aes(shape = Method)) +
    ggplot2::geom_line(linewidth = 1,
                       ggplot2::aes(linetype = Method)) +
    ggplot2::facet_wrap(SiteName~.,
                        ncol = 2,
                        scales = "free_y") +
    khroma::scale_color_muted() +
    ggplot2::labs(title = "Benchmark elevation over time",
                  x = "Field Season",
                  y = "Elevation (ft)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (plotly) {
    plt <- plotly::ggplotly(plt, tooltip = "text")
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
#' PlotLakeSurfaceElevation()
#' PlotLakeSurfaceElevation(site = "GRBA_L_DEAD0", plotly = TRUE)
#' }
PlotLakeSurfaceElevation <- function(park, site, field.season, include.title = TRUE, plotly = FALSE) {
  elev <- LakeSurfaceElevation(park = park, site = site, field.season = field.season)

  elev <- elev |>
    tidyr::complete(FieldSeason, tidyr::nesting(Park, SiteShort, SiteCode, SiteName)) |>
    dplyr::relocate(FieldSeason, .after = VisitDate) |>
    dplyr::filter((FieldSeason == "2018" & SurveyType == "Digital Level") | FieldSeason != "2018")

  plt <- FormatPlot(data = elev,
                    x.col = FieldSeason,
                    y.col = FinalElevation_ft,
                    plot.title = ifelse(include.title, "Lake surface elevation over time", ""),
                    x.lab = "Field Season",
                    y.lab = "Elevation (ft)") +
    ggplot2::aes(color = SiteName,
                 group = SiteName,
                 text = paste0("Field Season: ", FieldSeason, "<br>",
                               "Survey Type: ", SurveyType, "<br>",
                               "Elevation (ft): ", round(FinalElevation_ft, 2))) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_shape_discrete(na.translate = FALSE) +
    khroma::scale_color_muted()

    if (plotly) {
      plt <- plotly::ggplotly(plt, tooltip = "text")
    }

  return(plt)
}

#' Plot lake levels determined by all benchmarks where string method was used
#'
#' @param park
#' @param site
#' @param field.season
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' PlotStringComparisons()
#' PlotStringComparisons(site = "GRBA_L_DEAD0")
#' }
PlotStringComparisons <- function(park, site, field.season) {
  str <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "LakeLevelString")

  mean <- str |>
    dplyr::filter(VisitType == "Primary",
                  IsLakeDry == FALSE) |>
    dplyr::select(-c(VisitType, DPL, IsLakeDry)) |>
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, Benchmark, RM1_GivenElevation_m) |>
    dplyr::summarize(MeanHeight_ft = mean(Height_ft)) |>
    dplyr::ungroup() |>
    dplyr::mutate(GivenElevation_ft = measurements::conv_unit(RM1_GivenElevation_m, "m", "ft"),
                  LakeElevation_ft = GivenElevation_ft - MeanHeight_ft) |>
    tidyr::separate(Benchmark, c(NA, "Benchmark"), sep = "-", fill = "left") |>
    tidyr::complete(FieldSeason, tidyr::nesting(Park, SiteShort, SiteCode, SiteName))

  plt <- ggplot2::ggplot(mean,
                         ggplot2::aes(x = FieldSeason,
                                      y = LakeElevation_ft,
                                      group = Benchmark,
                                      color = Benchmark)) +
    ggplot2::geom_point(size = 2.5,
                        ggplot2::aes(shape = Benchmark)) +
    ggplot2::facet_wrap(SiteName~.,
                        ncol = 2,
                        scales = "free_y") +
    khroma::scale_color_muted() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Lake surface elevations by benchmark",
                  x = "Field Season",
                  y = "Elevation (ft)")

  return(plt)
}

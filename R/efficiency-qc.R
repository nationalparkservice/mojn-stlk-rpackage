#' Return list of streams and lakes that were not visited for annual monitoring during a field season
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, SampleFrame, FieldSeason, VisitDate
#' @export
#'
#' @examples
#' \dontrun{
#'     qcNoAnnualVisit()
#'     qcNoAnnualVisit(site = "GRBA_L_DEAD0", field.season = c("2012", "2013", "2014", "2015"))
#' }
qcNoAnnualVisit <- function(park, site, field.season) {

visit.data <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit")
visit <- visit.data |>
  dplyr::select(Park, Subunit, SiteShort, SiteCode, SiteName, SampleFrame, VisitDate, FieldSeason, VisitType, MonitoringStatus) |>
  dplyr::filter(VisitType == "Primary", SiteCode != "GRBA_S_BAKR2") |>
  tidyr::pivot_wider(id_cols = c(Park, Subunit, SiteShort, SiteCode, SiteName, SampleFrame),
                     names_from = FieldSeason,
                     values_from = VisitDate) |>
  tidyr::pivot_longer(!c(Park, Subunit, SiteShort, SiteCode, SiteName, SampleFrame), names_to = "FieldSeason", values_to = "VisitDate") |>
  dplyr::filter(is.na(VisitDate)) |>
  dplyr::select(Park, SiteShort, SiteCode, SiteName, SampleFrame, FieldSeason, VisitDate)

return(visit)

}


#' Return list of site visits that have any data categorized as "Raw" or "Provisional"
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     qcDPLCheck()
#'     qcDPLCheck(site = "GRBA_L_JHNS0", field.season = c("2018", "2019", "2020"))
#' }
qcDPLCheck <- function(park, site, field.season) {

  visit <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit")
  chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Chemistry")
  # bmi <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "BMI")
  channel <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Channel")
  clarity <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Clarity")
  lakesurvey <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "LakeLevelSurvey")
  lakestring <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "LakeLevelString")
  xsection <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WQStreamXSection")
  temp <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualityTemperature")
  ph <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualitypH")
  spcond <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualitySpCond")
  do <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "WaterQualityDO")

  visit.DPL <- visit |>
    dplyr::rename(Visit.DPL = DataProcessingLevel) |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, VisitType, Visit.DPL)
  chem.DPL <- chem |>
    dplyr::filter(SampleType == "Routine") |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, SampleFrame, DPL) |>
    dplyr::rename(Chem.DPL = DPL) |>
    dplyr::distinct()
  # bmi.DPL <- bmi |>
  #   dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) |>
  #   dplyr::rename(BMI.DPL = DPL) |>
  #   dplyr::distinct()
  channel.DPL <- channel |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, DPL) |>
    dplyr::rename(Channel.DPL = DPL) |>
    dplyr::distinct()
  clarity.DPL <- clarity |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) |>
    dplyr::rename(Clarity.DPL = DPL) |>
    dplyr::distinct()
  lakesurvey.DPL <- lakesurvey |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) |>
    dplyr::rename(LakeSurvey.DPL = DPL) |>
    dplyr::distinct()
  lakestring.DPL <- lakestring |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) |>
    dplyr::rename(LakeString.DPL = DPL) |>
    dplyr::distinct()
  xsection.DPL <- xsection |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) |>
    dplyr::rename(Xsection.DPL = DPL) |>
    dplyr::distinct()
  temp.DPL <- temp |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) |>
    dplyr::rename(TempC.DPL = DPL) |>
    dplyr::distinct()
  ph.DPL <- ph |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) |>
    dplyr::rename(pH.DPL = DPL) |>
    dplyr::distinct()
  spcond.DPL <- spcond |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) |>
    dplyr::rename(SpCond.DPL = DPL) |>
    dplyr::distinct()
  do.DPL <- do |>
    dplyr::select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) |>
    dplyr::rename(DO.DPL = DPL) |>
    dplyr::distinct()

  dpl <- visit.DPL |>
    dplyr::left_join(chem.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "VisitType")) |>
    # dplyr::left_join(bmi.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) |>
    dplyr::left_join(channel.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason")) |>
    dplyr::left_join(clarity.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) |>
    dplyr::left_join(lakesurvey.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) |>
    dplyr::left_join(lakestring.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) |>
    dplyr::left_join(xsection.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) |>
    dplyr::left_join(temp.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) |>
    dplyr::left_join(ph.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) |>
    dplyr::left_join(spcond.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) |>
    dplyr::left_join(do.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) |>
    unique() |>
    dplyr::filter_all(any_vars(. %in% c("Raw", "Provisional"))) |>
    dplyr::arrange(SampleFrame, FieldSeason, SiteCode)

  return(dpl)
}

#' Calculate daily mean values (daily median values for pH) for water quality parameters at streams based on hourly data. Determine the most frequent data grade level for each day based on hourly data. Include only those dates with greater than 80% completeness (greater than 19 hourly values). Long format for ease of plotting.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     WqDailyMeanLong()
#'     WqDailyMeanLong(site = "GRBA_S_LHMN1", field.season = c("2012", "2013", "2014", "2015"))
#' }
WqDailyMeanLong <- function(park, site, field.season) {

  wt <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "TimeseriesTemperature")
  ph <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "TimeseriespH")
  sc <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "TimeseriesSpCond")
  do.pct <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "TimeseriesDOpct")
  do.mgl <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "TimeseriesDOmgl")
  visit <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit")

  wt.long <- wt |>
    dplyr::filter(Approval == "Approved") |>
    dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) |>
    dplyr::rename(WaterTemperature_C = Value) |>
    dplyr::group_by(Park,
                    SiteCode,
                    SampleFrame,
                    FieldSeason,
                    Date) |>
    dplyr::summarise(Value = dplyr::case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(WaterTemperature_C)) > 77 ~ mean(WaterTemperature_C, na.rm = TRUE),
                                       !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(WaterTemperature_C)) > 19 ~ mean(WaterTemperature_C, na.rm = TRUE),
                                       TRUE ~ as.double(NA_integer_)),
                     Grade = dplyr::case_when(!is.na(WaterTemperature_C) ~ statip::mfv1(Grade, na_rm = TRUE))) |>
    unique() |>
    dplyr::mutate(Parameter = "Temperature") |>
    dplyr::mutate(Units = "C") |>
    dplyr::arrange(Park, SiteCode, Date) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::relocate(Parameter, .after = Date) |>
    dplyr::relocate(Units, .after = Parameter) |>
    dplyr::ungroup()

  ph.long <- ph |>
    dplyr::filter(Approval == "Approved") |>
    dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) |>
    dplyr::rename(pH = Value) |>
    dplyr::group_by(Park,
                    SiteCode,
                    SampleFrame,
                    FieldSeason,
                    Date) |>
    dplyr::summarise(Value = dplyr::case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(pH)) > 77 ~ median(pH, na.rm = TRUE),
                                       !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(pH)) > 19 ~ median(pH, na.rm = TRUE),
                                       TRUE ~ as.double(NA_integer_)),
                     Grade = dplyr::case_when(!is.na(pH) ~ statip::mfv1(Grade, na_rm = TRUE))) |>
    unique() |>
    dplyr::mutate(Parameter = "pH") |>
    dplyr::mutate(Units = "units") |>
    dplyr::arrange(Park, SiteCode, Date) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::relocate(Parameter, .after = Date) |>
    dplyr::relocate(Units, .after = Parameter) |>
    dplyr::ungroup()

  sc.long <- sc |>
    dplyr::filter(Approval == "Approved") |>
    dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) |>
    dplyr::rename(SpecificConductance_microS_per_cm = Value) |>
    dplyr::group_by(Park,
                    SiteCode,
                    SampleFrame,
                    FieldSeason,
                    Date) |>
    dplyr::summarise(Value = dplyr::case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(SpecificConductance_microS_per_cm)) > 77 ~ mean(SpecificConductance_microS_per_cm, na.rm = TRUE),
                                       !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(SpecificConductance_microS_per_cm)) > 19 ~ mean(SpecificConductance_microS_per_cm, na.rm = TRUE),
                                       TRUE ~ as.double(NA_integer_)),
                     Grade = dplyr::case_when(!is.na(SpecificConductance_microS_per_cm) ~ statip::mfv1(Grade, na_rm = TRUE))) |>
    unique() |>
    dplyr::mutate(Parameter = "SpCond") |>
    dplyr::mutate(Units = "uS/cm") |>
    dplyr::arrange(Park, SiteCode, Date) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::relocate(Parameter, .after = Date) |>
    dplyr::relocate(Units, .after = Parameter) |>
    dplyr::ungroup()

  do.pct.long <- do.pct |>
    dplyr::filter(Approval == "Approved") |>
    dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) |>
    dplyr::rename(DissolvedOxygen_percent = Value) |>
    dplyr::group_by(Park,
                    SiteCode,
                    SampleFrame,
                    FieldSeason,
                    Date) |>
    dplyr::summarise(Value = dplyr::case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(DissolvedOxygen_percent)) > 77 ~ mean(DissolvedOxygen_percent, na.rm = TRUE),
                                       !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(DissolvedOxygen_percent)) > 19 ~ mean(DissolvedOxygen_percent, na.rm = TRUE),
                                       TRUE ~ as.double(NA_integer_)),
                     Grade = dplyr::case_when(!is.na(DissolvedOxygen_percent) ~ statip::mfv1(Grade, na_rm = TRUE))) |>
    unique() |>
    dplyr::mutate(Parameter = "DO") |>
    dplyr::mutate(Units = "%") |>
    dplyr::arrange(Park, SiteCode, Date) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::relocate(Parameter, .after = Date) |>
    dplyr::relocate(Units, .after = Parameter) |>
    dplyr::ungroup()

  do.mgl.long <- do.mgl |>
    dplyr::filter(Approval == "Approved") |>
    dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) |>
    dplyr::rename(DissolvedOxygen_mgL = Value) |>
    dplyr::group_by(Park,
                    SiteCode,
                    SampleFrame,
                    FieldSeason,
                    Date) |>
    dplyr::summarise(Value = dplyr::case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(DissolvedOxygen_mgL)) > 77 ~ mean(DissolvedOxygen_mgL, na.rm = TRUE),
                                       !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(DissolvedOxygen_mgL)) > 19 ~ mean(DissolvedOxygen_mgL, na.rm = TRUE),
                                       TRUE ~ as.double(NA_integer_)),
                     Grade = dplyr::case_when(!is.na(DissolvedOxygen_mgL) ~ statip::mfv1(Grade, na_rm = TRUE))) |>
    unique() |>
    dplyr::mutate(Parameter = "DO") |>
    dplyr::mutate(Units = "mg/L") |>
    dplyr::arrange(Park, SiteCode, Date) |>
    dplyr::filter(!is.na(Value)) |>
    dplyr::relocate(Parameter, .after = Date) |>
    dplyr::relocate(Units, .after = Parameter) |>
    dplyr::mutate(Grade = as.character(Grade)) |>
    dplyr::ungroup()

  wq.long.int <- dplyr::bind_rows(wt.long, ph.long, sc.long, do.pct.long, do.mgl.long)

  gage.locations <- tibble::tibble(SiteShort = c("BAKR1", "LHMN1", "SNKE1", "SNKE3", "STRW1"),
                                   SiteCode = c("GRBA_S_BAKR1", "GRBA_S_LHMN1", "GRBA_S_SNKE1", "GRBA_S_SNKE3", "GRBA_S_STRW1"),
                                   SiteName = c("Baker Creek (Gage)", "Lehman Creek (Gage)", "Snake Creek (Lower)", "Snake Creek (Upper)", "Strawberry Creek (Gage)"))

  visit.names <- visit |>
    dplyr::select(SiteShort, SiteCode, SiteName) |>
    unique() |>
    dplyr::bind_rows(gage.locations)

  wq.long <- dplyr::left_join(wq.long.int, visit.names, by = c("SiteCode")) |>
    dplyr::relocate(SiteShort, .before = SiteCode) |>
    dplyr::relocate(SiteName, .after = SiteCode)

  return(wq.long)
}


#' Return summary of daily mean values (daily median values for pH) and grades for water quality parameters at streams.
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
#'     WqDailyMean()
#'     WqDailyMean(site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#' }
WqDailyMean <- function(park, site, field.season) {

wq.long <- WqDailyMeanLong(park = park, site = site, field.season = field.season)

wq.daily <- wq.long |>
  tidyr::unite(Parameter, c("Parameter", "Units")) |>
  tidyr::pivot_wider(names_from = Parameter,
                     values_from = c(Value, Grade)) |>
  plyr::rename(replace = c(Value_Temperature_C = "Temp_C",
                           Value_pH_units = "pH",
                           `Value_SpCond_uS/cm` = "SpCond_uScm",
                           `Value_DO_%` = "DO_pct",
                           `Value_DO_mg/L` = "DO_mgL",
                           Grade_Temperature_C = "Temp_C_Grade",
                           Grade_pH_units = "pH_Grade",
                           `Grade_SpCond_uS/cm` = "SpCond_uScm_Grade",
                           `Grade_DO_%` = "DO_pct_Grade",
                           `Grade_DO_mg/L` = "DO_mgL_Grade"),
                warn_missing = FALSE) |>
  dplyr::select(Park, SiteShort, SiteCode, SiteName, SampleFrame, Date, FieldSeason, Temp_C, Temp_C_Grade, pH, pH_Grade, SpCond_uScm, SpCond_uScm_Grade, DO_pct, DO_pct_Grade, everything())

  return(wq.daily)
}


#' Calculate the number and percentage of days of data for each water quality parameter for each field season between the index period of July 1 to September 15 (77 days).
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
#'     qcWqCompleteness()
#'     qcWqCompleteness(site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#' }
qcWqCompleteness <- function(park, site, field.season) {

  wq.long <- WqDailyMeanLong(park = park, site = site, field.season = field.season)

  wq.comp <- wq.long |>
    dplyr::mutate(Month = lubridate::month(Date),
                  Day = lubridate::day(Date)) |>
    dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) |>
    dplyr::group_by(Park,
                    SiteShort,
                    SiteCode,
                    SiteName,
                    SampleFrame,
                    FieldSeason,
                    Parameter,
                    Units) |>
    dplyr::summarise(CompletedDays = sum(!is.na(Value))) |>
    dplyr::mutate(PercentCompleteness = CompletedDays/77*100) |>
    dplyr::ungroup() |>
    tidyr::complete(FieldSeason, tidyr::nesting(Park, SiteShort, SiteCode, SiteName, SampleFrame, Parameter, Units), fill = list(CompletedDays = 0, PercentCompleteness = 0)) |>
    dplyr::relocate(FieldSeason, .after = SampleFrame) |>
    dplyr::arrange(SiteCode, FieldSeason, Parameter)

  wq.comp$PercentCompleteness <- round(wq.comp$PercentCompleteness, 2)

  return(wq.comp)
}


#' Calculate percentage of data rated at each grade level for each water quality parameter for each field season between the index period of July 1 to September 15 (77 days). Long format for ease of plotting.
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
#'     qcWqGradesLong()
#'     qcWqGradesLong(site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#' }
qcWqGradesLong <- function(park, site, field.season) {

wq.long <- WqDailyMeanLong(park = park, site = site, field.season = field.season)

wq.grds.long <- wq.long |>
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) |>
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) |>
  dplyr::select(-c(Month, Day)) |>
  dplyr::group_by(Park, SiteShort, SiteCode, SiteName, SampleFrame, FieldSeason, Parameter, Units, Grade) |>
  dplyr::summarise(Days = dplyr::n()) |>
  dplyr::mutate(Percent = Days/sum(Days)*100) |>
  dplyr::ungroup() |>
  tidyr::complete(FieldSeason, tidyr::nesting(Park, SiteShort, SiteCode, SiteName, SampleFrame, Parameter, Units), fill = list(Days = 0, Percent = 0)) |>
  dplyr::relocate(FieldSeason, .after = SampleFrame) |>
  dplyr::arrange(SiteCode, FieldSeason, Parameter, Grade)

return(wq.grds.long)
}


#' Calculate the percentage of data rated at each grade level for each water quality parameter for each field season between the index period of July 1 to September 15 (77 days).
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
#'     qcWqGrades()
#'     qcWqGrades(site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#' }
qcWqGrades <- function(park, site, field.season) {

wq.daily <- WqDailyMean(park = park, site = site, field.season = field.season)

wt.grds <- wq.daily |>
  dplyr::group_by(Park,
                  SiteShort,
                  SiteCode,
                  SiteName,
                  SampleFrame,
                  FieldSeason) |>
  dplyr::mutate(Month = lubridate::month(Date),
                    Day = lubridate::day(Date)) |>
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) |>
  dplyr::summarise(DaysExcellent = sum(Temp_C_Grade %in% c("Excellent", "Est. Excellent")),
                   DaysGood = sum(Temp_C_Grade %in% c("Good", "Est. Good")),
                   DaysFair = sum(Temp_C_Grade %in% c("Fair", "Est. Fair")),
                   DaysPoor = sum(Temp_C_Grade %in% c("Poor", "Est. Poor"))) |>
  dplyr::rowwise() |>
  dplyr::mutate(TotalDays = sum(c_across(where(is.integer)))) |>
  dplyr::mutate(PercentExcellent = DaysExcellent/TotalDays*100,
                PercentGood = DaysGood/TotalDays*100,
                PercentFair = DaysFair/TotalDays*100,
                PercentPoor = DaysPoor/TotalDays*100) |>
  dplyr::mutate(Parameter = "Temperature") |>
  dplyr::mutate(Units = "C") |>
  dplyr::ungroup()

ph.grds <- wq.daily |>
  dplyr::group_by(Park,
                  SiteShort,
                  SiteCode,
                  SiteName,
                  SampleFrame,
                  FieldSeason) |>
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) |>
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) |>
  dplyr::summarise(DaysExcellent = sum(pH_Grade %in% c("Excellent", "Est. Excellent")),
                   DaysGood = sum(pH_Grade %in% c("Good", "Est. Good")),
                   DaysFair = sum(pH_Grade %in% c("Fair", "Est. Fair")),
                   DaysPoor = sum(pH_Grade %in% c("Poor", "Est. Poor"))) |>
  dplyr::rowwise() |>
  dplyr::mutate(TotalDays = sum(c_across(where(is.integer)))) |>
  dplyr::mutate(PercentExcellent = DaysExcellent/TotalDays*100,
                PercentGood = DaysGood/TotalDays*100,
                PercentFair = DaysFair/TotalDays*100,
                PercentPoor = DaysPoor/TotalDays*100) |>
  dplyr::mutate(Parameter = "pH") |>
  dplyr::mutate(Units = "units") |>
  dplyr::ungroup()

sc.grds <- wq.daily |>
  dplyr::group_by(Park,
                  SiteShort,
                  SiteCode,
                  SiteName,
                  SampleFrame,
                  FieldSeason) |>
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) |>
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) |>
  dplyr::summarise(DaysExcellent = sum(SpCond_uScm_Grade %in% c("Excellent", "Est. Excellent")),
                   DaysGood = sum(SpCond_uScm_Grade %in% c("Good", "Est. Good")),
                   DaysFair = sum(SpCond_uScm_Grade %in% c("Fair", "Est. Fair")),
                   DaysPoor = sum(SpCond_uScm_Grade %in% c("Poor", "Est. Poor"))) |>
  dplyr::rowwise() |>
  dplyr::mutate(TotalDays = sum(c_across(where(is.integer)))) |>
  dplyr::mutate(PercentExcellent = DaysExcellent/TotalDays*100,
                PercentGood = DaysGood/TotalDays*100,
                PercentFair = DaysFair/TotalDays*100,
                PercentPoor = DaysPoor/TotalDays*100) |>
  dplyr::mutate(Parameter = "SpCond") |>
  dplyr::mutate(Units = "uS/cm") |>
  dplyr::ungroup()

do.pct.grds <- wq.daily |>
  dplyr::group_by(Park,
                  SiteShort,
                  SiteCode,
                  SiteName,
                  SampleFrame,
                  FieldSeason) |>
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) |>
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) |>
  dplyr::summarise(DaysExcellent = sum(DO_pct_Grade %in% c("Excellent", "Est. Excellent")),
                   DaysGood = sum(DO_pct_Grade %in% c("Good", "Est. Good")),
                   DaysFair = sum(DO_pct_Grade %in% c("Fair", "Est. Fair")),
                   DaysPoor = sum(DO_pct_Grade %in% c("Poor", "Est. Poor"))) |>
  dplyr::rowwise() |>
  dplyr::mutate(TotalDays = sum(c_across(where(is.integer)))) |>
  dplyr::mutate(PercentExcellent = DaysExcellent/TotalDays*100,
                PercentGood = DaysGood/TotalDays*100,
                PercentFair = DaysFair/TotalDays*100,
                PercentPoor = DaysPoor/TotalDays*100) |>
  dplyr::mutate(Parameter = "DO") |>
  dplyr::mutate(Units = "%") |>
  dplyr::ungroup()

if ("DO_mgL" %in% colnames(wq.daily)) {

do.mgl.grds <- wq.daily |>
  dplyr::group_by(Park,
                  SiteShort,
                  SiteCode,
                  SiteName,
                  SampleFrame,
                  FieldSeason) |>
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) |>
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) |>
  dplyr::summarise(DaysExcellent = sum(DO_mgL_Grade %in% c("Excellent", "Est. Excellent")),
                   DaysGood = sum(DO_mgL_Grade %in% c("Good", "Est. Good")),
                   DaysFair = sum(DO_mgL_Grade %in% c("Fair", "Est. Fair")),
                   DaysPoor = sum(DO_mgL_Grade %in% c("Poor", "Est. Poor"))) |>
  dplyr::rowwise() |>
  dplyr::mutate(TotalDays = sum(c_across(where(is.integer)))) |>
  dplyr::mutate(PercentExcellent = DaysExcellent/TotalDays*100,
                PercentGood = DaysGood/TotalDays*100,
                PercentFair = DaysFair/TotalDays*100,
                PercentPoor = DaysPoor/TotalDays*100) |>
  dplyr::mutate(Parameter = "DO") |>
  dplyr::mutate(Units = "mg/L") |>
  dplyr::ungroup()

} else {

do.mgl.grds <- tibble::tibble(Park = character(),
                              SiteShort = character(),
                              SiteCode = character(),
                              SiteName = character(),
                              SampleFrame = character(),
                              FieldSeason = character(),
                              Parameter = character(),
                              Units = character(),
                              DaysExcellent = integer(),
                              PercentExcellent = double(),
                              DaysGood = integer(),
                              PercentGood = double(),
                              DaysFair = integer(),
                              PercentFair = double(),
                              DaysPoor = integer(),
                              PercentPoor = double())

}

wq.grds <- dplyr::bind_rows(wt.grds, ph.grds, sc.grds, do.pct.grds, do.mgl.grds) |>
      dplyr::select(Park,
                    SiteShort,
                    SiteCode,
                    SiteName,
                    SampleFrame,
                    FieldSeason,
                    Parameter,
                    Units,
                    DaysExcellent,
                    PercentExcellent,
                    DaysGood,
                    PercentGood,
                    DaysFair,
                    PercentFair,
                    DaysPoor,
                    PercentPoor) |>
    tidyr::complete(FieldSeason, nesting(Park, SiteShort, SiteCode, SiteName, SampleFrame, Parameter, Units), fill = list(DaysExcellent = 0,
                                                                                                                          PercentExcellent = 0,
                                                                                                                          DaysGood = 0,
                                                                                                                          PercentGood = 0,
                                                                                                                          DaysFair = 0,
                                                                                                                          PercentFair = 0,
                                                                                                                          DaysPoor = 0,
                                                                                                                          PercentPoor = 0)) |>
    dplyr::relocate(FieldSeason, .after = SampleFrame) |>
    dplyr::arrange(SiteCode, FieldSeason, Parameter)

wq.grds$PercentExcellent <- round(wq.grds$PercentExcellent, 2)
wq.grds$PercentGood <- round(wq.grds$PercentGood, 2)
wq.grds$PercentFair <- round(wq.grds$PercentFair, 2)
wq.grds$PercentPoor <- round(wq.grds$PercentPoor, 2)

return(wq.grds)

}


#' Plot percent completeness for each water quality parameter for each stream for each field season.
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
#'     qcWqCompletenessPlot()
#'     qcWqCompletenessPlot(site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#' }
qcWqCompletenessPlot <- function(park, site, field.season) {

wq.comp <- qcWqCompleteness(park = park, site = site, field.season = field.season)

wq.comp.concat <- wq.comp |>
  tidyr::unite("Parameter", Parameter, Units, sep = "_")

wq.comp.concat$Parameter_f = factor(wq.comp.concat$Parameter, levels = c("Temperature_C", "pH_units", "SpCond_uS/cm", "DO_%", "DO_mg/L"))

wq.comp.plot <- ggplot2::ggplot(data = wq.comp.concat, ggplot2::aes(x = FieldSeason, y = PercentCompleteness)) +
  ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), color = "black") +
  ggplot2::facet_grid(Parameter_f~SiteCode) +
  ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom")

return(wq.comp.plot)

}


#' Plot the percentage of data rated at each grade level for each water quality parameter for each field season.
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
#'     qcWqGradesPlot()
#'     qcWqGradesPlot(site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#' }
qcWqGradesPlot <- function(park, site, field.season) {

wq.grds.long <- qcWqGradesLong(park = park, site = site, field.season = field.season)

wq.grds.concat <- wq.grds.long |>
  tidyr::unite("Parameter", Parameter, Units, sep = "_")

wq.grds.concat$Parameter_f = factor(wq.grds.concat$Parameter, levels = c("Temperature_C", "pH_units", "SpCond_uS/cm", "DO_%", "DO_mg/L"))
wq.grds.concat$Grade_f = factor(wq.grds.concat$Grade, levels = c("Excellent", "Est. Excellent", "Good", "Est. Good", "Fair", "Est. Fair", "Poor", "Est. Poor"))

wq.grds.plot <- ggplot2::ggplot(data = wq.grds.concat, ggplot2::aes(x = FieldSeason, y = Percent, fill = Grade_f)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(Parameter_f~SiteCode) +
  ggplot2::labs(fill = "Grade") +
  ggplot2::scale_fill_manual(values = c("forestgreen", "gold", "khaki1", "darkorange", "Red")) +
  ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom")

return(wq.grds.plot)

}


# NYI: Plot daily mean values for each water quality parameter for each stream for each year.

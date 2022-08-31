#' Return list of streams and lakes that were not visited for annual monitoring during a field season
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, SampleFrame, FieldSeason, VisitDate
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcNoAnnualVisit(conn)
#'     qcNoAnnualVisit(conn, site = "GRBA_L_DEAD0", field.season = c("2012", "2013", "2014", "2015"))
#'     CloseDatabaseConnection(conn)
#' }
qcNoAnnualVisit <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

visit.data <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Visit")
visit <- visit.data %>%
  select(Park, Subunit, SiteShort, SiteCode, SiteName, SampleFrame, VisitDate, FieldSeason, VisitType, MonitoringStatus) %>%
  filter(VisitType == "Primary", SiteCode != "GRBA_S_BAKR2") %>%
  pivot_wider(id_cols = c(Park, Subunit, SiteShort, SiteCode, SiteName, SampleFrame),
              names_from = FieldSeason,
              values_from = VisitDate) %>%
  pivot_longer(!c(Park, Subunit, SiteShort, SiteCode, SiteName, SampleFrame), names_to = "FieldSeason", values_to = "VisitDate") %>%
  filter(is.na(VisitDate)) %>%
  select(Park, SiteShort, SiteCode, SiteName, SampleFrame, FieldSeason, VisitDate)

return(visit)

}


#' Return list of site visits that have any data categorized as "Raw" or "Provisional"
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble with columns SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, VisitType, Visit.DPL, Chem.DPL, BMI.DPL, Channel.DPL, Clarity.DPL, LakeSurvey.DPL, LakeString.DPL, Xsection.DPL, TempC.DPL, pH.DPL, SpCond.DPL, DO.DPL
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcDPLCheck(conn)
#'     qcDPLCheck(conn, site = "GRBA_L_JHNS0", field.season = c("2018", "2019", "2020"))
#'     CloseDatabaseConnection(conn)
#' }
qcDPLCheck <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  visit <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Visit")
  chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")
  bmi <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "BMI")
  channel <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Channel")
  clarity <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Clarity")
  lakesurvey <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "LakeLevelSurvey")
  lakestring <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "LakeLevelString")
  xsection <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "WQStreamXSection")
  temp <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "WaterQualityTemperature")
  ph <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "WaterQualitypH")
  spcond <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "WaterQualitySpCond")
  do <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "WaterQualityDO")

  visit.DPL <- visit %>%
    rename(Visit.DPL = DataProcessingLevel) %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, SampleFrame, VisitType, Visit.DPL)
  chem.DPL <- chem %>%
    filter(SampleType == "Routine") %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, SampleFrame, DPL) %>%
    rename(Chem.DPL = DPL) %>%
    distinct()
  bmi.DPL <- bmi %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
    rename(BMI.DPL = DPL) %>%
    distinct()
  channel.DPL <- channel %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, DPL) %>%
    rename(Channel.DPL = DPL) %>%
    distinct()
  clarity.DPL <- clarity %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
    rename(Clarity.DPL = DPL) %>%
    distinct()
  lakesurvey.DPL <- lakesurvey %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
    rename(LakeSurvey.DPL = DPL) %>%
    distinct()
  lakestring.DPL <- lakestring %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
    rename(LakeString.DPL = DPL) %>%
    distinct()
  xsection.DPL <- xsection %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
    rename(Xsection.DPL = DPL) %>%
    distinct()
  temp.DPL <- temp %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
    rename(TempC.DPL = DPL) %>%
    distinct()
  ph.DPL <- ph %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
    rename(pH.DPL = DPL) %>%
    distinct()
  spcond.DPL <- spcond %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
    rename(SpCond.DPL = DPL) %>%
    distinct()
  do.DPL <- do %>%
    select(SiteCode, SiteName, VisitDate, FieldSeason, VisitType, DPL) %>%
    rename(DO.DPL = DPL) %>%
    distinct()

  dpl <- visit.DPL %>%
    left_join(chem.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "SampleFrame", "VisitType")) %>%
    left_join(bmi.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
    left_join(channel.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason")) %>%
    left_join(clarity.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
    left_join(lakesurvey.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
    left_join(lakestring.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
    left_join(xsection.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
    left_join(temp.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
    left_join(ph.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
    left_join(spcond.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
    left_join(do.DPL, by = c("SiteCode", "SiteName", "VisitDate", "FieldSeason", "VisitType")) %>%
    unique() %>%
    filter_all(any_vars(. %in% c("Raw", "Provisional"))) %>%
    arrange(SampleFrame, FieldSeason, SiteCode)

return(dpl)

}


#' Calculate daily mean values (daily median values for pH) for water quality parameters at streams based on hourly data. Determine the most frequent data grade level for each day based on hourly data. Include only those dates with greater than 80% completeness (greater than 19 hourly values). Long format for ease of plotting.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, SampleFrame, FieldSeason, Date, Parameter, Units, Value, Grade.
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     WqDailyMeanLong(conn)
#'     WqDailyMeanLong(conn, site = "GRBA_S_LHMN1", field.season = c("2012", "2013", "2014", "2015"))
#'     CloseDatabaseConnection(conn)
#' }
WqDailyMeanLong <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

  wt <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "TimeseriesTemperature")
  ph <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "TimeseriespH")
  sc <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "TimeseriesSpCond")
  do.pct <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "TimeseriesDOSat")
  do.mgl <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "TimeseriesDO")
  visit <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Visit")

  wt.long <- wt %>%
    dplyr::filter(Approval == "Approved") %>%
    dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
    dplyr::group_by(Park,
                    SiteCode,
                    SampleFrame,
                    FieldSeason,
                    Date) %>%
    dplyr::summarise(Value = case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(WaterTemperature_C)) > 77 ~ mean(WaterTemperature_C, na.rm = TRUE),
                                       !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(WaterTemperature_C)) > 19 ~ mean(WaterTemperature_C, na.rm = TRUE),
                                       TRUE ~ as.double(NA_integer_)),
                     Grade = case_when(!is.na(WaterTemperature_C) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
    unique() %>%
    dplyr::mutate(Parameter = "Temperature") %>%
    dplyr::mutate(Units = "C") %>%
    dplyr::arrange(Park, SiteCode, Date) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::relocate(Parameter, .after = Date) %>%
    dplyr::relocate(Units, .after = Parameter) %>%
    dplyr::ungroup()

  ph.long <- ph %>%
    dplyr::filter(Approval == "Approved") %>%
    dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
    dplyr::group_by(Park,
                    SiteCode,
                    SampleFrame,
                    FieldSeason,
                    Date) %>%
    dplyr::summarise(Value = case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(pH)) > 77 ~ median(pH, na.rm = TRUE),
                                       !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(pH)) > 19 ~ median(pH, na.rm = TRUE),
                                       TRUE ~ as.double(NA_integer_)),
                     Grade = case_when(!is.na(pH) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
    unique() %>%
    dplyr::mutate(Parameter = "pH") %>%
    dplyr::mutate(Units = "units") %>%
    dplyr::arrange(Park, SiteCode, Date) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::relocate(Parameter, .after = Date) %>%
    dplyr::relocate(Units, .after = Parameter) %>%
    dplyr::ungroup()

  sc.long <- sc %>%
    dplyr::filter(Approval == "Approved") %>%
    dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
    dplyr::group_by(Park,
                    SiteCode,
                    SampleFrame,
                    FieldSeason,
                    Date) %>%
    dplyr::summarise(Value = case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(SpecificConductance_microS_per_cm)) > 77 ~ mean(SpecificConductance_microS_per_cm, na.rm = TRUE),
                                       !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(SpecificConductance_microS_per_cm)) > 19 ~ mean(SpecificConductance_microS_per_cm, na.rm = TRUE),
                                       TRUE ~ as.double(NA_integer_)),
                     Grade = case_when(!is.na(SpecificConductance_microS_per_cm) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
    unique() %>%
    dplyr::mutate(Parameter = "SpCond") %>%
    dplyr::mutate(Units = "uS/cm") %>%
    dplyr::arrange(Park, SiteCode, Date) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::relocate(Parameter, .after = Date) %>%
    dplyr::relocate(Units, .after = Parameter) %>%
    dplyr::ungroup()

  do.pct.long <- do.pct %>%
    dplyr::filter(Approval == "Approved") %>%
    dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
    dplyr::group_by(Park,
                    SiteCode,
                    SampleFrame,
                    FieldSeason,
                    Date) %>%
    dplyr::summarise(Value = case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(DissolvedOxygen_percent)) > 77 ~ mean(DissolvedOxygen_percent, na.rm = TRUE),
                                       !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(DissolvedOxygen_percent)) > 19 ~ mean(DissolvedOxygen_percent, na.rm = TRUE),
                                       TRUE ~ as.double(NA_integer_)),
                     Grade = case_when(!is.na(DissolvedOxygen_percent) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
    unique() %>%
    dplyr::mutate(Parameter = "DO") %>%
    dplyr::mutate(Units = "%") %>%
    dplyr::arrange(Park, SiteCode, Date) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::relocate(Parameter, .after = Date) %>%
    dplyr::relocate(Units, .after = Parameter) %>%
    dplyr::ungroup()

  do.mgl.long <- do.mgl %>%
    dplyr::filter(Approval == "Approved") %>%
    dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
    dplyr::group_by(Park,
                    SiteCode,
                    SampleFrame,
                    FieldSeason,
                    Date) %>%
    dplyr::rename(DissolvedOxygen_mgL = DissolvedOxygen_mg_per_L) %>%
    dplyr::summarise(Value = case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(DissolvedOxygen_mgL)) > 77 ~ mean(DissolvedOxygen_mgL, na.rm = TRUE),
                                       !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(DissolvedOxygen_mgL)) > 19 ~ mean(DissolvedOxygen_mgL, na.rm = TRUE),
                                       TRUE ~ as.double(NA_integer_)),
                     Grade = case_when(!is.na(DissolvedOxygen_mgL) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
    unique() %>%
    dplyr::mutate(Parameter = "DO") %>%
    dplyr::mutate(Units = "mg/L") %>%
    dplyr::arrange(Park, SiteCode, Date) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::relocate(Parameter, .after = Date) %>%
    dplyr::relocate(Units, .after = Parameter) %>%
    dplyr::mutate(Grade = as.character(Grade)) %>%
    dplyr::ungroup()

  wq.long.int <- dplyr::bind_rows(wt.long, ph.long, sc.long, do.pct.long, do.mgl.long)

  gage.locations <- tibble::tibble(SiteShort = c("BAKR1", "SNKE1", "SNKE3", "STRW1"),
                                   SiteCode = c("GRBA_S_BAKR1", "GRBA_S_SNKE1", "GRBA_S_SNKE3", "GRBA_S_STRW1"),
                                   SiteName = c("Baker Creek (Gage)", "Snake Creek (Lower)", "Snake Creek (Upper)", "Strawberry Creek (Gage)"))

  visit.names <- visit %>%
    dplyr::select(SiteShort, SiteCode, SiteName) %>%
    unique() %>%
    dplyr::bind_rows(gage.locations)

  wq.long <- left_join(wq.long.int, visit.names, by = c("SiteCode")) %>%
    dplyr::relocate(SiteShort, .before = SiteCode) %>%
    dplyr::relocate(SiteName, .after = SiteCode)

  return(wq.long)

}


#' Return summary of daily mean values (daily median values for pH) and grades for water quality parameters at streams.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, SiteType, Date, FieldSeason, Temp_C, Temp_C_Grade, pH, pH_Grade, SpCond_uScm, SpCond_uScm_Grade, DO_pct, DO_pct_Grade, DO_mgL, DO_mgL_Grade
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     WqDailyMean(conn)
#'     WqDailyMean(conn, site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#'     CloseDatabaseConnection(conn)
#' }
WqDailyMean <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

wq.long <- WqDailyMeanLong(conn, path.to.data, park, site, field.season, data.source)

wq.daily <- wq.long %>%
  tidyr::unite(Parameter, c("Parameter", "Units")) %>%
  tidyr::pivot_wider(names_from = Parameter,
                     values_from = c(Value, Grade)) %>%
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
                warn_missing = FALSE) %>%
  dplyr::select(Park, SiteShort, SiteCode, SiteName, SampleFrame, Date, FieldSeason, Temp_C, Temp_C_Grade, pH, pH_Grade, SpCond_uScm, SpCond_uScm_Grade, DO_pct, DO_pct_Grade, everything())

return(wq.daily)

}


#' Calculate the number and percentage of days of data for each water quality parameter for each field season between the index period of July 1 to September 15 (77 days).
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcWqCompleteness(conn)
#'     qcWqCompleteness(conn, site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#'     CloseDatabaseConnection(conn)
#' }
qcWqCompleteness <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

wq.long <- WqDailyMeanLong(conn, path.to.data, park, site, field.season, data.source)

wq.comp <- wq.long %>%
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) %>%
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) %>%
  dplyr::group_by(Park,
                  SiteShort,
                  SiteCode,
                  SiteName,
                  SampleFrame,
                  FieldSeason,
                  Parameter,
                  Units) %>%
  dplyr::summarise(CompletedDays = sum(!is.na(Value))) %>%
  dplyr::mutate(PercentCompleteness = CompletedDays/77*100) %>%
  dplyr::ungroup() %>%
  tidyr::complete(FieldSeason, nesting(Park, SiteShort, SiteCode, SiteName, SampleFrame, Parameter, Units), fill = list(CompletedDays = 0, PercentCompleteness = 0)) %>%
  dplyr::relocate(FieldSeason, .after = SampleFrame) %>%
  dplyr::arrange(SiteCode, FieldSeason, Parameter)

wq.comp$PercentCompleteness <- round(wq.comp$PercentCompleteness, 2)

return(wq.comp)

}


#' Calculate percentage of data rated at each grade level for each water quality parameter for each field season between the index period of July 1 to September 15 (77 days). Long format for ease of plotting.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcWqGradesLong(conn)
#'     qcWqGradesLong(conn, site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#'     CloseDatabaseConnection(conn)
#' }
qcWqGradesLong <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

wq.long <- WqDailyMeanLong(conn, path.to.data, park, site, field.season, data.source)

wq.grds.long <- wq.long %>%
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) %>%
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) %>%
  dplyr::select(-c(Month, Day)) %>%
  dplyr::group_by(Park, SiteShort, SiteCode, SiteName, SampleFrame, FieldSeason, Parameter, Units, Grade) %>%
  dplyr::summarise(Days = n()) %>%
  dplyr::mutate(Percent = Days/sum(Days)*100) %>%
  dplyr::ungroup() %>%
  tidyr::complete(FieldSeason, nesting(Park, SiteShort, SiteCode, SiteName, SampleFrame, Parameter, Units), fill = list(Days = 0, Percent = 0)) %>%
  dplyr::relocate(FieldSeason, .after = SampleFrame) %>%
  dplyr::arrange(SiteCode, FieldSeason, Parameter, Grade)


return(wq.grds.long)

}


#' Calculate the percentage of data rated at each grade level for each water quality parameter for each field season between the index period of July 1 to September 15 (77 days).
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcWqGrades(conn)
#'     qcWqGrades(conn, site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#'     CloseDatabaseConnection(conn)
#' }
qcWqGrades <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

wq.daily <- WqDailyMean(conn, path.to.data, park, site, field.season, data.source)

wt.grds <- wq.daily %>%
  dplyr::group_by(Park,
                  SiteShort,
                  SiteCode,
                  SiteName,
                  SampleFrame,
                  FieldSeason) %>%
  dplyr::mutate(Month = lubridate::month(Date),
                    Day = lubridate::day(Date)) %>%
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) %>%
  dplyr::summarise(DaysExcellent = sum(Temp_C_Grade %in% c("Excellent", "Est. Excellent")),
                   DaysGood = sum(Temp_C_Grade %in% c("Good", "Est. Good")),
                   DaysFair = sum(Temp_C_Grade %in% c("Fair", "Est. Fair")),
                   DaysPoor = sum(Temp_C_Grade %in% c("Poor", "Est. Poor"))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(TotalDays = sum(c_across(where(is.integer)))) %>%
  dplyr::mutate(PercentExcellent = DaysExcellent/TotalDays*100,
                PercentGood = DaysGood/TotalDays*100,
                PercentFair = DaysFair/TotalDays*100,
                PercentPoor = DaysPoor/TotalDays*100) %>%
  dplyr::mutate(Parameter = "Temperature") %>%
  dplyr::mutate(Units = "C") %>%
  dplyr::ungroup()

ph.grds <- wq.daily %>%
  dplyr::group_by(Park,
                  SiteShort,
                  SiteCode,
                  SiteName,
                  SampleFrame,
                  FieldSeason) %>%
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) %>%
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) %>%
  dplyr::summarise(DaysExcellent = sum(pH_Grade %in% c("Excellent", "Est. Excellent")),
                   DaysGood = sum(pH_Grade %in% c("Good", "Est. Good")),
                   DaysFair = sum(pH_Grade %in% c("Fair", "Est. Fair")),
                   DaysPoor = sum(pH_Grade %in% c("Poor", "Est. Poor"))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(TotalDays = sum(c_across(where(is.integer)))) %>%
  dplyr::mutate(PercentExcellent = DaysExcellent/TotalDays*100,
                PercentGood = DaysGood/TotalDays*100,
                PercentFair = DaysFair/TotalDays*100,
                PercentPoor = DaysPoor/TotalDays*100) %>%
  dplyr::mutate(Parameter = "pH") %>%
  dplyr::mutate(Units = "units") %>%
  dplyr::ungroup()

sc.grds <- wq.daily %>%
  dplyr::group_by(Park,
                  SiteShort,
                  SiteCode,
                  SiteName,
                  SampleFrame,
                  FieldSeason) %>%
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) %>%
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) %>%
  dplyr::summarise(DaysExcellent = sum(SpCond_uScm_Grade %in% c("Excellent", "Est. Excellent")),
                   DaysGood = sum(SpCond_uScm_Grade %in% c("Good", "Est. Good")),
                   DaysFair = sum(SpCond_uScm_Grade %in% c("Fair", "Est. Fair")),
                   DaysPoor = sum(SpCond_uScm_Grade %in% c("Poor", "Est. Poor"))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(TotalDays = sum(c_across(where(is.integer)))) %>%
  dplyr::mutate(PercentExcellent = DaysExcellent/TotalDays*100,
                PercentGood = DaysGood/TotalDays*100,
                PercentFair = DaysFair/TotalDays*100,
                PercentPoor = DaysPoor/TotalDays*100) %>%
  dplyr::mutate(Parameter = "SpCond") %>%
  dplyr::mutate(Units = "uS/cm") %>%
  dplyr::ungroup()

do.pct.grds <- wq.daily %>%
  dplyr::group_by(Park,
                  SiteShort,
                  SiteCode,
                  SiteName,
                  SampleFrame,
                  FieldSeason) %>%
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) %>%
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) %>%
  dplyr::summarise(DaysExcellent = sum(DO_pct_Grade %in% c("Excellent", "Est. Excellent")),
                   DaysGood = sum(DO_pct_Grade %in% c("Good", "Est. Good")),
                   DaysFair = sum(DO_pct_Grade %in% c("Fair", "Est. Fair")),
                   DaysPoor = sum(DO_pct_Grade %in% c("Poor", "Est. Poor"))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(TotalDays = sum(c_across(where(is.integer)))) %>%
  dplyr::mutate(PercentExcellent = DaysExcellent/TotalDays*100,
                PercentGood = DaysGood/TotalDays*100,
                PercentFair = DaysFair/TotalDays*100,
                PercentPoor = DaysPoor/TotalDays*100) %>%
  dplyr::mutate(Parameter = "DO") %>%
  dplyr::mutate(Units = "%") %>%
  dplyr::ungroup()

if ("DO_mgL" %in% colnames(wq.daily)) {

do.mgl.grds <- wq.daily %>%
  dplyr::group_by(Park,
                  SiteShort,
                  SiteCode,
                  SiteName,
                  SampleFrame,
                  FieldSeason) %>%
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) %>%
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) %>%
  dplyr::summarise(DaysExcellent = sum(DO_mgL_Grade %in% c("Excellent", "Est. Excellent")),
                   DaysGood = sum(DO_mgL_Grade %in% c("Good", "Est. Good")),
                   DaysFair = sum(DO_mgL_Grade %in% c("Fair", "Est. Fair")),
                   DaysPoor = sum(DO_mgL_Grade %in% c("Poor", "Est. Poor"))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(TotalDays = sum(c_across(where(is.integer)))) %>%
  dplyr::mutate(PercentExcellent = DaysExcellent/TotalDays*100,
                PercentGood = DaysGood/TotalDays*100,
                PercentFair = DaysFair/TotalDays*100,
                PercentPoor = DaysPoor/TotalDays*100) %>%
  dplyr::mutate(Parameter = "DO") %>%
  dplyr::mutate(Units = "mg/L") %>%
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

wq.grds <- dplyr::bind_rows(wt.grds, ph.grds, sc.grds, do.pct.grds, do.mgl.grds) %>%
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
                    PercentPoor) %>%
    tidyr::complete(FieldSeason, nesting(Park, SiteShort, SiteCode, SiteName, SampleFrame, Parameter, Units), fill = list(DaysExcellent = 0,
                                                                                                                          PercentExcellent = 0,
                                                                                                                          DaysGood = 0,
                                                                                                                          PercentGood = 0,
                                                                                                                          DaysFair = 0,
                                                                                                                          PercentFair = 0,
                                                                                                                          DaysPoor = 0,
                                                                                                                          PercentPoor = 0)) %>%
    dplyr::relocate(FieldSeason, .after = SampleFrame) %>%
    dplyr::arrange(SiteCode, FieldSeason, Parameter)

wq.grds$PercentExcellent <- round(wq.grds$PercentExcellent, 2)
wq.grds$PercentGood <- round(wq.grds$PercentGood, 2)
wq.grds$PercentFair <- round(wq.grds$PercentFair, 2)
wq.grds$PercentPoor <- round(wq.grds$PercentPoor, 2)

return(wq.grds)

}


#' Plot percent completeness for each water quality parameter for each stream for each field season.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcWqCompletenessPlot(conn)
#'     qcWqCompletenessPlot(conn, site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#'     CloseDatabaseConnection(conn)
#' }
qcWqCompletenessPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

wq.comp <- qcWqCompleteness(conn, path.to.data, park, site, field.season, data.source)

wq.comp.concat <- wq.comp %>%
  tidyr::unite("Parameter", Parameter, Units, sep = "_")

wq.comp.concat$Parameter_f = factor(wq.comp.concat$Parameter, levels = c("Temperature_C", "pH_units", "SpCond_uS/cm", "DO_%", "DO_mg/L"))

wq.comp.plot <- ggplot(data = wq.comp.concat, aes(x = FieldSeason, y = PercentCompleteness)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  facet_grid(Parameter_f~SiteCode) +
  scale_x_discrete(breaks = scales::pretty_breaks()) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")

return(wq.comp.plot)

}


#' Plot the percentage of data rated at each grade level for each water quality parameter for each field season.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- OpenDatabaseConnection()
#'     qcWqGradesPlot(conn)
#'     qcWqGradesPlot(conn, site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
#'     CloseDatabaseConnection(conn)
#' }
qcWqGradesPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

wq.grds.long <- qcWqGradesLong(conn, path.to.data, park, site, field.season, data.source)

wq.grds.concat <- wq.grds.long %>%
  tidyr::unite("Parameter", Parameter, Units, sep = "_")

wq.grds.concat$Parameter_f = factor(wq.grds.concat$Parameter, levels = c("Temperature_C", "pH_units", "SpCond_uS/cm", "DO_%", "DO_mg/L"))
wq.grds.concat$Grade_f = factor(wq.grds.concat$Grade, levels = c("Excellent", "Est. Excellent", "Good", "Est. Good", "Fair", "Est. Fair", "Poor", "Est. Poor"))

wq.grds.plot <- ggplot(data = wq.grds.concat, aes(x = FieldSeason, y = Percent, fill = Grade_f)) +
  geom_col() +
  facet_grid(Parameter_f~SiteCode) +
  labs(fill = "Grade") +
  scale_fill_manual(values = c("forestgreen", "gold", "khaki1", "darkorange", "Red")) +
  scale_x_discrete(breaks = scales::pretty_breaks()) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")

return(wq.grds.plot)

}


# NYI: Plot daily mean values for each water quality parameter for each stream for each year.

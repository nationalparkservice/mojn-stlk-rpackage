#' List all laboratory values that have an "Information," "Warning," or "Critical" flag.
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
#' qcChemFlags()
#' qcChemFlags(site = c("GRBA_S_MILL1", "GRBA_S_PINE1", "GRBA_S_RDGE1"), field.season = c("2018", "2019", "2020"))
#' }
qcChemFlags <- function(park, site, field.season) {
    chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Chemistry")

    flags.list <- chem |>
      dplyr::filter(Flag %in% c("I", "W", "C")) |>
      dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType, Flag, FlagNote) |>
      dplyr::arrange(SampleFrame, VisitDate, SiteCode)

return(flags.list)

}


#' Calculate the relative percent difference (RPD) for laboratory duplicates and triplicates, flag results that exceed the 30% MQO threshold, and list all RPD values and flags.
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
#' qcChemLabDupes()
#' qcChemLabDupes(site = c("GRBA_L_DEAD0", "GRBA_L_JHNS0"), field.season = c("2018", "2019", "2020"))
#' }
qcChemLabDupes <- function(park, site, field.season) {
    chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Chemistry")

    lab.dupes <- chem |>
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType) |>
        dplyr::filter(SiteCode != "GRBA_L_STLL0s", SampleType %in% c("Routine", "Lab Duplicate", "Lab Triplicate"))

    lab.dupes.wide <- tidyr::pivot_wider(data = lab.dupes, names_from = SampleType, values_from = LabValue)

    lab.dupes.wide <- if ("Lab Duplicate" %in% names(lab.dupes.wide)) {

        dplyr::rename(lab.dupes.wide, LabDuplicate = `Lab Duplicate`)

    } else {

        dplyr::mutate(lab.dupes.wide, LabDuplicate = NA)

    }

    lab.dupes.wide <- if ("Lab Triplicate" %in% names(lab.dupes.wide)) {

        dplyr::rename(lab.dupes.wide, LabTriplicate = `Lab Triplicate`)

    } else {

        dplyr::mutate(lab.dupes.wide, LabTriplicate = NA)

    }

    lab.dupes.list <- lab.dupes.wide |>
        dplyr::filter(!is.na(LabDuplicate)) |>
        dplyr::mutate(RPD = round(((pmax(Routine, LabDuplicate) - pmin(Routine, LabDuplicate))/((pmax(Routine, LabDuplicate) + pmin(Routine, LabDuplicate))/2))*100, 2)) |>
        dplyr::mutate(RPD2 = round(((pmax(Routine, LabTriplicate) - pmin(Routine, LabTriplicate))/((pmax(Routine, LabTriplicate) + pmin(Routine, LabTriplicate))/2))*100, 2)) |>
        dplyr::mutate(RPDFlag = ifelse(RPD > 30 | RPD2 > 30, "RPD above laboratory precision MQO of 30%", NA)) |>
        dplyr::arrange(desc(RPD))

    return(lab.dupes.list)

}


#' Calculate the relative percent difference (RPD) for field duplicates, flag results that exceed the 30% MQO threshold, and list all RPD values and flags.
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
#' qcChemFieldDupes()
#' qcChemFieldDupes(site = c("GRBA_L_DEAD0", "GRBA_L_JHNS0"), field.season = c("2018", "2019", "2020"))
#' }
qcChemFieldDupes <- function(park, site, field.season) {
    chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Chemistry")

    field.dupes <- chem |>
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType) |>
        dplyr::filter(SiteCode != "GRBA_L_STLL0s", SampleType %in% c("Routine", "Field Duplicate"))

    field.dupes.wide <- tidyr::pivot_wider(data = field.dupes, names_from = SampleType, values_from = LabValue)


    field.dupes.wide <- if ("Field Duplicate" %in% names(field.dupes.wide)) {

        dplyr::rename(field.dupes.wide, FieldDuplicate = `Field Duplicate`)

    } else {

        dplyr::mutate(field.dupes.wide, FieldDuplicate = NA)

    }

    field.dupes.list <- field.dupes.wide |>
        dplyr::filter(!is.na(FieldDuplicate)) |>
        dplyr::mutate(RPD = round(((pmax(Routine, FieldDuplicate) - pmin(Routine, FieldDuplicate))/((pmax(Routine, FieldDuplicate) + pmin(Routine, FieldDuplicate))/2))*100, 2)) |>
        dplyr::mutate(RPDFlag = ifelse(RPD > 30, "RPD above laboratory precision MQO of 30%", NA)) |>
        dplyr::arrange(desc(RPD))

    return(field.dupes.list)

}


#' List all laboratory values from field blanks that exceed the minimum detection level (MDL) for that analyte.
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
#' qcChemFieldBlanks()
#' qcChemFieldBlanks(site = c("GRBA_L_DEAD0", "GRBA_L_JHNS0"), field.season = c("2018", "2019", "2020"))
#' }
qcChemFieldBlanks <- function(park, site, field.season) {
    chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Chemistry")
    lookup <- getMDLLookup()

    field.blanks <- chem |>
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType) |>
        dplyr::filter(SiteCode != "GRBA_L_STLL0s", SampleType %in% c("Field Blank")) |>
        dplyr::mutate(FieldSeason = as.double(FieldSeason))

    field.blanks.merged <- fuzzyjoin::fuzzy_inner_join(x = field.blanks,
                                                       y = lookup,
                                                       by = c("Characteristic" = "Characteristic", "Unit" = "Unit", "FieldSeason" = "StartYear", "FieldSeason" = "EndYear"),
                                                       match_fun = list(`==`, `==`, `>=`, `<=`))

    field.blanks.list <- field.blanks.merged |>
        dplyr::rename(Characteristic = Characteristic.x, Unit = Unit.x) |>
        dplyr::mutate(FieldSeason = as.character(FieldSeason)) |>
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, MDL) |>
        dplyr::filter(LabValue > MDL)

    return(field.blanks.list)

}


#' List all routine samples where total dissolved nitrogen (TDN) values exceeded total nitrogen (UTN) values, and flag whether the discrepancy was within precision limits or outside of the expected error.
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
#' qcChemTDN()
#' qcChemTDN(site = c("GRBA_L_DEAD0", "GRBA_L_JHNS0"), field.season = c("2018", "2019", "2020"))
#' }
qcChemTDN <- function(park, site, field.season) {
    chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Chemistry")

    TDN <- chem |>
        dplyr::filter(VisitType == "Primary", SampleType == "Routine", ReportingGroup == "Nutrient", Characteristic %in% c("UTN", "TDN", "NO3NO2-N")) |>
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, Unit, LabValue)

    TDN.wide <- tidyr::pivot_wider(data = TDN, names_from = Characteristic, values_from = LabValue)

    TDN.list <- TDN.wide |>
        dplyr::rename(NO3NO2 = `NO3NO2-N`) |>
        dplyr::mutate(TDNvUTN = ifelse(TDN > UTN, round(TDN - UTN, 2), NA)) |>
        dplyr::mutate(TDNFlag = ifelse(TDNvUTN > 0.01, "TDN is greater than UTN outside the normal limits of variability", "TDN is greater than UTN within precision limits")) |>
        dplyr::filter(!is.na(TDNvUTN))

    return(TDN.list)

}


#' List all routine samples where nitrate and nitrite (NO3NO2-N) values exceeded either total dissolved nitrogen (TDN) values or total nitrogen (UTN) values, and flag whether the discrepancy was within precision limits or outside of the expected error.
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
#' qcChemNO3NO2()
#' qcChemNO3NO2(site = c("GRBA_L_DEAD0", "GRBA_L_JHNS0"), field.season = c("2018", "2019", "2020"))
#' }
qcChemNO3NO2 <- function(park, site, field.season) {
    chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Chemistry")

    NO3NO2 <- chem |>
        dplyr::filter(VisitType == "Primary", SampleType == "Routine", ReportingGroup == "Nutrient", Characteristic %in% c("UTN", "TDN", "NO3NO2-N")) |>
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, Unit, LabValue)

    NO3NO2.wide <- tidyr::pivot_wider(data = NO3NO2, names_from = Characteristic, values_from = LabValue)

    NO3NO2.list <- NO3NO2.wide |>
        dplyr::rename(NO3NO2 = `NO3NO2-N`) |>
        dplyr::mutate(NO3NO2vUTN = ifelse(NO3NO2 > UTN, round(NO3NO2 - UTN, 3), NA)) |>
        dplyr::mutate(NO3NO2vTDN = ifelse(NO3NO2 > TDN, round(NO3NO2 - TDN, 3), NA)) |>
        dplyr::mutate(NO3NO2Flag = ifelse(NO3NO2vUTN > 0.01 | NO3NO2vTDN > 0.01, "NO3NO2 is greater than UTN and/or TDN outside the normal limits of variability", "NO3NO2 is greater than TDN and/or UTN within precision limits")) |>
        dplyr::filter(!is.na(NO3NO2vUTN | NO3NO2vTDN))

    return(NO3NO2.list)

}


#' List all routine samples where total dissolved phosphorous (TDP) values exceeded total phosphorus (UTP) values, and flag whether the discrepancy was within precision limits or outside of the expected error.
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
#' qcChemTDP()
#' qcChemTDP(site = c("GRBA_L_DEAD0", "GRBA_L_JHNS0"), field.season = c("2018", "2019", "2020"))
#' }
qcChemTDP <- function(park, site, field.season) {
    chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Chemistry")

    TDP <- chem |>
        dplyr::filter(VisitType == "Primary", SampleType == "Routine", ReportingGroup == "Nutrient", Characteristic %in% c("UTP", "TDP")) |>
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, Unit, LabValue)

    TDP.wide <- tidyr::pivot_wider(data = TDP, names_from = Characteristic, values_from = LabValue)

    TDP.list <- TDP.wide |>
        dplyr::mutate(TDPvUTP = ifelse(TDP>UTP, round(TDP - UTP, 3), NA)) |>
        dplyr::mutate(TDPFlag = ifelse(TDPvUTP > 0.002, "TDP is greater than UTP outside the limits of normal variability", "TDP is greater than UTP within precision limits")) |>
        dplyr::filter(!is.na(TDPvUTP))

    return(TDP.list)

}


#' Create tibble with MDL and ML values for each characteristic.
#'
#' @return A tibble with columns Characteristic, Unit, StartYear, EndYear, MDL, ML.
#' @export
#'
getMDLLookup <- function() {
    lookup <- tibble::tibble(Characteristic = c("ALK2", "Ca", "DOC", "Cl", "Mg", "NO3NO2-N", "UTN", "UTP", "K", "Na", "SO4-S"),
                             Unit = c("mg CaCO3/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
                             StartYear = c(2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009),
                             EndYear = c(2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024),
                             MDL = c(0.2, 0.06, 0.05, 0.01, 0.02, 0.001, 0.01, 0.002, 0.03, 0.01, 0.01),
                             ML = c(0.6, 0.19, 0.16, 0.03, 0.06, 0.003, 0.03, 0.006, 0.10, 0.03, 0.03))

    return(lookup)

}


#' List all routine laboratory values that are less than or equal to the minimum detection level (MDL) for that analyte.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, MDL, MDLFlag.
#' @export
#'
#' @examples
#' \dontrun{
#' qcChemMDL()
#' qcChemMDL(site = c("GRBA_L_DEAD0", "GRBA_L_JHNS0"), field.season = c("2018", "2019", "2020"))
#' }
qcChemMDL <- function(park, site, field.season) {
    chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Chemistry")
    lookup <- getMDLLookup()

    mdl <- chem |>
        dplyr::filter(VisitType == "Primary", SampleType == "Routine") |>
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue)

    mdl.merged <- fuzzyjoin::fuzzy_inner_join(x = mdl,
                                            y = lookup,
                                            by = c("Characteristic" = "Characteristic", "Unit" = "Unit", "FieldSeason" = "StartYear", "FieldSeason" = "EndYear"),
                                            match_fun = list(`==`, `==`, `>=`, `<=`))

    mdl.list <- mdl.merged |>
        dplyr::rename(Characteristic = Characteristic.x, Unit = Unit.x) |>
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, MDL) |>
        dplyr::mutate(MDLFlag = ifelse(LabValue <= MDL, "Value is less than or equal to the minimum detection level (MDL)", NA)) |>
        dplyr::filter(!is.na(MDLFlag)) |>
        dplyr::arrange(SampleFrame, VisitDate, SiteCode)

    return(mdl.list)

}


#' List all routine laboratory values that are less than or equal to the minimum level of quantitation (ML) for that analyte.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble with columns SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, ML, MLFlag.
#' @export
#'
#' @examples
#' \dontrun{
#' qcChemML()
#' qcChemML(site = c("GRBA_L_DEAD0", "GRBA_L_JHNS0"), field.season = c("2018", "2019", "2020"))
#' }
qcChemML <- function(park, site, field.season) {
    chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Chemistry")

    ml <- chem |>
        dplyr::filter(VisitType == "Primary", SampleType == "Routine") |>
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue)

    ml.merged <- fuzzyjoin::fuzzy_inner_join(x = ml,
                                              y = lookup,
                                              by = c("Characteristic" = "Characteristic", "Unit" = "Unit", "FieldSeason" = "StartYear", "FieldSeason" = "EndYear"),
                                              match_fun = list(`==`, `==`, `>=`, `<=`))

    ml.list <- ml.merged |>
        dplyr::rename(Characteristic = Characteristic.x, Unit = Unit.x) |>
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, ML) |>
        dplyr::mutate(MLFlag = ifelse(LabValue <= ML, "Value is less than or equal to the minimum level of quantification (ML)", NA)) |>
        dplyr::filter(!is.na(MLFlag)) |>
        dplyr::arrange(SampleFrame, VisitDate, SiteCode)

    return(ml.list)

}


#' Calculate acid neutralizing capacity (ANC) from alkalinity (ALK2) and fill missing years with NAs for ease of plotting
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, FieldSeason, SampleFrame, VisitDate, VisitType, SampleCollectionMethod, Characteristic, CharacteristicLabel, LabValue, ReportingGroup, SampleType, Flag, FlagNote, DPL, Unit
#' @export
#'
#' @examples
#' \dontrun{
#' ChemANC()
#' ChemANC(site = "GRBA_L_DEAD0", field.season = "2018")
#' }
ChemFormatted <- function(park, site, field.season) {

    chem <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Chemistry")

    chem.anc.rows <- chem  |>
        dplyr::filter(Characteristic == "ALK2") |>
        dplyr::mutate(Characteristic = "ANC",
               CharacteristicLabel = "Acid neutralizing capacity",
               Unit = "ueq/L",
               Flag = "NF",
               FlagNote = NA,
               LabValue = LabValue*20)

    chem.joined <- rbind(chem, chem.anc.rows) |>
      dplyr::mutate(Year = as.integer(FieldSeason))

    min.year <- min(chem.joined$Year)
    max.year <- max(chem.joined$Year)

    chem.formatted <- chem.joined |>
      dplyr::group_by(Park, SiteShort, SiteCode, SiteName, SampleFrame, Characteristic, CharacteristicLabel, Unit, ReportingGroup) |>
      tidyr::complete(Year = tidyr::full_seq(min.year:max.year, 1)) |>
      dplyr::ungroup() |>
      dplyr::mutate(FieldSeason = dplyr::case_when(is.na(FieldSeason) ~ as.character(Year),
                                                   TRUE ~ FieldSeason)) |>
      dplyr::select(SampleFrame, Park, SiteCode, SiteShort, SiteName, FieldSeason, VisitDate, VisitType, SampleType, SampleCollectionMethod, ReportingGroup, Characteristic, CharacteristicLabel, Unit, LabValue, Flag, FlagNote, DPL) |>
      dplyr::arrange(SiteCode, FieldSeason, Characteristic)

    return(chem.formatted)

}

#' Plot acid neutralizing capacity (ANC) at lakes, and include EPA thresholds
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
#' ChemLakeANCPlot()
#' ChemLakeANCPlot(site = "GRBA_L_DEAD0")
#' }
#'
ChemLakeANCPlot <- function(park, site, field.season) {

    chem.anc <- ChemFormatted(park = park, site = site, field.season = field.season)

    chem.lake.anc <- chem.anc |>
        dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                      SampleType %in% c("Routine") | is.na(SampleType),
                      !(SiteShort %in% c("STLL0s")),
                      SampleFrame == "Lake",
                      Characteristic == "ANC")

    thresholds <- data.frame(yintercept = c(20, 50, 100, 200), Lines = c("Acute", "Severe", "Elevated", "Moderately Acidic"))

    chem.lake.anc.plot <- ggplot2::ggplot(chem.lake.anc,
                                          ggplot2::aes(x = FieldSeason,
                                                       y = LabValue,
                                                       color = SiteShort,
                                                       group = SiteShort)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(linewidth = 1) +
      # ggplot2::facet_grid(~SiteShort, scales = "free_y") +
      ggplot2::ylab(label = "Acid Neutralizing Capacity (ueq/L)") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      ggplot2::labs(title = "Lake acid neutralizing capacity") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
      ggplot2::geom_hline(yintercept = c(20, 50, 100, 200), linetype = "dashed", color = "gray 20") +
      ggplot2::annotate("text", x = "2012", y = 200, label = "Moderate", vjust = 1) +
      ggplot2::annotate("text", x = "2012", y = 100, label = "Elevated", vjust = 1) +
      ggplot2::annotate("text", x = "2012", y = 50, label = "Severe", vjust = 1) +
      ggplot2::annotate("text", x = "2012", y = 20, label = "Acute", vjust = 1) +
      ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
      khroma::scale_color_muted()

    return(chem.lake.anc.plot)

}

#' Plot acid neutralizing capacity (ANC) at streams, and include EPA thresholds
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
#' ChemStreamANCPlot()
#' ChemStreamANCPlot(site = "GRBA_S_PINE1")
#' }
ChemStreamANCPlot <- function(park, site, field.season) {

    chem.anc <- ChemFormatted(park = park, site = site, field.season = field.season)

    chem.stream.anc <- chem.anc |>
        dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                      SampleType %in% c("Routine") | is.na(SampleType),
                      !(SiteShort %in% c("BAKR2", "LHMN1")),
                      SampleFrame == "Stream",
                      Characteristic == "ANC")

    thresholds <- data.frame(yintercept = c(20, 50, 100, 200), Lines = c("Acute", "Severe", "Elevated", "Moderately Acidic"))

    chem.stream.anc.plot <- ggplot2::ggplot(chem.stream.anc,
                                            ggplot2::aes(x = FieldSeason,
                                                         y = LabValue,
                                                         color = SiteShort,
                                                         group = SiteShort)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(linewidth = 1) +
      # ggplot2::facet_grid(~SiteShort, scales = "free_y") +
      ggplot2::ylab(label = "Acid Neutralizing Capacity (ueq/L)") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      ggplot2::labs(title = "Stream acid neutralizing capacity") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
      ggplot2::geom_hline(yintercept = c(20, 50, 100, 200), linetype = "dashed", color = "gray 20") +
      ggplot2::annotate("text", x = "2012", y = 200, label = "Moderate", vjust = 1) +
      ggplot2::annotate("text", x = "2012", y = 100, label = "Elevated", vjust = 1) +
      ggplot2::annotate("text", x = "2012", y = 50, label = "Severe", vjust = 1) +
      ggplot2::annotate("text", x = "2012", y = 20, label = "Acute", vjust = 1) +
      ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
      khroma::scale_color_muted()

    return(chem.stream.anc.plot)

}


#' Plot lake nutrient (UTN, TDN, NO2No3-N, UTP, TDP, DOC) concentration data for all parks and field seasons.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemLakeNutrientPlot <- function(park, site, field.season) {

    chem <- ChemFormatted(park = park, site = site, field.season = field.season)

    lake.nut <- chem |>
        dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                      SampleType %in% c("Routine") | is.na(SampleType),
                      !(SiteShort %in% c("STLL0s")),
                      SampleFrame == "Lake",
                      ReportingGroup == "Nutrient") |>
        dplyr::mutate(Nutrient = ifelse(Characteristic %in% c("UTN", "TDN", "NO3NO2-N"), "Nitrogen",
                                      ifelse(Characteristic %in% c("UTP", "TDP"), "Phosphorus",
                                           ifelse(Characteristic %in% c("DOC"), "Carbon", NA))))

    lake.nut$Nutrient_f = factor(lake.nut$Nutrient, levels = c("Nitrogen", "Phosphorus", "Carbon"))
    lake.nut$Characteristic_f = factor(lake.nut$Characteristic, levels = c("UTN", "TDN", "NO3NO2-N", "UTP", "TDP", "DOC"))

    lake.nut.plot <- ggplot2::ggplot(lake.nut,
                                     ggplot2::aes(x = FieldSeason,
                                         y = LabValue,
                                         color = Characteristic_f,
                                         group = Characteristic_f,
                                         text = paste0("Site Name: ", SiteName, "<br>",
                                                       "Field Season: ", FieldSeason, "<br>",
                                                       "Parameter: ", Characteristic_f, "<br>",
                                                       "Lab Value: ", LabValue))) +
      ggplot2::geom_point() +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::facet_grid(rows = ggplot2::vars(Nutrient_f),
                          cols = ggplot2::vars(SiteShort),
                          scales = "free_y") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      ggplot2::labs(title = "Lake nutrient concentrations",
                    x = "Field Season",
                    y = "Concentration (mg/L)",
                    color = "Nutrient") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
      ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
      ggplot2::scale_color_manual(values = c("midnightblue", "royalblue1", "lightskyblue", "firebrick4", "lightpink2", "goldenrod")) +
      ggplot2::theme(legend.position = "bottom")

    return(lake.nut.plot)

}


#' Plot lake nutrient (UTN, TDN, NO2No3-N, UTP, TDP, DOC) concentration data for all parks and field seasons split into facets.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemLakeNutrientSplitPlot <- function(park, site, field.season) {

  chem <- ChemFormatted(park = park, site = site, field.season = field.season)

  lake.nut <- chem |>
    dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                  SampleType %in% c("Routine") | is.na(SampleType),
                  !(SiteShort %in% c("STLL0s")),
                  SampleFrame == "Lake",
                  ReportingGroup == "Nutrient") |>
    dplyr::mutate(Nutrient = ifelse(Characteristic %in% c("UTN", "TDN", "NO3NO2-N"), "Nitrogen",
                                    ifelse(Characteristic %in% c("UTP", "TDP"), "Phosphorus",
                                           ifelse(Characteristic %in% c("DOC"), "Carbon", NA))))

  lake.nut$Nutrient_f = factor(lake.nut$Nutrient, levels = c("Nitrogen", "Phosphorus", "Carbon"))
  lake.nut$Characteristic_f = factor(lake.nut$Characteristic, levels = c("UTN", "TDN", "NO3NO2-N", "UTP", "TDP", "DOC"))

  lake.nut.plot <- ggplot2::ggplot(lake.nut,
                                   ggplot2::aes(x = FieldSeason,
                                                y = LabValue,
                                                group = Characteristic_f,
                                                text = paste0("Site Name: ", SiteName, "<br>",
                                                              "Field Season: ", FieldSeason, "<br>",
                                                              "Parameter: ", Characteristic_f, "<br>",
                                                              "Lab Value: ", LabValue))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_grid(rows = ggplot2::vars(Characteristic_f),
                        cols = ggplot2::vars(SiteShort),
                        scales = "free_y") +
    ggplot2::ylab(label = "Concentration (mg/L)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::labs(title = "Lake nutrient concentrations") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_discrete(breaks = scales::pretty_breaks())

  return(lake.nut.plot)

}


#' Plot lake nutrient (UTN, TDN, NO2No3-N, UTP, TDP) concentration data as overlapping bar plots for all parks and field seasons.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemLakeNutrientBarPlot <- function(park, site, field.season) {

    chem <- ChemFormatted(park = park, site = site, field.season = field.season)

    lake.nut.bar <- chem |>
        dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                      SampleType %in% c("Routine") | is.na(SampleType),
                      !(SiteShort %in% c("STLL0s")),
                      SampleFrame == "Lake",
                      ReportingGroup == "Nutrient") |>
        dplyr::mutate(Nutrient = ifelse(Characteristic %in% c("UTN", "TDN", "NO3NO2-N"), "Nitrogen",
                                        ifelse(Characteristic %in% c("UTP", "TDP"), "Phosphorus",
                                               ifelse(Characteristic %in% c("DOC"), "Carbon", NA))))

    lake.nut.bar$Nutrient_f = factor(lake.nut.bar$Nutrient, levels = c("Nitrogen", "Phosphorus", "Carbon"))
    lake.nut.bar$Characteristic_f = factor(lake.nut.bar$Characteristic, levels = c("UTN", "TDN", "NO3NO2-N", "UTP", "TDP", "DOC"))

    lake.nut.bar <- lake.nut.bar |>
      dplyr::arrange(match(Characteristic_f, c("UTN", "TDN", "NO3NO2-N", "UTP", "TDP", "DOC"), dplyr::desc(Characteristic_f))) |>
      dplyr::filter(Characteristic != "DOC")

    lake.nut.bar.plot <- ggplot2::ggplot(lake.nut.bar, ggplot2::aes(x = FieldSeason,
                                                           y = LabValue,
                                                           fill = Characteristic_f,
                                                           text = paste0("Field Season: ", FieldSeason, "<br>",
                                                                         "Lab Value: ", LabValue, "<br>",
                                                                         "Parameter: ", Characteristic_f))) +
      ggplot2::geom_bar(stat = "identity", position = "identity", color = "white") +
      ggplot2::facet_grid(Nutrient_f~SiteShort, scales = "free_y") +
      ggplot2::ylab(label = "Concentration (mg/L)") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom") +
      ggplot2::labs(title = "Lake nutrient concentrations", fill = "Nutrient") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
      ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
      ggplot2::scale_fill_manual(values = c("midnightblue", "royalblue1", "lightskyblue", "firebrick4", "lightpink2"))

    return(lake.nut.bar.plot)

}


#' Plot lake ion (ANC2, Na, Mg, K, Ca, SO4-S, Cl) concentration data for all parks and field seasons.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemLakeIonPlot <- function(park, site, field.season) {

    chem <- ChemFormatted(park = park, site = site, field.season = field.season)

    lake.ion <- chem |>
        dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                      SampleType %in% c("Routine") | is.na(SampleType),
                      !(SiteShort %in% c("STLL0s")),
                      SampleFrame == "Lake",
                      ReportingGroup == "Ion",
                      Characteristic %in% c("Na", "Mg", "K", "Ca", "Cl", "SO4-S")) |>
        dplyr::mutate(Ion = ifelse(Characteristic %in% c("Na", "Mg", "K", "Ca"), "Cation",
                                        ifelse(Characteristic %in% c("Cl", "SO4-S"), "Anion", NA)))

    lake.ion$Ion_f = factor(lake.ion$Ion, levels = c("Cation", "Anion"))
    lake.ion$Characteristic_f = factor(lake.ion$Characteristic, levels = c("Na", "Mg", "K", "Ca", "SO4-S", "Cl"))

    lake.ion.plot <- ggplot2::ggplot(lake.ion, ggplot2::aes(x = FieldSeason,
                                                   y = LabValue,
                                                   color = Characteristic_f,
                                                   group = Characteristic_f,
                                                   text = paste0("Site Name: ", SiteName, "<br>",
                                                                 "Field Season: ", FieldSeason, "<br>",
                                                                 "Parameter: ", Characteristic_f, "<br>",
                                                                 "Lab Value: ", LabValue))) +
      ggplot2::geom_point() +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::facet_grid(#rows = ggplot2::vars(Ion_f),
                          cols = ggplot2::vars(SiteShort),
                          scales = "free_y") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      ggplot2::labs(title = "Lake ion concentrations",
                    x = "Field Season",
                    y = "Concentration (mg/L)",
                    color = "Ion") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
      ggplot2:: scale_x_discrete(breaks = scales::pretty_breaks()) +
      khroma::scale_color_muted() +
      ggplot2::theme(legend.position = "bottom")

    return(lake.ion.plot)

}


#' Plot lake ion (ANC2, Na, Mg, K, Ca, SO4-S, Cl) concentration data for all parks and field seasons split into facets.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemLakeIonSplitPlot <- function(park, site, field.season) {

  chem <- ChemFormatted(park = park, site = site, field.season = field.season)

  lake.ion <- chem |>
    dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                  SampleType %in% c("Routine") | is.na(SampleType),
                  !(SiteShort %in% c("STLL0s")),
                  SampleFrame == "Lake",
                  ReportingGroup == "Ion",
                  Characteristic %in% c("Na", "Mg", "K", "Ca", "Cl", "SO4-S")) |>
    dplyr::mutate(Ion = ifelse(Characteristic %in% c("Na", "Mg", "K", "Ca"), "Cation",
                               ifelse(Characteristic %in% c("Cl", "SO4-S"), "Anion", NA)))

  lake.ion$Ion_f = factor(lake.ion$Ion, levels = c("Cation", "Anion"))
  lake.ion$Characteristic_f = factor(lake.ion$Characteristic, levels = c("Na", "Mg", "K", "Ca", "SO4-S", "Cl"))

  lake.ion.plot <- ggplot2::ggplot(lake.ion, ggplot2::aes(x = FieldSeason,
                                                          y = LabValue,
                                                          group = Characteristic_f,
                                                          text = paste0("Site Name: ", SiteName, "<br>",
                                                                        "Field Season: ", FieldSeason, "<br>",
                                                                        "Parameter: ", Characteristic_f, "<br>",
                                                                        "Lab Value: ", LabValue))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_grid(rows = ggplot2::vars(Characteristic_f),
                        cols = ggplot2::vars(SiteShort),
                        scales = "free_y") +
    ggplot2::ylab(label = "Concentration (mg/L)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::labs(title = "Lake ion concentrations") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2:: scale_x_discrete(breaks = scales::pretty_breaks()) +
    khroma::scale_color_muted()

  return(lake.ion.plot)

}


#' Plot stream nutrient (UTN, TDN, NO2No3-N, UTP, TDP, DOC) concentration data for all parks and field seasons.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemStreamNutrientPlot <- function(park, site, field.season) {

    chem <- ChemFormatted(park = park, site = site, field.season = field.season)

    stream.nut <- chem |>
        dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                      SampleType %in% c("Routine") | is.na(SampleType),
                      !(SiteShort %in% c("BAKR2", "LHMN1")),
                      SampleFrame == "Stream",
                      ReportingGroup == "Nutrient") |>
      dplyr::mutate(Nutrient = ifelse(Characteristic %in% c("UTN", "TDN", "NO3NO2-N"), "Nitrogen",
                                      ifelse(Characteristic %in% c("UTP", "TDP"), "Phosphorus",
                                             ifelse(Characteristic %in% c("DOC"), "Carbon", NA))))

    stream.nut$Nutrient_f = factor(stream.nut$Nutrient, levels = c("Nitrogen", "Phosphorus", "Carbon"))
    stream.nut$Characteristic_f = factor(stream.nut$Characteristic, levels = c("UTN", "TDN", "NO3NO2-N", "UTP", "TDP", "DOC"))

    stream.nut.plot <- ggplot2::ggplot(stream.nut, ggplot2::aes(x = FieldSeason,
                                                       y = LabValue,
                                                       color = Characteristic_f,
                                                       group = Characteristic_f,
                                                       text = paste0("Site Name: ", SiteName, "<br>",
                                                                     "Field Season: ", FieldSeason, "<br>",
                                                                     "Parameter: ", Characteristic_f, "<br>",
                                                                     "Lab Value: ", LabValue))) +
      ggplot2::geom_point() +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::facet_grid(rows = ggplot2::vars(Nutrient_f),
                          cols = ggplot2::vars(SiteShort),
                          scales = "free_y") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      ggplot2::labs(title = "Stream nutrient concentrations",
                    x = "Field Season",
                    y = "Concentration (mg/L)",
                    color = "Nutrient") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
      ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
      ggplot2::scale_color_manual(values = c("midnightblue", "royalblue1", "lightskyblue", "firebrick4", "lightpink2", "goldenrod")) +
      ggplot2::theme(legend.position = "bottom")

    return(stream.nut.plot)

}


#' Plot stream nutrient (UTN, TDN, NO2No3-N, UTP, TDP, DOC) concentration data for all parks and field seasons split into facets.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemStreamNutrientSplitPlot <- function(park, site, field.season) {

  chem <- ChemFormatted(park = park, site = site, field.season = field.season)

  stream.nut <- chem |>
    dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                  SampleType %in% c("Routine") | is.na(SampleType),
                  !(SiteShort %in% c("BAKR2", "LHMN1")),
                  SampleFrame == "Stream",
                  ReportingGroup == "Nutrient") |>
    dplyr::mutate(Nutrient = ifelse(Characteristic %in% c("UTN", "TDN", "NO3NO2-N"), "Nitrogen",
                                    ifelse(Characteristic %in% c("UTP", "TDP"), "Phosphorus",
                                           ifelse(Characteristic %in% c("DOC"), "Carbon", NA))))

  stream.nut$Nutrient_f = factor(stream.nut$Nutrient, levels = c("Nitrogen", "Phosphorus", "Carbon"))
  stream.nut$Characteristic_f = factor(stream.nut$Characteristic, levels = c("UTN", "TDN", "NO3NO2-N", "UTP", "TDP", "DOC"))

  stream.nut.plot <- ggplot2::ggplot(stream.nut, ggplot2::aes(x = FieldSeason,
                                                              y = LabValue,
                                                              group = Characteristic_f,
                                                              text = paste0("Site Name: ", SiteName, "<br>",
                                                                            "Field Season: ", FieldSeason, "<br>",
                                                                            "Parameter: ", Characteristic_f, "<br>",
                                                                            "Lab Value: ", LabValue))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_grid(rows = ggplot2::vars(Characteristic_f),
                        cols = ggplot2::vars(SiteShort),
                        scales = "free_y") +
    ggplot2::ylab(label = "Concentration (mg/L)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::labs(title = "Stream nutrient concentrations") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_discrete(breaks = scales::pretty_breaks())

  return(stream.nut.plot)

}


#' Plot stream nutrient (UTN, TDN, NO2No3-N, UTP, TDP) concentration data as overlapping bar plots for all parks and field seasons.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemStreamNutrientBarPlot <- function(park, site, field.season) {
    chem <- ChemFormatted(park = park, site = site, field.season = field.season)

    stream.nut.bar <- chem |>
        dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                      SampleType %in% c("Routine") | is.na(SampleType),
                      !(SiteShort %in% c("BAKR2", "LHMN1")),
                      SampleFrame == "Stream",
                      ReportingGroup == "Nutrient") |>
        dplyr::mutate(Nutrient = ifelse(Characteristic %in% c("UTN", "TDN", "NO3NO2-N"), "Nitrogen",
                                        ifelse(Characteristic %in% c("UTP", "TDP"), "Phosphorus",
                                               ifelse(Characteristic %in% c("DOC"), "Carbon", NA))))

    stream.nut.bar$Nutrient_f = factor(stream.nut.bar$Nutrient, levels = c("Nitrogen", "Phosphorus", "Carbon"))
    stream.nut.bar$Characteristic_f = factor(stream.nut.bar$Characteristic, levels = c("UTN", "TDN", "NO3NO2-N", "UTP", "TDP", "DOC"))

    stream.nut.bar <- stream.nut.bar |>
      dplyr::arrange(match(Characteristic_f, c("UTN", "TDN", "NO3NO2-N", "UTP", "TDP", "DOC"), desc(Characteristic_f))) |>
      dplyr::filter(Characteristic != "DOC")

    stream.nut.bar.plot <- ggplot2::ggplot(stream.nut.bar, ggplot2::aes(x = FieldSeason,
                                                               y = LabValue,
                                                               fill = Characteristic_f,
                                                               text = paste0("Field Season: ", FieldSeason, "<br>",
                                                                             "Lab Value: ", LabValue, "<br>",
                                                                             "Parameter: ", Characteristic_f))) +
      ggplot2::geom_bar(stat = "identity", position = "identity", color = "white") +
      ggplot2::facet_grid(Nutrient_f~SiteShort, scales = "free_y") +
      ggplot2::ylab(label = "Concentration (mg/L)") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom") +
      ggplot2::labs(title = "Stream nutrient concentrations", fill = "Nutrient") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
      ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
      ggplot2::scale_fill_manual(values = c("midnightblue", "royalblue1", "lightskyblue", "firebrick4", "lightpink2"))

    return(stream.nut.bar.plot)

}

#' Plot stream ion (ANC2, Na, Mg, K, Ca, SO4-S, Cl) concentration data for all parks and field seasons.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemStreamIonPlot <- function(park, site, field.season) {

    chem <- ChemFormatted(park = park, site = site, field.season = field.season)

    stream.ion <- chem |>
        dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                      SampleType %in% c("Routine") | is.na(SampleType),
                      !(SiteShort %in% c("BAKR2", "LHMN1")),
                      SampleFrame == "Stream",
                      ReportingGroup == "Ion",
                      Characteristic %in% c("Na", "Mg", "K", "Ca", "Cl", "SO4-S")) |>
      dplyr::mutate(Ion = ifelse(Characteristic %in% c("Na", "Mg", "K", "Ca"), "Cation",
                                 ifelse(Characteristic %in% c("Cl", "SO4-S"), "Anion", NA)))

    stream.ion$Ion_f = factor(stream.ion$Ion, levels = c("Cation", "Anion"))
    stream.ion$Characteristic_f = factor(stream.ion$Characteristic, levels = c("Na", "Mg", "K", "Ca", "SO4-S", "Cl"))

    stream.ion.plot <- ggplot2::ggplot(stream.ion, ggplot2::aes(x = FieldSeason,
                                                       y = LabValue,
                                                       color = Characteristic_f,
                                                       group = Characteristic_f,
                                                       text = paste0("Site Name: ", SiteName, "<br>",
                                                                     "Field Season: ", FieldSeason, "<br>",
                                                                     "Parameter: ", Characteristic_f, "<br>",
                                                                     "Lab Value: ", LabValue))) +
      ggplot2::geom_point() +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::facet_grid(#rows = ggplot2::vars(Ion),
                          cols = ggplot2::vars(SiteShort),
                          scales = "free_y") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      ggplot2::labs(title = "Stream ion concentrations",
                    x = "Field Season",
                    y = "Concentration (mg/L)",
                    color = "Ion") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
      ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
      khroma::scale_color_muted() +
      ggplot2::theme(legend.position = "bottom")

    return(stream.ion.plot)

}

#' Plot stream ion (ANC2, Na, Mg, K, Ca, SO4-S, Cl) concentration data for all parks and field seasons split into facets.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemStreamIonSplitPlot <- function(park, site, field.season) {

  chem <- ChemFormatted(park = park, site = site, field.season = field.season)

  stream.ion <- chem |>
    dplyr::filter(VisitType %in% c("Primary") | is.na(VisitType),
                  SampleType %in% c("Routine") | is.na(SampleType),
                  !(SiteShort %in% c("BAKR2", "LHMN1")),
                  SampleFrame == "Stream",
                  ReportingGroup == "Ion",
                  Characteristic %in% c("Na", "Mg", "K", "Ca", "Cl", "SO4-S")) |>
    dplyr::mutate(Ion = ifelse(Characteristic %in% c("Na", "Mg", "K", "Ca"), "Cation",
                               ifelse(Characteristic %in% c("Cl", "SO4-S"), "Anion", NA)))

  stream.ion$Ion_f = factor(stream.ion$Ion, levels = c("Cation", "Anion"))
  stream.ion$Characteristic_f = factor(stream.ion$Characteristic, levels = c("Na", "Mg", "K", "Ca", "SO4-S", "Cl"))

  stream.ion.plot <- ggplot2::ggplot(stream.ion, ggplot2::aes(x = FieldSeason,
                                                              y = LabValue,
                                                              # color = Characteristic_f,
                                                              group = Characteristic_f,
                                                              text = paste0("Site Name: ", SiteName, "<br>",
                                                                            "Field Season: ", FieldSeason, "<br>",
                                                                            "Parameter: ", Characteristic_f, "<br>",
                                                                            "Lab Value: ", LabValue))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_grid(rows = ggplot2::vars(Characteristic_f),
                        cols = ggplot2::vars(SiteShort),
                        scales = "free_y") +
    ggplot2::ylab(label = "Concentration (mg/L)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::labs(title = "Stream ion concentrations") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_discrete(breaks = scales::pretty_breaks())

  return(stream.ion.plot)

}

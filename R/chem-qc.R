#' List all laboratory values that were given an "Information," "Warning," or "Critical" flag.
#'
#' @param conn
#' @param path.to.data
#' @param park
#' @param site
#' @param field.season
#' @param data.source
#'
#' @return A tibble with columns SampleFrame, ...
#' @export
#'
#' @examples
qcChemFlags <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    flags <- chem %>%
        filter(DQF %in% c("I", "W", "C")) %>%
        select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType, DQF, DQFNote) %>%
        arrange(SampleFrame, VisitDate, SiteCode)

return(flags)

}


#' Calculate the relative percent difference (RPD) for laboratory duplicates, and list results that exceed the 30% MQO threshold.
#'
#' @param conn
#' @param path.to.data
#' @param park
#' @param site
#' @param field.season
#' @param data.source
#'
#' @return
#' @export
#'
#' @examples
qcChemLabDupes <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    lab.dupes <- chem %>%
        select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType) %>%
        filter(SiteCode != "GRBA_L_STLL0s", SampleType %in% c("Routine", "Lab Duplicate", "Lab Triplicate"))

    lab.dupes.wide <- pivot_wider(data = lab.dupes, names_from = SampleType, values_from = LabValue) %>%
        rename(LabDuplicate = `Lab Duplicate`, LabTriplicate = `Lab Triplicate`) %>%
        filter(!is.na(LabDuplicate)) %>%
        mutate(RPD = ((pmax(Routine, LabDuplicate) - pmin(Routine, LabDuplicate))/((pmax(Routine, LabDuplicate) + pmin(Routine, LabDuplicate))/2))*100) %>%
        mutate(RPD2 = ((pmax(Routine, LabTriplicate) - pmin(Routine, LabTriplicate))/((pmax(Routine, LabTriplicate) + pmin(Routine, LabTriplicate))/2))*100) %>%
        mutate(RPDFlag = ifelse(RPD > 30 | RPD2 > 30, "RPD above laboratory precision MQO of 30%", NA)) %>%
        arrange(desc(RPD))

    return(lab.dupes.wide)

}


#' Calculate the relative percent difference (RPD) for field duplicates, and list results that exceed the 30% MQO threshold.
#'
#' @param conn
#' @param path.to.data
#' @param park
#' @param site
#' @param field.season
#' @param data.source
#'
#' @return
#' @export
#'
#' @examples
qcChemFieldDupes <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    field.dupes <- chem %>%
        select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType) %>%
        filter(SiteCode != "GRBA_L_STLL0s", SampleType %in% c("Routine", "Field Duplicate"))

    field.dupes.wide <- pivot_wider(data = field.dupes, names_from = SampleType, values_from = LabValue) %>%
        rename(FieldDuplicate = `Field Duplicate`) %>%
        filter(!is.na(FieldDuplicate)) %>%
        mutate(RPD = ((pmax(Routine, FieldDuplicate) - pmin(Routine, FieldDuplicate))/((pmax(Routine, FieldDuplicate) + pmin(Routine, FieldDuplicate))/2))*100) %>%
        mutate(RPDFlag = ifelse(RPD > 30, "RPD above laboratory precision MQO of 30%", NA)) %>%
        arrange(desc(RPD))

    return(field.dupes.wide)

}


#' Calculate the relative percent difference (RPD) for field blanks, and list results that exceed the 30% MQO threshold.
#'
#' @param conn
#' @param path.to.data
#' @param park
#' @param site
#' @param field.season
#' @param data.source
#'
#' @return
#' @export
#'
#' @examples
qcChemFieldBlanks <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    field.blanks <- chem %>%
        select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, unit, LabValue, SampleType) %>%
        filter(SiteCode != "GRBA_L_STLL0s", SampleType %in% c("Routine", "Field Blank"))

    field.blanks.wide <- pivot_wider(data = field.blanks, names_from = SampleType, values_from = LabValue) %>%
        rename(FieldBlank = `Field Blank`) %>%
        filter(!is.na(FieldBlank)) %>%
        mutate(RPD = ((pmax(Routine, FieldBlank) - pmin(Routine, FieldBlank))/((pmax(Routine, FieldBlank) + pmin(Routine, FieldBlank))/2))*100) %>%
        mutate(RPDFlag = ifelse(RPD > 30, "RPD above laboratory precision MQO of 30%", NA)) %>%
        arrange(desc(RPD))

    return(field.blanks.wide)

}


#' List all routine samples where total dissolved nitrogen (TDN) values exceeded total nitrogen (UTN) values,
#' and flag whether the discrepancy was within precision limits or outside of the expected error.
#'
#' @param conn
#' @param path.to.data
#' @param park
#' @param site
#' @param field.season
#' @param data.source
#'
#' @return
#' @export
#'
#' @examples
qcChemTDN <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    TDN <- chem %>%
        filter(VisitType == "Primary", SampleType == "Routine", ReportingGroup == "Nutrient", Characteristic %in% c("UTN", "TDN", "NO3NO2-N")) %>%
        select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, Unit, LabValue)

    TDN.wide <- pivot_wider(data = TDN, names_from = Characteristic, values_from = LabValue) %>%
        rename(NO3NO2 = `NO3NO2-N`) %>%
        mutate(TDPvUTP = ifelse(TDN>UTN, round(TDN - UTN, 2), NA)) %>%
        mutate(TDNvUTNFlag = ifelse(TDNvUTN > 0.02, "TDN is greater than UTN outside of the expected error", "TDN is greater than UTN within limits of precision")) %>%
        filter(!is.na(TDNvUTN))

    return(TDN.wide)

}


#' List all routine samples where nitrate and nitrite (NO3NO2-N) values exceeded either total dissolved nitrogen (TDN) values or total nitrogen (UTN) values,
#' and flag whether the discrepancy was within precision limits or outside of the expected error.
#'
#' @param conn
#' @param path.to.data
#' @param park
#' @param site
#' @param field.season
#' @param data.source
#'
#' @return
#' @export
#'
#' @examples
qcChemNO3NO2 <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    NO3NO2 <- chem %>%
        filter(VisitType == "Primary", SampleType == "Routine", ReportingGroup == "Nutrient", Characteristic %in% c("UTN", "TDN", "NO3NO2-N")) %>%
        select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, Unit, LabValue)

    NO3NO2.wide <- pivot_wider(data = NO3NO2, names_from = Characteristic, values_from = LabValue) %>%
        rename(NO3NO2 = `NO3NO2-N`) %>%
        mutate(NO3NO2vN = ifelse(NO3NO2 > UTN, round(NO3NO2 - UTN, 2), ifelse(NO3NO2 > TDN, round(NO3NO2 - TDN, 2), NA))) %>%
        mutate(NO3NO2vNFlag = ifelse(NO3NO2vN > 0.02, "NO3NO2 is greater than UTN and/or TDN outside of the expected error", "NO3NO2 is greater than UTN and/or TDN within limits of precision")) %>%
        filter(!is.na(NO3NO2vN))

    return(NO3NO2.wide)

}


#' List all routine samples where total dissolved phosphorous (TDP) values exceeded total phosphorus (UTP) values,
#' and flag whether the discrepancy was within precision limits or outside of the expected error.
#'
#' @param conn
#' @param path.to.data
#' @param park
#' @param site
#' @param field.season
#' @param data.source
#'
#' @return
#' @export
#'
#' @examples
qcChemTDP <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    TDP <- chem %>%
        filter(VisitType == "Primary", SampleType == "Routine", ReportingGroup == "Nutrient", Characteristic %in% c("UTP", "TDP")) %>%
        select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, Unit, LabValue)

    TDP.wide <- pivot_wider(data = TDP, names_from = Characteristic, values_from = LabValue) %>%
        mutate(TDPvUTP = ifelse(TDP>UTP, round(TDP - UTP, 2), NA)) %>%
        mutate(TDPvUTPFlag = ifelse(TDPvUTP > 0.02, "TDP is greater than UTP outside of the expected error", "TDP is greater than UTP within limits of precision")) %>%
        filter(!is.na(TDPvUTP))

    return(TDP.wide)

}


#' Create data frame with MDL and ML values for each characteristic.
#'
#' @return
#' @export
#'
#' @examples
getMDLLookup <- function() {
    lookup <- data.frame(Characteristic = c("ALK2", "Ca", "DOC", "Cl", "Mg", "NO3NO2-N", "UTN", "UTP", "K", "Na", "SO4-S"),
                         Unit = c("mg CaCO3/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
                         StartYear = c(2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009),
                         EndYear = c(2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021),
                         MDL = c(0.2, 0.06, 0.05, 0.01, 0.02, 0.001, 0.01, 0.002, 0.03, 0.01, 0.01),
                         ML = c(0.6, 0.19, 0.16, 0.03, 0.06, 0.003, 0.03, 0.006, 0.10, 0.03, 0.03))

    return(lookup)

}


#' List all routine laboratory values that are less than or equal to the minimum detection level (MDL) for that characteristic.
#'
#' @param conn
#' @param path.to.data
#' @param park
#' @param site
#' @param field.season
#' @param data.source
#'
#' @return
#' @export
#'
#' @examples
qcMDL <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    mdl <- chem %>%
        dplyr::filter(VisitType == "Primary", SampleType == "Routine") %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue)

    mdl.merged <- fuzzyjoin::fuzzy_inner_join(x = mdl,
                                            y = lookup,
                                            by = c("Characteristic" = "Characteristic", "Unit" = "Unit", "FieldSeason" = "StartYear", "FieldSeason" = "EndYear"),
                                            match_fun = list(`==`, `==`, `>=`, `<=`)) %>%
        dplyr::rename(Characteristic = Characteristic.x, Unit = Unit.x) %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, MDL) %>%
        dplyr::mutate(MDLFlag = ifelse(LabValue <= MDL, "Value is less than or equal to the minimum detection level (MDL)", NA)) %>%
        dplyr::filter(!is.na(MDLFlag)) %>%
        dplyr::arrange(SampleFrame, VisitDate, SiteCode)

    return(mdl.merged)

}


#' List all routine laboratory values that are less than or equal to the minimum level of quantification (ML) for that characteristic.
#'
#' @param conn
#' @param path.to.data
#' @param park
#' @param site
#' @param field.season
#' @param data.source
#'
#' @return
#' @export
#'
#' @examples
qcML <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    ml <- chem %>%
        filter(VisitType == "Primary", SampleType == "Routine") %>%
        select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue)

    ml.merged <- fuzzyjoin::fuzzy_inner_join(x = ml,
                                              y = lookup,
                                              by = c("Characteristic" = "Characteristic", "Unit" = "Unit", "FieldSeason" = "StartYear", "FieldSeason" = "EndYear"),
                                              match_fun = list(`==`, `==`, `>=`, `<=`)) %>%
        dplyr::rename(Characteristic = Characteristic.x, Unit = Unit.x) %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, ML) %>%
        dplyr::mutate(MLFlag = ifelse(LabValue <= ML, "Value is less than or equal to the minimum level of quantification (ML)", NA)) %>%
        dplyr::filter(!is.na(MLFlag)) %>%
        dplyr::arrange(SampleFrame, VisitDate, SiteCode)

    return(ml.merged)

}

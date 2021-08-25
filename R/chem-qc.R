#' List all laboratory values that have an "Information," "Warning," or "Critical" flag.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType, DQF, DQFNote.
#' @export
#'
#' @examples
qcChemFlags <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    flags.list <- chem %>%
        filter(DQF %in% c("I", "W", "C")) %>%
        select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType, DQF, DQFNote) %>%
        arrange(SampleFrame, VisitDate, SiteCode)

return(flags.list)

}


#' Calculate the relative percent difference (RPD) for laboratory duplicates and triplicates, flag results that exceed the 30% MQO threshold, and list all RPD values and flags.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, Routine, LabDuplicate, LabTriplicate, RPD, RPD2, RPDFLag.
#' @export
#'
#' @examples
qcChemLabDupes <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    lab.dupes <- chem %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType) %>%
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

    lab.dupes.list <- lab.dupes.wide %>%
        dplyr::filter(!is.na(LabDuplicate)) %>%
        dplyr::mutate(RPD = round(((pmax(Routine, LabDuplicate) - pmin(Routine, LabDuplicate))/((pmax(Routine, LabDuplicate) + pmin(Routine, LabDuplicate))/2))*100, 2)) %>%
        dplyr::mutate(RPD2 = round(((pmax(Routine, LabTriplicate) - pmin(Routine, LabTriplicate))/((pmax(Routine, LabTriplicate) + pmin(Routine, LabTriplicate))/2))*100, 2)) %>%
        dplyr::mutate(RPDFlag = ifelse(RPD > 30 | RPD2 > 30, "RPD above laboratory precision MQO of 30%", NA)) %>%
        dplyr::arrange(desc(RPD))

    return(lab.dupes.list)

}


#' Calculate the relative percent difference (RPD) for field duplicates, flag results that exceed the 30% MQO threshold, and list all RPD values and flags.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, Routine, FieldDuplicate, RPD, RPDFLag.
#' @export
#'
#' @examples
qcChemFieldDupes <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    field.dupes <- chem %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType) %>%
        dplyr::filter(SiteCode != "GRBA_L_STLL0s", SampleType %in% c("Routine", "Field Duplicate"))

    field.dupes.wide <- tidyr::pivot_wider(data = field.dupes, names_from = SampleType, values_from = LabValue)


    field.dupes.wide <- if ("Field Duplicate" %in% names(field.dupes.wide)) {

        dplyr::rename(field.dupes.wide, FieldDuplicate = `Field Duplicate`)

    } else {

        dplyr::mutate(field.dupes.wide, FieldDuplicate = NA)

    }

    field.dupes.list <- field.dupes.wide %>%
        dplyr::filter(!is.na(FieldDuplicate)) %>%
        dplyr::mutate(RPD = round(((pmax(Routine, FieldDuplicate) - pmin(Routine, FieldDuplicate))/((pmax(Routine, FieldDuplicate) + pmin(Routine, FieldDuplicate))/2))*100, 2)) %>%
        dplyr::mutate(RPDFlag = ifelse(RPD > 30, "RPD above laboratory precision MQO of 30%", NA)) %>%
        dplyr::arrange(desc(RPD))

    return(field.dupes.list)

}


#' List all laboratory values from field blanks that exceed the minimum detection level (MDL) for that analyte.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, Routine, FieldBlank, RPD, RPDFLag.
#' @export
#'
#' @examples
qcChemFieldBlanks <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")
    lookup <- getMDLLookup()

    field.blanks <- chem %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, SampleType) %>%
        dplyr::filter(SiteCode != "GRBA_L_STLL0s", SampleType %in% c("Field Blank")) %>%
        dplyr::mutate(FieldSeason = as.double(FieldSeason))

    field.blanks.merged <- fuzzyjoin::fuzzy_inner_join(x = field.blanks,
                                                       y = lookup,
                                                       by = c("Characteristic" = "Characteristic", "Unit" = "Unit", "FieldSeason" = "StartYear", "FieldSeason" = "EndYear"),
                                                       match_fun = list(`==`, `==`, `>=`, `<=`))

    field.blanks.list <- field.blanks.merged %>%
        dplyr::rename(Characteristic = Characteristic.x, Unit = Unit.x) %>%
        dplyr::mutate(FieldSeason = as.character(FieldSeason)) %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, MDL) %>%
        dplyr::filter(LabValue > MDL)

    return(field.blanks.list)

}


#' List all routine samples where total dissolved nitrogen (TDN) values exceeded total nitrogen (UTN) values, and flag whether the discrepancy was within precision limits or outside of the expected error.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Unit, UTN, TDN, TDNvUTN, TDNFlag.
#' @export
#'
#' @examples
qcChemTDN <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    TDN <- chem %>%
        dplyr::filter(VisitType == "Primary", SampleType == "Routine", ReportingGroup == "Nutrient", Characteristic %in% c("UTN", "TDN", "NO3NO2-N")) %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, Unit, LabValue)

    TDN.wide <- tidyr::pivot_wider(data = TDN, names_from = Characteristic, values_from = LabValue)

    TDN.list <- TDN.wide %>%
        dplyr::rename(NO3NO2 = `NO3NO2-N`) %>%
        dplyr::mutate(TDNvUTN = ifelse(TDN > UTN, round(TDN - UTN, 2), NA)) %>%
        dplyr::mutate(TDNFlag = ifelse(TDNvUTN > 0.01, "TDN is greater than UTN outside the normal limits of variability", "TDN is greater than UTN within precision limits")) %>%
        dplyr::filter(!is.na(TDNvUTN))

    return(TDN.list)

}


#' List all routine samples where nitrate and nitrite (NO3NO2-N) values exceeded either total dissolved nitrogen (TDN) values or total nitrogen (UTN) values, and flag whether the discrepancy was within precision limits or outside of the expected error.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Unit, UTN, TDN, NO3NO2, NO3NO2vUTN, NO3NO2vTDN, NO3NO2Flag.
#' @export
#'
#' @examples
qcChemNO3NO2 <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    NO3NO2 <- chem %>%
        dplyr::filter(VisitType == "Primary", SampleType == "Routine", ReportingGroup == "Nutrient", Characteristic %in% c("UTN", "TDN", "NO3NO2-N")) %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, Unit, LabValue)

    NO3NO2.wide <- tidyr::pivot_wider(data = NO3NO2, names_from = Characteristic, values_from = LabValue)

    NO3NO2.list <- NO3NO2.wide %>%
        dplyr::rename(NO3NO2 = `NO3NO2-N`) %>%
        dplyr::mutate(NO3NO2vUTN = ifelse(NO3NO2 > UTN, round(NO3NO2 - UTN, 3), NA)) %>%
        dplyr::mutate(NO3NO2vTDN = ifelse(NO3NO2 > TDN, round(NO3NO2 - TDN, 3), NA)) %>%
        dplyr::mutate(NO3NO2Flag = ifelse(NO3NO2vUTN > 0.01 | NO3NO2vTDN > 0.01, "NO3NO2 is greater than UTN and/or TDN outside the normal limits of variability", "NO3NO2 is greater than TDN and/or UTN within precision limits")) %>%
        dplyr::filter(!is.na(NO3NO2vUTN | NO3NO2vTDN))

    return(NO3NO2.list)

}


#' List all routine samples where total dissolved phosphorous (TDP) values exceeded total phosphorus (UTP) values, and flag whether the discrepancy was within precision limits or outside of the expected error.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Unit, UTP, TDP, TDPvUTP, TDPFlag.
#' @export
#'
#' @examples
qcChemTDP <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    TDP <- chem %>%
        dplyr::filter(VisitType == "Primary", SampleType == "Routine", ReportingGroup == "Nutrient", Characteristic %in% c("UTP", "TDP")) %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, Unit, LabValue)

    TDP.wide <- tidyr::pivot_wider(data = TDP, names_from = Characteristic, values_from = LabValue)

    TDP.list <- TDP.wide %>%
        dplyr::mutate(TDPvUTP = ifelse(TDP>UTP, round(TDP - UTP, 3), NA)) %>%
        dplyr::mutate(TDPFlag = ifelse(TDPvUTP > 0.002, "TDP is greater than UTP outside the limits of normal variability", "TDP is greater than UTP within precision limits")) %>%
        dplyr::filter(!is.na(TDPvUTP))

    return(TDP.list)

}


#' Create tibble with MDL and ML values for each characteristic.
#'
#' @return A tibble with columns Characteristic, Unit, StartYear, EndYear, MDL, ML.
#' @export
#'
#' @examples
getMDLLookup <- function() {
    lookup <- tibble::tibble(Characteristic = c("ALK2", "Ca", "DOC", "Cl", "Mg", "NO3NO2-N", "UTN", "UTP", "K", "Na", "SO4-S"),
                             Unit = c("mg CaCO3/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
                             StartYear = c(2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009),
                             EndYear = c(2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021),
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
qcChemMDL <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")
    lookup <- getMDLLookup()

    mdl <- chem %>%
        dplyr::filter(VisitType == "Primary", SampleType == "Routine") %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue)

    mdl.merged <- fuzzyjoin::fuzzy_inner_join(x = mdl,
                                            y = lookup,
                                            by = c("Characteristic" = "Characteristic", "Unit" = "Unit", "FieldSeason" = "StartYear", "FieldSeason" = "EndYear"),
                                            match_fun = list(`==`, `==`, `>=`, `<=`))

    mdl.list <- mdl.merged %>%
        dplyr::rename(Characteristic = Characteristic.x, Unit = Unit.x) %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, MDL) %>%
        dplyr::mutate(MDLFlag = ifelse(LabValue <= MDL, "Value is less than or equal to the minimum detection level (MDL)", NA)) %>%
        dplyr::filter(!is.na(MDLFlag)) %>%
        dplyr::arrange(SampleFrame, VisitDate, SiteCode)

    return(mdl.list)

}


#' List all routine laboratory values that are less than or equal to the minimum level of quantitation (ML) for that analyte.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, ML, MLFlag.
#' @export
#'
#' @examples
qcChemML <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    ml <- chem %>%
        dplyr::filter(VisitType == "Primary", SampleType == "Routine") %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue)

    ml.merged <- fuzzyjoin::fuzzy_inner_join(x = ml,
                                              y = lookup,
                                              by = c("Characteristic" = "Characteristic", "Unit" = "Unit", "FieldSeason" = "StartYear", "FieldSeason" = "EndYear"),
                                              match_fun = list(`==`, `==`, `>=`, `<=`))

    ml.list <- ml.merged %>%
        dplyr::rename(Characteristic = Characteristic.x, Unit = Unit.x) %>%
        dplyr::select(SampleFrame, SiteCode, SiteName, FieldSeason, VisitDate, Characteristic, CharacteristicLabel, Unit, LabValue, ML) %>%
        dplyr::mutate(MLFlag = ifelse(LabValue <= ML, "Value is less than or equal to the minimum level of quantification (ML)", NA)) %>%
        dplyr::filter(!is.na(MLFlag)) %>%
        dplyr::arrange(SampleFrame, VisitDate, SiteCode)

    return(ml.list)

}


#' Plot lake nutrient (UTN, TDN, NO2No3-N, UTP, TDP, DOC) concentration data for all parks and field seasons.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemLakeNutrientPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    lake.nut <- chem %>%
        dplyr::filter(SampleType == "Routine", VisitType == "Primary", SampleFrame == "Lake", ReportingGroup == "Nutrient") %>%
        tidyr::complete(FieldSeason, nesting(Park, SiteShort, SiteCode, SiteName, SampleFrame, Characteristic, CharacteristicLabel, ReportingGroup))

    lake.nut$Characteristic_f = factor(lake.nut$Characteristic, levels = c("UTN", "TDN", "NO3NO2-N", "UTP", "TDP", "DOC"))

    lake.nut.plot <- ggplot2::ggplot(lake.nut, aes(x = FieldSeason, y = LabValue, group = Characteristic)) +
        geom_point() +
        geom_line() +
        facet_grid(Characteristic_f~SiteShort, scales = "free_y") +
        ylab(label = "Concentration (mg/L)") +
        theme(axis.text.x = element_text(angle = 90))

    return(lake.nut.plot)

}


#' Plot lake ion (ANC2, Na, Mg, K, Ca, SO4-S, Cl) concentration data for all parks and field seasons.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemLakeIonPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    lake.ion <- chem %>%
        dplyr::filter(SampleType == "Routine", VisitType == "Primary", SampleFrame == "Lake", ReportingGroup == "Ion") %>%
        tidyr::complete(FieldSeason, nesting(Park, SiteShort, SiteCode, SiteName, SampleFrame, Characteristic, CharacteristicLabel, ReportingGroup))

    lake.ion$Characteristic_f = factor(lake.ion$Characteristic, levels = c("ALK2", "Na", "Mg", "K", "Ca", "SO4-S", "Cl"))

    lake.ion.plot <- ggplot2::ggplot(lake.ion, aes(x = FieldSeason, y = LabValue, group = Characteristic)) +
        geom_point() +
        geom_line() +
        facet_grid(Characteristic_f~SiteShort, scales = "free_y") +
        ylab(label = "Concentration (mg/L)") +
        theme(axis.text.x = element_text(angle = 90))

    return(lake.ion.plot)

}


#' Plot stream nutrient (UTN, TDN, NO2No3-N, UTP, TDP, DOC) concentration data for all parks and field seasons.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemStreamNutrientPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    stream.nut <- chem %>%
        dplyr::filter(SampleType == "Routine", VisitType == "Primary", SampleFrame == "Stream", ReportingGroup == "Nutrient", SiteShort != "BAKR2") %>%
        tidyr::complete(FieldSeason, nesting(Park, SiteShort, SiteCode, SiteName, SampleFrame, Characteristic, CharacteristicLabel, ReportingGroup))

    stream.nut$Characteristic_f = factor(stream.nut$Characteristic, levels = c("UTN", "TDN", "NO3NO2-N", "UTP", "TDP", "DOC"))

    stream.nut.plot <- ggplot2::ggplot(stream.nut, aes(x = FieldSeason, y = LabValue, group = Characteristic)) +
        geom_point() +
        geom_line() +
        facet_grid(Characteristic_f~SiteShort, scales = "free_y") +
        ylab(label = "Concentration (mg/L)") +
        theme(axis.text.x = element_text(angle = 90))

    return(stream.nut.plot)

}


#' Plot stream ion (ANC2, Na, Mg, K, Ca, SO4-S, Cl) concentration data for all parks and field seasons.
#'
#' @inheritParams ReadAndFilterData
#'
#' @return A ggplot object
#' @export
#'
ChemStreamIonPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {

    chem <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Chemistry")

    stream.ion <- chem %>%
        dplyr::filter(SampleType == "Routine", VisitType == "Primary", SampleFrame == "Stream", ReportingGroup == "Ion", SiteShort != "BAKR2") %>%
        tidyr::complete(FieldSeason, nesting(Park, SiteShort, SiteCode, SiteName, SampleFrame, Characteristic, CharacteristicLabel, ReportingGroup))

    stream.ion$Characteristic_f = factor(stream.ion$Characteristic, levels = c("ALK2", "Na", "Mg", "K", "Ca", "SO4-S", "Cl"))

    stream.ion.plot <- ggplot2::ggplot(stream.ion, aes(x = FieldSeason, y = LabValue, group = Characteristic)) +
        geom_point() +
        geom_line() +
        facet_grid(Characteristic_f~SiteShort, scales = "free_y") +
        ylab(label = "Concentration (mg/L)") +
        theme(axis.text.x = element_text(angle = 90))

    return(stream.ion.plot)

}

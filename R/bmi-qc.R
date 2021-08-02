#' Pivot BMI data to long format
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
#' c <- OpenDatabaseConnection
#' bmi_long <- BMILong(c)  # Pivot all BMI data longer
#' bmi_long_mill <- BMILong(c, site = "GRBA_S_MILL1")  # Pivot BMI data from Mill Creek
#' bmi_long_bakr_2015 <- BMILong(c, site = c("GRBA_S_BAKR2", "GRBA_S_BAKR3"), field.season = "2015")
#' CloseDatabaseConnection(c)
#' }
BMILong <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  bmi <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "BMI")
  # Fix column names (will eventually be fixed in db and we can get rid of this code)
  if ("PlecopteraTaxa" %in% names(bmi)) {
    bmi %<>% dplyr::rename(PlecopteraTaxaCount = PlecopteraTaxa)
  }

  count_cols <- "^(?!(Split|Fixed|BigRare|Clinger)).+Count$"  # Regex for selecting count columns
  abundance_cols <- "^(?!(DominantFamily|DominantTaxa)).+Abundance$"  # Regex for selecting abundance columns

  count_pivot <- bmi %>%
    dplyr::select(!tidyselect::matches(abundance_cols, perl = TRUE)) %>%
    tidyr::pivot_longer(tidyselect::matches(count_cols, perl = TRUE), names_to = "TaxaGroup", values_to = "TaxaGroupCount") %>%
    dplyr::mutate(TaxaGroup = gsub("Count", "", TaxaGroup),
                  TaxaGroup = gsub("Taxa", "", TaxaGroup))

  abundance_pivot <- bmi %>%
    dplyr::select(!tidyselect::matches(count_cols, perl = TRUE)) %>%
    tidyr::pivot_longer(tidyselect::matches(abundance_cols, perl = TRUE), names_to = "TaxaGroup", values_to = "TaxaGroupAbundance") %>%
    dplyr::mutate(TaxaGroup = gsub("Abundance", "", TaxaGroup),
                  TaxaGroup = gsub("Taxa", "", TaxaGroup))

  bmi_long <- dplyr::inner_join(count_pivot, abundance_pivot, by = c("Park", "SiteShort", "SiteCode", "SiteName", "FieldSeason", "VisitDate", "VisitType", "SampleType", "SampleCollectionMethod", "DPL", "BMIMethod", "LabSampleNumber", "DateCollected", "FieldSplit", "LabSplit", "SampleArea_m2", "Abundance", "Richness", "DominantTaxaPercent", "LabNotes", "FieldNotes", "SplitCount", "FixedCount", "BigRareCount", "ShannonsDiversity", "SimpsonsDiversity", "Evenness", "DominantFamilyAbundance", "DominantFamily", "DominantTaxa", "DominantTaxaAbundance", "Hilsenhoff", "USFSCommunityToleranceQuo", "ClingerTaxaCount", "LongLivedTaxa", "LabName", "ID", "TaxaGroup"))

  # Throw error if join gets messed up somehow
  if ((nrow(bmi_long) != nrow(count_pivot)) | (nrow(bmi_long) != nrow(abundance_pivot))) {
    stop(paste("Something went wrong when pivoting BMI data. There are", nrow(count_pivot), "rows of count data,", nrow(abundance_pivot), "rows of abundance data, and joining them yields", nrow(bmi_long), "rows of data."))
  }

  return(bmi_long)
}

#' Check for discrepancies between taxa count and abundance
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
#' c <- OpenDatabaseConnection
#' bmi_issues <- QcBMIDiscrepancies(c)  # Get all instances of discrepancy between taxa count and abundance
#' bmi_issues_mill <- QcBMIDiscrepancies(c, site = "GRBA_S_MILL1")  # Look at issues for Mill Creek only
#' bmi_issues_bakr_2015 <- QcBMIDiscrepancies(c, site = c("GRBA_S_BAKR2", "GRBA_S_BAKR3"), field.season = "2015")  # Look at issues for Baker Creek sites in 2015
#' CloseDatabaseConnection(c)
#' }
qcBMIDiscrepancies <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  bmi_issues <- BMILong(conn, path.to.data, park, site, field.season, data.source) %>%
    select(Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, VisitType, SampleType, SampleCollectionMethod, BMIMethod, LabSampleNumber, TaxaGroup, TaxaGroupCount, TaxaGroupAbundance, LabNotes) %>%
    filter((TaxaGroupCount == 0 & TaxaGroupAbundance > 0) | (TaxaGroupAbundance == 0 & TaxaGroupCount > 0))

  return(bmi_issues)
}


#' Filter channel characteristic data by visit type
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
ChannelCharacteristics <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  data <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Channel") %>%
    dplyr::select(-DPL)

  visit <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Visit")

  channel <- dplyr::left_join(data, visit[, c("Park", "SiteShort", "SiteCode", "SiteName", "FieldSeason", "VisitDate", "VisitType")], by = c("Park", "SiteShort", "SiteCode", "SiteName", "FieldSeason", "VisitDate")) %>%
    dplyr::filter(VisitType == "Primary") %>%
    dplyr::select(-c(DPL, VisitType)) %>%
    dplyr::arrange(SiteCode, VisitDate, Transect)

  return(channel_characteristics)
}

#' Rank channel flow type by count for each BMI sample
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
ChannelFLow <-  function(conn, path.to.data, park, site, field.season, data.source = "database") {
  channel_flow <- ChannelCharacteristics(conn, path.to.data, park, site, field.season, data.source) %>%
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, ChannelType) %>%
    dplyr::summarize(Count = n()) %>%
    dplyr::mutate(Rank = min_rank(desc(Count))) %>%
    dplyr::relocate(Count, .after = Rank) %>%
    dplyr::rename(ChannelFLow = ChannelType) %>%
    dplyr::arrange(SiteCode, VisitDate, Rank) %>%
    dplyr::ungroup()

  return(channel_flow)
}

#' Rank channel substrate type by count for each BMI sample
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
ChannelSubstrate <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  channel_subtrate <- ChannelCharacteristics(conn, path.to.data, park, site, field.season, data.source) %>%
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, Substrate) %>%
    dplyr::summarize(Count = n()) %>%
    dplyr::mutate(Rank = min_rank(desc(Count))) %>%
    dplyr::relocate(Count, .after = Rank) %>%
    dplyr::arrange(SiteCode, VisitDate, Rank) %>%
    dplyr::ungroup()

  return(channel_substrate)
}

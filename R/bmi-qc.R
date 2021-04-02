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

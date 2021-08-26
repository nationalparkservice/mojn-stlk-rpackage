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
  bmi <- ReadAndFilterData(conn, data.source = "database", data.name = "BMI")
  # Fix column names (will eventually be fixed in db and we can get rid of this code)
  if ("PlecopteraTaxa" %in% names(bmi)) {
    bmi %<>% dplyr::rename(PlecopteraTaxaCount = PlecopteraTaxa)
  }
  if ("LongLivedTaxa" %in% names(bmi)) {
    bmi %<>% dplyr::rename(LongLivedTaxaCount = LongLivedTaxa)
  }
  if ("Richness" %in% names(bmi)) {
    bmi %<>% dplyr::rename(TotalCount = Richness)
  }
  if ("Abundance" %in% names(bmi)) {
    bmi %<>% dplyr::rename(TotalAbundance = Abundance)
  }
  if ("DominantTaxa" %in% names(bmi)) {
    bmi %<>% dplyr::rename(DominantTaxon = DominantTaxa)
  }
  if ("DominantTaxaAbundance" %in% names(bmi)) {
    bmi %<>% dplyr::rename(DominantTaxonAbundance = DominantTaxaAbundance)
  }
  if ("DominantTaxaPercent" %in% names(bmi)) {
    bmi %<>% dplyr::rename(DominantTaxonPercent = DominantTaxaPercent)
  }
  if (!("ClingerAbundance" %in% names (bmi))) {
    bmi %<>% dplyr::mutate(ClingerAbundance = NA) %>%
      dplyr::relocate(ClingerAbundance, .after = ClingerTaxaCount)
  }
  if (!("LongLivedAbundance" %in% names (bmi))) {
    bmi %<>% dplyr::mutate(LongLivedAbundance = NA) %>%
      dplyr::relocate(LongLivedAbundance, .after = LongLivedTaxaCount)
  }
  if (!("DominantTaxonCount" %in% names (bmi))) {
    bmi %<>% dplyr::mutate(DominantTaxonCount = NA)
  }
  if (!("DominantFamilyCount" %in% names (bmi))) {
    bmi %<>% dplyr::mutate(DominantFamilyCount = NA)
  }

  bmi %<>% dplyr::mutate(DominantFamilyPercent = DominantFamilyAbundance/TotalAbundance*100)

  count_cols <- "^(?!(Split|Fixed|BigRare)).+Count$"  # Regex for selecting count columns
  abundance_cols <- "^.+Abundance$"  # Regex for selecting abundance columns

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

  bmi_long <- dplyr::inner_join(count_pivot, abundance_pivot, by = c("Park", "SiteShort", "SiteCode", "SiteName", "FieldSeason", "VisitDate", "VisitType", "SampleType", "SampleCollectionMethod", "DPL", "BMIMethod", "LabSampleNumber", "DateCollected", "LabNotes", "FieldNotes", "SampleArea_m2", "FieldSplit", "LabSplit", "SplitCount", "FixedCount", "BigRareCount", "ShannonsDiversity", "SimpsonsDiversity", "Hilsenhoff", "Evenness", "USFSCommunityToleranceQuo", "DominantFamily", "DominantFamilyPercent", "DominantTaxon", "DominantTaxonPercent", "LabName", "ID", "TaxaGroup"))

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
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, VisitType, SampleType, SampleCollectionMethod, BMIMethod, LabSampleNumber, TaxaGroup, TaxaGroupCount, TaxaGroupAbundance, LabNotes.
#' @export
#'
#' @examples
#' \dontrun{
#' c <- OpenDatabaseConnection
#' bmi_issues <- qcBMIDiscrepancies(c)  # Get all instances of discrepancy between taxa count and abundance
#' bmi_issues_mill <- qcBMIDiscrepancies(c, site = "GRBA_S_MILL1")  # Look at issues for Mill Creek only
#' bmi_issues_bakr_2015 <- qcBMIDiscrepancies(c, site = c("GRBA_S_BAKR2", "GRBA_S_BAKR3"), field.season = "2015")  # Look at issues for Baker Creek sites in 2015
#' CloseDatabaseConnection(c)
#' }
qcBMIDiscrepancies <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  bmi_issues <- BMILong(conn, path.to.data, park, site, field.season, data.source) %>%
    select(Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, VisitType, SampleType, SampleCollectionMethod, BMIMethod, LabSampleNumber, TaxaGroup, TaxaGroupCount, TaxaGroupAbundance, LabNotes) %>%
    filter((TaxaGroupCount == 0 & TaxaGroupAbundance > 0) | (TaxaGroupAbundance == 0 & TaxaGroupCount > 0))

  return(bmi_issues)
}


#' Filter channel characteristic data by primary visit type
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, Transect, TransectSide, ChannelType, Substrate, Notes
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- OpenDatabaseConnection
#' channel <- ChannelCharacteristics(conn)
#' channel_STRW2_2016 <- ChannelCharacteristics(conn, site = "GRBA_S_STRW2", field.season = "2016")
#' CloseDatabaseConnection(conn)
#' }
ChannelCharacteristics <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  data <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Channel")
  visit <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name = "Visit")

  channel_characteristics <- dplyr::left_join(data, visit[, c("Park", "SiteShort", "SiteCode", "SiteName", "FieldSeason", "VisitDate", "VisitType")], by = c("Park", "SiteShort", "SiteCode", "SiteName", "FieldSeason", "VisitDate")) %>%
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
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, ChannelFlow, Rank, Count
#' @export
#'
#' \dontrun{
#' conn <- OpenDatabaseConnection
#' channel_flow <- ChannelFlow(conn)
#' channel_flow_STRW2_2016 <- ChannelFlow(conn, site = "GRBA_S_STRW2", field.season = "2016")
#' CloseDatabaseConnection(conn)
#' }
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
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, ChannelFlow, Rank, Count
#' @export
#'
#' @examples
#' conn <- OpenDatabaseConnection
#' channel_substrate <- ChannelSubstrate(conn)
#' channel_substrate_STRW2_2016 <- ChannelSubstrate(conn, site = "GRBA_S_STRW2", field.season = "2016")
#' CloseDatabaseConnection(conn)
#' }
ChannelSubstrate <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  channel_substrate <- ChannelCharacteristics(conn, path.to.data, park, site, field.season, data.source) %>%
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, Substrate) %>%
    dplyr::summarize(Count = n()) %>%
    dplyr::mutate(Rank = min_rank(desc(Count))) %>%
    dplyr::relocate(Count, .after = Rank) %>%
    dplyr::arrange(SiteCode, VisitDate, Rank) %>%
    dplyr::ungroup()

  return(channel_substrate)
}

#' Intermediate step used to pivot BMI data to an even longer format for ease of plotting
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
qcBMIFormatted <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  bmi_long <- BMILong(conn, path.to.data, park, site, field.season, data.source)

  bmi_formatted <- bmi_long %>%
    tidyr::pivot_longer(cols = c("TaxaGroupCount", "TaxaGroupAbundance"), names_to = "Metric", values_to = "Count")

  bmi_formatted$Metric[bmi_formatted$Metric == "TaxaGroupCount"] <- "Richness"
  bmi_formatted$Metric[bmi_formatted$Metric == "TaxaGroupAbundance"] <- "Abundance"

  bmi_formatted$TaxaGroupMetric <- paste(bmi_formatted$TaxaGroup, bmi_formatted$Metric, sep = " ")

  bmi_formatted %<>% dplyr::relocate(TaxaGroupMetric, .before = Count)

  return(bmi_formatted)

}


#' Plot overall richness and abundance metrics for each BMI sample.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A ggplot object
#' @export
#'
BMIGeneralMetricsPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  bmi.formatted <- qcBMIFormatted(conn, path.to.data, park, site, field.season, data.source)

  bmi.gen <- bmi.formatted %>%
    dplyr::filter(SampleType == "Routine", VisitType == "Primary", SiteShort != "BAKR2",
                  TaxaGroup %in% c("Total", "DominantFamily"))

  bmi.gen$Metric_f = factor(bmi.gen$Metric, levels = c("Richness", "Abundance"))
  bmi.gen$TaxaGroup_f = factor(bmi.gen$TaxaGroup, levels = c("Total", "DominantFamily"))

  bmi.gen.plot <- ggplot2::ggplot(bmi.gen, aes(x = FieldSeason, y = Count, group = TaxaGroup, color = TaxaGroup_f)) +
    geom_point() +
    geom_line() +
    facet_grid(Metric_f~SiteShort, scales = "free_y") +
    ylab(label = "Count") +
    theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
    labs(title = "BMI general metrics", color = "Group") +
    scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA))


  return(bmi.gen.plot)

}


#' Plot diversity-related metrics and indices for each BMI sample.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A ggplot object
#' @export
#'
BMIDiversityMetricsPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  bmi.formatted <- qcBMIFormatted(conn, path.to.data, park, site, field.season, data.source)

  bmi.div <- bmi.formatted %>%
    dplyr::filter(SampleType == "Routine", VisitType == "Primary", SiteShort != "BAKR2") %>%
    dplyr::select(-c("TaxaGroup", "Metric", "TaxaGroupMetric", "Count")) %>%
    dplyr::distinct() %>%
    tidyr::pivot_longer(cols = c("ShannonsDiversity", "SimpsonsDiversity", "Evenness", "Hilsenhoff"), names_to = "Metric", values_to = "Value")

  bmi.div$Metric[bmi.div$Metric == "Hilsenhoff"] <- "HilsenhoffIndex"

  bmi.div$Metric_f = factor(bmi.div$Metric, levels = c("ShannonsDiversity", "SimpsonsDiversity", "Evenness", "HilsenhoffIndex"))

  bmi.div.plot <- ggplot2::ggplot(bmi.div, aes(x = FieldSeason, y = Value, group = Metric)) +
    geom_point() +
    geom_line() +
    facet_grid(Metric_f~SiteShort, scales = "free_y") +
    ylab(label = "Value") +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = "BMI diversity metrics") +
    scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA))

  return(bmi.div.plot)

}


#' Plot tolerance-related richness and abundance metrics for each BMI sample.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A ggplot object
#' @export
#'
BMIToleranceMetricsPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  bmi.formatted <- BMIFormatted(conn, path.to.data, park, site, field.season, data.source)

  bmi.tol <- bmi.formatted %>%
    dplyr::filter(SampleType == "Routine", VisitType == "Primary", SiteShort != "BAKR2",
                  TaxaGroup %in% c("EPT", "Tolerant", "Intolerant", "LongLived"))

  bmi.tol$Metric_f = factor(bmi.tol$Metric, levels = c("Richness", "Abundance"))
  bmi.tol$TaxaGroup_f = factor(bmi.tol$TaxaGroup, levels = c("EPT", "Tolerant", "Intolerant", "LongLived"))

  bmi.tol.plot <- ggplot2::ggplot(bmi.tol, aes(x = FieldSeason, y = Count, color = TaxaGroup_f)) +
    geom_point() +
    geom_line(aes(group = TaxaGroup)) +
    facet_grid(Metric_f~SiteShort, scales = "free_y") +
    ylab(label = "Count") +
    theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
    labs(title = "BMI tolerance metrics", color = "Tolerance Group") +
    scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA))

  return(bmi.tol.plot)

}


#' Plot functional feeding group-related richness and abundance metrics for each BMI sample.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A ggplot object
#' @export
#'
BMIFunctionalMetricsPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  bmi.formatted <- BMIFormatted(conn, path.to.data, park, site, field.season, data.source)

  bmi.fun <- bmi.formatted %>%
    dplyr::filter(SampleType == "Routine", VisitType == "Primary", SiteShort != "BAKR2",
                  TaxaGroup %in% c("Shredder", "Scraper", "CollectorFilterer", "CollectorGatherer", "Clinger", "Predator"))

  bmi.fun$Metric_f = factor(bmi.fun$Metric, levels = c("Richness", "Abundance"))
  bmi.fun$TaxaGroup_f = factor(bmi.fun$TaxaGroup, levels = c("Shredder", "Scraper", "CollectorFilterer", "CollectorGatherer", "Clinger", "Predator"))

  bmi.fun.plot <- ggplot2::ggplot(bmi.fun, aes(x = FieldSeason, y = Count, color = TaxaGroup_f)) +
    geom_point() +
    geom_line(aes(group = TaxaGroup)) +
    facet_grid(Metric_f~SiteShort, scales = "free_y") +
    ylab(label = "Count") +
    theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
    labs(title = "BMI functional feeding group metrics", color = "Functional Feeding Group") +
    scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA))

  return(bmi.fun.plot)

}


#' Plot taxonomic-related richness and abundance metrics for each BMI sample.
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A ggplot object
#' @export
#'
BMITaxonomicMetricsPlot <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  bmi.formatted <- BMIFormatted(conn, path.to.data, park, site, field.season, data.source)

  bmi.tax <- bmi.formatted %>%
    dplyr::filter(SampleType == "Routine", VisitType == "Primary", SiteShort != "BAKR2",
                  TaxaGroup %in% c("Insect", "Coleoptera", "Diptera", "Ephemoeroptera", "Megaloptera", "Plecoptera","Tricoptera", "Chironomidae", "Elmidae", "NonInsect", "Mollusca", "Crustacea", "Oligochaete"))

  bmi.tax$Metric_f = factor(bmi.tax$Metric, levels = c("Richness", "Abundance"))
  bmi.tax$TaxaGroup_f = factor(bmi.tax$TaxaGroup, levels = c("Insect", "Coleoptera", "Diptera", "Ephemoeroptera", "Megaloptera", "Plecoptera", "Tricoptera", "Chironomidae", "Elmidae", "NonInsect", "Mollusca", "Crustacea", "Oligochaete"))

  bmi.tax.plot <- ggplot2::ggplot(bmi.tax, aes(x = FieldSeason, y = Count, color = TaxaGroup_f)) +
    geom_point() +
    geom_line(aes(group = TaxaGroup)) +
    facet_grid(Metric_f~SiteShort, scales = "free_y") +
    ylab(label = "Count") +
    theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
    labs(title = "BMI taxonomic group metrics", color = "Taxonomic Group") +
    scale_y_continuous(breaks = pretty_breaks(), limits = c(0, NA))

  return(bmi.tax.plot)

}


#################################

bmi.div.plot <- ggplot2::ggplot(bmi.div, aes(x = FieldSeason, y = Value, group = Metric, color = Metric)) +
  geom_point() +
  geom_line() +
  facet_grid(~SiteShort, scales = "free_y") +
  ylab(label = "Value") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
  labs(title = "BMI diversity metrics")

############################################

bmi.formatted <- BMIFormatted(conn, path.to.data, park, site, field.season, data.source)

bmi.tol <- bmi.formatted %>%
  dplyr::filter(SampleType == "Routine", VisitType == "Primary", SiteShort != "BAKR2",
                TaxaGroup %in% c("EPT", "Tolerant", "Intolerant", "LongLived"),
                TaxaGroupMetric != "LongLived Abundance")

bmi.tol$Metric_f = factor(bmi.tol$Metric, levels = c("Richness", "Abundance"))
bmi.tol$TaxaGroup_f = factor(bmi.tol$TaxaGroup, levels = c("EPT", "Tolerant", "Intolerant", "LongLived"))
bmi.tol$TaxaGroupMetric_f = factor(bmi.tol$TaxaGroupMetric, levels = c("EPT Richness", "Tolerant Richness", "Intolerant Richness", "LongLived Richness", "EPT Abundance", "Tolerant Abundance", "Intolerant Abundance"))

bmi.tol.plot <- ggplot2::ggplot(bmi.tol, aes(x = FieldSeason, y = Amount, color = Metric)) +
  geom_point() +
  geom_line(aes(group = TaxaGroup)) +
  facet_grid(Metric_f+TaxaGroup_f~SiteShort, scales = "free_y") +
  ylab(label = "Count") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "BMI tolerance metrics") +
  theme(legend.position = "none")

###########################################

bmi.formatted <- BMIFormatted(conn, path.to.data, park, site, field.season, data.source)

bmi.tax <- bmi.formatted %>%
  dplyr::filter(SampleType == "Routine", VisitType == "Primary", SiteShort != "BAKR2",
                TaxaGroup %in% c("Insect", "Coleoptera", "Diptera", "Ephemoeroptera", "Megaloptera", "Plecoptera","Tricoptera", "Chironomidae", "Elmidae", "NonInsect", "Mollusca", "Crustacea", "Oligochaete"))

bmi.tax$Metric_f = factor(bmi.tax$Metric, levels = c("Richness", "Abundance"))
bmi.tax$TaxaGroup_f = factor(bmi.tax$TaxaGroup, levels = c("Insect", "Coleoptera", "Diptera", "Ephemoeroptera", "Megaloptera", "Plecoptera","Tricoptera", "Chironomidae", "Elmidae", "NonInsect", "Mollusca", "Crustacea", "Oligochaete"))
bmi.tax$TaxaGroupMetric_f = factor(bmi.tax$TaxaGroupMetric, levels = c("Insect Richness", "Insect Abundance", "Coleoptera Richness", "Coleoptera Abundance", "Diptera Richness", "Diptera Abundance", "Ephemoeroptera Richness", "Ephemoeroptera Abundance", "Plecoptera Richness", "Plecoptera Abundance", "Megaloptera Richness", "Megaloptera Abundance", "Tricoptera Richness", "Tricoptera Abundance", "Chironomidae Richness", "Chironomidae Abundance", "Elmidae Richness", "Elmidae Abundance", "NonInsect Richness", "NonInsect Abundance", "Mollusca Richness", "Mollusca Abundance", "Crustacea Richness", "Crustacea Abundance", "Oligochaete Richness", "Oligochaete Abundance"))

bmi.tax.plot <- ggplot2::ggplot(bmi.tax, aes(x = FieldSeason, y = Amount, color = Metric)) +
  geom_point() +
  geom_line(aes(group = TaxaGroup)) +
  facet_grid(Metric_f+TaxaGroup_f~SiteShort, scales = "free_y") +
  ylab(label = "Count") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "BMI taxonomic group metrics") +
  theme(legend.position = "none")

#' Return BMI metrics data
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR1".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'   BMIMetricsLong()
#'   BMIMetricsLong(site = c("GRBA_S_MILL1", "GRBA_S_Pine1"), field.season = "2015")
#' }
BMIMetricsLong <- function(park, site, field.season) {
  metrics <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "BMIMetrics")
  visit <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "BMIVisit")
  meta <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit") # Delete once VisitType is added to BMIVisit table

  meta <- meta |> # Delete once VisitType is added to BMIVisit table
    dplyr::select(SiteCode, VisitDate, SiteShort, VisitType) |>
    unique()

  visit <- visit |> # Add VisitType to selected columns once added to BMIVisit table in AGOL
    dplyr::select(SampleID, Laboratory, Project, Park, SiteCode, SiteName, CollectionDate, FieldSeason, AnalysisType, SamplerType, Area, FieldSplit, LabSplit, SplitCount) |>
    dplyr::rename(VisitDate = CollectionDate) |>
    unique()

  metrics <- metrics |>
    dplyr::select(SampleID, Attribute, Value) |>
    unique()

  join <- metrics |>
    dplyr::left_join(visit, by = "SampleID", relationship = "many-to-one") |>
    dplyr::left_join(meta, by = c("SiteCode", "VisitDate"), relationship = "many-to-one") |>
    dplyr::select(Laboratory, SampleID, Project, Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, AnalysisType, SamplerType, Area, FieldSplit, LabSplit, SplitCount, Attribute, Value) |>
    unique()

  return(join)
}


#' Return BMI species data
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR1".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'   BMISpecies()
#'   BMISpecies(site = c("GRBA_S_MILL1", "GRBA_S_Pine1"), field.season = "2015")
#' }
BMISpecies <- function(park, site, field.season) {
  species <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "BMISpecies")
  visit <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "BMIVisit")

  visit <- visit |>
    dplyr::select(SampleID, Laboratory, Project, Park, SiteCode, SiteName, CollectionDate, FieldSeason, AnalysisType, SamplerType, Area, FieldSplit, LabSplit, SplitCount)

  filtered <- species |>
    dplyr::rename(Order = Order_) |>
    dplyr::filter(!(Phylum %in% c("Chordata"))) |>
    dplyr::mutate(Phylum = dplyr::case_when((Class =="Malacostraca") & (is.na(Phylum)) ~ "Arthropoda",
                                            (Class == "Branchiopoda") & (is.na(Phylum)) ~ "Arthropoda",
                                            (ScientificName == "Nematoda") & (is.na(Phylum)) ~ "Nematoda",
                                            (ScientificName == "Platyhelminthes") & (is.na(Phylum)) ~ "Platyhelminthes",
                                            (ScientificName == "Xenacoelomorpha") & (is.na(Phylum)) ~ "Xenacoelomorpha",
                                            (ScientificName == "Oligochaeta") & (is.na(Phylum)) ~ "Annelida",
                                            TRUE ~ Phylum),
                  Class = dplyr::case_when((ScientificName == "Oligochaeta") & (is.na(Class)) ~ "Clitellata",
                                            (ScientificName == "Collembola") & (is.na(Class)) ~ "Entognatha",
                                            TRUE ~ Class)) |>
    dplyr::select(SampleID, Phylum, Class, Order, Family, SubFamily, Genus, Species, ScientificName, OTUName, LifeStage, Notes, LabCount, BigRareCount)

  join <- filtered |>
    dplyr::left_join(visit, by = "SampleID", relationship = "many-to-many") |>
    dplyr::select(Laboratory, SampleID, Project, Park, SiteCode, SiteName, CollectionDate, FieldSeason, AnalysisType, SamplerType, Area, FieldSplit, LabSplit, SplitCount, Phylum, Class, Order, Family, SubFamily, Genus, Species, ScientificName, OTUName, LifeStage, Notes, LabCount, BigRareCount)

  return(join)
}


#' BROKEN! FIX! Check for discrepancies between taxa count and abundance
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
#'   qcBMIDiscrepancies()
#'   qcBMIDiscrepancies(site = "GRBA_S_MILL1")
#'   qcBMIDiscrepancies(site = c("GRBA_S_BAKR2", "GRBA_S_BAKR3"), field.season = "2015")
#' }
qcBMIDiscrepancies <- function(park, site, field.season) {
  import <- BMIFormatted(park = park, site = site, field.season = field.season)

  bmi_issues <- import |>
    dplyr::filter(Metric %in% c("Richness", "Density")) |>
    dplyr::mutate(Pivot = dplyr::case_when(Category %in% c("Overall") ~ paste0(Category),
                                           TRUE ~ paste0(Type))) |>
    dplyr::select(Park, SiteShort, SiteCode, SiteName, CollectionDate, FieldSeason, Metric, Pivot, Value) |>
    tidyr::pivot_wider(names_from = Pivot, values_from = Value)
    dplyr::filter((TaxaGroupRichness == 0 & TaxaGroupDensity > 0) | (TaxaGroupDensity == 0 & TaxaGroupRichness > 0))

  return(bmi_issues)
}


#' Return a table of channel characteristics data
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
#'   channel <- ChannelCharacteristics()
#'   channel <- ChannelCharacteristics(site = "GRBA_S_STRW2", field.season = "2016")
#' }
ChannelCharacteristics <- function(park, site, field.season) {
  channel <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Channel")
  visit <- ReadAndFilterData(park = park, site = site, field.season = field.season, data.name = "Visit")

  visit <- visit |>
    dplyr::select(SiteCode, VisitDate, VisitType)

  channel_characteristics <- channel |>
    dplyr::left_join(visit, by = c("SiteCode", "VisitDate")) |>
    dplyr::filter(VisitType == "Primary")|>
    dplyr::select(-c(DPL, VisitType)) |>
    dplyr::arrange(SiteCode, VisitDate, Transect)

  return(channel_characteristics)
}

#' Rank channel flow type by count for each BMI sample
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
#'   ChannelFlow()
#'   ChannelFlow(site = "GRBA_S_STRW2", field.season = "2016")
#' }
ChannelFlow <-  function(park, site, field.season) {
  flow <- ChannelCharacteristics(park = park, site = site, field.season = field.season)

  channel_flow <- flow |>
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, ChannelType) |>
    dplyr::summarize(Count = dplyr::n()) |>
    dplyr::mutate(Rank = dplyr::min_rank(dplyr::desc(Count))) |>
    dplyr::relocate(Count, .after = Rank) |>
    dplyr::rename(ChannelFlow = ChannelType) |>
    dplyr::arrange(SiteCode, VisitDate, Rank) |>
    dplyr::ungroup()

  return(channel_flow)
}

#' Rank channel substrate type by count for each BMI sample
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble with columns Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, ChannelFlow, Rank, Count
#' @export
#'
#' @examples
#' \dontrun{
#'   ChannelSubstrate()
#'   ChannelSubstrate(site = "GRBA_S_STRW2", field.season = "2016")
#' }
ChannelSubstrate <- function(park, site, field.season) {
  substrate <- ChannelCharacteristics(park = park, site = site, field.season = field.season)

  channel_substrate <- substrate |>
    dplyr::group_by(Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, Substrate) |>
    dplyr::summarize(Count = dplyr::n()) |>
    dplyr::mutate(Rank = dplyr::min_rank(dplyr::desc(Count))) |>
    dplyr::relocate(Count, .after = Rank) |>
    dplyr::arrange(SiteCode, VisitDate, Rank) |>
    dplyr::ungroup()

  return(channel_substrate)
}

#' Create additional filtering and labeling columns for ease of plotting
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A tibble
#' @export
#'
BMIFormatted <- function(park, site, field.season) {
  bmi.long <- BMIMetricsLong(park = park, site = site, field.season = field.season)

  bmi.formatted <- bmi.long |>
    dplyr::mutate(Metric = dplyr::case_when(grepl("Richness", Attribute) ~ "Richness",
                                            grepl("Density", Attribute) ~ "Density",
                                            grepl("Hilsenhoff|Shannons|Simpsons|Evenness", Attribute) ~ "Index",
                                            grepl("DominantFamily", Attribute) ~ "Fraction",
                                            grepl("DominantTaxon", Attribute) ~ "Fraction",
                                            TRUE ~ as.character(Attribute))) |>
    dplyr::mutate(Category = dplyr::case_when(Attribute %in% c("UniqueRichness") ~ "Overall",
                                              Attribute %in% c("Density") ~ "Overall",
                                              grepl("Feed", Attribute) ~ "Functional Feeding Group",
                                              grepl("Habit", Attribute) ~ "Habit",
                                              grepl("LongLived|Intolerant|Tolerant", Attribute) ~ "Sensitivity",
                                              grepl("Insecta|Ephemeroptera|Plecoptera|Trichoptera|Coleoptera|Elmidae|Diptera|Chironomidae|Megaloptera|Crustacea|NonInsects|Oligochaeta|Mollusca", Attribute) ~ "Taxa Group",
                                              grepl("DominantFamily", Attribute) ~ "Dominant Family",
                                              grepl("DominantTaxon", Attribute) ~ "Dominant Taxon",
                                              grepl("Hilsenhoff", Attribute) ~ "Hilsenhoff",
                                              grepl("Shannons", Attribute) ~ "Shannons",
                                              grepl("Simpsons", Attribute) ~ "Simpsons",
                                              grepl("Evenness", Attribute) ~ "Evenness",
                                              TRUE ~ as.character(Attribute))) |>
    dplyr::mutate(Type = dplyr::case_when(grepl("CollectorFilterer", Attribute) ~ "Collector Filterer",
                                          grepl("CollectorGatherer", Attribute) ~ "Collector Gatherer",
                                          grepl("Scraper", Attribute) ~ "Scraper",
                                          grepl("Shredder", Attribute) ~ "Shredder",
                                          grepl("Parasite", Attribute) ~ "Parasite",
                                          grepl("Predator", Attribute) ~ "Predator",
                                          grepl("PiercerHerbivore", Attribute) ~ "Piercer Herbivore",
                                          grepl("Clinger", Attribute) ~ "Clinger",
                                          grepl("Planktonic", Attribute) ~ "Planktonic",
                                          grepl("Skater", Attribute) ~ "Skater",
                                          grepl("Climber", Attribute) ~ "Climber",
                                          grepl("Crawler", Attribute) ~ "Crawler",
                                          grepl("Swimmer", Attribute) ~ "Swimmer",
                                          grepl("Burrower", Attribute) ~ "Burrower",
                                          grepl("Sprawler", Attribute) ~ "Sprawler",
                                          grepl("LongLived", Attribute) ~ "Long Lived",
                                          grepl("Intolerant", Attribute) ~ "Intolerant",
                                          grepl("Tolerant", Attribute) ~ "Tolerant",
                                          grepl("Insecta", Attribute) ~ "Insecta",
                                          grepl("Ephemeroptera", Attribute) ~ "Ephemeroptera",
                                          grepl("Plecoptera", Attribute) ~ "Plecoptera",
                                          grepl("Trichoptera", Attribute) ~ "Trichoptera",
                                          grepl("Coleoptera", Attribute) ~ "Coleoptera",
                                          grepl("Elmidae", Attribute) ~ "Elmidae",
                                          grepl("Diptera", Attribute) ~ "Diptera",
                                          grepl("Chironomidae", Attribute) ~ "Chironomidae",
                                          grepl("Megaloptera", Attribute) ~ "Megaloptera",
                                          grepl("Crustacea", Attribute) ~ "Crustacea",
                                          grepl("NonInsects", Attribute) ~ "NonInsects",
                                          grepl("Oligochaeta", Attribute) ~ "Oligochaeta",
                                          grepl("Mollusca", Attribute) ~ "Mollusca",
                                          TRUE ~ NA_character_)) |>
    dplyr::mutate(Label = dplyr::case_when(Category %in% c("Functional Feeding Group", "Habit", "Sensitivity") ~ paste0(Category, ": ", Type),
                                           Category %in% c("Taxa Group") ~ paste0(Category, ": ", Type),
                                           Category %in% c("Overall") ~ paste0(Category, " ", Metric),
                                           Metric %in% c("Index") ~ paste0(Metric, ": ", Category),
                                           Metric %in% c("Fraction") ~ paste0(Metric, ": ", Category),
                                           TRUE ~ NA_character_))

  return(bmi.formatted)

}


#' Plot overall richness and abundance metrics for each BMI sample.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A ggplot object
#' @export
#'
BMIGeneralMetricsPlot <- function(park, site, field.season) {
  bmi.formatted <- BMIFormatted(park = park, site = site, field.season = field.season)

  bmi.gen <- bmi.formatted |>
    dplyr::filter(AnalysisType == "Routine", VisitType == "Primary", SiteCode != "GRBA_S_BAKR2",
                  Category %in% c("Overall", "Dominant Family", "Dominant Taxon"))

  bmi.gen$Metric_f = factor(bmi.gen$Metric, levels = c("Richness", "Density", "Fraction"))
  bmi.gen$Category_f = factor(bmi.gen$Category, levels = c("Overall", "Dominant Family", "Dominant Taxon"))

  bmi.gen.plot <- ggplot2::ggplot(bmi.gen, ggplot2::aes(x = FieldSeason,
                                               y = Value,
                                               group = Category_f,
                                               color = Category_f,
                                               text = paste0("Field Season: ", FieldSeason, "<br>",
                                                             "Count: ", Value, "<br>",
                                                             "Category: ", Category_f))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_grid(Metric_f~SiteShort, scales = "free_y") +
    ggplot2::ylab(label = "Count") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom") +
    ggplot2::labs(title = "BMI general metrics", color = "Group") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
    khroma::scale_color_muted()


  return(bmi.gen.plot)

}


#' Plot diversity-related metrics and indices for each BMI sample.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A ggplot object
#' @export
#'
BMIDiversityMetricsPlot <- function(park, site, field.season) {
  bmi.formatted <- BMIFormatted(park = park, site = site, field.season = field.season)

  bmi.div <- bmi.formatted |>
    dplyr::filter(AnalysisType == "Routine", VisitType == "Primary", SiteShort != "BAKR2",
                  Metric == "Index")

  bmi.div$Category_f = factor(bmi.div$Category, levels = c("Shannons", "Simpsons", "Evenness", "Hilsenhoff"))

  bmi.div.plot <- ggplot2::ggplot(bmi.div, ggplot2::aes(x = FieldSeason,
                                               y = Value,
                                               group = Category_f,
                                               text = paste0("Field Season: ", FieldSeason, "<br>",
                                                             "Value: ", Value, "<br>",
                                                             "Metric: ", Category_f))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_grid(Category_f~SiteShort, scales = "free_y") +
    ggplot2::ylab(label = "Value") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::labs(title = "BMI diversity metrics") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_discrete(breaks = scales::pretty_breaks())

  return(bmi.div.plot)

}


#' Plot tolerance-related richness and abundance metrics for each BMI sample.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A ggplot object
#' @export
#'
BMISensitivityMetricsPlot <- function(park, site, field.season) {
  bmi.formatted <- BMIFormatted(park = park, site = site, field.season = field.season)

  bmi.tol <- bmi.formatted |>
    dplyr::filter(AnalysisType == "Routine", VisitType == "Primary", SiteShort != "BAKR2",
                  Category %in% c("Sensitivity"))

  bmi.tol$Metric_f = factor(bmi.tol$Metric, levels = c("Richness", "Density"))
  bmi.tol$Type_f = factor(bmi.tol$Type, levels = c("Tolerant", "Intolerant", "Long Lived"))

  bmi.tol.plot <- ggplot2::ggplot(bmi.tol, ggplot2::aes(x = FieldSeason,
                                               y = Value,
                                               color = Type_f,
                                               text = paste0("Field Season: ", FieldSeason, "<br>",
                                                             "Count: ", Value, "<br>",
                                                             "Sensitivity: ", Type_f))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(group = Type_f),
              linewidth = 1) +
    ggplot2::facet_grid(Metric_f~SiteShort, scales = "free_y") +
    ggplot2::ylab(label = "Count") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom") +
    ggplot2::labs(title = "BMI sensitivity metrics", color = "Sensitivity Group") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
    khroma::scale_color_muted()

  return(bmi.tol.plot)

}


#' Plot functional feeding group-related richness and abundance metrics for each BMI sample.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A ggplot object
#' @export
#'
BMIFunctionalMetricsPlot <- function(park, site, field.season) {
  bmi.formatted <- BMIFormatted(park = park, site = site, field.season = field.season)

  bmi.fun <- bmi.formatted |>
    dplyr::filter(AnalysisType == "Routine", VisitType == "Primary", SiteShort != "BAKR2",
                  Category %in% c("Functional Feeding Group"))

  bmi.fun$Metric_f = factor(bmi.fun$Metric, levels = c("Richness", "Density"))
  bmi.fun$Type_f = factor(bmi.fun$Type, levels = c("Collector Filterer", "Collector Gatherer", "Parasite", "Piercer Herbivore", "Predator", "Scraper", "Shredder"))

  bmi.fun.plot <- ggplot2::ggplot(bmi.fun, ggplot2::aes(x = FieldSeason,
                                               y = Value,
                                               color = Type_f,
                                               text = paste0("Field Season: ", FieldSeason, "<br>",
                                                             "Count: ", Value, "<br>",
                                                             "Functional Feeding Group: ", Type_f))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(group = Type),
              linewidth = 1) +
    ggplot2::facet_grid(Metric_f~SiteShort, scales = "free_y") +
    ggplot2::ylab(label = "Count") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom") +
    ggplot2::labs(title = "BMI functional feeding group metrics", color = "Functional Feeding Group") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
    khroma::scale_color_muted()

  return(bmi.fun.plot)

}


#' Plot habit-related richness and abundance metrics for each BMI sample.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A ggplot object
#' @export
#'
BMIHabitMetricsPlot <- function(park, site, field.season) {
  bmi.formatted <- BMIFormatted(park = park, site = site, field.season = field.season)

  bmi.hab <- bmi.formatted |>
    dplyr::filter(AnalysisType == "Routine", VisitType == "Primary", SiteShort != "BAKR2",
                  Category %in% c("Habit"))

  bmi.hab$Metric_f = factor(bmi.hab$Metric, levels = c("Richness", "Density"))
  bmi.hab$Type_f = factor(bmi.hab$Type, levels = c("Burrower", "Climber", "Clinger", "Crawler", "Planktonic", "Skater", "Sprawler", "Swimmer"))

  bmi.hab.plot <- ggplot2::ggplot(bmi.hab, ggplot2::aes(x = FieldSeason,
                                                        y = Value,
                                                        color = Type_f,
                                                        text = paste0("Field Season: ", FieldSeason, "<br>",
                                                                      "Count: ", Value, "<br>",
                                                                      "Habit: ", Type_f))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(group = Type),
                       linewidth = 1) +
    ggplot2::facet_grid(Metric_f~SiteShort, scales = "free_y") +
    ggplot2::ylab(label = "Count") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom") +
    ggplot2::labs(title = "BMI habit metrics", color = "Habit") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
    khroma::scale_color_muted()

  return(bmi.hab.plot)

}


#' Plot taxonomic-related richness and abundance metrics for each BMI sample.
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A ggplot object
#' @export
#'
BMITaxonomicMetricsPlot <- function(park, site, field.season) {
  bmi.formatted <- BMIFormatted(park = park, site = site, field.season = field.season)

  bmi.tax <- bmi.formatted |>
    dplyr::filter(AnalysisType == "Routine", VisitType == "Primary", SiteShort != "BAKR2",
                  Category %in% c("Taxa Group"))

  bmi.tax$Metric_f = factor(bmi.tax$Metric, levels = c("Richness", "Density"))
  bmi.tax$Type_f = factor(bmi.tax$Type, levels = c("Insecta", "Coleoptera", "Diptera", "Ephemeroptera", "Megaloptera", "Plecoptera", "Trichoptera", "Chironomidae", "Elmidae", "Crustacea", "Mollusca", "Oligochaeta", "NonInsects"))

  bmi.tax.plot <- ggplot2::ggplot(bmi.tax, ggplot2::aes(x = FieldSeason,
                                               y = Value,
                                               color = Type_f,
                                               text = paste0("Field Season: ", FieldSeason, "<br>",
                                                             "Count: ", Value, "<br>",
                                                             "Taxa Group: ", Type_f))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(group = Type),
              linewidth = 1) +
    ggplot2::facet_grid(Metric_f~SiteShort, scales = "free_y") +
    ggplot2::ylab(label = "Count") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom") +
    ggplot2::labs(title = "BMI taxonomic group metrics", color = "Taxonomic Group") +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA)) +
    ggplot2::scale_x_discrete(breaks = scales::pretty_breaks()) +
    khroma::scale_color_discreterainbow()

  return(bmi.tax.plot)

}


#' Pivot BMI data to long format (Deprecated data! Use for QC purposes only)
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
#' BMILong()
#' BMILong(site = "GRBA_S_MILL1")
#' BMILong(site = c("GRBA_S_BAKR2", "GRBA_S_BAKR3"), field.season = "2015")
#' }
BMILong <- function(park, site, field.season) {
  bmi <- ReadAndFilterData(data.name = "BMI") |>
    dplyr::rename(AnalysisType = SampleType,
                  SamplerType = SampleCollectionMethod,
                  SampleID = LabSampleNumber,
                  Area = SampleArea_m2) |>
    dplyr::select(-c(DPL, BMIMethod, DateCollected, LabNotes, FieldNotes, FixedCount, BigRareCount, DominantFamily, DominantTaxa, LabName)) |>
    dplyr::mutate(Laboratory = "NAMC",
                  Project = "STLK",
                  Delivery = "Deprecated") |>
    dplyr::relocate(Delivery, Laboratory, SampleID, Project, Park, SiteShort, SiteCode, SiteName, VisitDate, FieldSeason, VisitType, AnalysisType, SamplerType, Area, FieldSplit, LabSplit, SplitCount) |>
    dplyr::mutate(DominantFamily = DominantFamilyAbundance/Abundance) |>
    tidyr::pivot_longer(cols = Abundance:DominantFamily, names_to = "Attribute", values_to = "Value") |>
    dplyr::mutate(Attribute = dplyr::case_when(Attribute == "PlecopteraTaxa" ~ "PlecopteraTaxaCount",
                                               Attribute == "LongLivedTaxa" ~ "LongLivedTaxaCount",
                                               Attribute == "Richness" ~ "TotalCount",
                                               Attribute == "Abundance" ~ "TotalAbundance",
                                               Attribute == "DominantTaxa" ~ "DominantTaxon",
                                               Attribute == "InsectTaxaCount" ~ "InsectaTaxaCount",
                                               Attribute == "InsectAbundance" ~ "InsectaAbundance",
                                               Attribute == "DominantTaxaAbundance" ~ "DominantTaxonAbundance",
                                               Attribute == "DominantTaxaPercent" ~ "DominantTaxonPercent",
                                               TRUE ~ Attribute)) |>
    dplyr::mutate(Metric = dplyr::case_when(grepl("Count", Attribute) ~ "Richness",
                                            grepl("Abundance", Attribute) ~ "Density",
                                            grepl("Hilsenhoff|Shannons|Simpsons|Evenness|USFS", Attribute) ~ "Index",
                                            grepl("DominantFamily", Attribute) ~ "Fraction",
                                            grepl("DominantTaxon", Attribute) ~ "Fraction",
                                            TRUE ~ as.character(Attribute))) |>
    dplyr::mutate(Category = dplyr::case_when(Attribute %in% c("TotalCount") ~ "Overall",
                                              Attribute %in% c("TotalAbundance") ~ "Overall",
                                              grepl("Shredder|Scraper|Collector|Predator", Attribute) ~ "Functional Feeding Group",
                                              grepl("Clinger", Attribute) ~ "Habit",
                                              grepl("LongLived|Intolerant|Tolerant", Attribute) ~ "Sensitivity",
                                              grepl("Insect|Ephemeroptera|Plecoptera|Trichoptera|Coleoptera|Elmidae|Diptera|Chironomidae|Megaloptera|Crustacea|NonInsect|Oligochaete|Mollusca", Attribute) ~ "Taxa Group",
                                              grepl("DominantFamily", Attribute) ~ "Dominant Family",
                                              grepl("DominantTaxon", Attribute) ~ "Dominant Taxon",
                                              grepl("Hilsenhoff", Attribute) ~ "Hilsenhoff",
                                              grepl("Shannons", Attribute) ~ "Shannons",
                                              grepl("Simpsons", Attribute) ~ "Simpsons",
                                              grepl("Evenness", Attribute) ~ "Evenness",
                                              grepl("USFS", Attribute) ~ "USFS Community Tolerance Quotient",
                                              TRUE ~ as.character(Attribute))) |>
    dplyr::mutate(Type = dplyr::case_when(grepl("CollectorFilterer", Attribute) ~ "Collector Filterer",
                                          grepl("CollectorGatherer", Attribute) ~ "Collector Gatherer",
                                          grepl("Scraper", Attribute) ~ "Scraper",
                                          grepl("Shredder", Attribute) ~ "Shredder",
                                          grepl("Parasite", Attribute) ~ "Parasite",
                                          grepl("Predator", Attribute) ~ "Predator",
                                          grepl("PiercerHerbivore", Attribute) ~ "Piercer Herbivore",
                                          grepl("Clinger", Attribute) ~ "Clinger",
                                          grepl("Planktonic", Attribute) ~ "Planktonic",
                                          grepl("Skater", Attribute) ~ "Skater",
                                          grepl("Climber", Attribute) ~ "Climber",
                                          grepl("Crawler", Attribute) ~ "Crawler",
                                          grepl("Swimmer", Attribute) ~ "Swimmer",
                                          grepl("Burrower", Attribute) ~ "Burrower",
                                          grepl("Sprawler", Attribute) ~ "Sprawler",
                                          grepl("LongLived", Attribute) ~ "Long Lived",
                                          grepl("Intolerant", Attribute) ~ "Intolerant",
                                          grepl("Tolerant", Attribute) ~ "Tolerant",
                                          grepl("Insecta", Attribute) ~ "Insecta",
                                          grepl("Ephemeroptera", Attribute) ~ "Ephemeroptera",
                                          grepl("Plecoptera", Attribute) ~ "Plecoptera",
                                          grepl("Trichoptera", Attribute) ~ "Trichoptera",
                                          grepl("Coleoptera", Attribute) ~ "Coleoptera",
                                          grepl("Elmidae", Attribute) ~ "Elmidae",
                                          grepl("Diptera", Attribute) ~ "Diptera",
                                          grepl("Chironomidae", Attribute) ~ "Chironomidae",
                                          grepl("Megaloptera", Attribute) ~ "Megaloptera",
                                          grepl("Crustacea", Attribute) ~ "Crustacea",
                                          grepl("NonInsect", Attribute) ~ "NonInsects",
                                          grepl("Oligochaete", Attribute) ~ "Oligochaeta",
                                          grepl("Mollusca", Attribute) ~ "Mollusca",
                                          TRUE ~ NA_character_)) |>
    dplyr::mutate(Label = dplyr::case_when(Category %in% c("Functional Feeding Group", "Habit", "Sensitivity") ~ paste0(Category, ": ", Type),
                                           Category %in% c("Taxa Group") ~ paste0(Category, ": ", Type),
                                           Category %in% c("Overall") ~ paste0(Category, " ", Metric),
                                           Metric %in% c("Index") ~ paste0(Metric, ": ", Category),
                                           Metric %in% c("Fraction") ~ paste0(Metric, ": ", Category),
                                           TRUE ~ NA_character_))

  return(bmi)
}

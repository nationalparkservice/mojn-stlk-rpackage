% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chem-qc.R
\name{ChemLakeNutrientBarPlot}
\alias{ChemLakeNutrientBarPlot}
\title{Plot lake nutrient (UTN, TDN, NO2No3-N, UTP, TDP) concentration data as overlapping bar plots for all parks and field seasons.}
\usage{
ChemLakeNutrientBarPlot(park, site, field.season)
}
\arguments{
\item{park}{Optional. Four-letter park code to filter on, e.g. "GRBA".}

\item{site}{Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".}

\item{field.season}{Optional. Field season name to filter on, e.g. "2019".}
}
\value{
A ggplot object
}
\description{
Plot lake nutrient (UTN, TDN, NO2No3-N, UTP, TDP) concentration data as overlapping bar plots for all parks and field seasons.
}

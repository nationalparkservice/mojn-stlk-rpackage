% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wq-qc.R
\name{qcLakeWqFlags}
\alias{qcLakeWqFlags}
\title{Compile list of lake water quality values that have data quality flags.}
\usage{
qcLakeWqFlags(park, site, field.season)
}
\arguments{
\item{park}{Optional. Four-letter park code to filter on, e.g. "GRBA".}

\item{site}{Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".}

\item{field.season}{Optional. Field season name to filter on, e.g. "2019".}
}
\value{
A tibble
}
\description{
Compile list of lake water quality values that have data quality flags.
}

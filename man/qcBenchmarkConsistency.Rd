% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/levels-qc.R
\name{qcBenchmarkConsistency}
\alias{qcBenchmarkConsistency}
\title{Calculates mean and standard deviation of final corrected elevations for each benchmark across all field seasons}
\usage{
qcBenchmarkConsistency(park, site, field.season, sd_cutoff = NA)
}
\arguments{
\item{park}{Optional. Four-letter park code to filter on, e.g. "GRBA".}

\item{site}{Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".}

\item{field.season}{Optional. Field season name to filter on, e.g. "2019".}

\item{sd_cutoff}{Optional. If specified, only return benchmarks where standard deviation of final corrected elevations is greater than or equal to \code{sd_cutoff}.}
}
\value{
A tibble
}
\description{
Calculates mean and standard deviation of final corrected elevations for each benchmark across all field seasons
}
\examples{
\dontrun{
    qcBenchmarkConsistency()
    qcBenchmarkConsistency(site = "GRBA_L_BAKR0", field.season = c("2016", "2017", "2018", "2019"))
}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efficiency-qc.R
\name{qcWqGrades}
\alias{qcWqGrades}
\title{Calculate the percentage of data rated at each grade level for each water quality parameter for each field season between the index period of July 1 to September 15 (77 days).}
\usage{
qcWqGrades(park, site, field.season)
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
Calculate the percentage of data rated at each grade level for each water quality parameter for each field season between the index period of July 1 to September 15 (77 days).
}
\examples{
\dontrun{
    qcWqGrades()
    qcWqGrades(site = "GRBA_S_BAKR1", field.season = c("2018", "2019", "2020"))
}
}

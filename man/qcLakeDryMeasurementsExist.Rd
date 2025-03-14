% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clarity-qc.R
\name{qcLakeDryMeasurementsExist}
\alias{qcLakeDryMeasurementsExist}
\title{List clarity records where lake is dry but clarity measurements exist}
\usage{
qcLakeDryMeasurementsExist(park, site, field.season)
}
\arguments{
\item{park}{Optional. Four-letter park code to filter on, e.g. "GRBA".}

\item{site}{Optional. Site code to filter on, e.g. "GRBA_L_DEAD0".}

\item{field.season}{Optional. Field season name to filter on, e.g. "2019".}
}
\value{
A tibble
}
\description{
List clarity records where lake is dry but clarity measurements exist
}
\examples{
\dontrun{
    qcLakeDryMeasurementsExist()
    qcLakeDryMeasurementsExist(site = "GRBA_L_BAKR0", field.season = "2019")
}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi-qc.R
\name{ChannelCharacteristics}
\alias{ChannelCharacteristics}
\title{Return a table of channel characteristics data}
\usage{
ChannelCharacteristics(park, site, field.season)
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
Return a table of channel characteristics data
}
\examples{
\dontrun{
  channel <- ChannelCharacteristics()
  channel <- ChannelCharacteristics(site = "GRBA_S_STRW2", field.season = "2016")
}
}

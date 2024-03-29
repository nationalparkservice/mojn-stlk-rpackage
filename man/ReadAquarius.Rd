% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ReadAquarius}
\alias{ReadAquarius}
\title{Read Streams and Lakes data from Aquarius}
\usage{
ReadAquarius(conn, data.name)
}
\arguments{
\item{conn}{Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.}

\item{data.name}{The name of the data table. E.g. "TimeseriesDO". See details for full list of data name options.}
}
\value{
A tibble of Aquarius data, wrangled and formatted.
}
\description{
Read Streams and Lakes data from Aquarius
}
\details{
\code{data.name} options are: TimeseriesDO, TimeseriesDOSat, TimeseriespH, TimeseriesSpCond, TimeseriesTemperature
}

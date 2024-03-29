% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi-qc.R
\name{ChannelSubstrate}
\alias{ChannelSubstrate}
\title{Rank channel substrate type by count for each BMI sample}
\usage{
ChannelSubstrate(
  conn,
  path.to.data,
  park,
  site,
  field.season,
  data.source = "database"
)
}
\arguments{
\item{conn}{Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.}

\item{path.to.data}{The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.}

\item{park}{Optional. Four-letter park code to filter on, e.g. "GRBA".}

\item{site}{Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".}

\item{field.season}{Optional. Field season name to filter on, e.g. "2019".}

\item{data.source}{Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.}
}
\value{
A tibble with columns Park, SiteShort, SiteCode, SiteName, FieldSeason, VisitDate, ChannelFlow, Rank, Count
}
\description{
Rank channel substrate type by count for each BMI sample
}
\examples{
\dontrun{
conn <- OpenDatabaseConnection
channel_substrate <- ChannelSubstrate(conn)
channel_substrate_STRW2_2016 <- ChannelSubstrate(conn, site = "GRBA_S_STRW2", field.season = "2016")
CloseDatabaseConnection(conn)
}
}

#' Open a Connection to the MOJN Streams and Lakes Database
#'
#' @param use.mojn.default Connect to the live MOJN Streams and Lakes database? MOJN staff should use this option. Defaults to \code{TRUE}.
#' @param drv DBI driver to use. Defaults to \code{odbc::odbc()}.
#' @param ... Additional arguments to \code{\link[pool]{dbPool}}. Ignored if \code{use.mojn.default} is \code{TRUE}.
#'
#' @return A database connection pool object
#' @export
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom stats median
#'
#' @examples
#' \dontrun{
#' conn <- OpenDatabaseConnection()
#' }
OpenDatabaseConnection <- function(use.mojn.default = TRUE, drv = odbc::odbc(), ...) {
  if (use.mojn.default) {
    params <- readr::read_csv("M:/MONITORING/StreamsLakes/Data/Database/ConnectFromR/stlk-database-conn.csv") %>%
      as.list()
    params$drv <- drv
    my.pool <- do.call(pool::dbPool, params)
  } else {
    my.pool <- pool::dbPool(drv = drv, ...)
  }

  #Connect to Aquarius
  tryCatch({timeseries$connect("https://aquarius.nps.gov/aquarius", "aqreadonly", "aqreadonly")
    aq <<- timeseries},
    error = function(e) {
      aq <<- NA
      warning(paste("Could not connect to Aquarius. Verify that you are on the NPS network and that Aquarius is not down.", "Error message:", e, sep = "\n"))
    }
  )

  conn <- list(db = my.pool,
               aquarius = aq)

  return(conn)
}

#' Close a connection to the Streams and Lakes Database
#'
#' @param conn A database connection pool object generated from a call to \code{OpenDatabaseConnection()}
#'
#' @return None.
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- OpenDatabaseConnection()
#' CloseDatabaseConnection(conn)
#' }
CloseDatabaseConnection <- function(conn) {
  pool::poolClose(conn$db)
  if (isS4(conn$aquarius)) {
    conn$aquarius$disconnect()
  }
}

#' Get column specifications
#'
#' @return A list of column specifications for each table of data.
#'
GetColSpec <- function() {
  col.spec <- list(
    Site = readr::cols(
      Lat_WGS84 = readr::col_double(),
      Lon_WGS84 = readr::col_double(),
      X_UTM_NAD83_11N = readr::col_double(),
      Y_UTM_NAD83_11N = readr::col_double(),
      .default = readr::col_character()
    ),
    Visit = readr::cols(
      VisitDate = readr::col_date(),
      IsLakeDry = readr::col_logical(),
      DataStoreReferenceCode = readr::col_integer(),
      .default = readr::col_character()
    ),
    BMI = readr::cols(
      VisitDate = readr::col_date(),
      DateCollected = readr::col_date(),
      FieldSplit = readr::col_integer(),
      LabSplit = readr::col_double(),
      SampleArea_m2 = readr::col_double(),
      Abundance = readr::col_integer(),
      Richness = readr::col_integer(),
      DominantTaxaPercent = readr::col_double(),
      SplitCount = readr::col_integer(),
      FixedCount = readr::col_integer(),
      BigRareCount = readr::col_integer(),
      ShannonsDiversity = readr::col_double(),
      SimpsonsDiversity = readr::col_double(),
      Evenness = readr::col_double(),
      EPTTaxaCount = readr::col_integer(),
      EPTTaxaAbundance = readr::col_integer(),
      DominantFamilyAbundance = readr::col_integer(),
      DominantTaxaAbundance = readr::col_integer(),
      Hilsenhoff = readr::col_double(),
      IntolerantTaxaCount = readr::col_integer(),
      IntolerantTaxaAbundance = readr::col_integer(),
      TolerantTaxaCount = readr::col_integer(),
      TolerantTaxaAbundance = readr::col_integer(),
      USFSCommunityToleranceQuo = readr::col_integer(),
      ShredderTaxaCount = readr::col_integer(),
      ShredderAbundance = readr::col_integer(),
      ScraperTaxaCount = readr::col_integer(),
      ScraperAbundance = readr::col_integer(),
      CollectorFiltererCount = readr::col_integer(),
      CollectorFiltererAbundance = readr::col_integer(),
      CollectorGathererCount = readr::col_integer(),
      CollectorGathererAbundance = readr::col_integer(),
      PredatorTaxaCount = readr::col_integer(),
      PredatorTaxaAbundance = readr::col_integer(),
      ClingerTaxaCount = readr::col_integer(),
      LongLivedTaxa = readr::col_integer(),
      EphemeropteraTaxaCount = readr::col_integer(),
      EphemeropteraTaxaAbundance = readr::col_integer(),
      PlecopteraTaxa = readr::col_integer(),
      PlecopteraTaxaAbundance = readr::col_integer(),
      TrichopteraTaxaCount = readr::col_integer(),
      TrichopteraAbundance = readr::col_integer(),
      ColeopteraTaxaCount = readr::col_integer(),
      ColeopteraAbundance = readr::col_integer(),
      ElmidaeTaxaCount = readr::col_integer(),
      ElmidaeAbundance = readr::col_integer(),
      MegalopteraTaxaCount = readr::col_integer(),
      MegalopteraAbundance = readr::col_integer(),
      DipteraTaxaCount = readr::col_integer(),
      DipteraAbundance = readr::col_integer(),
      ChironomidaeTaxaCount = readr::col_integer(),
      ChironomidaeAbundance = readr::col_integer(),
      CrustaceaTaxaCount = readr::col_integer(),
      CrustaceaAbundance = readr::col_integer(),
      OligochaeteTaxaCount = readr::col_integer(),
      OligochaeteAbundance = readr::col_integer(),
      MolluscaTaxaCount = readr::col_integer(),
      MolluscaAbundance = readr::col_integer(),
      InsectTaxaCount = readr::col_integer(),
      InsectAbundance = readr::col_integer(),
      NonInsectTaxaCount = readr::col_integer(),
      NonInsectAbundance = readr::col_integer(),
      ID = readr::col_integer(),
      .default = readr::col_character()
    ),
    Channel = readr::cols(
      VisitDate = readr::col_date(),
      .default = readr::col_character()
    ),
    Chemistry = readr::cols(
      VisitDate = readr::col_date(),
      LabValue = readr::col_double(),
      .default = readr::col_character()
    ),
    Clarity = readr::cols(
      VisitDate = readr::col_date(),
      IsLakeDry = readr::col_logical(),
      DepthToBottom_m = readr::col_double(),
      SecchiDepth_m = readr::col_double(),
      .default = readr::col_character()
    ),
    LakeLevelString = readr::cols(
      VisitDate = readr::col_date(),
      RM1_GivenElevation_m = readr::col_double(),
      IsLakeDry = readr::col_logical(),
      Height_ft = readr::col_double(),
      .default = readr::col_character()
    ),
    LakeLevelSurvey = readr::cols(
      VisitDate = readr::col_date(),
      CTE = readr::col_double(),
      NumberOfInstrumentSetups = readr::col_integer(),
      RodTemperatureSetup1_F = readr::col_double(),
      RodTemperatureSetup2_F = readr::col_double(),
      RodTemperatureSetup3_F = readr::col_double(),
      NumberOfBenchmarksUsed = readr::col_integer(),
      RM1_GivenElevation_m = readr::col_double(),
      Height_ft = readr::col_double(),
      .default = readr::col_character()
    ),
    WaterQualityDO = readr::cols(
      VisitDate = readr::col_date(),
      MeasurementNum = readr::col_integer(),
      MeasurementDepth_m = readr::col_double(),
      DissolvedOxygen_percent = readr::col_double(),
      DissolvedOxygen_mg_per_L = readr::col_double(),
      .default = readr::col_character()
    ),
    WaterQualitypH = readr::cols(
      VisitDate = readr::col_date(),
      MeasurementNum = readr::col_integer(),
      MeasurementDepth_m = readr::col_double(),
      pH = readr::col_double(),
      .default = readr::col_character()
    ),
    WaterQualitySpCond = readr::cols(
      VisitDate = readr::col_date(),
      MeasurementNum = readr::col_integer(),
      MeasurementDepth_m = readr::col_double(),
      SpecificConductance_microS_per_cm = readr::col_double(),
      .default = readr::col_character()
    ),
    WaterQualityTemperature = readr::cols(
      VisitDate = readr::col_date(),
      MeasurementNum = readr::col_integer(),
      MeasurementDepth_m = readr::col_double(),
      WaterTemperature_C = readr::col_double(),
      .default = readr::col_character()
    ),
    WQStreamXSection = readr::cols(
      VisitDate = readr::col_date(),
      pH = readr::col_double(),
      DissolvedOxygen_mg_per_L = readr::col_double(),
      SpecificConductance_microS_per_cm = readr::col_double(),
      WaterTemperature_C = readr::col_double(),
      .default = readr::col_character()
    )
  )

  return(col.spec)
}

#' Get column specifications for Aquarius data that have been written to csv.
#'
#' @return A list of column specifications for each csv of Aquarius data.
#'
GetAquariusColSpec <- function() {
  col.spec.aq <- list(
    TimeseriesDO = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      DissolvedOxygen_mg_per_L = readr::col_double(),
      .default = readr::col_character()
    ),
    TimeseriesDOSat = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      DissolvedOxygen_percent = readr::col_double(),
      .default = readr::col_character()
    ),
    TimeseriespH = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      pH = readr::col_double(),
      .default = readr::col_character()
    ),
    TimeseriesSpCond = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      SpecificConductance_microS_per_cm = readr::col_double(),
      .default = readr::col_character()
    ),
    TimeseriesTemperature = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      WaterTemperature_C = readr::col_double(),
      .default = readr::col_character()
    )
  )

  return(col.spec.aq)
}

#' Read Streams and Lakes data from Aquarius
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param data.name The name of the data table. E.g. "TimeseriesDO". See details for full list of data name options.
#'
#' @return A tibble of Aquarius data, wrangled and formatted.
#'
#' @details \code{data.name} options are: TimeseriesDO, TimeseriesDOSat, TimeseriespH, TimeseriesSpCond, TimeseriesTemperature
#'
ReadAquarius <- function(conn, data.name) {
  if (!isS4(conn$aquarius)) {
    stop("Aquarius connection does not exist.")
  }
  timeseries <- conn$aquarius
  aq_data <- tibble::tibble()
  sites <- c("GRBA_S_BAKR1", "GRBA_S_LHMN1", "GRBA_S_SNKE1", "GRBA_S_SNKE3", "GRBA_S_STRW1")
  identifiers <- tibble::tibble(data_name = c("TimeseriesDO", "TimeseriesDOSat", "TimeseriespH", "TimeseriesSpCond", "TimeseriesTemperature"),
                                identifier = c("O2 (Dis).Cumulative@", "Dis Oxygen Sat.Cumulative@", "pH.Cumulative@", "Sp Cond.Cumulative@", "Water Temp.Cumulative@"),
                                col_name = c("DissolvedOxygen_mg_per_L", "DissolvedOxygen_percent", "pH", "SpecificConductance_microS_per_cm", "WaterTemperature_C"))

  aq_identifier <- identifiers[identifiers$data_name == data.name, ]$identifier
  aq_col_name <- identifiers[identifiers$data_name == data.name, ]$col_name

  for (location in sites) {
    site.imp <- timeseries$getTimeSeriesData(paste0(aq_identifier, location))

    site.data <- site.imp$Points

    site.data %<>%
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
      dplyr::rename(!!aq_col_name := NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) %>%
      dplyr::filter(Approval == "Approved") %>%
      dplyr::mutate(SiteCode = location) %>%
      dplyr::mutate(Park = "GRBA", SampleFrame = "Stream")

    site.data$DateTime <- lubridate::ymd_hms(site.data$DateTime, tz = "America/Los_Angeles", quiet = TRUE)

    site.data %<>%
      dplyr::mutate(FieldSeason = ifelse(lubridate::month(DateTime) < 10,
                                         lubridate::year(DateTime),
                                         lubridate::year(DateTime) + 1)) %>%
      dplyr::select(Park,
                    SampleFrame,
                    SiteCode,
                    FieldSeason,
                    DateTime,
                    !!aq_col_name,
                    Grade,
                    Approval)

    aq_data <- rbind(aq_data, site.data) %>%
      tibble::as_tibble()
  }

  return(aq_data)
}

#' Read Streams and Lakes data from database or .csv
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the live Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#' @param data.name The name of the analysis view or the csv file containing the data. E.g. "CalibrationDO", "DischargeVolumetric". See details for full list of data name options.
#'
#' @return A tibble of filtered data.
#'
#' @details \code{data.name} options are: Site, Visit, BMI, Channel, Chemistry, Clarity, WaterQualityDO, WaterQualitypH, WaterQualitySpCond, WaterQualityTemperature, WQStreamXSection, TimeseriesDO, TimeseriesDOSat, TimeseriespH, TimeseriesSpCond, TimeseriesTemperature
#'
ReadAndFilterData <- function(conn, path.to.data, park, site, field.season, data.source = "database", data.name) {
  col.spec <- GetColSpec()
  col.spec.aq <- GetAquariusColSpec()
  col.spec.all <- c(col.spec, col.spec.aq)

  if (!(data.source %in% c("database", "local"))) {
    stop("Please choose either 'database' or 'local' for data.source")
  }

  if (data.source == "database" & data.name %in% names(col.spec)) {
    filtered.data <- dplyr::tbl(conn$db, dbplyr::in_schema("analysis", data.name)) %>%
      dplyr::collect() %>%
      dplyr::mutate_if(is.character, trimws) %>%
      dplyr::mutate_if(is.character, dplyr::na_if, "")
  } else if (data.source == "database" & data.name %in% names(col.spec.aq)) {
    ## Read Aquarius data
    filtered.data <- ReadAquarius(conn, data.name)
  } else if (data.source == "local") {
    filtered.data <- readr::read_csv(file.path(path.to.data, paste0(data.name, ".csv")), na = "", col_types = col.spec.all[[data.name]])
    if(data.name %in% names(col.spec.aq) & "DateTime" %in% names(filtered.data)) {
      filtered.data$DateTime <- lubridate::with_tz(filtered.data$DateTime, "America/Los_Angeles")
    }
  }

  if (!missing(park)) {
    filtered.data %<>%
      dplyr::filter(Park == park)
    if (nrow(filtered.data) == 0) {
      warning(paste0(data.name, ": Data are not available for the park specified"))
    }
  }

  if (!missing(site) & nrow(filtered.data) > 0) {
    filtered.data %<>%
      dplyr::filter(SiteCode %in% site)

    if (nrow(filtered.data) == 0) {
      warning(paste0(data.name, ": Data are not available for the site specified"))
    }
  }

  if ("FieldSeason" %in% names(filtered.data)) {
    filtered.data %<>% dplyr::mutate(FieldSeason = as.character(FieldSeason))
  }

  if (!missing(field.season) & ("FieldSeason" %in% colnames(filtered.data)) & nrow(filtered.data) > 0) {
    filtered.data %<>%
      dplyr::filter(FieldSeason %in% field.season)
    if (nrow(filtered.data) == 0) {
      warning(paste0(data.name, ": Data are not available for one or more of the field seasons specified"))
    }
  }

  return(filtered.data)
}

#' Save Streams and Lakes analysis views as a set of .csv files
#'
#' @param conn A database connection pool object generated from a call to \code{OpenDatabaseConnection()}.
#' @param dest.folder The folder in which to save the .csv files.
#' @param create.folders Should \code{dest.folder} be created automatically if it doesn't exist? Defaults to \code{FALSE}.
#' @param overwrite Should existing data be automatically overwritten? Defaults to \code{FALSE}.
#'
#' @return None.
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- OpenDatabaseConnection()
#' SaveDataToCsv(conn, "C:/Users/myusername/Documents/R/streamsandlakes-data", TRUE, TRUE)
#' CloseDatabaseConnection(conn)
#' }
SaveDataToCsv <- function(conn, dest.folder, create.folders = FALSE, overwrite = FALSE) {
  analysis.views <- names(GetColSpec())
  aq.data <- names(GetAquariusColSpec())
  dest.folder <- file.path(dirname(dest.folder), basename(dest.folder)) # Get destination directory in a consistent format. Seems like there should be a better way to do this.
  file.paths <- c(file.path(dest.folder, paste0(analysis.views, ".csv")),
                  file.path(dest.folder, paste0(aq.data, ".csv")))

  # Validate inputs
  if (!dir.exists(dest.folder)) {
    if (create.folders) {
      dir.create(dest.folder, recursive = TRUE)
    } else {
      stop("Destination folder does not exist. To create it automatically, set create.folders to TRUE.")
    }
  }

  if (!overwrite & any(file.exists(file.paths))) {
    stop("Saving data in the folder provided would overwrite existing data. To automatically overwrite existing data, set overwrite to TRUE.")
  }

  # Write each analysis view in the database to csv

  for (view.name in analysis.views) {
    df <- dplyr::tbl(conn$db, dbplyr::in_schema("analysis", view.name)) %>%
      dplyr::collect()
    readr::write_csv(df, file.path(dest.folder, paste0(view.name, ".csv")), na = "", append = FALSE, col_names = TRUE)
  }

  #write calculated summary tables to csv

  df <- StreamWqMedian(conn)
  readr::write_csv(df, file.path(dest.folder, paste0("WQStreamXSection_CALCULATED", ".csv")), na = "", append = FALSE, col_names = TRUE)

  df <- LakeWqMedian(conn)
  readr::write_csv(df, file.path(dest.folder, paste0("WaterQuality_CALCULATED", ".csv")), na = "", append = FALSE, col_names = TRUE)

  df <- LakeSurfaceElevation(conn)
  readr::write_csv(df, file.path(dest.folder, paste0("LakeLevel_CALCULATED", ".csv")), na = "", append = FALSE, col_names = TRUE)

  # Write each Aquarius data table to csv

  for (aq.name in aq.data) {
    tryCatch(
      {
        df <- ReadAquarius(conn, aq.name)
        # Include time zone in dates
        if("DateTime" %in% names(df)) {
          df$DateTime <- format(df$DateTime, "%y-%m-%d %H:%M:%S %z")
        }
        readr::write_csv(df, file.path(dest.folder, paste0(aq.name, ".csv")), na = "", append = FALSE, col_names = TRUE)
      },
      error = function(e) {
        if (e$message == "Aquarius connection does not exist.") {
          warning(paste0("Could not connect to Aquarius. Skipping", aq.name, ".csv"))
        }
        else {e}
      }
    )
  }
}

#' Raw data dump
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Spring code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.source Character string indicating whether to access data in the desert springs database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return A list of dataframes containing raw streams and lakes data.
#' @export
#'
GetRawData <- function(conn, path.to.data, park, site, field.season, data.source = "database") {
  data.dump <- list()
  db.names <- names(GetColSpec())
  aq.names <- names(GetAquariusColSpec())

  data.names <- c(db.names, aq.names)

  for (data.name in data.names) {
    tryCatch(data.dump[[data.name]] <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name),
             error = function(e) {
               if (e$message == "Aquarius connection does not exist.") {
                 warning(paste0("Cannot connect to Aquarius. ", data.name, " omitted from data."))
               } else {e}
             }
    )
    # try(data.dump[[data.name]] <- ReadAndFilterData(conn, path.to.data, park, site, field.season, data.source, data.name))
  }

  return(data.dump)
}

#' Get the name of a site from the site code
#'
#' @param conn Database connection generated from call to \code{OpenDatabaseConnection()}. Ignored if \code{data.source} is \code{"local"}.
#' @param path.to.data The directory containing the csv data exports generated from \code{SaveDataToCsv()}. Ignored if \code{data.source} is \code{"database"}.
#' @param site.code Spring code to get the name for, e.g. "GRBA_L_BAKR0".
#' @param data.source Character string indicating whether to access data in the Streams and Lakes database (\code{"database"}, default) or to use data saved locally (\code{"local"}). In order to access the most up-to-date data, it is recommended that you select \code{"database"} unless you are working offline or your code will be shared with someone who doesn't have access to the database.
#'
#' @return The name of the site
#' @export
#'
GetSiteName <- function(conn, path.to.data, site.code, data.source = "database") {
  site <- ReadAndFilterData(conn, path.to.data, site = site.code, data.source = data.source, data.name = "Site")
  site %<>% dplyr::select("SiteCode", "SiteName") %>%
    unique() %>%
    dplyr::filter(SiteCode == site.code)

  return(site$SiteName)
}

#' Apply some standard formatting to a ggplot object.
#'
#' @param plot.title The title of the plot.
#' @param sub.title Optional custom plot subtitle.
#' @param x.lab X axis label.
#' @param y.lab Y axis label.
#' @param rotate.x.labs Boolean indicating whether to rotate x axis labels 90 degrees.
#' @param ymax Optional maximum y limit.
#' @param ymin Optional minimum y limit.
#' @param xmax Optional maximum x limit.
#' @param xmin Optional minimum x limit.
#' @param data Data frame containing the data to be plotted.
#' @param x.col Column name of independent variable. If plot type only requires one variable (e.g. histogram), use only one of x.col or y.col.
#' @param y.col Column name of dependent variable. If plot type only requires one variable (e.g. histogram), use only one of x.col or y.col.
#' @param facet.col Column to facet on. If this results in only one facet, it will be used as a subtitle instead.
#' @param n.col.facet Number of columns of facet grid.
#' @param sample.size.col Column containing sample size labels.
#' @param sample.size.loc Either 'xaxis' or 'plot'. 'xaxis' will add sample size to each x axis label. 'plot' will add sample size to the facet label (or subtitle, if only one facet).
#' @param facet.as.subtitle If only one facet, use facet name as subtitle? Defaults to TRUE.
#' @param transform.x Optional x axis transformation. One of 'log10', 'sqrt', or 'reverse'.
#' @param transform.y Optional y axis transformation. One of 'log10', 'sqrt', or 'reverse'.
#'
#' @return A ggplot object.
#'
#' @export
#'
FormatPlot <- function(data, x.col, y.col, facet.col, n.col.facet = 2, sample.size.col, sample.size.loc, plot.title = '', sub.title = '', facet.as.subtitle = TRUE, x.lab = '', y.lab = '', rotate.x.labs = FALSE, ymax, ymin, xmax, xmin, transform.x, transform.y) {

  # Allow for 1 or 2 variables
  if (!missing(y.col) & !missing(x.col)) {
    y.col <- dplyr::enquo(y.col)
    x.col <- dplyr::enquo(x.col)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x.col, y = !!y.col))
  } else if (!missing(x.col)) {
    x.col <- dplyr::enquo(x.col)
    p <- ggplot2::ggplot(data, ggplot2::aes(!!x.col))
  } else if (!missing(y.col)) {
    y.col <- dplyr::enquo(y.col)
    p <- ggplot2::ggplot(data, ggplot2::aes(!!y.col))
  }


  # Create facets if >1 event group, otherwise create subtitle
  if (!missing(facet.col)) {
    facet.col <- dplyr::enquo(facet.col)
    facets <- unique(dplyr::select(data, !!facet.col))
    if (nrow(facets) > 1) {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(!!facet.col), ncol = n.col.facet, scales = 'free_y')
    } else if (sub.title == '' & facet.as.subtitle) {
      sub.title <- facets
    }
  }

  # Add sample size information to either x axis labels or facet/subtitle
  if (!missing(sample.size.col) & !missing(sample.size.loc)) {
    sample.size.col <- dplyr::enquo(sample.size.col)
    if (sample.size.loc == 'xaxis') {
      data %<>% dplyr::mutate(!!x.col := paste0(!!x.col, '\n', !!sample.size.col))
    } else if (sample.size.loc == 'plot' & !missing(facet.col)) {
      data %<>% dplyr::mutate(!!facet.col := paste0(!!facet.col, ' (', !!sample.size.col, ')'))
    } else {
      facet.col <- sample.size.col
    }
  }

  # Add title and subtitle if not blank
  if (!missing(plot.title) & plot.title != '') {
    p <- p + ggplot2::labs(title = plot.title)
  }
  if (!missing(sub.title) & sub.title != '') {
    p <- p + ggplot2::labs(subtitle = sub.title)
  }

  # Add x and y axis titles if not blank
  if (x.lab != "") {
    p <- p + ggplot2::xlab(x.lab)
  } else {
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  if (y.lab != "") {
    p <- p + ggplot2::ylab(y.lab)
  } else {
    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }

  # Rotate x labels 90 degrees if rotate.x.labs is TRUE
  if (!missing(rotate.x.labs)) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  }

  # Set ymin and ymax if provided
  if (!missing(ymin) & !missing(ymax)) {
    p <- p + ggplot2::expand_limits(y = c(ymin, ymax))
  } else if (!missing(ymax)) {
    p <- p + ggplot2::expand_limits(y = ymax)
  } else if (!missing(ymin)) {
    p <- p + ggplot2::expand_limits(y = ymin)
  }

  # Set xmin and xmax if provided
  if (!missing(xmin) & !missing(xmax)) {
    p <- p + ggplot2::expand_limits(x = c(xmin, xmax))
  } else if (!missing(xmax)) {
    p <- p + ggplot2::expand_limits(x = xmax)
  } else if (!missing(xmin)) {
    p <- p + ggplot2::expand_limits(x = xmin)
  }

  # Tranform x axis, if transformation specified
  if (!missing(transform.x)) {
    if (transform.x == 'log10') {
      p <- p + ggplot2::scale_x_log10()
    } else if (transform.x == 'sqrt') {
      p <- p + ggplot2::scale_x_sqrt()
    } else if (transform.x == 'reverse') {
      p <- p + ggplot2::scale_x_reverse()
    } else {
      stop(paste0("The x transformation specified, '", transform.x, "' is not a valid option."))
    }
  }

  # Transform y axis, if transformation specified
  if (!missing(transform.y)) {
    if (transform.y == 'log10') {
      p <- p + ggplot2::scale_y_log10()
    } else if (transform.y == 'sqrt') {
      p <- p + ggplot2::scale_y_sqrt()
    } else if (transform.y == 'reverse') {
      p <- p + ggplot2::scale_y_reverse()
    } else {
      stop(paste0("The y transformation specified, '", transform.y, "' is not a valid option."))
    }
  }

  return(p)
}

#' Test for dataframe equivalence
#'
#' @param result Actual data frame
#' @param expected Expected data frame
#' @param ignore_col_order Ignore order of columns in dataframe? Defaults to FALSE.
#' @param ignore_row_order Ignore order of rows in dataframe? Defaults to TRUE.
#'
#' @return If test passes, nothing. If it fails, description of failure.
#'
#' @export
#'
expect_dataframe_equal <- function(result, expected, ignore_col_order = FALSE, ignore_row_order = TRUE) {
  # Check for same columns
  cols_match <- ifelse(ignore_col_order,
                       all(names(result) %in% names(expected)) & all(names(expected) %in% names(result)),
                       names(result) == names(expected))

  # Rearrange columns to match if ignoring column order
  if (cols_match & ignore_col_order) {
    result <- dplyr::select(result, names(expected))
  }
  # Rearrange row order to match if ignoring row order
  if (ignore_row_order) {
    result <- dplyr::arrange_at(result, names(result))
    expected <- dplyr::arrange_at(expected, names(expected))
  }
  # Compare dataframes
  test_result <- all.equal(result, expected, check.attributes = FALSE, use.names = TRUE, check.names = TRUE)

  return(testthat::expect_true(test_result, label = test_result))
}

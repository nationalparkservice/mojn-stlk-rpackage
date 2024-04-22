#' @importFrom magrittr %>% %<>%

pkg_globals <- new.env(parent = emptyenv())

# Load data from global package environment
get_data <- function(data.name) {
  if (!missing(data.name)) {
    if (!(data.name %in% c(names(GetColSpec()), names(GetAGOLColSpec()), names(GetAquariusColSpec())))) {
      stop("Invalid data table name. Use names(streamsandlakes:::GetColSpec()) to see valid options for data.name.")
    }
    tryCatch({data <- get(data.name, pkg_globals)},
             error = function(e) {
               if (grepl(".*object.* not found.*", e$message, ignore.case = TRUE)) {
                 stop(paste0("Could not find data. Did you remember to call LoadStreamsAndLakes?\n\tOriginal error: ", e$message))
               }
               else {e}
             })
  } else {
    tryCatch({
      data <- lapply(c(names(GetColSpec()), names(GetAGOLColSpec()), names(GetAquariusColSpec())), get, pkg_globals)
      names(data) <- c(names(GetColSpec()), names(GetAGOLColSpec()), names(GetAquariusColSpec()))
    },
    error = function(e) {
      if (grepl(".*object.* not found.*", e$message, ignore.case = TRUE)) {
        stop(paste0("Could not find data. Did you remember to call LoadStreamsAndLakes?\n\tOriginal error: ", e$message))
      }
      else {e}
    }
    )

  }

  return(data)
}

#' Open a connection to the Streams and Lakes Database
#'
#' @param use.mojn.default Connect to the live MOJN Streams and Lakes database? MOJN staff should use this option. Defaults to \code{TRUE}.
#' @param drv DBI driver to use. Defaults to \code{odbc::odbc()}.
#' @param ... Additional arguments to \code{\link[pool]{dbPool}}. Ignored if \code{use.mojn.default} is \code{TRUE}.
#'
#' @return A database connection pool object
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
    params <- readr::read_csv("M:/MONITORING/StreamsLakes/Data/Database/ConnectFromR/stlk-database-conn.csv", col_types = "cccc", ) |>
      as.list()
    params$drv <- drv
    my.pool <- do.call(pool::dbPool, params)
  } else {
    my.pool <- pool::dbPool(drv = drv, ...)
  }

  #Connect to Aquarius
  tryCatch({#fetchaquarius::connectToAquarius("aqreadonly", "aqreadonly")
    timeseries$connect("https://aquarius.nps.gov/aquarius", "aqreadonly", "aqreadonly")
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

#' Get column specifications for SQL database
#'
#' @return A list of column specifications for each table of SQL data.
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
    # BMI = readr::cols(
    #   VisitDate = readr::col_date(),
    #   DateCollected = readr::col_date(),
    #   FieldSplit = readr::col_integer(),
    #   LabSplit = readr::col_double(),
    #   SampleArea_m2 = readr::col_double(),
    #   Abundance = readr::col_integer(),
    #   Richness = readr::col_integer(),
    #   DominantTaxaPercent = readr::col_double(),
    #   SplitCount = readr::col_integer(),
    #   FixedCount = readr::col_integer(),
    #   BigRareCount = readr::col_integer(),
    #   ShannonsDiversity = readr::col_double(),
    #   SimpsonsDiversity = readr::col_double(),
    #   Evenness = readr::col_double(),
    #   EPTTaxaCount = readr::col_integer(),
    #   EPTTaxaAbundance = readr::col_integer(),
    #   DominantFamilyAbundance = readr::col_integer(),
    #   DominantTaxaAbundance = readr::col_integer(),
    #   Hilsenhoff = readr::col_double(),
    #   IntolerantTaxaCount = readr::col_integer(),
    #   IntolerantTaxaAbundance = readr::col_integer(),
    #   TolerantTaxaCount = readr::col_integer(),
    #   TolerantTaxaAbundance = readr::col_integer(),
    #   USFSCommunityToleranceQuo = readr::col_integer(),
    #   ShredderTaxaCount = readr::col_integer(),
    #   ShredderAbundance = readr::col_integer(),
    #   ScraperTaxaCount = readr::col_integer(),
    #   ScraperAbundance = readr::col_integer(),
    #   CollectorFiltererCount = readr::col_integer(),
    #   CollectorFiltererAbundance = readr::col_integer(),
    #   CollectorGathererCount = readr::col_integer(),
    #   CollectorGathererAbundance = readr::col_integer(),
    #   PredatorTaxaCount = readr::col_integer(),
    #   PredatorTaxaAbundance = readr::col_integer(),
    #   ClingerTaxaCount = readr::col_integer(),
    #   LongLivedTaxa = readr::col_integer(),
    #   EphemeropteraTaxaCount = readr::col_integer(),
    #   EphemeropteraTaxaAbundance = readr::col_integer(),
    #   PlecopteraTaxa = readr::col_integer(),
    #   PlecopteraTaxaAbundance = readr::col_integer(),
    #   TrichopteraTaxaCount = readr::col_integer(),
    #   TrichopteraAbundance = readr::col_integer(),
    #   ColeopteraTaxaCount = readr::col_integer(),
    #   ColeopteraAbundance = readr::col_integer(),
    #   ElmidaeTaxaCount = readr::col_integer(),
    #   ElmidaeAbundance = readr::col_integer(),
    #   MegalopteraTaxaCount = readr::col_integer(),
    #   MegalopteraAbundance = readr::col_integer(),
    #   DipteraTaxaCount = readr::col_integer(),
    #   DipteraAbundance = readr::col_integer(),
    #   ChironomidaeTaxaCount = readr::col_integer(),
    #   ChironomidaeAbundance = readr::col_integer(),
    #   CrustaceaTaxaCount = readr::col_integer(),
    #   CrustaceaAbundance = readr::col_integer(),
    #   OligochaeteTaxaCount = readr::col_integer(),
    #   OligochaeteAbundance = readr::col_integer(),
    #   MolluscaTaxaCount = readr::col_integer(),
    #   MolluscaAbundance = readr::col_integer(),
    #   InsectTaxaCount = readr::col_integer(),
    #   InsectAbundance = readr::col_integer(),
    #   NonInsectTaxaCount = readr::col_integer(),
    #   NonInsectAbundance = readr::col_integer(),
    #   .default = readr::col_character()
    # ),
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

#' Get column specifications for AGOL database
#'
#' @return A list of column specifications for each table of Aquarius data.
#'
GetAGOLColSpec <- function() {
  col.spec <- list(
    BMIMetrics = readr::cols(
      SampleID = readr::col_integer(),
      CollectionDate = readr::col_date(),
      Value = readr::col_double(),
      .default = readr::col_character()
    ),
    BMISpecies = readr::cols(
      SampleID = readr::col_integer(),
      CollectionDate = readr::col_date(),
      LabCount = readr::col_integer(),
      BigRareCount = readr::col_integer(),
      Sample_ID = readr::col_double(),
      .default = readr::col_character()
    ),
    BMIVisit = readr::cols(
      SampleID = readr::col_integer(),
      NAMC_Latitude = readr::col_double(),
      NAMC_Longitude = readr::col_double(),
      Customer_Latitude = readr::col_double(),
      Customer_Longitude = readr::col_double(),
      CollectionDate = readr::col_date(),
      Area = readr::col_double(),
      FieldSplit = readr::col_double(),
      LabSplit = readr::col_double(),
      SplitCount = readr::col_integer(),
      .default = readr::col_character()
    ),
    CalibrationDO = readr::cols(
      VisitDate = readr::col_date(),
      CalibrationDate = readr::col_date(),
      BarometricPressure_mmHg = readr::col_double(),
      PreCalibrationReading_percent = readr::col_double(),
      PreCalibrationTemperature_C = readr::col_double(),
      PostCalibrationReading_percent = readr::col_double(),
      PostCalibrationTemperature_C = readr::col_double(),
      .default = readr::col_character()
    ),
    CalibrationpH = readr::cols(
      VisitDate = readr::col_date(),
      CalibrationDate = readr::col_date(),
      StandardValue_pH = readr::col_double(),
      TemperatureCorrectedStd_pH = readr::col_double(),
      PreCalibrationReading_pH = readr::col_double(),
      PreCalibrationTemperature_C = readr::col_double(),
      PostCalibrationReading_pH = readr::col_double(),
      PostCalibrationTemperature_C = readr::col_double(),
      .default = readr::col_character()
    ),
    CalibrationSpCond = readr::cols(
      VisitDate = readr::col_date(),
      CalibrationDate = readr::col_date(),
      StandardValue_microS_per_cm = readr::col_double(),
      PreCalibrationReading_microS_per_cm = readr::col_double(),
      PostCalibrationReading_microS_per_cm = readr::col_double(),
      .default = readr::col_character()
    )
  )

  return(col.spec)
}

#' Get column specifications for Aquarius database
#'
#' @return A list of column specifications for each table of Aquarius data.
#'
GetAquariusColSpec <- function() {
  col.spec <- list(
    TimeseriesDOmgl = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      Value = readr::col_double(),
      .default = readr::col_character()
    ),
    TimeseriesDOpct = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      Value = readr::col_double(),
      .default = readr::col_character()
    ),
    TimeseriespH = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      Value = readr::col_double(),
      .default = readr::col_character()
    ),
    TimeseriesSpCond = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      Value = readr::col_double(),
      .default = readr::col_character()
    ),
    TimeseriesTemperature = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      Value = readr::col_double(),
      .default = readr::col_character()
    ),
    TimeseriesDischarge = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      Value = readr::col_double(),
      .default = readr::col_character()
    ),
    TimeseriesWaterLevel = readr::cols(
      DateTime = readr::col_datetime("%y-%m-%d %H:%M:%S %z"),
      Value = readr::col_double(),
      .default = readr::col_character()
    )
  )

  return(col.spec)
}

#' Read data from the Aquarius database
#'
#' @return A list of tibbles
#'
ReadAquarius <- function(...) {
  conn <- OpenDatabaseConnection(...)

  if (!isS4(conn$aquarius)) {
    stop("Aquarius connection does not exist.")
  }

  timeseries <- conn$aquarius
  data <- list()
  wt_data <- tibble::tibble()
  ph_data <- tibble::tibble()
  sc_data <- tibble::tibble()
  domgl_data <- tibble::tibble()
  dopct_data <- tibble::tibble()
  q_data <- tibble::tibble()
  wl_data <- tibble::tibble()

  stream <- c("GRBA_S_BAKR1", "GRBA_S_LHMN1", "GRBA_S_SNKE1", "GRBA_S_SNKE2", "GRBA_S_STRW1")
  discharge <- c("GRBA_S_BAKR1", "GRBA_S_SNKE1", "GRBA_S_SNKE2", "GRBA_S_STRW1")
  lake <- c("GRBA_L_BAKR0", "GRBA_L_BRWN0", "GRBA_L_DEAD0", "GRBA_L_JHNS0", "GRBA_L_STLL0", "GRBA_L_TRSA0")


  for (location in stream) {
    site.imp <- timeseries$getTimeSeriesData(paste0("Water Temp.Cumulative@", location))
    site.data <- site.imp$Points |>
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) |>
      dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = "GRBA", SampleFrame = "Stream")
    wt_data <- rbind(wt_data, site.data)
   }

  wt_data <- list(wt_data)
  names(wt_data) <- "TimeseriesTemperature"

  for (location in stream) {
    site.imp <- timeseries$getTimeSeriesData(paste0("pH.Cumulative@", location))
    site.data <- site.imp$Points |>
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) |>
      dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = "GRBA", SampleFrame = "Stream")
    ph_data <- rbind(ph_data, site.data)
  }

  ph_data <- list(ph_data)
  names(ph_data) <- "TimeseriespH"

  for (location in c("GRBA_S_BAKR1", "GRBA_S_STRW1")) {
    site.imp <- timeseries$getTimeSeriesData(paste0("Sp Cond.Cumulative@", location))
    site.data <- as.data.frame(site.imp$Points) |>
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) |>
      dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = "GRBA", SampleFrame = "Stream")
    sc_data <- rbind(sc_data, site.data)
  }

  sc_data <- list(sc_data)
  names(sc_data) <- "TimeseriesSpCond"

  for (location in stream) {
    site.imp <- timeseries$getTimeSeriesData(paste0("O2 (Dis).Cumulative@", location))
    site.data <- site.imp$Points |>
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) |>
      dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = "GRBA", SampleFrame = "Stream")
    domgl_data <- rbind(domgl_data, site.data)
  }

  domgl_data <- list(domgl_data)
  names(domgl_data) <- "TimeseriesDOmgl"

  for (location in stream) {
    site.imp <- timeseries$getTimeSeriesData(paste0("Dis Oxygen Sat.Cumulative@", location))
    site.data <- site.imp$Points |>
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) |>
      dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = "GRBA", SampleFrame = "Stream")
    dopct_data <- rbind(dopct_data, site.data)
  }

  dopct_data <- list(dopct_data)
  names(dopct_data) <- "TimeseriesDOpct"

  for (location in discharge) {
    site.imp <- timeseries$getTimeSeriesData(paste0("Discharge.Cumulative@", location))
    site.data <- site.imp$Points |>
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) |>
      dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = "GRBA", SampleFrame = "Stream")
    q_data <- rbind(q_data, site.data)
  }

  q_data <- list(q_data)
  names(q_data) <- "TimeseriesDischarge"

  for (location in lake) {
    site.imp <- timeseries$getTimeSeriesData(paste0("Water Level.Cumulative@", location))
    site.data <- site.imp$Points |>
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) |>
      dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = "GRBA", SampleFrame = "Lake")
    wl_data <- rbind(wl_data, site.data)
  }

  wl_data <- list(wl_data)
  names(wl_data) <- "TimeseriesWaterLevel"

  data <- c(wt_data, ph_data, sc_data, domgl_data, dopct_data, q_data, wl_data)

  # Tidy up the data
  data <- lapply(data, function(df) {
    df |>
      dplyr::mutate(DateTime = lubridate::ymd_hms(DateTime, tz = "America/Los_Angeles", quiet = TRUE)) |>
      dplyr::mutate(FieldSeason = ifelse(lubridate::month(DateTime) < 10,
                                         lubridate::year(DateTime),
                                         lubridate::year(DateTime) + 1))
    })

  CloseDatabaseConnection(conn)
  return(data)
}

# Custom function that takes a ggplotly figure and its facets as arguments.
# The upper x-values for each domain is set programmatically, but you can adjust
# the look of the figure by adjusting the width of the facet domain and the
# corresponding annotations labels through the domain_offset variable
fixfacets <- function(figure, facets, domain_offset){

  # split x ranges from 0 to 1 into
  # intervals corresponding to number of facets
  # xHi = highest x for shape
  xHi <- seq(0, 1, len = n_facets+1)
  xHi <- xHi[2:length(xHi)]

  xOs <- domain_offset

  # Shape manipulations, identified by dark grey backround: "rgba(217,217,217,1)"
  # structure: p$x$layout$shapes[[2]]$
  shp <- fig$x$layout$shapes
  j <- 1
  for (i in seq_along(shp)){
    if (shp[[i]]$fillcolor=="rgba(217,217,217,1)" & (!is.na(shp[[i]]$fillcolor))){
      #$x$layout$shapes[[i]]$fillcolor <- 'rgba(0,0,255,0.5)' # optionally change color for each label shape
      fig$x$layout$shapes[[i]]$x1 <- xHi[j]
      fig$x$layout$shapes[[i]]$x0 <- (xHi[j] - xOs)
      #fig$x$layout$shapes[[i]]$y <- -0.05
      j<-j+1
    }
  }

  # annotation manipulations, identified by label name
  # structure: p$x$layout$annotations[[2]]
  ann <- fig$x$layout$annotations
  annos <- facets
  j <- 1
  for (i in seq_along(ann)){
    if (ann[[i]]$text %in% annos){
      # but each annotation between high and low x,
      # and set adjustment to center
      fig$x$layout$annotations[[i]]$x <- (((xHi[j]-xOs)+xHi[j])/2)
      fig$x$layout$annotations[[i]]$xanchor <- 'center'
      #print(fig$x$layout$annotations[[i]]$y)
      #fig$x$layout$annotations[[i]]$y <- -0.05
      j<-j+1
    }
  }

  # domain manipulations
  # set high and low x for each facet domain
  xax <- names(fig$x$layout)
  j <- 1
  for (i in seq_along(xax)){
    if (!is.na(pmatch('xaxis', lot[i]))){
      #print(p[['x']][['layout']][[lot[i]]][['domain']][2])
      fig[['x']][['layout']][[xax[i]]][['domain']][2] <- xHi[j]
      fig[['x']][['layout']][[xax[i]]][['domain']][1] <- xHi[j] - xOs
      j<-j+1
    }
  }

  return(fig)
}

#' Read Streams and Lakes data from database or .csv
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Site code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#' @param data.name The name of the analysis view or the csv file containing the data. E.g. "CalibrationDO", "DischargeVolumetric". See details for full list of data name options.
#'
#' @return A tibble of filtered data.
#' @export
#'
#' @details \code{data.name} options are: Site, Visit, BMIVisit, BMIMetrics, BMISpecies, Channel, Chemistry, Clarity, WaterQualityDO, WaterQualitypH, WaterQualitySpCond, WaterQualityTemperature, WQStreamXSection, CalibrationDO, CalibrationpH, CalibrationSpCond, TimeseriesDOmgl, TimeseriesDOpct, TimeseriespH, TimeseriesSpCond, TimeseriesTemperature, TimeseriesDischarge, TimeseriesWaterLevel
#'
ReadAndFilterData <- function(park, site, field.season, data.name) {
  filtered.data <- get_data(data.name)

  if (!missing(field.season)) {
    field.season <- as.character(field.season)
  }

  if (!missing(park)) {
    filtered.data %<>%
      dplyr::filter(Park %in% park) # Changed to allow filtering of multiple parks
    if (nrow(filtered.data) == 0) {
      warning(paste0(data.name, ": Data are not available for the park specified"))
    }
  }

  if (!missing(site) & nrow(filtered.data) > 0) {
    filtered.data %<>%
      dplyr::filter(SiteCode == site)

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
#' @param aquarius Include Aquarius data?
#' @param calculated Include calculated data (median stream wq, median lake wq, and lake surface elevation)?
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
SaveDataToCsv <- function(conn, dest.folder, create.folders = FALSE, overwrite = FALSE, aquarius = TRUE, calculated = TRUE) {
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
    df <- ReadAndFilterData(data.name = view.name)
    readr::write_csv(df, file.path(dest.folder, paste0(view.name, ".csv")), na = "", append = FALSE, col_names = TRUE)
  }

  #write calculated summary tables to csv

  if (calculated) {
    df <- StreamWqMedian()
    readr::write_csv(df, file.path(dest.folder, paste0("WQStreamXSection_CALCULATED", ".csv")), na = "", append = FALSE, col_names = TRUE)

    df <- LakeWqMedian()
    readr::write_csv(df, file.path(dest.folder, paste0("WaterQuality_CALCULATED", ".csv")), na = "", append = FALSE, col_names = TRUE)

    df <- LakeSurfaceElevation()
    readr::write_csv(df, file.path(dest.folder, paste0("LakeLevel_CALCULATED", ".csv")), na = "", append = FALSE, col_names = TRUE)
  }

  if (aquarius) {
    # Write each Aquarius data table to csv

    for (aq.name in aq.data) {
      tryCatch(
        {
          df <- ReadAquarius(aq.name)
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
}

#' Raw data dump
#'
#' @param park Optional. Four-letter park code to filter on, e.g. "GRBA".
#' @param site Optional. Spring code to filter on, e.g. "GRBA_L_BAKR0".
#' @param field.season Optional. Field season name to filter on, e.g. "2019".
#'
#' @return A list of dataframes containing raw streams and lakes data
#' @export
#'
GetRawData <- function(park, site, field.season) {
  data.dump <- list()
  db.names <- names(GetColSpec())
  bmi.names <- names(GetAGOLColSpec())
  aq.names <- names(GetAquariusColSpec())

  data.names <- c(db.names, bmi.names, aq.names)

  for (data.name in data.names) {
    tryCatch(data.dump[[data.name]] <- ReadAndFilterData(path.to.data, park, site, field.season, data.name),
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
  site %<>% dplyr::select("SiteCode", "SiteName") |>
    unique() |>
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

#' Fetch BMI data from AGOL and do preliminary data wrangling
#'
#' @param bmi_url URL to AGOL BMI database
#' @param agol_username Authentication token (not needed for public layers)
#'
#' @return A list of data frames and metadata
#' @export
#'
fetchAndWrangleAGOL <- function(bmi_url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_HYDRO_BMI_Database/FeatureServer",
                                calibration_url = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Calibration_Database/FeatureServer",
                                agol_username = "mojn_data",
                                show_col_types = FALSE) {
  # Import BMI database
  raw_bmi <- fetchagol::fetchRawData(bmi_url, agol_username)
  raw_bmi <- fetchagol::cleanData(raw_bmi)

  # Remove unwanted data and metadata
  raw_bmi$data[['BMI_Metadata']] <- NULL
  raw_bmi$metadata[['BMI_Metadata']] <- NULL

  names(raw_bmi$data) <- c("BMISpecies", "BMIMetrics", "BMIVisit")

  raw_bmi$data <- lapply(raw_bmi$data, function(df) {
      df |>
        dplyr::filter(SiteCode %in% c("GRBA_S_BAKR2", "GRBA_S_BAKR3", "GRBA_S_LHMN2", "GRBA_S_MILL1", "GRBA_S_PINE1", "GRBA_S_RDGE1", "GRBA_S_SFBW1", "GRBA_S_SHNG1", "GRBA_S_SNKE4", "GRBA_S_STRW2")) |>
        dplyr::mutate(CollectionDate = as.Date(CollectionDateText, tz = "America/Los_Angeles"))
    })

  # Import WQ calibration database
  raw_agol <- fetchagol::fetchRawData(calibration_url, agol_username)
  raw_agol <- fetchagol::cleanData(raw_agol)

  # Combine data frames from different AGOL feature layers into one list
  raw_data <- c(raw_bmi$data, raw_agol$data)

  invisible(raw_data)
}

#' Read data from the Streams and Lakes AGOL feature layer
#'
#' @param ...
#'
#' @return A list of tibbles
#'
ReadAGOL <- function(...) {
  #Placeholder until more STLK data beyond BMI are moved from SQL to AGOL
  data <- fetchAndWrangleAGOL() # Change name of variable to bmi once there are more data
  # agol <- fetchAndWrangleAGOL()$data

  # data <- c(bmi, agol)

  return(data)
}

#' Read data from the Streams and Lakes SQL database
#'
#' @param ... Optional arguments to be passed to `OpenDatabaseConnection()`
#'
#' @return A list of tibbles
#'
ReadSqlDatabase <- function(...) {
  col.spec <- GetColSpec()
  conn <- OpenDatabaseConnection(...)
  data <- lapply(names(col.spec), function(data.name){
    df <- dplyr::tbl(conn$db, dbplyr::in_schema("analysis", data.name)) |>
      dplyr::collect()
    return(df)
  })

  names(data) <- names(col.spec)
  CloseDatabaseConnection(conn)
  return(data)
}

#' Title
#'
#' @param data_path
#'
#' @return
#' @export
#'
#' @examples
ReadCSV <- function(data_path) {
  data_path <- normalizePath(data_path)
  col.spec <- GetColSpec()
  is_zip <- grepl("\\.zip", data_path, ignore.case = TRUE)

  if(is_zip) {
    file_list <- basename(unzip(data_path, list = TRUE)$Name)
  } else {
    file_list <- list.files(data_path)
  }
  # Make sure that files in folder are valid CSVs
  expected_files <- paste0(names(col.spec), ".csv")
  if (!all(expected_files %in% file_list)) {
    missing_files <- setdiff(expected_files, file_list)
    missing_files <- paste(missing_files, collapse = "\n")
    stop(paste0("The folder provided is missing required data. Missing files:\n", missing_files))
  }

  # Read data
  if (is_zip) {  # Unzip into a temporary directory to read files
    temp_dir <- tempdir()
    # Use this trycatch so that even if there's an error unzipping or reading, the temp dir will be deleted
    tryCatch({
      unzip(data_path, overwrite = TRUE, exdir = temp_dir, junkpaths = TRUE)
      data <- lapply(names(col.spec), function(data.name){
        file_path <- file.path(temp_dir, paste0(data.name, ".csv"))
        df <- readr::read_csv(file = file_path, col_types = col.spec[[data.name]], locale = readr::locale(encoding = "UTF-8"))
        return(df)
      })
    },
    finally = unlink(temp_dir, recursive = TRUE)
    )
  } else {  # Read files from data path
    data <- lapply(names(col.spec), function(data.name){
      file_path <- file.path(data_path, paste0(data.name, ".csv"))
      df <- readr::read_csv(file = file_path, col_types = col.spec[[data.name]], locale = readr::locale(encoding = "UTF-8"))
      return(df)
    })
  }

  names(data) <- names(col.spec)
  return(data)
}

#' Load raw data into package environment
#' @description Run this function before you do anything else.
#'
#' @param data_path A path or URL to the data. Accepted inputs:
#' * 2 URLs to the AGOL feature services containing the data (bmi_db and calibration_db)
#' * a folder containing the data in csv format
#' * a .zip file containing the data in csv format
#' * `"database"` (connect to the SQL server database)
#' * `"aquarius"` (connect to the Aquarius database)
#' @param use_default_sql Use default SQL database? Ignored if `data_path != "database"`.
#' @param sql_drv Driver to use to connect to database. Ignored if `data_path != "database"`.
#' @param agol_username
#' @param agol_password
#' @param ...
#'
#' @return Invisibly return a list containing all raw data
#' @export
#'
#' @examples
#' \dontrun{
#' LoadStreamsAndLakes()  # Read from all sources (e.g., AGOL, SQL, Aquarius, CSVs)
#' LoadStreamsAndLakes("aquarius")  # Read from Aquarius database only
#' LoadStreamsAndLakes("path/to/csv/folder")  # Read from folder of CSVs
#' LoadStreamsAndLakes("path/to/zipped/csvs.zip")  # Read from zip file of CSVs
#' }
LoadStreamsAndLakes <- function(data_path = c("database", "aquarius",
                                              bmi_db = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_HYDRO_BMI_Database/FeatureServer",
                                              calibration_db = "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_Calibration_Database/FeatureServer"),
                                use_default_sql = TRUE, sql_drv = odbc::odbc(), agol_username = "mojn_data", agol_password = rstudioapi::askForPassword(paste("Please enter the password for AGOL account", agol_username)), ...) {

  # Figure out the format of the data
  agol_regex <- "^https:\\/\\/services1\\.arcgis\\.com\\/[^\\\\]+\\/arcgis\\/rest\\/services\\/[^\\\\]+\\/FeatureServer\\/?$"
  is_agol <- ifelse(any(grepl(agol_regex, data_path) == TRUE), TRUE, FALSE)
  is_db <- ifelse(any(grepl("^database$", data_path, ignore.case = TRUE) == TRUE), TRUE, FALSE)
  is_aquarius <- ifelse(any(grepl("^aquarius$", data_path, ignore.case = TRUE) == TRUE), TRUE, FALSE)
  if (!is_agol & !is_db) {
    # Standardize data path
    data_path <- normalizePath(data_path[1], mustWork = TRUE)
  }
  is_zip <- grepl("\\.zip$", data_path[1], ignore.case = TRUE) && file.exists(data_path[1])
  is_folder <- dir.exists(data_path[1])

  data <- list()

  if (is_agol) {  # Read from AGOL feature layer
    agol <- ReadAGOL(...)
    data <- append(data, agol)
  }

  if(is_aquarius) { # Read from Aquarius
    aquarius <- ReadAquarius(...)
    data <- append(data, aquarius)
  }

  if (is_db) {  # Read from SQL Server database
    sql <- ReadSqlDatabase(...)
    data <- append(data, sql)
  }

  if (is_zip | is_folder) {  # Read from folder of CSVs (may be zipped)
    csv <- ReadCSV(data_path[1])
    data <- append(data, csv)
  }

  if (!is_agol & !is_db & !is_zip & !is_folder) {
    stop(paste("Data path", data_path[1], "is invalid. See `?LoadStreamsAndLakes` for more information."))
  }

  # Tidy up the data
  data <- lapply(data, function(df) {
    df |>
      dplyr::mutate_if(is.character, utf8::utf8_encode) |>
      dplyr::mutate_if(is.character, trimws, whitespace = "[\\h\\v]") |>  # Trim leading and trailing whitespace
      dplyr::mutate_if(is.character, dplyr::na_if, "") |>  # Replace empty strings with NA
      dplyr::mutate_if(is.character, dplyr::na_if, "\\\\n") |>  # Replace newlines with NA
      dplyr::mutate_if(is.numeric, dplyr::na_if, -9999) |>  # Replace -9999 or -999 with NA
      dplyr::mutate_if(is.numeric, dplyr::na_if, -999) |>
      dplyr::mutate_if(is.character, dplyr::na_if, "NA") |>  # Replace "NA" strings with NA
      dplyr::mutate_if(is.character, stringr::str_replace_all, pattern = "[\\v|\\n]+", replacement = ";  ")  # Replace newlines with semicolons - reading certain newlines into R can cause problems
  })

  # Actually load the data into an environment for the package to use
  tbl_names <- names(data)
  lapply(tbl_names, function(n) {assign(n, data[[n]], envir = pkg_globals)})

  invisible(data)
}

#' Write BMI data to CSV
#'
#' @inheritParams fetchagol::writeToFiles
#'
#' @export
writeBMI <- function(all_data, data_dir = here::here("data", "final"), dictionary_dir = here::here("data", "dictionary"),
                     dictionary_filenames = c(tables = "data_dictionary_tables.txt",
                                              attributes = "data_dictionary_attributes.txt",
                                              categories = "data_dictionary_categories.txt"),
                     verbose = FALSE, removeColumns = TRUE, cols_to_remove = c("Editor", "Creator"), ...)
{
  fetchagol::writeToFiles(all_data = all_data, data_dir = data_dir, dictionary_dir = dictionary_dir, lookup_dir = NA, verbose = verbose, removeColumns = TRUE, cols_to_remove = c("Editor", "Creator"))
}

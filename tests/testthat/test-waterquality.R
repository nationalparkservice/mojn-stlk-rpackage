context("Water quality preprocessing and QC")

dummy_do <- tibble::tibble(Park = "GRBA",
                           SiteShort = c(rep("S0001", 6), rep("S0002", 3)),
                           SiteCode = c(rep("GRBA_L_S0001", 6), rep("GRBA_L_S0002", 3)),
                           SiteName = c(rep("Lake 1", 6), rep("Lake 2", 3)),
                           VisitDate = c(rep("2020-11-01", 6), rep("2019-11-01", 3)),
                           FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                           WQDataCollected = "Yes",
                           MeasurementNum = c(1:3, 1:3, 1:2, 1),
                           MeasurementDepth_m = as.double(c(rep(0, 3), rep(1, 3), rep(2, 2), 1)),
                           DissolvedOxygen_percent = as.double(c(90:92, rep(NA, 3), 90:91, 90)),
                           DissolvedOxygen_mg_per_L = as.double(c(8:10, 8:10, 8:9, 8)),
                           Flag = "NF",
                           FlagNote = c(rep(NA, 3), rep("Broken instrument", 3), rep("Measurement did not stabilize", 2), "Bees"),
                           DOInstrument = "MOJN YSI",
                           VisitType = "Primary",
                           DPL = c(rep("Accepted", 6), rep("Raw", 3)),
                           MonitoringStatus = "Sampled")

dummy_ph <- tibble::tibble(Park = "GRBA",
                           SiteShort = c(rep("S0001", 6), rep("S0002", 3)),
                           SiteCode = c(rep("GRBA_L_S0001", 6), rep("GRBA_L_S0002", 3)),
                           SiteName = c(rep("Lake 1", 6), rep("Lake 2", 3)),
                           VisitDate = c(rep("2020-11-01", 6), rep("2019-11-01", 3)),
                           FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                           WQDataCollected = "Yes",
                           MeasurementNum = c(1:3, 1:3, 1:2, 1),
                           MeasurementDepth_m = as.double(c(rep(0, 3), rep(1, 3), rep(2, 2), 1)),
                           pH = as.double(c(6:8, 6, NA, 8, 6:7, 7)),
                           Flag = "NF",
                           FlagNote = c(rep(NA, 3), rep("Broken instrument", 3), rep("Measurement did not stabilize", 2), "Bees"),
                           pHInstrument = "MOJN YSI",
                           VisitType = "Primary",
                           DPL = c(rep("Accepted", 6), rep("Raw", 3)),
                           MonitoringStatus = "Sampled")

dummy_spcond <- tibble::tibble(Park = "GRBA",
                           SiteShort = c(rep("S0001", 6), rep("S0002", 3)),
                           SiteCode = c(rep("GRBA_L_S0001", 6), rep("GRBA_L_S0002", 3)),
                           SiteName = c(rep("Lake 1", 6), rep("Lake 2", 3)),
                           VisitDate = c(rep("2020-11-01", 6), rep("2019-11-01", 3)),
                           FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                           WQDataCollected = "Yes",
                           MeasurementNum = c(1:3, 1:3, 1:2, 1),
                           MeasurementDepth_m = as.double(c(rep(0, 3), rep(1, 3), rep(2, 2), 1)),
                           SpecificConductance_microS_per_cm = c(500:502, 500:502, 500:501, 500),
                           Flag = "NF",
                           FlagNote = c(rep(NA, 3), rep("Broken instrument", 3), rep("Measurement did not stabilize", 2), "Bees"),
                           SpCondInstrument = "MOJN YSI",
                           VisitType = "Primary",
                           DPL = c(rep("Accepted", 6), rep("Raw", 3)),
                           MonitoringStatus = "Sampled")

dummy_temp <- tibble::tibble(Park = "GRBA",
                               SiteShort = c(rep("S0001", 6), rep("S0002", 3)),
                               SiteCode = c(rep("GRBA_L_S0001", 6), rep("GRBA_L_S0002", 3)),
                               SiteName = c(rep("Lake 1", 6), rep("Lake 2", 3)),
                               VisitDate = c(rep("2020-11-01", 6), rep("2019-11-01", 3)),
                               FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                               WQDataCollected = "Yes",
                               MeasurementNum = c(1:3, 1:3, 1:2, 1),
                               MeasurementDepth_m = as.double(c(rep(0, 3), rep(1, 3), rep(2, 2), 1)),
                               WaterTemperature_C = as.double(c(15:17, NA, NA, 15, 15:16, 15)),
                               Flag = "NF",
                               FlagNote = c(rep(NA, 3), rep("Broken instrument", 3), rep("Measurement did not stabilize", 2), "Bees"),
                               TempInstrument = "MOJN YSI",
                               VisitType = "Primary",
                               DPL = c(rep("Accepted", 6), rep("Raw", 3)),
                               MonitoringStatus = "Sampled")

dummy_streamwq <- tibble::tibble(Park = "GRBA",
                                 SiteShort = c(rep("S0003", 3), "S0004"),
                                 SiteCode = c(rep("GRBA_S_S0003", 3), "GRBA_S_S0004"),
                                 SiteName = c(rep("Spring 3", 3), "Spring 4"),
                                 FieldSeason = c(rep("2021", 3), "2020"),
                                 VisitDate = c(rep("2020-11-03", 3), "2019-11-04"),
                                 VisitType = "Primary",
                                 DPL = "Accepted",
                                 Notes = NA,
                                 TransectSide = c("Center", "Left", "Right", "Not Applicable"),
                                 pHInstrument = "MOJN YSI",
                                 pH = as.double(c(7:9, 7)),
                                 pHFlag = "NF",
                                 DOInstrument = "MOJN_YSI",
                                 DissolvedOxygen_mg_per_L = as.double(c(8:10, 8)),
                                 DOFlag = "NF",
                                 SpCondInstrument = "MOJN_YSI",
                                 SpecificConductance_microS_per_cm = c(500:502, 500),
                                 SpCondFlag = "NF",
                                 TemperatureInstrument = "MOJN YSI",
                                 WaterTemperature_C = as.double(c(15:17, 20)),
                                 TemperatureFlag = "NF",
                                 FlagNote = "DO is weird")

dummy_visit <- tibble::tibble(Park = "GRBA",
                              Subunit = c("Subunit 1", "Subunit 2", "Subunit 3", "Subunit 4"),
                              SiteShort = c("S0001", "S0002", "S0003", "S0004"),
                              SiteCode = c("GRBA_L_S0001", "GRBA_L_S0002", "GRBA_S_S0003", "GRBA_S_S0004"),
                              SiteName = c("Lake 1", "Lake 2", "Spring 3", "Spring 4"),
                              VisitDate = c("2020-11-01", "2019-11-01", "2020-11-03", "2019-11-04"),
                              FieldSeason = c("2021", "2020", "2021", "2020"),
                              VisitType = "Primary",
                              MonitoringStatus = "Sampled",
                              SampleFrame = c("Lake", "Lake", "Stream", "Stream"),
                              IsLakeDry = 0,
                              SiteProtectedStatus = "Not Protected",
                              CloudCover = "No Data",
                              Precipitation = "No Data",
                              Temperature = "No Data",
                              WindSpeed = "No Data",
                              Protocol = "STLK v1.0",
                              DataStoreReferenceCode = "123000",
                              Notes = "These are some notes",
                              DataProcessingLevel = c("Accepted", "Accepted", "Raw", "Raw"))

dir <- "temp-test-csv"
if (dir.exists(dir)) {
  unlink(dir, recursive = TRUE)
}
dir.create(dir)
readr::write_csv(dummy_do, file.path(dir, "WaterQualityDO.csv"), na = "")
readr::write_csv(dummy_ph, file.path(dir, "WaterQualitypH.csv"), na = "")
readr::write_csv(dummy_spcond, file.path(dir, "WaterQualitySpCond.csv"), na = "")
readr::write_csv(dummy_temp, file.path(dir, "WaterQualityTemperature.csv"), na = "")
readr::write_csv(dummy_streamwq, file.path(dir, "WQStreamXSection.csv"), na = "")
readr::write_csv(dummy_visit, file.path(dir, "Visit.csv"), na = "")


test_that("Lake wq median output as expected", {
  result <- LakeWqMedian(path.to.data = dir, data.source = "local")
  expected <- tibble::tibble(Park = "GRBA",
                             FieldSeason = c("2021", "2021", "2020", "2020"),
                             SiteCode = c("GRBA_L_S0001", "GRBA_L_S0001", "GRBA_L_S0002", "GRBA_L_S0002"),
                             VisitDate = as.Date(c("2020-11-01", "2020-11-01", "2019-11-01", "2019-11-01")),
                             VisitType = "Primary",
                             SampleFrame = "Lake",
                             MeasurementDepth_m = c(0, 1, 2, 1),
                             DOMedian_mg_per_L = c(9, 9, 8.5, 8),
                             DOmgLCount = as.integer(c(3, 3, 2, 1)),
                             DOMedian_percent = c(91, NA, 90.5, 90),
                             DOPercentCount = as.integer(c(3, 0, 2, 1)),
                             DOFlag = "NF",
                             pHMedian = c(7, 7, 6.5, 7),
                             pHCount = as.integer(c(3, 2, 2, 1)),
                             pHFlag = "NF",
                             SpCondMedian_microS_per_cm = c(501, 501, 500.5, 500),
                             SpCondCount = as.integer(c(3, 3, 2, 1)),
                             SpCondFlag = "NF",
                             TemperatureMedian_C = c(16, 15, 15.5, 15),
                             TemperatureCount = as.integer(c(3, 1, 2, 1)),
                             TemperatureFlag = "NF",
                             FlagNote = c(NA, "Broken instrument", "Measurement did not stabilize", "Bees"),
                             DPL = c("Accepted", "Accepted", "Raw", "Raw")
                             )

  expect_dataframe_equal(result, expected, ignore_col_order = TRUE)
})

test_that("Stream wq median output as expected", {
  result <- StreamWqMedian(path.to.data = dir, data.source = "local")
  expected <- tibble::tibble(Park = "GRBA",
                             FieldSeason = c("2021", "2020"),
                             SiteCode = c("GRBA_S_S0003", "GRBA_S_S0004"),
                             VisitDate = as.Date(c("2020-11-03", "2019-11-04")),
                             VisitType = "Primary",
                             SampleFrame = "Stream",
                             DOMedian_mg_per_L = c(9, 8),
                             DOmgLCount = as.integer(c(3, 1)),
                             DOFlag = "NF",
                             pHMedian = c(8, 7),
                             pHCount = as.integer(c(3, 1)),
                             pHFlag = "NF",
                             SpCondMedian_microS_per_cm = c(501, 500),
                             SpCondCount = as.integer(c(3, 1)),
                             SpCondFlag = "NF",
                             TemperatureMedian_C = c(16, 20),
                             TemperatureCount = as.integer(c(3, 1)),
                             TemperatureFlag = "NF",
                             DPL = "Accepted",
                             FlagNote = "DO is weird")
  expect_dataframe_equal(result, expected, ignore_col_order = TRUE)
})

test_that("Lake wq median filters by season", {
  result <- LakeWqMedian(field.season = "2020", path.to.data = dir, data.source = "local")

  expect_equal(nrow(result), 2)
  expect_equal(unique(result$FieldSeason), "2020")
  expect_warning(LakeWqMedian(field.season = "9999", path.to.data = dir, data.source = "local"), regexp = ".*Data are not available.*")
})

test_that("Lake wq median filters by park", {
  result <- LakeWqMedian(park = "GRBA", path.to.data = dir, data.source = "local")

  expect_equal(nrow(result), 4)
  expect_equal(unique(result$Park), "GRBA")
  expect_warning(LakeWqMedian(park = "asdf", path.to.data = dir, data.source = "local"), regexp = ".*Data are not available.*")
})

test_that("Lake wq median filters by site", {
  result <- LakeWqMedian(site = "GRBA_L_S0001", path.to.data = dir, data.source = "local")

  expect_equal(nrow(result), 2)
  expect_equal(unique(result$SiteCode), "GRBA_L_S0001")
  expect_warning(LakeWqMedian(site = "GRBA_L_XXXXX", path.to.data = dir, data.source = "local"), regexp = ".*Data are not available.*")
})


test_that("Stream wq median filters by season", {
  result <- StreamWqMedian(field.season = "2020", path.to.data = dir, data.source = "local")

  expect_equal(nrow(result), 1)
  expect_equal(unique(result$FieldSeason), "2020")
  expect_warning(StreamWqMedian(field.season = "9999", path.to.data = dir, data.source = "local"), regexp = ".*Data are not available.*")
})

test_that("Stream wq median filters by park", {
  result <- StreamWqMedian(park = "GRBA", path.to.data = dir, data.source = "local")

  expect_equal(nrow(result), 2)
  expect_equal(unique(result$Park), "GRBA")
  expect_warning(StreamWqMedian(park = "asdf", path.to.data = dir, data.source = "local"), regexp = ".*Data are not available.*")
})

test_that("Stream wq median filters by site", {
  result <- StreamWqMedian(site = "GRBA_S_S0003", path.to.data = dir, data.source = "local")

  expect_equal(nrow(result), 1)
  expect_equal(unique(result$SiteCode), "GRBA_S_S0003")
  expect_warning(StreamWqMedian(site = "GRBA_L_XXXXX", path.to.data = dir, data.source = "local"), regexp = ".*Data are not available.*")
})


test_that("Passing sanity checks return empty data frames", {
  lake_result <- qcLakeWqSanity(path.to.data = dir, data.source = "local")
  stream_result <- qcStreamWqSanity(path.to.data = dir, data.source = "local")

  expect_equal(nrow(lake_result), 0)
  expect_equal(names(lake_result), c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "Parameter", "Units", "Median", "Flag", "FlagNote", "MeasurementDepth_m"))
  expect_true(tibble::is_tibble(lake_result))

  expect_equal(nrow(stream_result), 0)
  expect_equal(names(stream_result), c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "Parameter", "Units", "Median", "Flag", "FlagNote"))
  expect_true(tibble::is_tibble(stream_result))
})

test_that("Passing flag checks return empty data frames", {
  lake_result <- qcLakeWqFlags(path.to.data = dir, data.source = "local")
  stream_result <- qcStreamWqFlags(path.to.data = dir, data.source = "local")

  expect_equal(nrow(lake_result), 0)
  expect_equal(names(lake_result), c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "Parameter", "Units", "Median", "Flag", "FlagNote", "MeasurementDepth_m"))
  expect_true(tibble::is_tibble(lake_result))

  expect_equal(nrow(stream_result), 0)
  expect_equal(names(stream_result), c("Park", "FieldSeason", "SiteCode", "SampleFrame", "VisitDate", "VisitType", "Parameter", "Units", "Median", "Flag", "FlagNote"))
  expect_true(tibble::is_tibble(stream_result))
})

# Create data that will fail qc checks
dummy_do_bad <- dummy_do
dummy_do_bad$DissolvedOxygen_percent[1:3] <- 120
dummy_do_bad$DissolvedOxygen_mg_per_L[4:6] <- 12.1
dummy_do_bad$Flag[1:3] <- "I"

dummy_ph_bad <- dummy_ph
dummy_ph_bad$pH[1:3] <- 11
dummy_ph_bad$Flag[9] <- "C"

dummy_spcond_bad <- dummy_spcond
dummy_spcond_bad$SpecificConductance_microS_per_cm <- 500
dummy_spcond_bad$SpecificConductance_microS_per_cm[1:3] <- 2000
dummy_spcond_bad$Flag[4:6] <- "W"

dummy_temp_bad <- dummy_temp
dummy_temp_bad$WaterTemperature_C[1:3] <- 21
dummy_temp_bad$Flag[1:3] <- "I"

dummy_streamwq_bad <- dummy_streamwq
dummy_streamwq_bad$pH[1:3] <- 3
dummy_streamwq_bad$DissolvedOxygen_mg_per_L[1:3] <- 13
dummy_streamwq_bad$SpecificConductance_microS_per_cm[1:3] <- 1000.1
dummy_streamwq_bad$WaterTemperature_C[1:3] <- 25
dummy_streamwq_bad$pHFlag[1:3] <- "I"
dummy_streamwq_bad$DOFlag[1:3] <- "C"
dummy_streamwq_bad$SpCondFlag[1:3] <- "W"
dummy_streamwq_bad$TemperatureFlag[1:3] <- "I"

readr::write_csv(dummy_do_bad, file.path(dir, "WaterQualityDO.csv"), na = "", append = FALSE)
readr::write_csv(dummy_ph_bad, file.path(dir, "WaterQualitypH.csv"), na = "", append = FALSE)
readr::write_csv(dummy_spcond_bad, file.path(dir, "WaterQualitySpCond.csv"), na = "", append = FALSE)
readr::write_csv(dummy_temp_bad, file.path(dir, "WaterQualityTemperature.csv"), na = "", append = FALSE)
readr::write_csv(dummy_streamwq_bad, file.path(dir, "WQStreamXSection.csv"), na = "", append = FALSE)

lake_result <- qcLakeWqSanity(path.to.data = dir, data.source = "local")
stream_result <- qcStreamWqSanity(path.to.data = dir, data.source = "local")

test_that("DO sanity check works", {
  expect_equal(lake_result[lake_result$Parameter == "DO", ]$Median, c(120.0, 12.1))
  expect_equal(stream_result[stream_result$Parameter == "DO", ]$Median, 13)
})

test_that("pH sanity check works", {
  expect_equal(lake_result[lake_result$Parameter == "pH", ]$Median, 11)
  expect_equal(stream_result[stream_result$Parameter == "pH", ]$Median, 3)
})

test_that("SpCond sanity check works", {
  expect_equal(lake_result[lake_result$Parameter == "SpCond", ]$Median, 2000)
  expect_equal(stream_result[stream_result$Parameter == "SpCond", ]$Median, 1000.1)
})

test_that("Temperature sanity check works", {
  expect_equal(lake_result[lake_result$Parameter == "Temperature", ]$Median, 21)
  expect_equal(stream_result[stream_result$Parameter == "Temperature", ]$Median, 25)
})


lake_result <- qcLakeWqFlags(path.to.data = dir, data.source = "local")
stream_result <- qcStreamWqFlags(path.to.data = dir, data.source = "local")

test_that("DO flag check works", {
  expect_equal(lake_result[lake_result$Parameter == "DO", ]$Flag, c("I", "I"))
  expect_equal(lake_result[lake_result$Parameter == "DO", ]$Median, c(120, 9))
  expect_equal(stream_result[stream_result$Parameter == "DO", ]$Flag, "C")
  expect_equal(stream_result[stream_result$Parameter == "DO", ]$Median, 13)
})

test_that("pH flag check works", {
  expect_equal(lake_result[lake_result$Parameter == "pH", ]$Flag, "C")
  expect_equal(lake_result[lake_result$Parameter == "pH", ]$Median, 7)
  expect_equal(stream_result[stream_result$Parameter == "pH", ]$Flag, "I")
  expect_equal(stream_result[stream_result$Parameter == "pH", ]$Median, 3)
})

test_that("SpCond flag check works", {
  expect_equal(lake_result[lake_result$Parameter == "SpCond", ]$Flag, "W")
  expect_equal(lake_result[lake_result$Parameter == "SpCond", ]$Median, 500)
  expect_equal(stream_result[stream_result$Parameter == "SpCond", ]$Flag, "W")
  expect_equal(stream_result[stream_result$Parameter == "SpCond", ]$Median, 1000.1)
})

test_that("Temperature flag check works", {
  expect_equal(lake_result[lake_result$Parameter == "Temperature", ]$Flag, "I")
  expect_equal(lake_result[lake_result$Parameter == "Temperature", ]$Median, 21)
  expect_equal(stream_result[stream_result$Parameter == "Temperature", ]$Flag, "I")
  expect_equal(stream_result[stream_result$Parameter == "Temperature", ]$Median, 25)
})

# Remove temporary csv files
unlink(dir, recursive = TRUE)


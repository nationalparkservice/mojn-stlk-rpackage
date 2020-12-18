context("Water quality")

dummy.do <- tibble::tibble(Park = "GRBA",
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
                           Flag = c(rep("NF", 3), rep("C", 3), rep("W", 2), "I"),
                           FlagNote = c(rep(NA, 3), rep("Broken instrument", 3), rep("Measurement did not stabilize", 2), "Bees"),
                           DOInstrument = "MOJN YSI",
                           VisitType = "Primary",
                           DPL = c(rep("Accepted", 6), rep("Raw", 3)),
                           MonitoringStatus = "Sampled")

dummy.ph <- tibble::tibble(Park = "GRBA",
                           SiteShort = c(rep("S0001", 6), rep("S0002", 3)),
                           SiteCode = c(rep("GRBA_L_S0001", 6), rep("GRBA_L_S0002", 3)),
                           SiteName = c(rep("Lake 1", 6), rep("Lake 2", 3)),
                           VisitDate = c(rep("2020-11-01", 6), rep("2019-11-01", 3)),
                           FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                           WQDataCollected = "Yes",
                           MeasurementNum = c(1:3, 1:3, 1:2, 1),
                           MeasurementDepth_m = as.double(c(rep(0, 3), rep(1, 3), rep(2, 2), 1)),
                           pH = as.double(c(6:8, 6, NA, 8, 6:7, 7)),
                           Flag = c(rep("NF", 3), rep("I", 3), rep("C", 2), "W"),
                           FlagNote = c(rep(NA, 3), rep("Broken instrument", 3), rep("Measurement did not stabilize", 2), "Bees"),
                           pHInstrument = "MOJN YSI",
                           VisitType = "Primary",
                           DPL = c(rep("Accepted", 6), rep("Raw", 3)),
                           MonitoringStatus = "Sampled")

dummy.spcond <- tibble::tibble(Park = "GRBA",
                           SiteShort = c(rep("S0001", 6), rep("S0002", 3)),
                           SiteCode = c(rep("GRBA_L_S0001", 6), rep("GRBA_L_S0002", 3)),
                           SiteName = c(rep("Lake 1", 6), rep("Lake 2", 3)),
                           VisitDate = c(rep("2020-11-01", 6), rep("2019-11-01", 3)),
                           FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                           WQDataCollected = "Yes",
                           MeasurementNum = c(1:3, 1:3, 1:2, 1),
                           MeasurementDepth_m = as.double(c(rep(0, 3), rep(1, 3), rep(2, 2), 1)),
                           SpecificConductance_microS_per_cm = c(2000:2002, 2000:2002, 2000:2001, 2000),
                           Flag = c(rep("I", 3), rep("C", 3), rep("W", 2), "I"),
                           FlagNote = c(rep(NA, 3), rep("Broken instrument", 3), rep("Measurement did not stabilize", 2), "Bees"),
                           SpCondInstrument = "MOJN YSI",
                           VisitType = "Primary",
                           DPL = c(rep("Accepted", 6), rep("Raw", 3)),
                           MonitoringStatus = "Sampled")

dummy.temp <- tibble::tibble(Park = "GRBA",
                               SiteShort = c(rep("S0001", 6), rep("S0002", 3)),
                               SiteCode = c(rep("GRBA_L_S0001", 6), rep("GRBA_L_S0002", 3)),
                               SiteName = c(rep("Lake 1", 6), rep("Lake 2", 3)),
                               VisitDate = c(rep("2020-11-01", 6), rep("2019-11-01", 3)),
                               FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                               WQDataCollected = "Yes",
                               MeasurementNum = c(1:3, 1:3, 1:2, 1),
                               MeasurementDepth_m = as.double(c(rep(0, 3), rep(1, 3), rep(2, 2), 1)),
                               WaterTemperature_C = as.double(c(15:17, NA, NA, 15, 15:16, 15)),
                               Flag = c(rep("I", 3), rep("C", 3), rep("W", 2), "I"),
                               FlagNote = c(rep(NA, 3), rep("Broken instrument", 3), rep("Measurement did not stabilize", 2), "Bees"),
                               TempInstrument = "MOJN YSI",
                               VisitType = "Primary",
                               DPL = c(rep("Accepted", 6), rep("Raw", 3)),
                               MonitoringStatus = "Sampled")

dummy.streamwq <- tibble::tibble(Park = "GRBA",
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
                                 DOFlag = "I",
                                 SpCondInstrument = "MOJN_YSI",
                                 SpecificConductance_microS_per_cm = c(1000:1002, 1000),
                                 SpCondFlag = "NF",
                                 TemperatureInstrument = "MOJN YSI",
                                 WaterTemperature_C = as.double(c(20:22, 20)),
                                 TemperatureFlag = "NF",
                                 FlagNote = "DO is weird")

dummy.visit <- tibble::tibble(Park = "GRBA",
                              Subunit = c("Subunit 1", "Subunit 2", "Subunit 3", "Subunit 4"),
                              SiteShort = c("S0001", "S0002", "S0003", "S0004"),
                              SiteCode = c("GRBA_L_S0001", "GRBA_L_S0002", "GRBA_S_S0003", "GRBA_S_S0004"),
                              SiteName = c("Lake 1", "Lake 2", "Spring 3", "Spring 4"),
                              VisitDate = c("2020-11-01", "2019-11-01", "2020-11-03", "2019-11-04"),
                              FieldSeason = c("2021", "2020", "2021", "2020"),
                              VisitType = "Primary",
                              MonitoringStatus = "Sampled",
                              SiteType = c("Lake", "Lake", "Stream", "Stream"),
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
dir.create(dir)
readr::write_csv(dummy.do, file.path(dir, "WaterQualityDO.csv"), na = "")
readr::write_csv(dummy.ph, file.path(dir, "WaterQualitypH.csv"), na = "")
readr::write_csv(dummy.spcond, file.path(dir, "WaterQualitySpCond.csv"), na = "")
readr::write_csv(dummy.temp, file.path(dir, "WaterQualityTemperature.csv"), na = "")
readr::write_csv(dummy.streamwq, file.path(dir, "WQStreamXSection.csv"), na = "")
readr::write_csv(dummy.visit, file.path(dir, "Visit.csv"), na = "")


test_that("Lake wq median output as expected", {
  result <- LakeWqMedian(path.to.data = dir, data.source = "local")
  expected <- tibble::tibble(Park = "GRBA",
                             FieldSeason = c("2021", "2021", "2020", "2020"),
                             SiteCode = c("GRBA_L_S0001", "GRBA_L_S0001", "GRBA_L_S0002", "GRBA_L_S0002"),
                             VisitDate = as.Date(c("2020-11-01", "2020-11-01", "2019-11-01", "2019-11-01")),
                             VisitType = "Primary",
                             SiteType = "Lake",
                             MeasurementDepth_m = c(0, 1, 2, 1),
                             DOMedian_mg_per_L = c(9, 9, 8.5, 8),
                             DOmgLCount = as.integer(c(3, 3, 2, 1)),
                             DOMedian_percent = c(91, NA, 90.5, 90),
                             DOPercentCount = as.integer(c(3, 0, 2, 1)),
                             DOFlag = c("NF", "C", "W", "I"),
                             pHMedian = c(7, 7, 6.5, 7),
                             pHCount = as.integer(c(3, 2, 2, 1)),
                             pHFlag = c("NF", "I", "C", "W"),
                             SpCondMedian_microS_per_cm = c(2001, 2001, 2000.5, 2000),
                             SpCondCount = as.integer(c(3, 3, 2, 1)),
                             SpCondFlag = c("I", "C", "W", "I"),
                             TemperatureMedian_C = c(16, 15, 15.5, 15),
                             TemperatureCount = as.integer(c(3, 1, 2, 1)),
                             TemperatureFlag = c("I", "C", "W", "I"),
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
                             SiteType = "Stream",
                             DOMedian_mg_per_L = c(9, 8),
                             DOmgLCount = as.integer(c(3, 1)),
                             DOFlag = "I",
                             pHMedian = c(8, 7),
                             pHCount = as.integer(c(3, 1)),
                             pHFlag = "NF",
                             SpCondMedian_microS_per_cm = c(1001, 1000),
                             SpCondCount = as.integer(c(3, 1)),
                             SpCondFlag = "NF",
                             TemperatureMedian_C = c(21, 20),
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


# Remove temporary csv files
unlink(dir, recursive = TRUE)


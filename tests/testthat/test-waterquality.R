context("Water quality")
skip("Test not written yet")

dummy.do <- tibble::tibble(Park = "GRBA",
                           SiteShort = c(rep("S0001", 6), rep("S0002", 3)),
                           SiteCode = c(rep("GRBA_L_S0001", 6), rep("GRBA_L_S0002", 3)),
                           SiteName = c(rep("Lake 1", 6), rep("Lake 2", 3)),
                           VisitDate = c(rep("2020-11-01", 3), rep("2020-11-02", 3), rep("2019-11-01", 3)),
                           FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                           WQDataCollected = "Yes",
                           MeasurementNum = c(1:3, 1:3, 1:2, 1),
                           MeasurementDepth_m = c(0:2, 0:2, 0:1, 0),
                           DissolvedOxygen_percent = c(90:92, rep(NA, 3), 90:91, 90),
                           DissolvedOxygen_mg_per_L = c(8:10, 8:10, 8:9, 8),
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
                           VisitDate = c(rep("2020-11-01", 3), rep("2020-11-02", 3), rep("2019-11-01", 3)),
                           FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                           WQDataCollected = "Yes",
                           MeasurementNum = c(1:3, 1:3, 1:2, 1),
                           MeasurementDepth_m = c(0:2, 0:2, 0:1, 0),
                           pH = c(6:8, 6, NA, 8, 6:7, 7),
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
                           VisitDate = c(rep("2020-11-01", 3), rep("2020-11-02", 3), rep("2019-11-01", 3)),
                           FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                           WQDataCollected = "Yes",
                           MeasurementNum = c(1:3, 1:3, 1:2, 1),
                           MeasurementDepth_m = c(0:2, 0:2, 0:1, 0),
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
                               VisitDate = c(rep("2020-11-01", 3), rep("2020-11-02", 3), rep("2019-11-01", 3)),
                               FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                               WQDataCollected = "Yes",
                               MeasurementNum = c(1:3, 1:3, 1:2, 1),
                               MeasurementDepth_m = c(0:2, 0:2, 0:1, 0),
                               WaterTemperature_C = c(15:17, NA, NA, 15, 15:16, 15),
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
                                 pH = c(7:9, 7),
                                 pHFlag = "NF",
                                 DOInstrument = "MOJN_YSI",
                                 DissolvedOxygen_mg_per_L = c(8:10, 8),
                                 DOFlag = "I",
                                 SpCondInstrument = "MOJN_YSI",
                                 SpecificConductance_microS_per_cm = c(1000:1002, 1000),
                                 SpCondFlag = "NF",
                                 TemperatureInstrument = "MOJN YSI",
                                 WaterTemperature_C = c(20:22, 20),
                                 TemperatureFlag = "NF",
                                 FlagNote = "DO is weird")

dummy.visit <- tibble::tibble(Park = "GRBA",
                              Subunit = c(rep("Subunit 1", 6), rep("Subunit 2", 3)),
                              SiteShort = c(rep("S0001", 6), rep("S0002", 3)),
                              SiteCode = c(rep("GRBA_L_S0001", 6), rep("GRBA_L_S0002", 3)),
                              SiteName = c(rep("Lake 1", 6), rep("Lake 2", 3)),
                              VisitDate = c(rep("2020-11-01", 3), rep("2020-11-02", 3), rep("2019-11-01", 3)),
                              FieldSeason = c(rep("2021", 6), rep("2020", 3)),
                              VisitType = "Primary",
                              MonitoringStatus = "Sampled",
                              SiteType = "Lake",
                              IsLakeDry = 0,
                              SiteProtectedStatus = "Not Protected",
                              CloudCover = "No Data",
                              Precipitation = "No Data",
                              Temperature = "No Data",
                              WindSpeed = "No Data",
                              Protocol = "STLK v1.0",
                              DataStoreReferenceCode = "123000",
                              Notes = "These are some notes",
                              DataProcessingLevel = c(rep("Accepted", 6), rep("Raw", 3)))

dir <- "temp-test-csv"
dir.create(dir)
readr::write_csv(dummy.do, file.path(dir, "WaterQualityDO.csv"))
readr::write_csv(dummy.ph, file.path(dir, "WaterQualitypH.csv"))
readr::write_csv(dummy.spcond, file.path(dir, "WaterQualitySpCond.csv"))
readr::write_csv(dummy.temp, file.path(dir, "WaterQualityTemperature.csv"))
readr::write_csv(dummy.streamwq, file.path(dir, "WQStreamXSection.csv"))
readr::write_csv(dummy.visit, file.path(dir, "Visit.csv"))

# test_that("Stream and lake wq median fxns output data frames with same column names", {
#
#
# })

# test_that("Lake wq median correctly handles diff. numbers of measurements", {
#
# })
#
# test_that("Lake wq median correctly handles NA values", {
#
# })
#
# test_that("Lake wq median filters by park and season", {
#
# })
#
# test_that("Stream wq median correctly handles diff. numbers of measurements", {
#
# })
#
# test_that("Stream wq median correctly handles NA values", {
#
# })


# Remove temporary csv files
unlink(dir, recursive = TRUE)
test_that("Stream wq median filters by park and season", {

})

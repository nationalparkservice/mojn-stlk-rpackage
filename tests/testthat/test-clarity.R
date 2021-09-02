context("Clarity")

dummy.clarity <- tibble::tibble(Park = c("GRBA","GRBA","GRBA","GRBA","GRBA","GRBA"),
                                SiteShort = c("BAKR0","BRWN0","DEAD0","JHNS0","STLL0", "STLL0"),
                                SiteCode = c("GRBA_L_BAKR0","GRBA_L_BRWN0","GRBA_L_DEAD0","GRBA_L_JHNS0","GRBA_L_STLL0","GRBA_L_STLL0"),
                                SiteName = c("Baker Lake","Brown Lake","Dead Lake","Johnson Lake","Stella Lake","Stella Lake"),
                                VisitDate = as.Date(c("2016-09-21","2016-09-20","2017-09-20","2017-09-19","2018-09-27","2019-09-27")),
                                FieldSeason = c("2016","2016","2017","2017","2018","2019"),
                                IsLakeDry = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
                                SurfaceCalm = c("N","Y", NA,"N","N","Y"),
                                OnBottom = c("N","N","Y", NA,"Y","N"),
                                DepthToBottom_m = c(8.5,NA,8.5,8.5,NA,12.2),
                                SecchiDepth_m = c(9.5,7.5,1,7.5,1,NA),
                                VisitType = c("Primary","Primary","Primary","Primary","Primary","Primary"),
                                DPL = c("Accepted","Accepted","Accepted","Accepted","Accepted","Accepted")
                                )

dir <- "temp-test-csv"
if (dir.exists(dir)) {
  unlink(dir, recursive = TRUE)
}
dir.create(dir)
readr::write_csv(dummy.clarity, file.path(dir, "Clarity.csv"),na = "")

test_that("qcSecchiGTDepth finds secchi depth greater than lake depth", {
  expected <- tibble::tibble(Park = c("GRBA"),
                             SiteShort = c("BAKR0"),
                             SiteCode = c("GRBA_L_BAKR0"),
                             SiteName = c("Baker Lake"),
                             VisitDate = as.Date(c("2016-09-21")),
                             FieldSeason = c("2016"),
                             IsLakeDry = c(FALSE),
                             SurfaceCalm = c("N"),
                             OnBottom = c("N"),
                             DepthToBottom_m = c(8.5),
                             SecchiDepth_m = c(9.5),
                             VisitType = c("Primary"),
                             DPL = c("Accepted"))
  result <- qcSecchiGTDepth(path.to.data = dir, data.source = "local")
  expect_dataframe_equal(result,expected)
})

test_that("qcLakeNotDryMeasurementsMissing finds lake is not dry but measurements are missing", {
  expected <- tibble::tibble(Park = c("GRBA","GRBA","GRBA","GRBA"),
                             SiteShort = c("BRWN0","DEAD0","JHNS0","STLL0"),
                             SiteCode = c("GRBA_L_BRWN0","GRBA_L_DEAD0","GRBA_L_JHNS0","GRBA_L_STLL0"),
                             SiteName = c("Brown Lake","Dead Lake","Johnson Lake","Stella Lake"),
                             VisitDate = as.Date(c("2016-09-20","2017-09-20","2017-09-19","2018-09-27")),
                             FieldSeason = c("2016","2017","2017","2018"),
                             IsLakeDry = c(FALSE,FALSE,FALSE,FALSE),
                             SurfaceCalm = c("Y",NA,"N","N"),
                             OnBottom = c("N","Y",NA,"Y"),
                             DepthToBottom_m = c(NA,8.5,8.5,NA),
                             SecchiDepth_m = c(7.5,1,7.5,1),
                             VisitType = c("Primary","Primary","Primary","Primary"),
                             DPL = c("Accepted","Accepted","Accepted","Accepted"))
  result <- qcLakeNotDryMeasurementsMissing(path.to.data = dir, data.source = "local")
  expect_dataframe_equal(result,expected)
})

test_that("qcSecchiDepthMissing finds disk is not on bottom but secchi depth is missing", {
  expected <- tibble::tibble(Park = c("GRBA"),
                             SiteShort = c("STLL0"),
                             SiteCode = c("GRBA_L_STLL0"),
                             SiteName = c("Stella Lake"),
                             VisitDate = as.Date(c("2019-09-27")),
                             FieldSeason = c("2019"),
                             IsLakeDry = c(FALSE),
                             SurfaceCalm = c("Y"),
                             OnBottom = c("N"),
                             DepthToBottom_m = c(12.2),
                             SecchiDepth_m = as.numeric(c(NA)),
                             VisitType = c("Primary"),
                             DPL = c("Accepted"))
  result <- qcSecchiDepthMissing(path.to.data = dir, data.source = "local")
  expect_dataframe_equal(result,expected)
})

# Remove temporary csv files
unlink(dir, recursive = TRUE)

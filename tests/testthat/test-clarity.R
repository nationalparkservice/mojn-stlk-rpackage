context ("Clarity")


dummy.clarity <- tibble::tibble(Park = c("GRBA","GRBA","GRBA","GRBA","GRBA"),
                                SiteShort = c("BAKR0","BRWN0","DEAD0","JHNS0","STLL0"),
                                SiteCode = c("GRBA_L_BAKR0","GRBA_L_BRWN0","GRBA_L_DEAD0","GRBA_L_JHNS0","GRBA_L_STLL0"),
                                SiteName = c("Baker Lake","Brown Lake","Dead Lake","Johnson Lake","Stella Lake"),
                                VisitDate = as.Date(c("2016-09-21","2016-09-20","2017-09-20","2017-09-19","2018-09-27")),
                                FieldSeason = c("2016","2016","2017","2017","2018"),
                                IsLakeDry = c(FALSE,FALSE,FALSE,FALSE,FALSE),
                                SurfaceCalm = c("N","Y","Y","Y","N"),
                                OnBottom = c("N","Y","Y","Y","Y"),
                                DepthToBottom_m = c(8.5,111,8.5,8.5,111),
                                SecchiDepth_m = c(9.5,7.5,1,7.5,1),
                                VisitType = c("Primary","Primary","Primary","Primary","Primary"),
                                DPL = c("Accepted","Accepted","Accepted","Accepted","Accepted")
                                )

dir <- "temp-test-csv"
dir.create(dir)
readr::write_csv(dummy.clarity, file.path(dir, "Clarity.csv"))

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

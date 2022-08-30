context("Completeness and grade percentage QC")

dummy.completeness <- tibble::tibble(Park = c("GRBA", "GRBA", "GRBA", "GRBA", "GRBA", "GRBA", "GRBA", "PARA"),
                                     Subunit = c("GRBA Unknown", "GRBA Unknown", "GRBA Unknown", "GRBA Unknown", "GRBA Unknown", "GRBA Unknown", "GRBA Unknown", "PARA Unknown"),
                                     SiteCode = c("GRBA_L_L001", "GRBA_L_L002", "GRBA_S_S003", "GRBA_S_S003", "GRBA_L004", "GRBA_S_S005", "GRBA_S_S006", "PARA_S_S001"),
                                     SiteName = c("GRBA L1", "GRBA L2", "GRBA S3", "GRBA S3", "GRBA L4", "GRBA S5", "GRBA S6", "PARA S1"),
                                     VisitDate = c("2018-12-03", "2019-01-21", "2018-12-04", "2019-01-21", "2019-11-20", "2019-11-20", "2019-12-05", "2019-04-17"),
                                     FieldSeason = c("2019", "2019", "2019", "2019", "2020", "2020", "2020", "2019"),
                                     SampleFrame = c("Lake", "Lake", "Stream", "Stream", "Lake", "Lake", "Stream", "Stream"),
                                     VisitType = c("Primary", "Primary", "Primary", "Replicate", "Primary", "Primary", "Primary", "Primary"),
                                     MonitoringStatus = c("Sampled", "Sampled", "Sampled", "Sampled", "Not sampled - Inaccessible", "Sampled", "Sampled", "Sampled")
                                     )

dir <- "temp-test-csv"
if (dir.exists(dir)) {
  unlink(dir, recursive = TRUE)
}
dir.create(dir)
readr::write_csv(dummy.completeness, file.path(dir, "Visit.csv"))

test_that("QcCompleteness works as expected", {
  expected <- tibble::tibble(Park = c("GRBA", "GRBA", "GRBA", "GRBA"),
                             FieldSeason = c("2019", "2019", "2020", "2020"),
                             SampleFrame = c("Lake", "Stream", "Lake", "Stream"),
                             MonitoringStatus = c("Sampled", "Sampled", "Sampled", "Sampled"),
                             Count = as.integer(c(1, 1, 2, 1)),
                             Percent = c(1/20*100, 1/60*100, 2/10*100, 1/35*100))
  expected$Percent <- round(expected$Percent, 3)
  result <- QcCompleteness(path.to.data = dir, data.source = "local")
  result_GRBA <- QcCompleteness(path.to.data = dir, data.source = "local", park = "GRBA")
  result_2019 <- QcCompleteness(path.to.data = dir, data.source = "local", field.season = "2019")
  result_CAMO <- QcCompleteness(path.to.data = dir, data.source = "local", park = "CAMO")
  expect_dataframe_equal(result, expected)
  expect_dataframe_equal(result_GRBA, dplyr::filter(expected, Park == "GRBA"))
  expect_dataframe_equal(result_2019, dplyr::filter(expected, FieldSeason == "2019"))
  expect_equal(nrow(result_CAMO), 0)
})

# Remove temporary CSV files
unlink(dir, recursive = TRUE)

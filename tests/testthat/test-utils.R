context("Utility functions")
skip_if_not(dir.exists('M:/MONITORING/StreamsLakes/Data/Database/ConnectFromR/stlk-database-conn.csv'), message = "Skipped - no VPN connection")

test_that("GetSiteName retrieves the correct site name for the site code provided", {
  conn <- OpenDatabaseConnection()

  expect_equal(GetSiteName(conn, site.code = "GRBA_L_STLL0"), "Stella Lake")
  expect_equal(GetSiteName(conn, site.code = "GRBA_S_MILL1"), "Mill Creek")
  expect_warning(GetSiteName(conn, site.code = "asdf"), "Site: Data are not available for the site specified")

  CloseDatabaseConnection(conn)
})

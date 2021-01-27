context("Database connection")
skip_if_not(file.exists('M:/MONITORING/StreamsLakes/Data/Database/ConnectFromR/stlk-database-conn.csv'), message = "Skipped - no VPN connection")

test_that("Connection to MOJN STLK database is successful", {
  conn <- OpenDatabaseConnection()
  result <- pool::dbGetQuery(conn, "SELECT TOP 1 Park FROM analysis.Site WHERE Park = 'GRBA'")
  CloseDatabaseConnection(conn)

  expect_equal(result$Park, "GRBA")
})

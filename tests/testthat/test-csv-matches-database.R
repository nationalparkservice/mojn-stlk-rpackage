context("Reading from database and csv")
skip_if_not(file.exists('M:/MONITORING/StreamsLakes/Data/Database/ConnectFromR/stlk-database-conn.csv'), message = "Skipped - no VPN connection")

# Write temporary csv files
conn <- OpenDatabaseConnection()
dir <- "temp-test-csv/dbtest"
if (dir.exists(dir)) {
  unlink(dir, recursive = TRUE)
}
SaveDataToCsv(conn, dir, create.folders = TRUE, overwrite = TRUE)

data.names <- c(names(GetColSpec()),
                names(GetAquariusColSpec()))

for (d.name in data.names) {
  test_that(paste0(d.name, ".csv matches data read from database"), {
    db <- ReadAndFilterData(conn, data.name = d.name)
    csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = d.name)
    expect_dataframe_equal(db, csv)
  })
}

CloseDatabaseConnection(conn)

# Remove temporary csv's
unlink(dir, recursive = TRUE)

context("Reading from database and csv")
skip_if_not(file.exists('M:/MONITORING/StreamsLakes/Data/Database/ConnectFromR/stlk-database-conn.csv'), message = "Skipped - no VPN connection")

# Write temporary csv files
conn <- OpenDatabaseConnection()
dir <- "temp_test-csv"
SaveDataToCsv(conn, dir, create.folders = TRUE, overwrite = TRUE)
CloseDatabaseConnection(conn)

data.names <- c(names(GetColSpec()),
                names(GetAquariusColSpec()))

for (d.name in data.names) {
  test_that(paste0(d.name, ".csv matches data read from database"), {
    c <- OpenDatabaseConnection()
    db <- ReadAndFilterData(c, data.name = d.name)
    CloseDatabaseConnection(c)
    csv <- ReadAndFilterData(path.to.data = dir, data.source = "local", data.name = d.name)

    expect_dataframe_equal(db, csv)
  })
}

# Remove temporary csv's
unlink(dir, recursive = TRUE)

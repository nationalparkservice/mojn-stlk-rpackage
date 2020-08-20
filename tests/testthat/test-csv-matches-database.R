context("Reading from database and csv")

# Write temporary csv files
conn <- OpenDatabaseConnection()
dir <- "temp_test-csv"
SaveDataToCsv(conn, dir, create.folders = TRUE, overwrite = FALSE)
CloseDatabaseConnection(conn)

data.names <- names(GetColSpec())

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

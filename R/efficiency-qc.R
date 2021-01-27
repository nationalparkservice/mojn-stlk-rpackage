# You must have the packages "jsonlite" and "httr" installed.
install.packages("jsonlite") # Fast JSON parser
install.packages("httr") # Hadley's nice HTTP requests library

# Make sure the following packages are in your library.
library(jsonlite)
library(httr)
library(dplyr)
library(magrittr)
library(lubridate)
library(statip)
library(tidyr)

# The timeseries_client.R API wrapper:
#     timeseries_client.R must be in your working directory.
source("timeseries_client.R")

# Connects to https://aquarius.nps.gov/aquarius as "aqreadonly"
timeseries$connect("https://aquarius.nps.gov/aquarius", "aqreadonly", "aqreadonly")

# Designate permanent stream gaging locations with water quality sondes.

sites <- c("GRBA_S_BAKR1", "GRBA_S_LHMN1", "GRBA_S_SNKE1", "GRBA_S_SNKE3", "GRBA_S_STRW1")

# Create tibble for continuous water temperature data. Import and tidy data from Aquarius.

wt.all.data <- tibble()

for (location in sites) {
      wt.imp <- timeseries$getTimeSeriesData(paste0("Water Temp.Cumulative@", location))

      wt.data <- wt.imp$Points

      wt.data %<>%
            dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
            dplyr::rename(WaterTemperature_C = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) %>%
            dplyr::filter(Approval == "Approved") %>%
            dplyr::mutate(SiteCode = location) %>%
            dplyr::mutate(Park = "GRBA", SiteType = "Stream")

      wt.data$DateTime <- lubridate::ymd_hms(wt.data$DateTime, tz = "America/Los_Angeles")

      wt.data %<>%
            dplyr::mutate(FieldSeason = ifelse(lubridate::month(DateTime) < 10,
                                               lubridate::year(DateTime),
                                               lubridate::year(DateTime) + 1)) %>%
            dplyr::select(Park,
                          SiteType,
                          SiteCode,
                          FieldSeason,
                          DateTime,
                          WaterTemperature_C,
                          Grade,
                          Approval)

      wt.all.data <- rbind(wt.all.data, wt.data) %>% tibble::as_tibble()
}

# Create tibble for continuous pH data. Import and tidy data from Aquarius.

ph.all.data <- tibble()

for (location in sites) {
      ph.imp <- timeseries$getTimeSeriesData(paste0("pH.Cumulative@", location))

      ph.data <- ph.imp$Points

      ph.data %<>%
            dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
            dplyr::rename(pH = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) %>%
            dplyr::filter(Approval == "Approved") %>%
            dplyr::mutate(SiteCode = location) %>%
            dplyr::mutate(Park = "GRBA", SiteType = "Stream")

      ph.data$DateTime <- lubridate::ymd_hms(ph.data$DateTime, tz = "America/Los_Angeles")

      ph.data %<>%
            dplyr::mutate(FieldSeason = ifelse(lubridate::month(DateTime) < 10,
                                               lubridate::year(DateTime),
                                               lubridate::year(DateTime) + 1)) %>%
            dplyr::select(Park,
                          SiteType,
                          SiteCode,
                          FieldSeason,
                          DateTime,
                          pH,
                          Grade,
                          Approval)

      ph.all.data <- rbind(ph.all.data, ph.data) %>% tibble::as_tibble()
}

# Create tibble for continuous specific conductance data. Import and tidy data from Aquarius.

sc.all.data <- tibble()

for (location in sites) {
      sc.imp <- timeseries$getTimeSeriesData(paste0("Sp Cond.Cumulative@", location))

      sc.data <- sc.imp$Points

      sc.data %<>%
            dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
            dplyr::rename(SpecificConductance_microS_per_cm = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) %>%
            dplyr::filter(Approval == "Approved") %>%
            dplyr::mutate(SiteCode = location) %>%
            dplyr::mutate(Park = "GRBA", SiteType = "Stream")

      sc.data$SpecificConductance_microS_per_cm = sc.data$SpecificConductance_microS_per_cm*1000
      sc.data$DateTime <- lubridate::ymd_hms(sc.data$DateTime, tz = "America/Los_Angeles")

      sc.data %<>%
            dplyr::mutate(FieldSeason = ifelse(lubridate::month(DateTime) < 10,
                                         lubridate::year(DateTime),
                                         lubridate::year(DateTime) + 1)) %>%
            dplyr::select(Park,
                          SiteType,
                          SiteCode,
                          FieldSeason,
                          DateTime,
                          SpecificConductance_microS_per_cm,
                          Grade,
                          Approval)

        sc.all.data <- rbind(sc.all.data, sc.data) %>% tibble::as_tibble()
    }

# Create tibble for continuous dissolved oxygen data. Import and tidy data from Aquarius.

do.all.data <- tibble()

for (location in sites) {
      do.percent.imp <- timeseries$getTimeSeriesData(paste0("Dis Oxygen Sat@", location))
      do.mgl.imp <- timeseries$getTimeSeriesData(paste0("O2 (Dis)@", location))

      do.percent.data <- do.percent.data$Points
      do.mgl.data <- do.mgl.data$Points

      do.percent.data %<>%
            dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
            dplyr::rename(DissolvedOxygen_percent = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) %>%
            dplyr::filter(Approval == "Approved") %>%
            dplyr::mutate(SiteCode = location) %>%
            dplyr::mutate(Park = "GRBA", SiteType = "Stream")

      do.percent.data$DateTime <- lubridate::ymd_hms(do.percent.data$DateTime, tz = "America/Los_Angeles")

      do.percent.data %<>%
            dplyr::mutate(FieldSeason = ifelse(lubridate::month(DateTime) < 10,
                                               lubridate::year(DateTime),
                                               lubridate::year(DateTime) + 1)) %>%
            dplyr::select(Park,
                          SiteType,
                          SiteCode,
                          FieldSeason,
                          DateTime,
                          DissolvedOxygen_percent,
                          DissolvedOxygen_mgL,
                          Grade,
                          Approval)

  do.all.data <- rbind(do.all.data, do.data) %>% tibble::as_tibble()
}


# Filter relevant data, specify column names, add metadata, and standardize time zones for Baker Creek parameter data
bakr1.wt.data <- bakr1.wt.imp$Points
bakr1.wt.data %<>%
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
      dplyr::rename(WaterTemperature_C = NumericValue1, Grade = GradeName1, Approval = ApprovalName1) %>%
      dplyr::filter(Approval == "Approved") %>%
      dplyr::mutate(SiteCode = "GRBA_S_BAKR1")

bakr1.wt.data$Timestamp <- lubridate::ymd_hms(bakr1.wt.data$Timestamp, tz = "America/Los_Angeles")

bakr1.ph.data <- bakr1.ph.imp$Points
bakr1.ph.data %<>%
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
      dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1) %>%
      dplyr::filter(Approval == "Approved") %>%
      dplyr::mutate(Parameter = "pH")

bakr1.ph.data$Timestamp <- lubridate::ymd_hms(bakr1.ph.data$Timestamp, tz ="America/Los_Angeles")

bakr1.sc.data <- bakr1.sc.imp$Points
bakr1.sc.data %<>%
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
      dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1) %>%
      dplyr::mutate(Value = Value*1000) %>%
      dplyr::filter(Approval == "Approved") %>%
      dplyr::mutate(Parameter = "SpecificConductance_microS_per_cm")

bakr1.sc.data$Timestamp <- lubridate::ymd_hms(bakr1.sc.data$Timestamp, tz = "America/Los_Angeles")

bakr1.do.data <- bakr1.do.imp$Points
bakr1.do.data %<>%
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
      dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1) %>%
      dplyr::filter(Approval == "Approved") %>%
      dplyr::mutate(Parameter = "DissolvedOxygen_percent")

bakr1.do.data$Timestamp <- lubridate::ymd_hms(bakr1.do.data$Timestamp, tz = "America/Los_Angeles")

# Stack Baker Creek parameter data into one dataframe
bakr1.wq.data <- rbind(bakr1.wt.data, bakr1.ph.data, bakr1.sc.data, bakr1.do.data) %>%
                 dplyr::mutate(Park = "GRBA", SiteCode = "GRBA_S_BAKR1", SiteType = "Stream") %>%
                 dplyr::rename(DateTime = Timestamp) %>%
                 dplyr::mutate(FieldSeason = lubridate::year(DateTime)) %>%
                 dplyr::select(Park,
                               SiteCode,
                               SiteType,
                               FieldSeason,
                               DateTime,
                               Parameter,
                               Value,
                               Grade,
                               Approval)

# Calculate the mean (for water temperature, specific conductance, and dissolved oxygen)
#     and median (for pH) values for each day based on hourly data.
# Determine the most frequent data grade level for each day based on hourly data.
# Include only those dates with greater than 80% completeness (greater than 19 hourly values).
bakr1.wq.daily <- bakr1.wq.data %>%
      dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
      dplyr::group_by(Park,
                      SiteCode,
                      SiteType,
                      FieldSeason,
                      Date,
                      Parameter) %>%
      dplyr::summarise(DailyValue = case_when(Parameter == "pH" & sum(!is.na(Value)) > 19 ~ median(Value, na.rm = TRUE),
                                              !Parameter == "pH" & sum(!is.na(Value)) > 19 ~ mean(Value, na.rm = TRUE),
                                              TRUE ~ as.double(NA_integer_)),
                      DailyGrade = case_when(!is.na(DailyValue) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
      unique() %>%
      dplyr::arrange(Park, SiteCode, Parameter, Date)

# Calculate the number of days of data for each parameter for each year
#       between the index period of July 1 to September 15 (77 days).
# Calculate the percentage of days with data for each parameter for each year
#       between the index period of July 1 to September 15 (77 days).
bakr1.wq.comp <- bakr1.wq.daily %>%
      dplyr::group_by(Park,
                      SiteCode,
                      SiteType,
                      FieldSeason,
                      Parameter) %>%
      dplyr::filter
      dplyr::summarise(CompletedDays = sum(!is.na(DailyValue)),
                       StartDate = min(Date),
                       EndDate = max(Date)) %>%
      dplyr::mutate(PossibleDays = lubridate::time_length(interval(StartDate, EndDate), "days") + 1,
                    PercentCompleteness = CompletedDays/PossibleDays*100) %>%
      dplyr::select(Park,
                    SiteCode,
                    SiteType,
                    FieldSeason,
                    Parameter,
                    StartDate,
                    EndDate,
                    CompletedDays,
                    PossibleDays,
                    PercentCompleteness)
      dplyr::arrange(Park, SiteCode, Parameter)

# Calculate the percentage of data rated at each grade level for each year.
# MAKE SKINNY. GIVE GRADE ITS OWN COLUMN.
bakr1.wq.grds <- bakr1.wq.daily %>%
      dplyr::group_by(Park,
                  SiteCode,
                  SiteType,
                  FieldSeason,
                  Parameter) %>%
      dplyr::summarise(DaysExcellent = sum(DailyGrade %in% c("Excellent", "Est. Excellent")),
                       DaysGood = sum(DailyGrade %in% c("Good", "Est. Good")),
                       DaysFair = sum(DailyGrade %in% c("Fair", "Est. Fair")),
                       DaysPoor = sum(DailyGrade %in% c("Poor", "Est. Poor"))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(TotalDays = sum(c_across(where(is.integer)))) %>%
      dplyr::mutate(PercentExcellent = DaysExcellent/TotalDays*100,
                    PercentGood = DaysGood/TotalDays*100,
                    PercentFair = DaysFair/TotalDays*100,
                    PercentPoor = DaysPoor/TotalDays*100) %>%
      dplyr::select(Park,
                    SiteCode,
                    SiteType,
                    FieldSeason,
                    Parameter,
                    DaysExcellent,
                    PercentExcellent,
                    DaysGood,
                    PercentGood,
                    DaysFair,
                    PercentFair,
                    DaysPoor,
                    PercentPoor) %>%
      dplyr::arrange(Park, SiteCode, Parameter)

# Import continuous discharge data from Aquarius

#
bakr1.wq.plot <- bakr1.wq.daily %>%
      complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
      filter(Parameter == "WaterTemperature_C")

ggplot(bakr1.wq.plot) +
  geom_line(aes(x = Date, y = DailyValue))

bakr1.wq.plot2 <- bakr1.wq.grds %>%
      filter(Parameter == "pH")

ggplot(bakr1.wq.plot2) +
  geom_bar(aes(x = Date, y = is.numeric))
>>>>>>> efficiency-qc

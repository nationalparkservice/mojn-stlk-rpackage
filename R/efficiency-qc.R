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


# Read in water quality data from Aquarius.
wt <- ReadAquarius(c, "TimeSeriesTemperature")
ph <- ReadAquarius(c, "TimeseriespH")
sc <- ReadAquarius(c, "TimeseriesSpCond")
do.pct <- ReadAquarius(c, "TimeseriesDOSat")
do.mgl <- ReadAquarius(c, "TimeseriesDO")

# Calculate the mean (for water temperature, specific conductance, and dissolved oxygen)
#     and median (for pH) values for each day based on hourly data.
# Determine the most frequent data grade level for each day based on hourly data.
# Include only those dates with greater than 80% completeness (greater than 19 hourly values).

ph.daily <- ph %>%
            dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
            dplyr::group_by(Park,
                            SiteCode,
                            SiteType,
                            FieldSeason,
                            Date) %>%
            dplyr::summarise(pH.Daily = case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(pH)) > 77 ~ median(pH, na.rm = TRUE),
                                                  !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(pH)) > 19 ~ median(pH, na.rm = TRUE),
                                                  TRUE ~ as.double(NA_integer_)),
                             ph.Grade = case_when(!is.na(pH) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
            unique() %>%
            dplyr::arrange(Park, SiteCode, Date) %>%
            dplyr::filter(!is.na(pH.Daily))

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

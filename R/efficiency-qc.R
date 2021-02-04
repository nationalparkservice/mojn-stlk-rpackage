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
source("timeseries_client.R")
wt <- ReadAquarius(c, "TimeSeriesTemperature")
ph <- ReadAquarius(c, "TimeseriespH")
sc <- ReadAquarius(c, "TimeseriesSpCond")
do.pct <- ReadAquarius(c, "TimeseriesDOSat")
do.mgl <- ReadAquarius(c, "TimeseriesDO")

# Calculate the mean (for water temperature, specific conductance, and dissolved oxygen)
#     and median (for pH) values for each day based on hourly data.
# Determine the most frequent data grade level for each day based on hourly data.
# Include only those dates with greater than 80% completeness (greater than 19 hourly values).
wt.daily <- wt %>%
            dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
            dplyr::group_by(Park,
                            SiteCode,
                            SiteType,
                            FieldSeason,
                            Date) %>%
            dplyr::summarise(WaterTemperature_C = case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(WaterTemperature_C)) > 77 ~ median(WaterTemperature_C, na.rm = TRUE),
                                                                  !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(WaterTemperature_C)) > 19 ~ median(WaterTemperature_C, na.rm = TRUE),
                                                                  TRUE ~ as.double(NA_integer_)),
                             Grade = case_when(!is.na(WaterTemperature_C) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
            unique() %>%
            dplyr::arrange(Park, SiteCode, Date) %>%
            dplyr::filter(!is.na(WaterTemperature_C))

ph.daily <- ph %>%
            dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
            dplyr::group_by(Park,
                            SiteCode,
                            SiteType,
                            FieldSeason,
                            Date) %>%
            dplyr::summarise(pH = case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(pH)) > 77 ~ median(pH, na.rm = TRUE),
                                                  !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(pH)) > 19 ~ median(pH, na.rm = TRUE),
                                                  TRUE ~ as.double(NA_integer_)),
                             Grade = case_when(!is.na(pH) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
            unique() %>%
            dplyr::arrange(Park, SiteCode, Date) %>%
            dplyr::filter(!is.na(pH))

sc.daily <- sc %>%
            dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
            dplyr::group_by(Park,
                            SiteCode,
                            SiteType,
                            FieldSeason,
                            Date) %>%
            dplyr::summarise(SpecificConductance_microS_per_cm = case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(SpecificConductance_microS_per_cm)) > 77 ~ median(SpecificConductance_microS_per_cm, na.rm = TRUE),
                                                                           !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(SpecificConductance_microS_per_cm)) > 19 ~ median(SpecificConductance_microS_per_cm, na.rm = TRUE),
                                                                           TRUE ~ as.double(NA_integer_)),
                             Grade = case_when(!is.na(SpecificConductance_microS_per_cm) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
            unique() %>%
            dplyr::arrange(Park, SiteCode, Date) %>%
            dplyr::filter(!is.na(SpecificConductance_microS_per_cm))

do.pct.daily <- do.pct %>%
                dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
                dplyr::group_by(Park,
                                SiteCode,
                                SiteType,
                                FieldSeason,
                                Date) %>%
                dplyr::summarise(DissolvedOxygen_percent = case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(DissolvedOxygen_percent)) > 77 ~ median(DissolvedOxygen_percent, na.rm = TRUE),
                                                                               !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(DissolvedOxygen_percent)) > 19 ~ median(DissolvedOxygen_percent, na.rm = TRUE),
                                                                               TRUE ~ as.double(NA_integer_)),
                                 Grade = case_when(!is.na(DissolvedOxygen_percent) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
                unique() %>%
                dplyr::arrange(Park, SiteCode, Date) %>%
                dplyr::filter(!is.na(DissolvedOxygen_percent))

do.mgl.daily <- do.mgl %>%
                dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
                dplyr::group_by(Park,
                                SiteCode,
                                SiteType,
                                FieldSeason,
                                Date) %>%
                dplyr::summarise(DissolvedOxygen_mgL = case_when(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012 & sum(!is.na(DissolvedOxygen_mgL)) > 77 ~ median(DissolvedOxygen_mgL, na.rm = TRUE),
                                                                 !(SiteCode == "GRBA_S_SNKE1" & FieldSeason == 2012) & sum(!is.na(DissolvedOxygen_mgL)) > 19 ~ median(DissolvedOxygen_mgL, na.rm = TRUE),
                                                                 TRUE ~ as.double(NA_integer_)),
                                 Grade = case_when(!is.na(DissolvedOxygen_mgL) ~ statip::mfv1(Grade, na_rm = TRUE))) %>%
                unique() %>%
                dplyr::arrange(Park, SiteCode, Date) %>%
                dplyr::filter(!is.na(DissolvedOxygen_mgL))

# Calculate the number of days of data for each parameter for each year
#       between the index period of July 1 to September 15 (77 days).
# Calculate the percentage of days with data for each parameter for each year
#       between the index period of July 1 to September 15 (77 days).
wt.comp <- wt.daily %>%
           dplyr::group_by(Park,
                           SiteCode,
                           SiteType,
                           FieldSeason) %>%
           dplyr::mutate(Month = lubridate::month(Date),
                         Day = lubridate::day(Date)) %>%
           dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) %>%
           dplyr::summarise(CompletedDays = sum(!is.na(WaterTemperature_C))) %>%
           dplyr::mutate(PercentCompleteness = CompletedDays/77*100)

wt.comp.new <- wt.daily %>%
  dplyr::group_by(Park,
                  SiteCode,
                  SiteType,
                  FieldSeason) %>%
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) %>%
  dplyr::filter(Month ==6 | Month == 7 | Month == 8 | Month == 9 ) %>%
  dplyr::summarise(CompletedDays = sum(!is.na(WaterTemperature_C))) %>%
  dplyr::mutate(PercentCompleteness = CompletedDays/122*100)


# Calculate the percentage of data rated at each grade level for each year.
# MAKE SKINNY. GIVE GRADE ITS OWN COLUMN.
wt.grds <- wt.daily %>%
      dplyr::group_by(Park,
                      SiteCode,
                      SiteType,
                      FieldSeason) %>%
      dplyr::mutate(Month = lubridate::month(Date),
                    Day = lubridate::day(Date)) %>%
      dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) %>%
      dplyr::summarise(DaysExcellent = sum(Grade %in% c("Excellent", "Est. Excellent")),
                       DaysGood = sum(Grade %in% c("Good", "Est. Good")),
                       DaysFair = sum(Grade %in% c("Fair", "Est. Fair")),
                       DaysPoor = sum(Grade %in% c("Poor", "Est. Poor"))) %>%
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
                    DaysExcellent,
                    PercentExcellent,
                    DaysGood,
                    PercentGood,
                    DaysFair,
                    PercentFair,
                    DaysPoor,
                    PercentPoor) %>%
      dplyr::arrange(Park, SiteCode)

wt.grds2 <- wt.daily %>%
  dplyr::group_by(Park,
                  SiteCode,
                  SiteType,
                  FieldSeason) %>%
  dplyr::mutate(Month = lubridate::month(Date),
                Day = lubridate::day(Date)) %>%
  dplyr::filter(Month == 7 | Month == 8 | (Month == 9 & Day <= 15)) %>%
  dplyr::count(Grade) %>%
  dplyr::rename(Days = n) %>%
  dplyr::mutate(TotalDays = sum(Days)) %>%
  dplyr::mutate(Percent = Days/TotalDays*100)

# TEST OF VARIOUS PLOTS
bakr1.wq.plot <- bakr1.wq.daily %>%
      complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
      filter(Parameter == "WaterTemperature_C")

ggplot(bakr1.wq.plot) +
  geom_line(aes(x = Date, y = DailyValue))

bakr1.wq.plot2 <- bakr1.wq.grds %>%
      filter(Parameter == "pH")

ggplot(bakr1.wq.plot2) +
  geom_bar(aes(x = Date, y = is.numeric))

ggplot(data = wt.comp.new, aes(x = FieldSeason, y = PercentCompleteness)) +
      geom_bar(stat="identity", position=position_dodge(), color="black") +
      facet_grid(~SiteCode) +
      scale_x_discrete()

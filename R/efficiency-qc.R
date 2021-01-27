# # You must have the packages "jsonlite" and "httr" installed.
# install.packages("jsonlite") # Fast JSON parser
# install.packages("httr") # Hadley's nice HTTP requests library
#
# # Make sure the following packages are in your library.
# library(jsonlite)
# library(httr)
# library(dplyr)
# library(magrittr)
# library(lubridate)
# library(statip)
# library(tidyr)
#
# # The timeseries_client.R API wrapper:
# #     timeseries_client.R must be in your working directory.
# source("timeseries_client.R")
#
# # Connects to https://aquarius.nps.gov/aquarius as "aqreadonly"
# timeseries$connect("https://aquarius.nps.gov/aquarius", "aqreadonly", "aqreadonly")
#
# # Import continuous water quality data from Aquarius for Baker Creek at the park boundary
# bakr1.wt.imp <- timeseries$getTimeSeriesData("Water Temp.Cumulative@GRBA_S_BAKR1")
# bakr1.ph.imp <- timeseries$getTimeSeriesData("pH.Cumulative@GRBA_S_BAKR1")
# bakr1.sc.imp <- timeseries$getTimeSeriesData("Sp Cond.Cumulative@GRBA_S_BAKR1")
# bakr1.do.imp <- timeseries$getTimeSeriesData("Dis Oxygen Sat.Cumulative@GRBA_S_BAKR1")
#
# # Import continuous water quality data from Aquarius for Lehman Creek at the park boundary
# lhmn1.wt.imp <- timeseries$getTimeSeriesData("Water Temp.Cumulative@GRBA_S_LHMN1")
# lhmn1.ph.imp <- timeseries$getTimeSeriesData("pH.Cumulative@GRBA_S_LHMN1")
# lhmn1.sc.imp <- timeseries$getTimeSeriesData("Sp Cond.Cumulative@GRBA_S_LHMN1")
# lhmn1.do.imp <- timeseries$getTimeSeriesData("Dis Oxygen Sat.Cumulative@GRBA_S_LHMN1")
#
# # Import continuous water quality data from Aquarius for Strawberry Creek at the park boundary
# strw1.wt.imp <- timeseries$getTimeSeriesData("Water Temp.Cumulative@GRBA_S_STRW1")
# strw1.ph.imp <- timeseries$getTimeSeriesData("pH.Cumulative@GRBA_S_STRW1")
# strw1.sc.imp <- timeseries$getTimeSeriesData("Sp Cond.Cumulative@GRBA_S_STRW1")
# strw1.do.imp <- timeseries$getTimeSeriesData("Dis Oxygen Sat.Cumulative@GRBA_S_STRW1")
#
# # Import continuous water quality data from Aquarius for Snake Creek (Lower) at the park boundary
# snke1.wt.imp <- timeseries$getTimeSeriesData("Water Temp.Cumulative@GRBA_S_SNKE1")
# snke1.ph.imp <- timeseries$getTimeSeriesData("pH.Cumulative@GRBA_S_SNKE1")
# snke1.sc.imp <- timeseries$getTimeSeriesData("Sp Cond.Cumulative@GRBA_S_SNKE1")
# snke1.do.imp <- timeseries$getTimeSeriesData("Dis Oxygen Sat.Cumulative@GRBA_S_SNKE1")
#
# # Import continuous water quality data from Aquarius for Snake Creek (Upper) above the pipeline
# snke3.wt.imp <- timeseries$getTimeSeriesData("Water Temp.Cumulative@GRBA_S_SNKE3")
# snke3.ph.imp <- timeseries$getTimeSeriesData("pH.Cumulative@GRBA_S_SNKE3")
# snke3.sc.imp <- timeseries$getTimeSeriesData("Sp Cond.Cumulative@GRBA_S_SNKE3")
# snke3.do.imp <- timeseries$getTimeSeriesData("Dis Oxygen Sat.Cumulative@GRBA_S_SNKE3")
#
# # Filter relevant data, specify column names, add metadata, and standardize time zones for Baker Creek parameter data
# bakr1.wt.data <- bakr1.wt.imp$Points
# bakr1.wt.data %<>%
#       dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
#       dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1) %>%
#       dplyr::filter(Approval == "Approved") %>%
#       dplyr::mutate(Parameter = "WaterTemperature_C")
#
# bakr1.wt.data$Timestamp <- lubridate::ymd_hms(bakr1.wt.data$Timestamp, tz = "America/Los_Angeles")
#
# bakr1.ph.data <- bakr1.ph.imp$Points
# bakr1.ph.data %<>%
#       dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
#       dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1) %>%
#       dplyr::filter(Approval == "Approved") %>%
#       dplyr::mutate(Parameter = "pH")
#
# bakr1.ph.data$Timestamp <- lubridate::ymd_hms(bakr1.ph.data$Timestamp, tz ="America/Los_Angeles")
#
# bakr1.sc.data <- bakr1.sc.imp$Points
# bakr1.sc.data %<>%
#       dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
#       dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1) %>%
#       dplyr::mutate(Value = Value*1000) %>%
#       dplyr::filter(Approval == "Approved") %>%
#       dplyr::mutate(Parameter = "SpecificConductance_microS_per_cm")
#
# bakr1.sc.data$Timestamp <- lubridate::ymd_hms(bakr1.sc.data$Timestamp, tz = "America/Los_Angeles")
#
# bakr1.do.data <- bakr1.do.imp$Points
# bakr1.do.data %<>%
#       dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) %>%
#       dplyr::rename(Value = NumericValue1, Grade = GradeName1, Approval = ApprovalName1) %>%
#       dplyr::filter(Approval == "Approved") %>%
#       dplyr::mutate(Parameter = "DissolvedOxygen_percent")
#
# bakr1.do.data$Timestamp <- lubridate::ymd_hms(bakr1.do.data$Timestamp, tz = "America/Los_Angeles")
#
# # Stack Baker Creek parameter data into one dataframe
# bakr1.wq.data <- rbind(bakr1.wt.data, bakr1.ph.data, bakr1.sc.data, bakr1.do.data) %>%
#                  dplyr::mutate(Park = "GRBA", SiteCode = "GRBA_S_BAKR1", SiteType = "Stream") %>%
#                  dplyr::rename(DateTime = Timestamp) %>%
#                  dplyr::mutate(FieldSeason = lubridate::year(DateTime)) %>%
#                  dplyr::select(Park,
#                                SiteCode,
#                                SiteType,
#                                FieldSeason,
#                                DateTime,
#                                Parameter,
#                                Value,
#                                Grade,
#                                Approval)
#
# # Calculate the mean (for water temperature, specific conductance, and dissolved oxygen)
# #     and median (for pH) values for each day based on hourly data.
# # Determine the most frequent data grade level for each day based on hourly data.
# bakr1.wq.daily <- bakr1.wq.data %>%
#       dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
#       dplyr::group_by(Park,
#                       SiteCode,
#                       SiteType,
#                       FieldSeason,
#                       Date,
#                       Parameter) %>%
#       dplyr::summarise(DailyValue = mean(Value, na.rm = TRUE),
#                        DailyGrade = statip::mfv1(Grade, na_rm = TRUE)) %>%
#       dplyr::arrange(Park, SiteCode, Parameter, Date)
#
# bakr1.wq.daily <- bakr1.wq.data %>%
#   dplyr::mutate(Date = as.Date(DateTime, format = "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
#   dplyr::group_by(Park,
#                   SiteCode,
#                   SiteType,
#                   FieldSeason,
#                   Date,
#                   Parameter) %>%
#       dplyr::summarise(DailyValue = ifelse(Parameter == "pH",
#                                            median(Value, na.rm = TRUE),
#                                            mean(Value, na.rm = TRUE)),
#                        DailyGrade = statip::mfv1(Grade, na_rm = TRUE)) %>%
#       dplyr::arrange(Park, SiteCode, Parameter, Date)
#
#
#
# # OLD CODE CHUNKS
#       dplyr::summarise(DailyValue = ifelse(Parameter %in% c("WaterTemperature_C", "SpecificConductance_microS_per_cm", "DissolvedOxygen_percent"),
#                                            mean(Value, na.rm = TRUE),
#                                            median(Value, na.rm = TRUE)),
#                        DailyGrade = ifelse(is.nan(statip::mfv1(Grade, na_rm = TRUE)),
#                                            NA,
#                                            statip::mfv1(Grade, na_rm = TRUE)))
#
# # If a day has at least 80% completeness (>19 of 24 possible events recorded),
#         # calculate the mean or median value and determine the most frequent data grade level for that day.
# # Otherwise, return NA.
# bak1.wq.daily <- bak1.wq.data %>%
#       dplyr::mutate(Date = as.Date(Timestamp, format = "%Y-%m-%d")) %>%
#       dplyr::group_by(Date) %>%
#       dplyr::summarise(wt.num.day = ifelse(count>19, mean(wt.num, na.rm = TRUE), NA),
#                        wt.grade.day = ifelse(count>19, max(wt.grade, na.rm = TRUE), NA),
#                        ph.num.day = ifelse(count>19, median(ph.num, na.rm = TRUE), NA))
#
# # Calculate the number of days of data for each parameter for each year.
# # Calculate the percentage of days with data for each parameter for each year.
# bakr1.wq.comp <- bakr1.wq.daily %>%
#       dplyr::group_by(Park,
#                       SiteCode,
#                       SiteType,
#                       FieldSeason,
#                       Parameter) %>%
#       dplyr::summarise(Count = sum(!is.na(DailyValue)),
#                        StartDate = min(Date),
#                        EndDate = max(Date)) %>%
#       dplyr::mutate(Percent = Count/_____________)*100,
#                     ph.pct.yr = ph.ct.yr/pmax(wt.ct.yr, ph.ct.yr, sc.ct.yr, do.ct.yr)*100,
#                     sc.pct.yr = sc.ct.yr/pmax(wt.ct.yr, ph.ct.yr, sc.ct.yr, do.ct.yr)*100,
#                     do.pct.yr = do.ct.yr/pmax(wt.ct.yr, ph.ct.yr, sc.ct.yr, do.ct.yr)*100) %>%
#       dplyr::select(Year,
#                     date.start,
#                     date.end,
#                     wt.ct.yr,
#                     wt.pct.yr,
#                     ph.ct.yr,
#                     ph.pct.yr,
#                     sc.ct.yr,
#                     sc.pct.yr,
#                     do.ct.yr,
#                     do.pct.yr)
#
# test <- bakr1.wq.comp %>%
#         dplyr::mutate(Days = lubridate::time_length(interval(StartDate, EndDate), "days"))
#
# # Calculate the percentage of data rated at each grade level for each year.
# bakr1.wq.grds <- bakr1.wq.daily %>%
#       dplyr::mutate(Year = year(Date)) %>%
#       dplyr::group_by(Year) %>%
#       dplyr::summarise(excellent = sum(wt.grade.day == "Excellent" | wt.grade.day == "Est. Excellent"),
#                        good = sum(wt.grade.day == "Good" | wt.grade.day == "Est. Good"),
#                        fair = sum(wt.grade.day == "Fair" | wt.grade.day == "Est. Fair"),
#                        poor = sum(wt.grade.day == "Poor" | wt.grade.day == "Est. Poor")) %>%
#       dplyr::mutate(excellent.pct = excellent/rowSums(select(., excellent, good, fair, poor))*100,
#                     good.pct = good/rowSums(select(., excellent, good, fair, poor))*100,
#                     fair.pct = fair/rowSums(select(., excellent, good, fair, poor))*100,
#                     poor.pct = poor/rowSums(select(., excellent, good, fair, poor))*100) %>%
#       dplyr::select(Year,
#                     excellent.pct,
#                     good.pct,
#                     fair.pct,
#                     poor.pct) %>%
#       dplyr::rename(Year = Year,
#                     Excellent_or_Est_Percent = excellent.pct,
#                     Good_or_Est_Percent = good.pct,
#                     Fair_or_Est_Percent = fair.pct,
#                     Poor_or_Est_Percent = poor.pct)
#
# # Import continuous discharge data from Aquarius
#
# #
# bak1.wq.plot <- bak1.wq.daily %>%
#       complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
#
# ggplot(bak1.wq.plot) +
#   geom_line(aes(x = Date, y = ph.num.day))
#
#

context ("WaterQuality")

dummy.waterquality <- tibble::tibble(Park = c("GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA"),
                                SiteCode = c("GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_STLL0","GRBA_L_STLL0","GRBA_L_STLL0","GRBA_L_STLL0","GRBA_L_STLL0","GRBA_L_STLL0","GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_STLL0","GRBA_L_STLL0","GRBA_L_STLL0"),
                                SiteName = c("Johnson Lake","Johnson Lake","Johnson Lake","Johnson Lake","Johnson Lake","Johnson Lake","Stella Lake","Stella Lake","Stella Lake","Stella Lake","Stella Lake","Stella Lake","Johnson Lake","Johnson Lake","Johnson Lake","Johnson Lake","Johnson Lake","Johnson Lake","Stella Lake","Stella Lake","Stella Lake"),
                                FieldSeason = c("2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2017","2018","2018","2018","2018","2018","2018","2018","2018","2018"),
                                VisitDate = as.Date(c(2017-09-19,2017-09-19,2017-09-19,2017-09-19,2017-09-19,2017-09-19,2017-09-20,2017-09-20,2017-09-20,2017-09-20,2017-09-20,2017-09-20,2018-09-18,2018-09-18,2018-09-18,2018-09-18,2018-09-18,2018-09-18,2018-09-19,2018-09-19,2018-09-19)),
                                StartTime = c(12:15:00,12:15:00,12:15:00,12:15:00,12:15:00,12:15:00,9:45:00,9:45:00,9:45:00,9:45:00,9:45:00,9:45:00,12:20:00,12:20:00,12:20:00,12:20:00,12:20:00,12:20:00,9:50:00,9:50:00,9:50:00),
                                IsLakeDry = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                                MeasurementDepth_m = c(0,0.91,1.83,2.74,3.66,4.42,0,0.15,0.3,0.61,0.76,0.46,0,0.91,1.83,2.74,3.66,3.96,0,0.3,0.55),
                                pH = c(8.87,8.74,8.68,8.61,8.62,9.06,8.59,8.58,8.56,8.46,8.49,8.49,9.53,9.54,9.56,9.54,9.6,8.91,8.41,8.2,8.06),
                                DissolvedOxygen_percent = c(51.9,49.6,44.4,46.5,50,40.1,53.7,52.3,53,53.9,50,53.4,73.8,73.3,74.8,74.1,75.1,73.5,67.4,66.7,66.3),
                                DissolvedOxygen_mg_per_L = c(6.02,5.84,5.3,5.54,5.93,4.6,6.63,6.44,6.51,6.63,6.06,6.54,8.41,8.46,8.59,8.56,8.72,8.47,7.99,7.91,7.86),
                                SpecificConductance_microS_per_cm = c(33,33,32,34,32,34,27,27,27,25,28,27,38.5,38.7,38.7,38.6,38.9,33.6,41.7,40.3,40.3),
                                WaterTemperature_C = c(8.36,8.26,8.05,7.8,7.72,8.97,6.35,6.34,6.4,6.47,6.65,6.7,9.7,9.4,9.3,9,8.9,8.9,8,7.9,8),
                                Notes = c("Note","Note","Note","Note","Note","Note","Note","Note","Note","Note","Note","Note","Note","Note","Note","Note","Note","Note","Note","Note","Note"),
                                DPL = c("Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted"),
                                DQF_pH = c("NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF"),
                                DQF_DO = c("NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF"),
                                DQF_SpCond = c("NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF"),
                                DQF_Temp = c("NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF","NF")
)

dir <- "temp-test-csv"
dir.create(dir)
readr::write_csv(dummy.waterquality, file.path(dir, "WaterQuality.csv"))



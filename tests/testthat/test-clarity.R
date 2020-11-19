context ("Clarity")
skip("Test not written yet")

dummy.clarity <- tibble::tibble(Park = c("GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA","GRBA"),
                                SiteCode = c("BAKR0","BAKR0","BAKR0","BRWN0","BRWN0","BRWN0","DEAD0","DEAD0","DEAD0","JHNS0","JHNS0","JHNS0","STLL0","STLL0","STLL0","TRSA0","TRSA0","TRSA0"),
                                SiteCode = c("GRBA_L_BAKR0","GRBA_L_BAKR0","GRBA_L_BAKR0","GRBA_L_BRWN0","GRBA_L_BRWN0","GRBA_L_BRWN0","GRBA_L_DEAD0","GRBA_L_DEAD0","GRBA_L_DEAD0","GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_JHNS0","GRBA_L_STLL0","GRBA_L_STLL0","GRBA_L_STLL0","GRBA_L_TRSA0","GRBA_L_TRSA0","GRBA_L_TRSA0"),
                                SiteName = c("Baker Lake","Baker Lake","Baker Lake","Brown Lake","Brown Lake","Brown Lake","Dead Lake","Dead Lake","Dead Lake","Johnson Lake","Johnson Lake","Johnson Lake","Stella Lake","Stella Lake","Stella Lake","Teresa Lake","Teresa Lake","Teresa Lake"),
                                VisitDate = as.Date(c("2016-09-21","2017-09-16","2018-09-20","2016-09-20","2017-09-20","2018-09-19","2016-09-28","2017-09-19","2018-09-27","2016-09-28","2017-09-18","2018-09-18","2016-09-20","2017-10-01","2018-09-18","2016-09-20","2017-09-20","2018-09-09")),
                                FieldSeason = c("2016","2017","2018","2016","2017","2018","2016","2017","2018","2016","2017","2018","2016","2017","2018","2016","2017","2018"),
                                IsLakeDry = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
                                SurfaceCalm = c("N","Y","Y","Y","N","N","Y","N","Y","Y","N","N","N","N","Y","Y","N","Y"),
                                OnBottom = c("N","Y","Y","Y","Y","Y","Y","N","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y"),
                                DepthToBottom_m = c(1.95,1.6,2.04,0.34,0.76,0.37,0.27,2.21,0.09,4.36,4.42,3.96,0.64,0.76,0.55,0.79,1.34,0.7),
                                SecchiDepth_m = c(1.75,,,,,,,11.44,,,,,,,,,,),
                                VisitType = c("Primary","Primary","Primary","Primary","Primary","Primary","Primary","Primary","Primary","Primary","Primary","Primary","Primary","Primary","Primary","Primary","Primary","Primary"),
                                DPL = c("Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted","Accepted")
                                )


dir <- "temp-test-csv"
dir.create(dir)
readr::write_csv(dummy.clarity, file.path(dir, "Clarity.csv"))



# Set-up ----
source("Scripts/Functions/functions.R")
packages<-c("fs","tseries","urca","kableExtra","gt","tidyverse","rtf","broom",
            "readxl","haven","tidyquant","forecast","dint","chron","lubridate",
            "arrow","magrittr")
package_fn(packages)


# Reading data / saving into Rdata ----
#df<-read_dta(file = "Data/Raw Data/usa_all.dta") #now not required
#write_parquet(x= df, sink = "Data/Raw Data/us_data.parquet") #Save for compact data, beter for using with other platforms too
#save(df, file = "Data/Raw Data/usa_data.Rdata" )# Save for compact data for use within R
#df<-read_parquet("Data/Raw Data/us_data.parquet") Read only if more categories required
#load("Data/Raw Data/usa_data.Rdata")


# Data subset and save ----

#df %<>% 
        #filter(bppcat %in% c(seq(911,914),seq(931,934))) #filter for required categories

#write_parquet(x= df, sink = "Data/Raw Data/us_data_paper.parquet")
df<-read_parquet("Data/Raw Data/us_data_paper.parquet") # read filtered data




df %<>%
        arrange(date) %>% 
        mutate(week=week(ymd(date))) %>% 
        relocate(week) ## create week and arrange




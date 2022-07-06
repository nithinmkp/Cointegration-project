# Set-up ----
source("Scripts/Functions/functions.R")
packages<-c("fs","kableExtra","gt","tidyverse","rtf","broom",
            "readxl","haven","tidyquant","forecast","dint","chron","lubridate",
            "arrow","magrittr","timetk","nombre","tsbox","huxtable","here",
            "modelsummary")
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



df1<-df %>% 
        select(rid_id,cat_url,bppcat,date,price0) ## subset of the data

df_split<-split(df1,df1$bppcat) ## split it into dataframes by bppcat number

bppcatcode<-data.frame(code=names(df_split),
                       product=c("Woodpulp","Wastepaper","Paper",
                                 "Paperboard","Newspapers","Periodicals",
                                 "Book publishing","Book printing"))

# Calculate weekly mean and median prices ----

week_bppcat_list<-map(df_split,week_bppcat_function) %>% 
        set_names(bppcatcode$product)

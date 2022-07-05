# Set-up ----
source("Scripts/Functions/functions.R")
packages<-c("fs","tseries","urca","kableExtra","gt","tidyverse","rtf","broom",
            "readxl","haven","tidyquant","forecast","dint","chron","lubridate",
            "arrow","magrittr","timetk","nombre","tsbox","huxtable")
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

# Calculate weekly mean and median prices ----

week_bppcat_list<-map(df_split,week_bppcat_function) %>% 
        set_names(cardinal(as.numeric(names(df_split))))


# Stationarity Analysis ----

source("Scripts/Functions/unitroots.R") # Load my custom functions
safe_unit<-safely(unitroot_fn) #safely compute
unit_tests<-map(week_bppcat_list,safe_unit) 
unit_results<-map(unit_tests,pluck("result")) %>% 
        discard(~is.null(.x)) # results table

varnames <- rep(c("Statistic", "P-Value", "Lag Length"), 3)

map(unit_results,~rename_fn(df=.x,col_ind = -1,new_names = varnames)) %>% 
        map(~kbl(.x, booktabs = T) %>%
                    kable_classic(latex_options = "scale_down") %>%
                    add_header_above(c(" " = 1, "ADF Test" = 3, "PP Test" = 3,
                                       "KPSS Test" = 3))) # view results in viewer pane


tabs <- RTF(file = "unit_tables.rtf") #name of table

iwalk(unit_results, ~ table_fn(tabs, x=.y, y=.x))
done(tabs)

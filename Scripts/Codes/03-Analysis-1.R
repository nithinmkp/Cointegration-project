# Analysis Part ----

## Stationarity Analysis ----

### Unit root tests (tseries package) ----

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


table_list<-map(week_bppcat_list,~ts_fn(df=.x,cols=c("mean_price","median_price")))



results_comb<-table_list %>% map(. %>% map(safely(unitroot_fn)) )%>% 
        map(. %>% 
                    map(pluck("result")) %>% 
                    map_df(bind_rows))




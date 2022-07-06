# Analysis Part ----

## Descriptive Statistics ----
dir_create("Tables/Summary Statistics")

imap(week_bppcat_list,~datasummary_skim(data=.x,histogram=F,
                                        title=paste0("Summary Statistics: ",.y),
                                        output = "kableExtra")) # For you to view

sum_table_list<-imap(week_bppcat_list,~datasummary_skim(data=.x,histogram=F,
                        title=paste0("Summary Statistics: ",.y),
                        output = "latex")) 
tibble(x=sum_table_list,
       file=paste0("Tables/Summary Statistics/",names(week_bppcat_list),".tex")) %>% 
        pwalk(save_kable) # output saved to latex file


## Stationarity Analysis ----

### Unit root tests (tseries package) ----

source("Scripts/Functions/unitroots.R") # Load my custom functions
safe_unit<-safely(unitroot_fn) #safely compute
unit_tests<-map(week_bppcat_list,safe_unit) 
unit_results<-map(unit_tests,pluck("result")) %>% 
        discard(~is.null(.x)) %>% 
        map(~.x %>% mutate(
                across(-1,.fns = round,2)
        ))# results table

varnames <- rep(c("Statistic", "P-Value", "Lag Length"), 3)

map(unit_results,~rename_fn(df=.x,col_ind = -1,new_names = varnames)) %>% 
        map(~kbl(.x, booktabs = T) %>%
                    kable_classic(latex_options = "scale_down") %>%
                    add_header_above(c(" " = 1, "ADF Test" = 3, "PP Test" = 3,
                                       "KPSS Test" = 3))) # view results in viewer pane


dir_create("Tables/Results/Unit-root")

tabs <- RTF(file = "Tables/Results/Unit-root/unit_tables.rtf") #name of table (word format)

iwalk(unit_results, ~ table_fn(tabs, x=.y, y=.x))
done(tabs)



table_list<-map(week_bppcat_list,~ts_fn(df=.x,cols=c("mean_price","median_price")))



results_comb<-table_list %>% map(. %>% map(safely(unitroot_fn))) %>% 
        map(. %>% 
                    map(pluck("result")) %>% 
                    map_df(bind_rows)) %>% 
        discard(~nrow(.x)==0) %>% 
        map(~.x %>% mutate(
                across(-1,.fns = round,2)
        ))

results_comb  %>% 
        imap(~kbl(.x, booktabs = T,format = "latex") %>%
                    kable_classic(latex_options = "scale_down") %>%
                    add_header_above(c(" " = 1, "ADF Test" = 3, "PP Test" = 3,
                                       "KPSS Test" = 3)) %>% 
                    save_kable(paste0("Tables/Results/Unit-root/unit_root-",.y,".tex")))


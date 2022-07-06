
# Set-up ----

source("Scripts/Functions/functions.R")
unit_packages<-c("tseries","urca","broom","data.table")
package_fn(unit_packages)
# Unit root tests-Table ---------------------------------------------------

unit_tests<-function(x){
        
        list("ADF"=adf.test(x) %>% tidy() ,
             "PP"=pp.test(x) %>% tidy(),
             "KPSS"=kpss.test(x) %>% tidy()
        )
}

unitroot_fn<-function(x){
        x<-data.frame(x)
        
        r1<-map(x,~unit_tests(.x)) %>% map(bind_rows)
        methods<-map_dfr(r1,"method")
        methods<-methods[,1,drop=T]
        r2<-r1  %>% 
                map_df(bind_rows,.id = "Variable")%>% 
                dplyr::select(-alternative,
                       "Lag-length"=parameter) %>% 
                pivot_wider(names_from = "method",
                            values_from = c("statistic","p.value","Lag-length"),
                            names_glue = "{method}_{.value}") %>% 
                dplyr::select(Variable,starts_with(methods))
        return(r2)
        
}

# Complete TS table -------------------------------------------------------

ts_fn <- function(df, cols, order = 1) {
        df<-data.frame(df)
        for (col in cols) {
                df <- mutate(df, "ln_{col}" := log(get(col)))
        }
        varnames <- df %>%
                dplyr::select(cols, starts_with("ln")) %>%
                names()
        
        diff_fn <- function(x) {
                assign(paste0("diff_", x), diff(x))
        }
        
        diff_lst <- df %>%
                dplyr::select(varnames) %>%
                map(diff_fn) %>%
                map(bind_cols) %>%
                map_dfc(bind_cols) %>%
                setNames(paste0("diff_", varnames))
        
        df <- as_tibble(df)
        ln_df <- df %>% dplyr::select(starts_with("ln"))
        df <- df %>% dplyr::select(-starts_with("ln"))
        
        
        lst <- list(
                levels = df,
                log_levels = ln_df,
                diff = diff_lst
        )
        return(lst)
}

# URCA tables ----
unit_urca_fn<-function(x){
        res<-list(ur.df(x,type = "none",selectlags = "AIC"),
                  ur.df(x,type = "drift",selectlags = "AIC"),
                  ur.df(x,type = "trend",selectlags = "AIC"),
                  ur.pp(x,type = "Z-tau",model = "constant"),
                  ur.pp(x,type = "Z-tau",model = "trend"),
                  ur.ers(x, type = "DF-GLS",model = "constant",lag.max = 4),
                  ur.ers(x, type = "DF-GLS",model = "trend",lag.max = 4))
        ntests<-length(res)
        res
        
}
za_function<-function(x){
        res<-list( ur.za(x,model = "intercept",lag=4),
                   ur.za(x,model = "trend",lag=4),
                   ur.za(x,model = "both",lag=4))
}
kpss_function<-function(x){
        res<-list(ur.kpss(x,type = "mu",lags = "short"),
                  ur.kpss(x,type = "tau",lags = "short"))
}


test_stats<-function(x){
        
        unit_stats<-map(x,~unit_urca_fn(.x)) %>% unlist()
        za_stats<-map(x,~za_function(.x)) %>% unlist()
        kpss_stats<-map(x,~kpss_function(.x))%>% unlist()
        results<-list(unit_stats=unit_stats,
                      za_stats=za_stats,
                      kpss_stats=kpss_stats)
        
        
}

stationary_table_fn<-function(x){
        res1<-x$unit_stats %>% map_df(.,~data.table("Test Statistic"=round(.x@teststat,3),Model=.x@model,
                                                    Test=.x@test.name),.id="Variable")
        res2<-x$za_stats %>% map_df(.,~data.table("Test Statistic"=round(.x@teststat,3),
                                                  Model=.x@model,
                                                  "Breakpoint"=.x@bpoint,
                                                  Test=.x@test.name),.id="Variable")
        res3<-x$kpss_stats %>% map_df(.,~data.table("Test Statistic"=round(.x@teststat,3),
                                                    Test=.x@test.name),.id="Variable")
        final_list<-lst(unittest=res1,
                        za_test=res2,
                        kpss_test=res3) 
        final_table<-final_list %>% reduce(full_join) %>% 
                select(Variable,Test,Model,dplyr::everything())
        
        
}



critcial_table_fn<-function(x){
        res1<-x$unit_stats %>% map(~data.table(round(.x@cval,3),
                                               .x@test.name,
                                               .x@model,
                                               keep.rownames = T)) %>% list.rbind()
        res1<-res1[!duplicated(res1),]
        setnames(res1,c("rn","V2","V3"),c("Statistic","Test","Model"))
        res2<-x$za_stats %>% map(~tibble(rn="",
                                         "1pct"=.x@cval[1],
                                         "5pct"=.x@cval[2],
                                         "10pct"=.x@cval[3],
                                         .x@model),
                                 keep.rownames = T) %>% list.rbind() %>% 
                as.data.table()
        setnames(res2,c("rn",".x@model"),c("Statistic","Model"))
        
        res2<-res2[!duplicated(res2),]
        res3<-x$kpss_stats %>% map(~data.table(.x@cval,
                                               .x@type,
                                               keep.rownames = T)) %>% list.rbind()
        res3<-res3[!duplicated(res3),]
        setnames(res3,c("rn","V2"),c("Statistic","Type"))
        
        final<-list(unittest=res1,
                    za_test=res2,
                    kpss_test=res3)
        
        
        
}
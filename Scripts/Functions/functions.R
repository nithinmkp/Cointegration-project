
# Functions ---------------------------------------------------------------

## Package Function ---- 
package_fn<-function(pkg){
  new.pkg<-setdiff(pkg,installed.packages()[,"Package"])
  if(length(new.pkg)){
    install.packages(new.pkg,dependencies = T)
    sapply(pkg,library,character.only=T)
  }else{
    sapply(pkg,library,character.only=T)
  }
}


## type conversion function ---- 
convert_fn<-function(df, col_ind,fn,...) {
  df <- df %>% mutate(across(.cols = col_ind, .fns = fn,...))
}

## function to calculate weekly mean prices ----
week_bppcat_function<- function(df){
        df %>% 
                group_by(date) %>%   
                summarise(mean_price=mean(price0,na.rm=T),
                          median_price=median(price0,na.rm=T)) %>% 
                tk_xts(date_var = date) %>% 
                apply.weekly(mean,na.rm=T)
}

## Rename Function ----
rename_fn <- function(df, col_ind, new_names) {
        names(df)[col_ind] <- new_names
        return(df)
}

## Table Function to word ----
table_fn <- function(dest, x, y) {
        addParagraph(dest, x, "\n")
        addTable(dest, y
        )
        addParagraph(dest, "\n")
}

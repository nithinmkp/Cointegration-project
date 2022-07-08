
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

## Data arrange (for adding dates) ----
data_arrange_fn<-function(x){
        ind<-index(x)
        x<-data.frame(x)
        rownames(x)<-NULL
        x  %>% 
                mutate(date=ind) %>% 
                select(date,dplyr::everything())
}
## Data Plot Function ----
data_plot_fn<- function(x,labtitle){
        x %>% data_arrane_fn() %>% 
                pivot_longer(-date,
                             names_to = "Series",
                             values_to = "Values") %>% 
                ggplot()+
                aes(x=date,
                    y=Values)+
                geom_line()+
                facet_wrap(~Series,scales = "free",
                           nrow=2)+
                scale_x_date(date_breaks = "12 week", date_labels = "%d-%b-%Y")+
                theme_tq()+
                labs(x="Date",
                     y="Price",
                     title = labtitle)
        
}



# Writing critical value table to word ----

word_fn<-function(tab,x,y){
        tab_name<-RTF(file = paste0(tab,".doc"))
        tab_fn<-function(x,y){
                addParagraph(tab_name,x,"\n")
                addTable(tab_name,y)
                addParagraph(tab_name,"\n")      
        }
        walk2(x,y,tab_fn)
        done(tab_name)
}

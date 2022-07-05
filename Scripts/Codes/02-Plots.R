# Plots ----
## Preliminary Plots ----
dir_create("Plots") #create plot folder

plot_list<-imap(week_bppcat_list,data_plot_fn) #preliminary plot of all series
tibble(plot=plot_list,
       filename=paste0("Plots/",bppcatcode$product,".jpg"))%>% 
        pwalk(ggsave,width = 29.5,
              height = 22.50,
              units = "cm",
              dpi = 300)

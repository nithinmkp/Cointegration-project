
# Functions ---------------------------------------------------------------

## ---- Package Function
package_fn<-function(pkg){
  new.pkg<-setdiff(pkg,installed.packages()[,"Package"])
  if(length(new.pkg)){
    install.packages(new.pkg,dependencies = T)
    sapply(pkg,library,character.only=T)
  }else{
    sapply(pkg,library,character.only=T)
  }
}

## ---- KD plot function
kd_fn<- function(df,xvar,catvar,alph=0.2,siz=1.5,xlab,ylab="Density",
                 title=NA,leg_title,gr=F,legend.pos.h=0.29,
                 legend.pos.t=1,ylab_acur=2,leg_row=3,pal_name){
  ncolors<-df %>% select({{catvar}}) %>% n_distinct()
  #my_colors<-colorRampPalette(brewer.pal(8,"Set2"))(ncolors)
  p1<-df %>% ggplot(aes(x=.data[[xvar]]))+
    geom_density(aes(color=.data[[catvar]]),
                 alpha=alph,
                 size=siz,
                 key_glyph="smooth")+
    theme_bw()+
    theme(legend.direction  ="vertical",
          legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="darkblue"),
          legend.text = element_text(size = 9),
          legend.title.align = 0.5,
          legend.justification = "top",
          legend.position = c(legend.pos.h,legend.pos.t))+
    guides(col=guide_legend(nrow=leg_row, byrow=T))+
    labs(x=xlab,y=ylab,
         color=leg_title)+
    scale_color_manual(values = met.brewer(name = pal_name,n=ncolors))
  if(gr==T){
    p1+scale_x_continuous(labels = label_percent(accuracy = ylab_acur))
  }else{
    p1
  }
}

## ---- type conversion function
convert_fn<-function(df, col_ind,fn,...) {
  df <- df %>% mutate(across(.cols = col_ind, .fns = fn,...))
}

#
# ------------------------------------------------------------------------------
# Script: code_11.4_Figure5_by_region.R
# Purpose:
#   Create Figure 5-style visualisations summarising projected reductions in
#   RSV-associated hospitalisations and deaths, stratified by WHO region.
#
# Inputs:
#   - model_data/df_res.all.RDS (model results across scenarios/parameter sets)
#   - functions.R
#
# Outputs:
#   - Figure files saved via `ggsave()` (see file names in the script).
#
# Usage:
#   source("code_11.4_Figure5_by_region.R")
# ------------------------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(ggsci)
library(rio)
library(ggforce)
library(janitor)
library(MCMCpack)
library(showtext)
library(patchwork)
library(scales)
font_add_google(name = "Amatic SC", family = "amatic-sc")
showtext_auto()
source('functions.R')

df_res.all<-rio::import('model_data/df_res.all.RDS') %>%
  mutate(params_str=map(params,~paste0(.x,collapse = '_')))

df_res.all %>% distinct(params_str)

# Functions ----
add_by<-function(df_res,bywhat){
  df_res %>%
    summarise(across(c(starts_with('pop_infant'),starts_with('N_')),~sum(.x,na.rm=T)),.by=c(index,{{bywhat}})) %>%
    mutate(hr_ctr=N_hos_ctr/pop_infant_2026,
           hr_pre=N_hos_pre/pop_infant_2026,
           mt_ctr=N_dea_ctr/pop_infant_2026,
           mt_pre=N_dea_pre/pop_infant_2026)
}

sum_by<-function(df,bywhat){
  df %>%
    dplyr::summarise(across(where(is.numeric) & !index,list(q500=~median(.x),q025=~quantile(.x,probs = 0.025),q975=~quantile(.x,probs = 0.975))),.by={{bywhat}})
}

per_to_str<-function(per,trim=T){
  sign<-if_else(per==0,' ',if_else(per>0,'+','-'))
  num_str <- sprintf("%s%.1f", sign, round(abs(per * 100),1))
  padded<-sprintf("%6s%%", num_str)
  if(trim==T) trimws(padded)
  else padded
}

gen_change_percent<-function(sum_by_region_with_global){
  sum_by_region_with_global %>%
    transmute(Region=toupper(WHORegion),
              pop_infant_2019,pop_infant_2026,
              N_hos_2019_q500,N_hos_ctr_q500,N_hos_pre_q500,
              dec_hos=N_hos_ctr_q500/N_hos_2019_q500-1,
              dec_hos2=N_hos_pre_q500/N_hos_ctr_q500-1,
              N_dea_2019_q500,N_dea_ctr_q500,N_dea_pre_q500,
              dec_dea=N_dea_ctr_q500/N_dea_2019_q500-1,
              dec_dea2=N_dea_pre_q500/N_dea_ctr_q500-1,
    ) %>%
    mutate(across(c(dec_hos,dec_hos2,dec_dea,dec_dea2),~per_to_str(.x)))
}

get_global_change_percent<-function(df){
  df %>% filter(Region=='GLOBAL') %>% dplyr::select(dec_hos,dec_hos2,dec_dea,dec_dea2) %>%
    unlist()
}


# Figure 5----
getFigure5_by_region <- function(sum_by_region,df_res.main,barwidth=.3,showlabel=FALSE) {
  labels <- if_else(rep(showlabel,3),
                    c('2019','2026 \n (no implementation)','2026 \n (status quo implementation)'),
                    rep('',3))
  df_plot5<-sum_by_region %>%
    dplyr::select(WHORegion,matches('N_.*(pre|ctr|2019)_q500')) %>%
    transmute(WHORegion,
              hos_2019=round(N_hos_2019_q500),dea_2019=round(N_dea_2019_q500),# 2019
              hosctr_2026=round(N_hos_ctr_q500),
              hos_2026=round(N_hos_pre_q500),
              deactr_2026=round(N_dea_ctr_q500),
              dea_2026=round(N_dea_pre_q500),
    ) %>%
    pivot_longer(cols=-WHORegion,names_to=c('N','year'),names_pattern = "(.*)_(.*)")
  
  sum_by_region_with_global<-bind_rows(
    df_res.main %>% add_by(WHORegion),
    df_res.main %>% mutate(WHORegion='Global') %>% add_by(WHORegion)
  ) %>% 
    sum_by(WHORegion) %>%
    rename(pop_infant_2019=pop_infant_2019_q500,
           pop_infant_2026=pop_infant_2026_q500) %>%
    dplyr::select(-matches('pop_.*_q(025|975)'))
  
  # max hos and death
  total_hos<-df_plot5 %>%
    filter(str_detect(N,'hos')) %>%
    summarise(value=sum(value),.by=c(N,year)) %>%
    arrange(year,desc(N)) %>% pull(value)
  
  
  total_dea<-df_plot5 %>%
    filter(str_detect(N,'dea')) %>%
    summarise(value=sum(value),.by=c(N,year)) %>%
    arrange(year,desc(N)) %>% pull(value) 
  
  # Global chnage percent
  dec_hos<-get_global_change_percent(gen_change_percent(sum_by_region_with_global))[1:2]
  dec_dea<-get_global_change_percent(gen_change_percent(sum_by_region_with_global))[3:4]
  
  

  ### p51 ----
  
  data_23<-df_plot5 %>%
    filter(str_detect(N,'hos'),year==2026) %>%
    mutate(WHORegion=factor(WHORegion,levels=c('Sear','Afr','Emr','Amr','Wpr','Eur'))) %>%
    arrange(N,WHORegion) %>%
    group_by(N) %>%
    mutate(cum=cumsum(value)) %>%
    pivot_wider(id_cols = WHORegion,names_from = N,values_from = cum) %>%
    mutate(y1=hosctr,
           y2=hos,
           y_1=lag(y1,default=0),y_2=lag(y2,default=0),
           y_3=y2,y_4=y1,
           x1=2+barwidth*0.5,x2=3-barwidth*0.5,x3=3-barwidth*0.5,x4=2+barwidth*0.5)
  
  data_12<-df_plot5 %>%
    filter(str_detect(N,'hos')) %>%
    filter(str_detect(N,'ctr')|year==2019) %>%
    mutate(WHORegion=factor(WHORegion,levels=c('Sear','Afr','Emr','Amr','Wpr','Eur'))) %>%
    arrange(year,WHORegion) %>%
    group_by(year) %>%
    mutate(cum=cumsum(value)) %>%
    pivot_wider(id_cols = WHORegion,names_from = year,values_from = cum) %>%
    mutate(y1=`2019`,
           y2=`2026`,
           y_1=lag(y1,default=0),y_2=lag(y2,default=0),
           y_3=y2,y_4=y1,
           x1=1+barwidth*0.5,x2=2-barwidth*0.5,x3=2-barwidth*0.5,x4=1+barwidth*0.5)
  
  
  df_polygon_12<-1:6 %>%
    map_dfr(~{
      data.frame(WHORegion=data_12[[.x,'WHORegion']],
                 x=c(data_12[[.x,'x1']],data_12[[.x,'x2']],data_12[[.x,'x3']],data_12[[.x,'x4']]),
                 y=c(data_12[[.x,'y_1']],data_12[[.x,'y_2']],data_12[[.x,'y_3']],data_12[[.x,'y_4']]))
    })
  
  df_polygon_23<-1:6 %>%
    map_dfr(~{
      data.frame(WHORegion=data_23[[.x,'WHORegion']],
                 x=c(data_23[[.x,'x1']],data_23[[.x,'x2']],data_23[[.x,'x3']],data_23[[.x,'x4']]),
                 y=c(data_23[[.x,'y_1']],data_23[[.x,'y_2']],data_23[[.x,'y_3']],data_23[[.x,'y_4']]))
    })
  
  
  df_percent_23<-sum_by_region %>%
    dplyr::select(WHORegion,matches('N_.*(pre|ctr)_q500')) %>%
    adorn_totals() %>%
    transmute(WHORegion,dec_hos=format(round((1-N_hos_pre_q500/N_hos_ctr_q500)*100,1),nsmall=1),
              dec_dea=format(round((1-N_dea_pre_q500/N_dea_ctr_q500)*100,1),nsmall=1),
              dec_hos_str=paste0(ifelse(N_hos_pre_q500==N_hos_ctr_q500,'','-'),dec_hos,'%'),
              dec_dea_str=paste0(ifelse(N_dea_pre_q500==N_dea_ctr_q500,'','-'),dec_dea,'%')) %>%
    left_join(data_23) %>%
    transmute(WHORegion,dec_hos_str,dec_dea_str,y=(y_1+y_2+y_3+y_4)/4)
  
  
  
  df_percent_12<-df_plot5 %>%
    filter(str_detect(N,'ctr') | year==2019) %>%
    mutate(WHORegion=factor(WHORegion,levels=c('Sear','Afr','Emr','Amr','Wpr','Eur'))) %>%
    pivot_wider(id_cols = WHORegion,names_from = c(N,year),values_from = value) %>%
    transmute(WHORegion,dec_hos=format(round((hosctr_2026/hos_2019-1)*100,1),nsmall=1,trim = TRUE),
              dec_dea=format(round((deactr_2026/dea_2019-1)*100,1),nsmall=1,trim = TRUE),
              dec_hos_str=paste0(ifelse(hosctr_2026/hos_2019>1,'+',''),dec_hos,'%'),
              dec_dea_str=paste0(ifelse(deactr_2026/dea_2019>1,'+',''),dec_dea,'%'),
              dec_hos_str=format(dec_hos_str,width = 6, justify = "right")) %>%
    left_join(data_12) %>%
    transmute(WHORegion,dec_hos_str,dec_dea_str,y=(y_1+y_2+y_3+y_4)/4)
  
  
  p51<-df_plot5 %>%
    filter(str_detect(N,'hos')) %>%
    mutate(WHORegion=str_to_upper(WHORegion),
           linetype=ifelse(str_detect(N,'ctr'),'solid','solid'),
           group=interaction(N,year),
           group=factor(group,levels=c('hos.2019','hosctr.2026','hos.2026'))
    ) %>%
    arrange(N,year,value) %>%
    mutate(WHORegion=factor(WHORegion,levels=unique(WHORegion))) %>%
    ggplot()+
    geom_col(aes(group,value,fill=WHORegion,linetype=I(linetype)),width=barwidth,color='gray2',linewidth=.2)+
    scale_fill_manual(name=NULL,
                      values=c(AFR="#0392cf", AMR="#ffad60", EMR="#E6B800", EUR="#7bc043", SEAR="#d81159",WPR="#6a4c93")
    )+
    scale_y_continuous(expand = expansion(mult=c(0,.03),add=c(0,NULL)),labels = scales::label_comma())+
    #scale_x_discrete(labels = c('2019','2026 \n (counter-factual scenario)','2026 \n (status quo scenario)'))+
    scale_x_discrete(labels = labels )+
    theme_bw()+
    theme(legend.position = 'none',
          panel.grid=element_blank(),
          panel.border = element_blank(),
          axis.ticks.length = unit(.15, "cm"),
          axis.line = element_line(size=.25),
          axis.ticks = element_line(size=.25),
          axis.title.x = element_blank())+
    #labs(y='RSV-associated hospital admissions')+
    labs(y=NULL)+
    geom_polygon(data=df_polygon_12,aes(x,y,fill = str_to_upper(WHORegion)),alpha=.2,
                 color = "black", size = .1)+
    geom_label(data=df_percent_12,aes(x=1.5,y=y,label=dec_hos_str),
               size=4,fontface='plain')+
    geom_polygon(data=df_polygon_23,aes(x,y,fill = str_to_upper(WHORegion)),alpha=.2,
                 color = "black", size = .1)+
    geom_label(data=df_percent_23,aes(x=2.5,y=y,label=dec_hos_str),
               size=4,fontface='plain')+
    annotate(geom='segment',x=1+barwidth*0.5,xend=3+barwidth*0.75,y=total_hos[1],yend=total_hos[1],
             linewidth=.2,linetype='dashed',color='#A51C30')+
    annotate(geom='segment',x=2+barwidth*0.5,xend=3+barwidth*0.75,y=total_hos[2],yend=total_hos[2],
             linewidth=.2,linetype='dashed',color='#A51C30')+
    annotate(geom='segment',x=3+barwidth*0.5,xend=3+barwidth*0.75,y=total_hos[3],yend=total_hos[3],
             linewidth=.2,linetype='dashed',color='#A51C30')+
    annotate(geom='segment',x=3+barwidth*0.70,xend=3+barwidth*0.70,y=total_hos[1],yend=total_hos[2],
             linewidth=.2,arrow = arrow(length = unit(0.15, "cm")),color='#A51C30')+
    annotate(geom='segment',x=3+barwidth*0.70,xend=3+barwidth*0.70,y=total_hos[2],yend=total_hos[3],
             linewidth=.2,arrow = arrow(length = unit(0.15, "cm")),color='#A51C30')+
    annotate(geom='label',x=3+barwidth*0.75,y=mean(total_hos[1:2]),label=dec_hos[1],hjust=0-.05,vjust=.5,size=4,fontface='plain',color='#A51C30')+
    annotate(geom='label',x=3+barwidth*0.75,y=mean(total_hos[2:3]),label=dec_hos[2],hjust=0-.05,vjust=.5,size=4,fontface='plain',color='#A51C30')+
    annotate(geom='label',x=3+barwidth*0.75,y=mean(total_hos[1:2]),label=dec_hos[1],hjust=0-.05,vjust=.5,size=4,fontface='plain',label.size=NA)+
    annotate(geom='label',x=3+barwidth*0.75,y=mean(total_hos[2:3]),label=dec_hos[2],hjust=0-.05,vjust=.5,size=4,fontface='plain',label.size=NA)
  
  p51
  
  ### p52 ----
  
  data_23<-df_plot5 %>%
    filter(str_detect(N,'dea'),year==2026) %>%
    mutate(WHORegion=factor(WHORegion,levels=c('Afr','Sear','Emr','Wpr','Amr','Eur'))) %>%
    arrange(N,WHORegion) %>%
    group_by(N) %>%
    mutate(cum=cumsum(value)) %>%
    pivot_wider(id_cols = WHORegion,names_from = N,values_from = cum) %>%
    mutate(y1=deactr,
           y2=dea,
           y_1=lag(y1,default=0),y_2=lag(y2,default=0),
           y_3=y2,y_4=y1,
           x1=2+barwidth*0.5,x2=3-barwidth*0.5,x3=3-barwidth*0.5,x4=2+barwidth*0.5)
  
  
  data_12<-df_plot5 %>%
    filter(str_detect(N,'dea')) %>%
    filter(str_detect(N,'ctr')|year==2019) %>%
    mutate(WHORegion=factor(WHORegion,levels=c('Afr','Sear','Emr','Wpr','Amr','Eur'))) %>%
    arrange(year,WHORegion) %>%
    group_by(year) %>%
    mutate(cum=cumsum(value)) %>%
    pivot_wider(id_cols = WHORegion,names_from = year,values_from = cum) %>%
    mutate(y1=`2019`,
           y2=`2026`,
           y_1=lag(y1,default=0),y_2=lag(y2,default=0),
           y_3=y2,y_4=y1,
           x1=1+barwidth*0.5,x2=2-barwidth*0.5,x3=2-barwidth*0.5,x4=1+barwidth*0.5)
  
  
  df_polygon_12<-1:6 %>%
    map_dfr(~{
      data.frame(WHORegion=data_12[[.x,'WHORegion']],
                 x=c(data_12[[.x,'x1']],data_12[[.x,'x2']],data_12[[.x,'x3']],data_12[[.x,'x4']]),
                 y=c(data_12[[.x,'y_1']],data_12[[.x,'y_2']],data_12[[.x,'y_3']],data_12[[.x,'y_4']]))
    })
  
  df_polygon_23<-1:6 %>%
    map_dfr(~{
      data.frame(WHORegion=data_23[[.x,'WHORegion']],
                 x=c(data_23[[.x,'x1']],data_23[[.x,'x2']],data_23[[.x,'x3']],data_23[[.x,'x4']]),
                 y=c(data_23[[.x,'y_1']],data_23[[.x,'y_2']],data_23[[.x,'y_3']],data_23[[.x,'y_4']]))
    })
  
  
  df_percent_23<-sum_by_region %>%
    dplyr::select(WHORegion,matches('N_.*(pre|ctr)_q500')) %>%
    adorn_totals() %>%
    transmute(WHORegion,dec_hos=format(round((1-N_hos_pre_q500/N_hos_ctr_q500)*100,1),nsmall=1),
              dec_dea=format(round((1-N_dea_pre_q500/N_dea_ctr_q500)*100,1),nsmall=1),
              dec_hos_str=paste0(ifelse(N_hos_pre_q500==N_hos_ctr_q500,'','-'),dec_hos,'%'),
              dec_dea_str=paste0(ifelse(N_dea_pre_q500==N_dea_ctr_q500,'','-'),dec_dea,'%')) %>%
    mutate(dec_dea_str=recode(dec_dea_str,'-0.0%'=' 0.0%')) %>%
    left_join(data_23) %>%
    transmute(WHORegion,dec_hos_str,dec_dea_str,y=(y_1+y_2+y_3+y_4)/4)
  
  df_percent_12<-df_plot5 %>%
    filter(str_detect(N,'ctr') | year==2019) %>%
    mutate(WHORegion=factor(WHORegion,levels=c('Sear','Emr','Afr','Wpr','Amr','Eur'))) %>%
    pivot_wider(id_cols = WHORegion,names_from = c(N,year),values_from = value) %>%
    transmute(WHORegion,dec_hos=format(round((hosctr_2026/hos_2019-1)*100,1),nsmall=1,trim = TRUE),
              dec_dea=format(round((deactr_2026/dea_2019-1)*100,1),nsmall=1,trim = TRUE),
              dec_hos_str=paste0(ifelse(hosctr_2026/hos_2019>1,'+',''),dec_hos,'%'),
              dec_dea_str=paste0(ifelse(deactr_2026/dea_2019>1,'+',''),dec_dea,'%'),
              dec_hos_str=format(dec_hos_str,width = 6, justify = "right")) %>%
    left_join(data_12) %>%
    transmute(WHORegion,dec_hos_str,dec_dea_str,y=(y_1+y_2+y_3+y_4)/4)
  
  p52<-df_plot5 %>%
    filter(str_detect(N,'dea')) %>%
    mutate(WHORegion=str_to_upper(WHORegion),
           linetype=ifelse(str_detect(N,'ctr'),'solid','solid'),
           group=interaction(N,year),
           group=factor(group,levels=c('dea.2019','deactr.2026','dea.2026'))
    ) %>%
    arrange(N,year,value) %>%
    mutate(WHORegion=factor(WHORegion,levels=unique(WHORegion))) %>%
    ggplot()+
    geom_col(aes(group,value,fill=WHORegion,linetype=I(linetype)),width=barwidth,color='gray2',linewidth=.2)+
    scale_fill_manual(name=NULL,
                      values=c(AFR="#0392cf", AMR="#ffad60", EMR="#E6B800", EUR="#7bc043", SEAR="#d81159",WPR="#6a4c93")
    )+
    scale_y_continuous(expand = expansion(mult=c(0,0.03),add=c(0,NULL)),
                       breaks=c(seq(0,70000,by=10000)),
                       labels = scales::label_comma())+
    #scale_x_discrete(labels = c('2019','2026 \n (counter-factual scenario)','2026 \n (status quo scenario)'))+
    scale_x_discrete(labels = labels )+
    geom_polygon(data=df_polygon_23,aes(x,y,fill = str_to_upper(WHORegion)),alpha=.2,
                 color = "black", size = .1)+
    geom_label(data=df_percent_23,aes(x=2.5,y=y,label=dec_dea_str),
               size=4,fontface='plain')+
    geom_polygon(data=df_polygon_12,aes(x,y,fill = str_to_upper(WHORegion)),alpha=.2,
                 color = "black", size = .1)+
    geom_label(data=df_percent_12,aes(x=1.5,y=y,label=dec_dea_str),
               size=4,fontface='plain')+
    #labs(y='RSV-attributable deaths')+
    labs(y=NULL)+
    theme_bw()+
    theme(panel.grid=element_blank(),
          panel.border = element_blank(),
          axis.ticks.length = unit(.15, "cm"),
          axis.line = element_line(size=.25),
          axis.ticks = element_line(size=.25),
          axis.title.x = element_blank(),
          legend.key.size = unit(0.75,'cm'),
          legend.text = element_text(size=14))+
    annotate(geom='segment',x=1-barwidth*0.75,xend=2-barwidth*0.5,y=total_dea[2],yend=total_dea[2],
             linewidth=.2,linetype='dashed',color='#A51C30')+
    annotate(geom='segment',x=2+barwidth*0.5,xend=3+barwidth*0.75,y=total_dea[2],yend=total_dea[2],
             linewidth=.2,linetype='dashed',color='#A51C30')+
    annotate(geom='segment',x=1-barwidth*0.75,xend=1-barwidth*0.5,y=total_dea[1],yend=total_dea[1],
             linewidth=.2,linetype='dashed',color='#A51C30')+
    annotate(geom='segment',x=3+barwidth*0.5,xend=3+barwidth*0.75,y=total_dea[3],yend=total_dea[3],
             linewidth=.2,linetype='dashed',color='#A51C30')+
    annotate(geom='segment',x=1-barwidth*0.7,xend=1-barwidth*0.7,y=total_dea[1],yend=total_dea[2],
             linewidth=.2,arrow = arrow(length = unit(0.15, "cm")),color='#A51C30')+
    annotate(geom='segment',x=3+barwidth*0.7,xend=3+barwidth*0.7,y=total_dea[2],yend=total_dea[3],
             linewidth=.2,arrow = arrow(length = unit(0.15, "cm")),color='#A51C30')+
    annotate(geom='label',x=1-barwidth*0.75,y=mean(total_dea[1:2]),label=dec_dea[1],hjust=1+.05,vjust=.5,size=4,fontface='plain',color='#A51C30')+
    annotate(geom='label',x=3+barwidth*0.75,y=mean(total_dea[2:3]),label=dec_dea[2],hjust=0-.05,vjust=.5,size=4,fontface='plain',color='#A51C30')+
    annotate(geom='label',x=1-barwidth*0.75,y=mean(total_dea[1:2]),label=dec_dea[1],hjust=1+.05,vjust=.5,size=4,fontface='plain',label.size=NA)+
    annotate(geom='label',x=3+barwidth*0.75,y=mean(total_dea[2:3]),label=dec_dea[2],hjust=0-.05,vjust=.5,size=4,fontface='plain',label.size=NA)
  
  p52
  
  
  p5<-p51 + p52 + plot_layout(guides = "collect") +
    plot_annotation()
  
  p5
}

unique(df_res.all$params_str)
df_res.main<-df_res.all %>% filter(params_str=='c(0.8, 0.05, 0.005)_c(0.83, 0.81, 0.71, 0.77)_c(1, 1)')

sum_by_region<-df_res.main %>% add_by(WHORegion) %>% sum_by(WHORegion)

## Figures (main analysis) ----
getFigure5_by_region(sum_by_region,df_res.main)
ggsave('pdf/Figure5_by_region.pdf',width = 13,height = 8)

## Figures (sensitive analysis) ----
df_res.all %>% 
  group_split(params) %>% 
  walk(~{
    df_res.main<-.x
    params=.x[[1,'params_str']] %>% str_replace_all('[, ]','_')
    print(params)
    
    sum_by_region<-df_res.main %>% add_by(WHORegion) %>% sum_by(WHORegion) %>%
      rename(pop_infant_2019=pop_infant_2019_q500,
             pop_infant_2026=pop_infant_2026_q500) %>%
      dplyr::select(-matches('pop_.*_q(025|975)'))
    
    sum_by_region_with_global<-bind_rows(
      df_res.main %>% add_by(WHORegion),
      df_res.main %>% mutate(WHORegion='Global') %>% add_by(WHORegion)
    ) %>% 
      sum_by(WHORegion) %>%
      rename(pop_infant_2019=pop_infant_2019_q500,
             pop_infant_2026=pop_infant_2026_q500) %>%
      dplyr::select(-matches('pop_.*_q(025|975)'))
    
    options(scipen = 999)
    sum_by_region_with_global %>%
      mutate(dec_hos_23=round(abs(N_hos_pre_q500/N_hos_ctr_q500-1)*100,1),
             dec_dea_23=round(abs(N_dea_pre_q500/N_dea_ctr_q500-1)*100,1),
             dec_hos_23_str=sprintf('↓%2.1f%%',dec_hos_23),
             dec_dea_23_str=sprintf('↓%2.1f%%',dec_dea_23)
      ) %>%
      mutate(across(starts_with('N_hos'),~round(.x/1000)*1000),
             across(starts_with('N_dea'),~round(.x/100)*100),
             across(matches('(hr_)'),~round(.x,1)),
             across(matches('(mt_)'),~round(.x,2))) %>%
      transmute(Region=toupper(WHORegion),
                N_Hos_ctr=sprintf('%s\n(%s-%s)',N_hos_ctr_q500,N_hos_ctr_q025,N_hos_ctr_q975),
                R_Hos_ctr=sprintf('%.1f\n(%.1f-%.1f)',hr_ctr_q500,hr_ctr_q025,hr_ctr_q975),
                N_Dea_ctr=sprintf('%s\n(%s-%s)',N_dea_ctr_q500,N_dea_ctr_q025,N_dea_ctr_q975),
                R_Dea_ctr=sprintf('%.2f\n(%.2f-%.2f)',mt_ctr_q500,mt_ctr_q025,mt_ctr_q975),
                N_Hos_pre=sprintf('%s (%s)\n(%s-%s)',N_hos_pre_q500,dec_hos_23_str,N_hos_pre_q025,N_hos_pre_q975),
                R_Hos_pre=sprintf('%.1f (%s)\n(%.1f-%.1f)',hr_pre_q500,dec_hos_23_str,hr_pre_q025,hr_pre_q975),
                N_Dea_pre=sprintf('%s (%s)\n(%s-%s)',N_dea_pre_q500,dec_dea_23_str,N_dea_pre_q025,N_dea_pre_q975),
                R_Dea_pre=sprintf('%.2f (%s)\n(%.2f-%.2f)',mt_pre_q500,dec_dea_23_str,mt_pre_q025,mt_pre_q975),
      ) %>%
      relocate(Region,
               N_Hos_ctr,N_Hos_pre, R_Hos_ctr,R_Hos_pre,
               N_Dea_ctr,N_Dea_pre, R_Dea_ctr,R_Dea_pre) %>%
      arrange(Region) %>%
      `[`(c(1:4,6,7,5),) %>%
      createAppendixTable(sprintf('docs/Model_Result_Main_%s.xlsx',params))
    
    getFigure5_by_region(sum_by_region,df_res.main)
    ggsave(filename = sprintf('pdf/Figure5_by_region_%s.pdf',params),width = 13,height = 8)
  })

plots_by_region<-df_res.all %>% 
  group_split(params) %>% 
  map(~{
    df_res.main<-.x
    params=.x[[1,'params_str']] %>% str_replace_all('[, ]','_')
    print(params)
    
    sum_by_region<-df_res.main %>% add_by(WHORegion) %>% sum_by(WHORegion) %>%
      rename(pop_infant_2019=pop_infant_2019_q500,
             pop_infant_2026=pop_infant_2026_q500) %>%
      dplyr::select(-matches('pop_.*_q(025|975)'))
    
    sum_by_region_with_global<-bind_rows(
      df_res.main %>% add_by(WHORegion),
      df_res.main %>% mutate(WHORegion='Global') %>% add_by(WHORegion)
    ) %>% 
      sum_by(WHORegion) %>%
      rename(pop_infant_2019=pop_infant_2019_q500,
             pop_infant_2026=pop_infant_2026_q500) %>%
      dplyr::select(-matches('pop_.*_q(025|975)'))
    
    getFigure5_by_region(sum_by_region,df_res.main,showlabel = TRUE)
  })


plots_by_region[[1]]
# ➡️ -plots_by_region ---
export(plots_by_region,'rda/plots_by_region.rds')

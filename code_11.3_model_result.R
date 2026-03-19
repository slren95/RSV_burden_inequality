#
# ------------------------------------------------------------------------------
# Script: code_11.3_model_result.R
# Purpose:
#   Post-process and summarise model outputs (regional/global aggregates,
#   percentage changes, appendix tables) for reporting and downstream plotting.
#
# Inputs:
#   - rda/code_11.2_model.RData (model outputs)
#   - model_data/df_res.all.RDS (if referenced downstream)
#   - functions.R
#
# Outputs:
#   - Summary tables written to `docs/` via `createAppendixTable()` and related
#     exports.
#   - Derived objects for figures/tables used in subsequent scripts.
#
# Usage:
#   source("code_11.3_model_result.R")
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
library(echarts4r)
library(grid)
font_add_google(name = "Amatic SC", family = "amatic-sc")
showtext_auto()

source('functions.R')

load('rda/code_11.2_model.RData')


df_res.main %>%
  add_by(WHORegion) %>%
  sum_by(WHORegion) %>%
  dplyr::select(WHORegion,matches('N_.*(pre|ctr|2019)_q500')) %>%
  adorn_totals() %>%
  mutate(dec_hos12=round((N_hos_ctr_q500/N_hos_2019_q500-1)*100,2),
         dec_dea12=round((N_dea_ctr_q500/N_dea_2019_q500-1)*100,2),
         dec_hos23=round((1-N_hos_pre_q500/N_hos_ctr_q500)*100,2),
         dec_dea23=round((1-N_dea_pre_q500/N_dea_ctr_q500)*100,2))



res_all_2019<-bind_rows(
  df_res.main %>% add_by(WHORegion),
  df_res.main %>% mutate(WHORegion='Global') %>% add_by(WHORegion)
) %>%
  sum_by(WHORegion) %>%
  arrange(WHORegion) %>%
  dplyr::select(WHORegion,matches('N_.*(2019)_q500')) %>%
  transmute(Group=WHORegion,N.Hos=N_hos_2019_q500/1000,N.Mor_all=N_dea_2019_q500) %>%
  `[`(c(1:4,6:7,5),)

res_all_2019
# Group     N.Hos N.Mor_all
# 1    Afr  547.2545 34413.862
# 2    Amr  389.0417  2451.213
# 3    Emr  408.1141  9867.641
# 4    Eur  178.1293  1088.691
# 6   Sear  746.4282 14200.354
# 7    Wpr  206.3965  3161.882
# 5 Global 2510.4481 66271.124


bind_rows(
  df_res.main %>% add_by(WHORegion),
  df_res.main %>% mutate(WHORegion='Global') %>% add_by(WHORegion)
) %>% 
  sum_by(WHORegion) %>%
  dplyr::select(WHORegion,matches('N_.*(ctr)_q500')) %>%
  left_join(res_all_2019 %>% dplyr::select(Group,N.Hos,N.Mor_all), # replaced
            by=c('WHORegion'='Group'))  %>% # cases in 2019  #SEAR 56900
  mutate(dec_hos=round(((N_hos_ctr_q500/(N.Hos*1000))-1)*100,2),
         dec_dea=round(((N_dea_ctr_q500/(N.Mor_all))-1)*100,2)) %>%
  mutate(N_hos_ctr_q500=round(N_hos_ctr_q500/1000)*1000,
         N_dea_ctr_q500=round(N_dea_ctr_q500/100)*100,
         N.Mor_all=round(N.Mor_all/100)*100)


## Appexidx Table
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

# by Income
sum_by_income<-df_res.main %>% add_by(Income2019) %>% sum_by(Income2019) %>%
  rename(pop_infant_2019=pop_infant_2019_q500,
         pop_infant_2026=pop_infant_2026_q500) %>%
  dplyr::select(-matches('pop_.*_q(025|975)'))

## Population change 2019-2026----
sum_by_region_with_global %>%
  dplyr::select(WHORegion,matches('pop_infant')) %>%
  mutate(pop_add=round(pop_infant_2026/pop_infant_2019-1,3))

# WHORegion pop_infant_2019 pop_infant_2026 pop_add
# 1       Amr                14648                13130  -0.104
# 2       Emr                17381                19440   0.118
# 3       Afr                35412                39952   0.128
# 4       Eur                10913                 9419  -0.137
# 5       Wpr                23413                14476  -0.382
# 6      Sear                34293                32969  -0.039
# 7    Global               136060               129386  -0.049

## ➡️  Appendix table ----
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
  createAppendixTable('docs/Model_Result_Main.xlsx')

# Paper Result section-Descrease  ----
per_to_str<-function(per,trim=T){
  sign<-if_else(per==0,' ',if_else(per>0,'+','-'))
  num_str <- sprintf("%s%.1f", sign, round(abs(per * 100),1))
  padded<-sprintf("%6s%%", num_str)
  if(trim==T) trimws(padded)
  else padded
}
per_to_str(c(-0.022,0.1596))

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

gen_change_percent(sum_by_region_with_global)

# Global HOS -2.1% -5.0% (based on q50)
# Global DEA +7.3% -1.2% (based on q50)

get_global_change_percent<-function(df){
  df %>% filter(Region=='GLOBAL') %>% dplyr::select(dec_hos,dec_hos2,dec_dea,dec_dea2) %>%
    unlist()
}

get_global_change_percent(gen_change_percent(sum_by_region_with_global))

## Figure 5----
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

df_plot5

# max hos and death
total_hos<-df_plot5 %>%
  filter(str_detect(N,'hos')) %>%
  summarise(value=sum(value),.by=c(N,year)) %>%
  arrange(year,desc(N)) %>% pull(value)


total_dea<-df_plot5 %>%
  filter(str_detect(N,'dea')) %>%
  summarise(value=sum(value),.by=c(N,year)) %>%
  arrange(year,desc(N)) %>% pull(value) 


# change percent
dec_hos<-get_global_change_percent(gen_change_percent(sum_by_region_with_global))[1:2]
dec_dea<-get_global_change_percent(gen_change_percent(sum_by_region_with_global))[3:4]
### p51 ----

barwidth<-0.3 

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
         x1=2+barwidth/2,x2=3-barwidth/2,x3=3-barwidth/2,x4=2+barwidth/2)

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
         x1=1+barwidth/2,x2=2-barwidth/2,x3=2-barwidth/2,x4=1+barwidth/2)


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
  geom_col(aes(group,value,fill=WHORegion,linetype=I(linetype)),width=barwidth,color='gray2',linewidth=.2,alpha=1)+
  scale_fill_manual(name=NULL,
                    values=c(AFR="#0392cf", AMR="#ffad60", EMR="#E6B800", EUR="#7bc043", SEAR="#d81159",WPR="#6a4c93")
  )+
  scale_y_continuous(expand = expansion(mult=c(0,.03),add=c(0,NULL)),labels = scales::label_comma())+
  scale_x_discrete(labels = c('','',''))+
  theme_bw()+
  theme(#legend.position = 'none',
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25),
        axis.title.x = element_blank(),
        legend.key.size = unit(0.75,'cm'),
        legend.text = element_text(size=14))+
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
           linewidth=.2,linetype='dashed')+
  annotate(geom='segment',x=2+barwidth*0.5,xend=3+barwidth*0.75,y=total_hos[2],yend=total_hos[2],
           linewidth=.2,linetype='dashed')+
  annotate(geom='segment',x=3+barwidth*0.5,xend=3+barwidth*0.75,y=total_hos[3],yend=total_hos[3],
           linewidth=.2,linetype='dashed')+
  annotate(geom='segment',x=3+barwidth*0.70,xend=3+barwidth*0.70,y=total_hos[1],yend=total_hos[2],
          linewidth=.2,arrow = arrow(length = unit(0.15, "cm")))+
  annotate(geom='segment',x=3+barwidth*0.70,xend=3+barwidth*0.70,y=total_hos[2],yend=total_hos[3],
           linewidth=.2,arrow = arrow(length = unit(0.15, "cm")))+
  annotate(geom='label',x=3+barwidth*0.75,y=mean(total_hos[1:2]),label=dec_hos[1],hjust=0,vjust=.5,size=4,fontface='plain',color='red')+
  annotate(geom='label',x=3+barwidth*0.75,y=mean(total_hos[1:2]),label=dec_hos[1],hjust=0,vjust=.5,size=4,fontface='plain',color='black',label.size=NA)+
  annotate(geom='label',x=3+barwidth*0.75,y=mean(total_hos[2:3]),label=dec_hos[2],hjust=0,vjust=.5,size=4,fontface='plain')

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
         x1=2+barwidth/2,x2=3-barwidth/2,x3=3-barwidth/2,x4=2+barwidth/2)


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
         x1=1+barwidth/2,x2=2-barwidth/2,x3=2-barwidth/2,x4=1+barwidth/2)


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
  scale_x_discrete(labels = c('','',''))+
  theme_bw()+
  geom_polygon(data=df_polygon_23,aes(x,y,fill = str_to_upper(WHORegion)),alpha=.2,
               color = "black", size = .1)+
  geom_label(data=df_percent_23,aes(x=2.5,y=y,label=dec_dea_str),
            size=4,fontface='plain')+
  geom_polygon(data=df_polygon_12,aes(x,y,fill = str_to_upper(WHORegion)),alpha=.2,
               color = "black", size = .1)+
  geom_label(data=df_percent_12,aes(x=1.5,y=y,label=dec_dea_str),
            size=4,fontface='plain')+
  labs(y=NULL)+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25),
        axis.title.x = element_blank())+
  annotate(geom='segment',x=1-barwidth*0.75,xend=2-barwidth*0.5,y=total_dea[2],yend=total_dea[2],
           linewidth=.2,linetype='dashed')+
  annotate(geom='segment',x=2+barwidth*0.5,xend=3+barwidth*0.75,y=total_dea[2],yend=total_dea[2],
           linewidth=.2,linetype='dashed')+
  annotate(geom='segment',x=1-barwidth*0.75,xend=1-barwidth*0.5,y=total_dea[1],yend=total_dea[1],
           linewidth=.2,linetype='dashed')+
  annotate(geom='segment',x=3+barwidth*0.5,xend=3+barwidth*0.75,y=total_dea[3],yend=total_dea[3],
           linewidth=.2,linetype='dashed')+
  annotate(geom='segment',x=1-barwidth*0.7,xend=1-barwidth*0.7,y=total_dea[1],yend=total_dea[2],
           linewidth=.2,arrow = arrow(length = unit(0.15, "cm")))+
  annotate(geom='segment',x=3+barwidth*0.7,xend=3+barwidth*0.7,y=total_dea[2],yend=total_dea[3],
           linewidth=.2,arrow = arrow(length = unit(0.15, "cm")))+
  annotate(geom='label',x=1-barwidth*0.75,y=mean(total_dea[1:2]),label=dec_dea[1],hjust=1,vjust=.5,size=4,fontface='plain')+
  annotate(geom='label',x=3+barwidth*0.75,y=mean(total_dea[2:3]),label=dec_dea[2],hjust=0,vjust=.5,size=4,fontface='plain')

p52


p5<-p51 + p52 + plot_layout(guides = "collect") +
  plot_annotation()

p5
#ggsave(plot=p5,'pdf/Figure5.pdf',width = 13,height = 8)



# Before and after hos_vs_mor -----
load('rda/code_02_hospitalisation.RData')
load('rda/code_05_overall_mortality.RData')


## by region ----
df_wide<-left_join(
  left_join(
    hos_rate_res_WHO %>%
      filter(AGEGR=='0-<12m') %>% transmute(WHORegion=Group,HR_2019=IR.est),
    com_mor_res_WHO_with_Global %>% transmute(WHORegion,MT_2019=m0012_rate.P500)
  ),
  sum_by_region %>%
    transmute(WHORegion,HR_2026=hr_pre_q500,MT_2026=mt_pre_q500)
) %>%
  filter(WHORegion!='Global')

df_long<-df_wide %>%
  pivot_longer(cols=c(2:5),names_sep='_',names_to = c('.value','year')) %>%
  mutate(year=factor(year,levels=c(2019,2026),labels=c('Pre','Post'))) %>%
  left_join(df_wide) %>%
  mutate(WHORegion=toupper(WHORegion))

df_long %>%
  ggplot(aes(HR,MT,color=WHORegion))+
  geom_segment(aes(x=HR_2019,xend=HR_2026,y=MT_2019,yend=MT_2026),linewidth = .2,
               arrow = arrow(length = unit(.1,'inch')))+
  geom_point(aes(),size=2)+
  scale_color_manual(name=NULL,
                     values=c(AFR="#0392cf", AMR="#ffad60", EMR="#E6B800", EUR="#7bc043", SEAR="#d81159",WPR="#6a4c93")
  )+
  scale_x_continuous(limits = c(0,30),expand = expansion(add=c(0,NA)))+
  theme_bw()

### Population change ----
df_pop_change<-left_join(df_hr %>% 
                           separate(region_income,into=c('region','income'),sep='_') %>%
                           transmute(region,income,HR=exp(est)*1000) %>%
                           pivot_wider(names_from = income,values_from = HR,names_glue = "{income}_HR"),
                         df_pop %>%
                           transmute(region_income,popadd=pop_infant_2026-pop_infant_2019,pop=pop_infant_2019) %>%
                           dplyr::summarise(popadd=sum(popadd,na.rm = T),pop=sum(pop,na.rm = T),.by=region_income) %>%
                           separate(region_income,into=c('region','income'),sep='_') %>%
                           pivot_wider(names_from = income,values_from = c(pop,popadd)))

df_pop %>% count(is.na(pop_infant_2019),is.na(pop_infant_2026))

df_pop %>% filter(is.na(pop_infant_2019)) # added 11 countriy population

e_charts(df_pop_change,region) %>%
  e_bar(popadd_L,name='L') %>%
  e_bar(popadd_LM,name='LM') %>%
  e_bar(popadd_UM,name='UM') %>%
  e_bar(popadd_H,name='H')

e_charts(df_pop_change,region) %>%
  e_bar(H_HR,name='H') %>%
  e_bar(L_HR,name='LM') %>%
  e_bar(LM_HR,name='LM') %>%
  e_bar(UM_HR,name='UM')

df_pop_long<-df_pop_change %>%
  set_names(~ifelse(str_detect(.x,'_HR'),paste0('HR_',sub('_HR',"",.x)),.x)) %>%
  pivot_longer(cols =-region,names_sep = '_',names_to = c('var','income'))

df_long_afr<-filter(df_pop_long,region=='Sear')

ggplot(filter(df_long_afr,var!='HR'))+
  geom_col(aes(income,value,fill=var),position = position_dodge())+
  facet_wrap(vars(region),nrow=3)+
  geom_point(data = filter(df_long_afr, var == 'HR'),
             aes(income, value * max(df_long_afr$value,na.rm=T) / 25, group = 1), 
             size = 3,shape=21) +  # 缩放HR以匹配左侧坐标
  scale_y_continuous(
    name = "Infant Population/Infant Population Change (millions)",  # 左侧坐标轴
    sec.axis = sec_axis(~ . * 25 / max(df_long_afr$value,na.rm=T), 
                        name = "Hospital admission rate(cases per 1000)")  # 右侧坐标轴
  )+
  scale_fill_lancet(alpha = .6,name=NULL,labels=c('Popultaion(2019)','Popultaion Change(2026-2019)'))+
  theme_bw()+
  theme(legend.position = 'top')

regions<-c("Afr", "Amr", "Emr", "Eur", "Sear", "Wpr")
plot_list<-regions %>%
  map(~{
    df_long_afr<-filter(df_pop_long,region==.x) %>%
      mutate(income=factor(income,levels=c('L','LM','UM','H')))
    ggplot(filter(df_long_afr,var!='HR'))+
      geom_col(aes(income,value,fill=var),position = position_dodge())+
      facet_wrap(vars(region),nrow=3)+
      geom_point(data = filter(df_long_afr, var == 'HR'),
                 aes(income, value * max(df_long_afr$value,na.rm=T) / 25, group = 1), 
                 size = 1.5,shape=21) +  # 缩放HR以匹配左侧坐标
      scale_y_continuous(
        name = "Population/Population Change (thousands)",  # 左侧坐标轴
        sec.axis = sec_axis(~ . * 25 / max(df_long_afr$value,na.rm=T), 
                            name = "Hospital admission rate(cases per 1000)")  # 右侧坐标轴
      )+
      scale_fill_lancet(alpha = .6,name=NULL,labels=c('Infant popultaion in 2019','Infant popultaion change(2026 vs 2019)'))+
      theme_bw()+
      theme(legend.position = 'top')
  })

wrap_plots(plot_list) + plot_layout(axes = 'collect',axis_titles = 'collect',
                                    guides = 'collect') &
  theme(legend.position = 'top',
        text=element_text(size=20))

ggsave('Figures/HR_pop_2019_2026.tiff',width = 9,height = 4.5,dpi = 200)


## by region-income ----
df_res_by_region_income<-add_by(df_res.main,region_income) %>%
  filter(region_income!='Wpr_L')

sum_by_region_income<-sum_by(df_res_by_region_income,region_income)

df_wide2<-left_join(
  hos_rate_res_WHO.income %>%
    filter(AGEGR=='0-<12m') %>% transmute(WHORegion=WHO,Income,HR_2019=IR.est),
  com_mor_res_WHO.income %>% transmute(WHORegion,Income=Income2019,MT_2019=m0012_rate.P500)
) %>%
  left_join(sum_by_region_income %>%
              separate(region_income,sep='_',into=c('WHORegion','Income')) %>%
              transmute(WHORegion,Income,HR_2026=hr_pre_q500,MT_2026=mt_pre_q500))

df_long2<-df_wide2 %>%
  pivot_longer(cols=c(3:6),names_sep='_',names_to = c('.value','year')) %>%
  mutate(year=factor(year,levels=c(2019,2026),labels=c('Pre','Post'))) %>%
  left_join(df_wide2) %>%
  mutate(WHORegion=toupper(WHORegion))

df_long2 %>%
  ggplot(aes(HR,MT,color=Income))+
  geom_segment(aes(x=HR_2019,xend=HR_2026,y=MT_2019,yend=MT_2026),linewidth = .2,
               arrow = arrow(length = unit(.1,'inch')))+
  geom_point(aes(),size=2)+
  scale_color_lancet()+
  facet_wrap(vars(WHORegion),scales = 'fixed')+
  theme_bw()


# 2026 without prophylaxis and with prophylaxis
df_res.main %>% 
  filter(ISOCountry=='CHL') %>%
  sum_by(ISOCountry) %>%
  dplyr::select(ISOCountry,matches('N_.*(pre|ctr|2019)_q500')) %>%
  adorn_totals() %>%
  mutate(dec_hos12=round((N_hos_ctr_q500/N_hos_2019_q500-1)*100,2),
         dec_dea12=round((N_dea_ctr_q500/N_dea_2019_q500-1)*100,2),
         dec_hos23=round((1-N_hos_pre_q500/N_hos_ctr_q500)*100,2),
         dec_dea23=round((1-N_dea_pre_q500/N_dea_ctr_q500)*100,2)) %>% view()


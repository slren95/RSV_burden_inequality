#
# ------------------------------------------------------------------------------
# Script: code_11.2_model.R
# Purpose:
#   Run the project’s static deterministic / Monte Carlo model to project RSV-
#   associated hospital admissions and deaths under:
#   - a 2026 no-implementation scenario, and
#   - a June-2025 product use (status quo implementation) scenario.
#
# Inputs:
#   - model_data/*: model inputs prepared by earlier scripts (population, product
#     type/status, burden parameters, etc.; see `import()` calls below).
#   - rda/df_pop_center_nir.rds: country metadata and grouping variables.
#   - functions.R
#
# Outputs:
#   - Model result objects saved under `rda/` / `model_data/` (see `save.image()`,
#     `export()`, and `write.*()` calls in the script body).
#
# Usage:
#   source("code_11.2_model.R")
#
# Notes:
#   Run from the project root; the script assumes a specific input file layout.
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


## Population unit 1000 ----
# Countries Categary 
df_pop<-left_join(
  import('rda/df_pop_center_nir.rds') %>% 
    transmute(ISOCountry,WHORegion,region_ns_zone,Country.Name,WHOName),
  import('model_data/df_pop_2019_2026.rds')) %>%
  mutate(region_income=paste(WHORegion,Income2019,sep='_'),.after=WHORegion)
# Table S0 Country List ----
df_pop %>%
  transmute(Country=WHOName,`WHO Region`=case_when(
    WHORegion=='Afr'~'African Region',
    WHORegion=='Amr'~'Region of the Americas',
    WHORegion=='Emr'~'Eastern Mediterranean Region',
    WHORegion=='Eur'~'European Region',
    WHORegion=='Wpr'~'Western Pacific Region',
    WHORegion=='Sear'~'South-East Asian Region '
  ),`Income level`=case_when(
    Income2019=='H'~'High',
    Income2019=='UM'~'Upper-middle',
    Income2019=='LM'~'Lower-middle',
    Income2019=='L'~'Low'
  ),Country=case_when(Country=='pop 2015'~'Botswana',T~Country)) %>%
  arrange(Country) %>% 
  createAppendixTable('docs/List_of_countries.xlsx')



# infant population change
df_pop %>%
  dplyr::summarise(across(starts_with('pop_infant'),~sum(.x,na.rm=T)),.by=WHORegion) %>%
  adorn_totals() %>%
  mutate(inc_percent=100*(pop_infant_2026/pop_infant_2019-1))

# WHORegion pop_infant_2019 pop_infant_2026 inc_percent
# Emr           17381           19440   11.846269
# Afr           35412           39952   12.820513
# Eur           10913            9419  -13.690094
# Amr           14648           13130  -10.363190
# Wpr           23413           14476  -38.171102
# Sear           34293           32969   -3.860846
# Total          136060          129386   -4.905189

## Nirsevimab or Abrysvo promotion type ----
df_type<-import('model_data/df_type.rds')

df_type %>% count(nir) # 38 Lic 16 NIP
df_type %>% count(abr) # 52 Lic 12 NIP

# RSVPreF Strategy ----
df_type %>%
  filter(str_detect(abr,'NIP|Lic'))
# AUT BEL USA ARG URY # seasonal
# other year-round

# Real-world Uptake Nir PreF  ----

# ESP 90.1
# USA 51.2 30.5
# FRA 76.5
# LUX 83.8
# ITA 68.7? 部分地区


# CHL 98.3 90.0 # https://nirse.isci.cl/strategy/

df_type %>% filter(ISOCountry %in% c('ESP','USA','FRA','LUX','ITA','CHL','IRL','GBR','ARG'))
# ISOCountry          nir          abr        Country ChineseName   EU nir_abr
# 1        FRA NIP-included NIP-included         France        法国    y NIP-NIP
# 2        IRL NIP-included     Licensed        Ireland      爱尔兰    y NIP-Lic
# 3        ITA NIP-included     Licensed          Italy      意大利    y NIP-Lic
# 4        LUX NIP-included NIP-included     Luxembourg      卢森堡    y NIP-NIP
# 5        ESP NIP-included     Licensed          Spain      西班牙    y NIP-Lic
# 6        ARG     Licensed NIP-included      Argentina      阿根廷 <NA> Lic-NIP
# 7        CHL NIP-included     Licensed          Chile        智利 <NA> NIP-Lic
# 8        GBR     Licensed NIP-included United Kingdom        英国 <NA> Lic-NIP
# 9        USA NIP-included NIP-included  United States        美国 <NA> NIP-NIP

## Hospitalization Rate and Mortality----
# code_07_plot_for_incidence
hr_all<-import('rda/hr_all.rds')

df_hr<-bind_rows(
  filter(hr_all,str_detect(Group2,'_')),
  data.frame(Group2=setdiff(df_pop$region_income,hr_all$Group2)) %>%
    mutate(Income=gsub('.*_(.*)$','\\1',Group2),
           replace_by=Income) %>%
    left_join(hr_all %>% filter(Group2 %in%c ('H','L','LM','UM')) %>%
                dplyr::select(-Income),by=c('Income'='Group2'))
) %>%
  transmute(AGEGR,region_income=Group2,replace_by,est,se)

df_hr %>%
  mutate(y=exp(est),ymin=exp(est-1.96*se),ymax=exp(est+1.96*se),
         replaced=ifelse(is.na(replace_by),'No',"Yes")) %>%
  separate(region_income,sep = '_',into = c('region','income'),remove = F) %>%
  complete(region,income) %>% 
  mutate(income=factor(income,levels=c('L','LM','UM','H'))) %>%
  ggplot(aes(income,y,colour = income))+
  geom_point(aes(shape=replaced),size=2)+
  geom_linerange(aes(ymin=ymin,ymax=ymax))+
  facet_wrap(vars(region),nrow=2,scales = 'free')

#code_11_previous_mortality_mc_data
df_mor_mc<-import('model_data/mr_mc_all.rds')
summary(df_mor_mc$mor_all)
hist(df_mor_mc$mor_all,breaks = 50,main='')


df_mor_mc2<-df_mor_mc %>%
  dplyr::select(ISOCountry,index,mor_all) %>%
  nest(.by = ISOCountry) %>%
  mutate(mort=map(data,~pull(.x,mor_all)))
# GBD mortality Sensitive analysis
df_mor_mc.GBD<-import('model_data/mr_mc_all2.rds')

df_mor_mc2.GBD<-df_mor_mc.GBD %>%
  dplyr::select(ISOCountry,index,mor_all) %>%
  nest(.by = ISOCountry) %>%
  mutate(mort=map(data,~pull(.x,mor_all)))


## VE vaccine  AE antibody under strategy----

genAE<-function(VE=0.82){
  mat<-expand.grid(
    age_index=1:12,
    bir_index=1:12
  ) %>%
    mutate(adminstrate=case_when(bir_index<7 & age_index==8-bir_index~'1',
                                 bir_index>6 & age_index==1~'1',
                                 T~'0'),
           x=age_index+bir_index-1,
           y=case_when(bir_index<7~7-bir_index,
                       T~19-bir_index)) %>%
    arrange(bir_index,age_index) %>%
    group_by(bir_index) %>%
    dplyr::mutate(ad_before=str_sub(paste(adminstrate,collapse=''),1,row_number()),
           ad_before=str_replace(ad_before,'^0+',''),
           VE=case_when(ad_before==''~0,
                        str_length(ad_before)<=6~VE,
                        str_length(ad_before)>6~0
                        #str_length(ad_before)>6~0.8-0.133*((str_length(ad_before))-6)
           )) %>%
    pivot_wider(id_cols = bir_index,names_from = age_index,names_prefix = 'age_',values_from = VE) %>%
    `[`(c(7:12,1:6),-1) %>% as.matrix()
  image(t(mat[12:1,]))
  mat
}
genAE(0.8)

# Maternal vaccine
genVE<-function(VE=0.82,isSeasonal=TRUE){
  index<-if_else(isSeasonal,6,0)
  mat<-expand.grid(
    age_index=1:12,
    bir_index=1:12
  ) %>%
    mutate(adminstrate=case_when(#bir_index<7 & age_index==8-bir_index~'1',
                                 bir_index>index & age_index==1~'1',
                                 T~'0'),
           x=age_index+bir_index-1,
           y=case_when(bir_index<7~7-bir_index,
                       T~19-bir_index)) %>%
    arrange(bir_index,age_index) %>%
    group_by(bir_index) %>%
    dplyr::mutate(ad_before=str_sub(paste(adminstrate,collapse=''),1,row_number()),
                  ad_before=str_replace(ad_before,'^0+',''),
                  VE=case_when(ad_before==''~0,
                               str_length(ad_before)<=6~VE,
                               str_length(ad_before)>6~0
                               #str_length(ad_before)>6~0.8-0.133*((str_length(ad_before))-6)
                  )) %>%
    pivot_wider(id_cols = bir_index,names_from = age_index,names_prefix = 'age_',values_from = VE) %>%
    `[`(c(7:12,1:6),-1) %>% as.matrix()
  image(t(mat[12:1,]))
  mat
}
genVE(0.8)
genVE(isSeasonal = F)

### Antibody effect plot 
plotAE<-function(against='hos'){
  VE=case_when(against=='hos'~0.83,
               against=="dea"~0.81)
  df_demo<-expand.grid(
    age_index=1:12,
    bir_index=1:12
  ) %>%
    dplyr::mutate(adminstrate=case_when(bir_index<7 & age_index==8-bir_index~'1',
                                 bir_index>6 & age_index==1~'1',
                                 T~'0'),
           x=age_index+bir_index-1,
           y=case_when(bir_index<7~7-bir_index,
                       T~19-bir_index)) %>%
    arrange(bir_index,age_index) %>%
    group_by(bir_index) %>%
    dplyr::mutate(ad_before=str_sub(paste(adminstrate,collapse=''),1,row_number()),
           ad_before=str_replace(ad_before,'^0+',''),
           VE=case_when(ad_before==''~0,
                        str_length(ad_before)<=6~VE,
                        str_length(ad_before)>6~0
                        #str_length(ad_before)>6~0.8-0.133*((str_length(ad_before))-6)
           ))
  
  
  ggplot(df_demo,aes(age_index,y))+
    #geom_tile(aes(fill=VE),colour='gray',linewidth=.3)+
    geom_tile(aes(fill=factor(sign(VE))),colour='gray',linewidth=.3)+
    #geom_text(aes(label=ifelse(VE==0,'',sprintf('%.2f',VE))))+
    geom_text(data=filter(df_demo,adminstrate==1),
              aes(label="\u21A3",x=age_index-.5,y=y+.4),
              hjust=0,vjust=0,size=6,angle=-45,fontface='bold',family = "Arial")+
    geom_text(data=data.frame(),aes(x=1:12,y=12.8,
                                    label=sprintf('%s-<%sm',1:12-1,1:12)),size = 4)+
    # geom_segment(data=data.frame(x=c(.25,.25),
    #                              y=c(6.45,6.55),
    #                              xend=c(.25,.25),
    #                              yend=c(0.5,12.5),
    #                              color=c("#00468BFF","#ED0000FF")),
    #              aes(x=x,y=y,xend=xend,yend=yend,color=I(color)),
    #              alpha=.5,linewidth=5)+
    geom_segment(data=data.frame(x=c(.25),
                                 y=c(6.55),
                                 xend=c(.25),
                                 yend=c(12.5),
                                 color=c("#ED0000FF")),
                 aes(x=x,y=y,xend=xend,yend=yend,color=I(color)),
                 alpha=.5,linewidth=3)+
    theme_minimal()+
    scale_x_continuous(expand = expansion(),breaks=NULL)+
    scale_y_continuous(expand = expansion(add=c(NA,.5)),
                       breaks=c(3,9),labels=c('','RSV season'))+
    theme(axis.text.y = element_text(angle = 90,hjust = 0,face = 'bold',size=12),
          legend.position = 'bottom',legend.text=element_text(size=13))+
    labs(x='',y='',fill=NULL)+
    scale_fill_manual(values = c('1'='#87ceeb','0'=NA),
                      labels = 'Time window of protection',na.value = 'white',breaks = '1')
    # guides(fill = guide_legend(
    #   title.position = "top",
    #   title.hjust = 0.5,
    #   label.position = "bottom",  # label below the key
    #   keywidth = unit(3,"lines"),
    #   keyheight = unit(1,"lines"),
    #   direction = "vertical"
    # ))
    # scale_fill_continuous(low='white',high='skyblue')+
    # guides(fill=guide_colorsteps(theme = theme(
    #   legend.key.width  = unit(1, "lines"),
    #   legend.key.height = unit(10, "lines")
    # )))
}
plotAE('hos')
ggsave('pdf/Eff_Plot_mAb_Hos.pdf',width = 9,height = 6)
plotAE('dea')
ggsave('pdf/Eff_Plot_mAb_Dea.pdf',width = 9,height = 6)

### Vaccine effect plot
plotVE<-function(against='hos',isSeasonal=TRUE){
  index<-if_else(isSeasonal,6,0)
  VE=case_when(against=='hos'~0.71,
               against=="dea"~0.77)
  df_demo<-expand.grid(
    age_index=1:12,
    bir_index=1:12
  ) %>%
    dplyr::mutate(adminstrate=case_when(#bir_index<7 & age_index==8-bir_index~'1',
                                        bir_index>index & age_index==1~'1',
                                        T~'0'),
                  x=age_index+bir_index-1,
                  y=case_when(bir_index<7~7-bir_index,
                              T~19-bir_index)) %>%
    arrange(bir_index,age_index) %>%
    group_by(bir_index) %>%
    dplyr::mutate(ad_before=str_sub(paste(adminstrate,collapse=''),1,row_number()),
                  ad_before=str_replace(ad_before,'^0+',''),
                  VE=case_when(ad_before==''~0,
                               str_length(ad_before)<=6~VE,
                               str_length(ad_before)>6~0
                               #str_length(ad_before)>6~0.8-0.133*((str_length(ad_before))-6)
                  ))
  
  
  ggplot(df_demo,aes(age_index,y))+
    #geom_tile(aes(fill=VE),colour='gray',linewidth=.3)+
    geom_tile(aes(fill=factor(sign(VE))),colour='gray',linewidth=.3)+
    #geom_text(aes(label=ifelse(VE==0,'',sprintf('%.2f',VE))))+
    geom_text(data=filter(df_demo,adminstrate==1,bir_index>6),
              aes(label="",x=age_index-.5,y=y+.4),
              hjust=0,vjust=0,size=6,angle=-45,fontface='bold',family = "Arial")+
    geom_text(data=data.frame(),aes(x=1:12,y=12.8,
                                    label=sprintf('%s-<%sm',1:12-1,1:12)))+
    # geom_segment(data=data.frame(x=c(.25),
    #                              y=if_else(isSeasonal,6.5,0.5),
    #                              xend=c(.25),
    #                              yend=c(12.5),
    #                              color=c("#ED0000FF")),
    #              aes(x=x,y=y,xend=xend,yend=yend,color=I(color)),
    #              alpha=.5,linewidth=5)+
    geom_segment(data=data.frame(x=c(.25),
                                 y=c(6.55),
                                 xend=c(.25),
                                 yend=c(12.5),
                                 color=c("#ED0000FF")),
                 aes(x=x,y=y,xend=xend,yend=yend,color=I(color)),
                 alpha=if_else(isSeasonal,0.5,0),linewidth=3)+
    theme_minimal()+
    scale_x_continuous(expand = expansion(),breaks=NULL)+
    scale_y_continuous(expand = expansion(add=c(NA,.5)),
                       breaks=if_else(isSeasonal,9,6),labels=if_else(isSeasonal,'RSV season','Year-round'))+
    theme(axis.text.y = element_text(angle = 90,hjust = 0,face = 'bold',size=12))+
    labs(x='',y='',fill='Protected')+
    scale_fill_manual(values = c('1'='#87ceeb','0'=NA),
                      labels = NULL,na.value = 'white',breaks = '1')+
    guides(fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",  # label 在 key 下方
      keywidth = unit(2.5,"lines"),
      keyheight = unit(1,"lines"),
      direction = "vertical"
    ))
    # labs(x='',y='',fill='Effectiveness')+
    # scale_fill_continuous(low='white',high='skyblue')+
    # guides(fill=guide_colorbar(theme = theme(
    #   legend.key.width  = unit(1, "lines"),
    #   legend.key.height = unit(10, "lines")
    # )))
}

# plotVE('hos')
# ggsave('pdf/Eff_Plot_Vaccine_Hos.pdf',width = 9,height = 6)
# plotVE('dea')
# ggsave('pdf/Eff_Plot_Vaccine_Dea.pdf',width = 9,height = 6)
# plotVE('hos',isSeasonal = FALSE)
# ggsave('pdf/Eff_Plot_Vaccine_Hos_year_round.pdf',width = 9,height = 6)
# plotVE('dea',isSeasonal = FALSE)
# ggsave('pdf/Eff_Plot_Vaccine_Dea_year_round.pdf',width = 9,height = 6)

##Coverage ----
genCov<-function(cov=0.8){
  matrix(rep(cov,12*12),nrow = 12)
}
genCov()

plotVE2<-function(against='hos',isSeasonal=TRUE){
  index<-if_else(isSeasonal,6,0)
  VE=case_when(against=='hos'~0.71,
               against=="dea"~0.77)
  df_demo<-expand.grid(
    age_index=-1:12,
    bir_index=1:12
  ) %>%
    dplyr::mutate(adminstrate=case_when(
      bir_index>index & age_index==-1~'1',
      T~'0'),
      x=age_index+bir_index-1,
      y=case_when(bir_index<7~7-bir_index,
                  T~19-bir_index)) %>%
    arrange(bir_index,age_index) %>%
    group_by(bir_index) %>%
    dplyr::mutate(ad_before=str_sub(paste(adminstrate,collapse=''),1,row_number()),
                  ad_before=str_replace(ad_before,'^0+',''),
                  VE=case_when(ad_before==''~0,
                               str_length(ad_before)<=2~0,
                               str_length(ad_before)-2<=6~VE,
                               str_length(ad_before)-2>6~0
                               #str_length(ad_before)>6~0.8-0.133*((str_length(ad_before))-6)
                  )) %>%
    mutate(fill=case_when(
      age_index<1~2,
      T~sign(VE)
    ))
  
  
  ggplot(df_demo,aes(age_index,y))+
    #geom_tile(aes(fill=VE),colour='gray',linewidth=.3)+
    geom_tile(aes(fill=factor(fill)),colour='gray',linewidth=.3)+
    #geom_text(aes(label=ifelse(VE==0,'',sprintf('%.2f',VE))))+
    geom_text(data=filter(df_demo,adminstrate==1,bir_index>6),
              aes(label="",x=age_index-.5,y=y+.4),
              hjust=0,vjust=0,size=6,angle=-45,fontface='bold',family = "Arial")+
    geom_text(data=data.frame(),size=3.5,lineheight = 0.8,aes(x=-1:12,y=12.8,
                                    label=c('32-<36w\nGA','36-<40w\nGA',sprintf('%s-<%sm',1:12-1,1:12))))+
    geom_text(data=filter(df_demo,adminstrate==1),
              aes(label="\u21A3",x=age_index-.5,y=y+.4),
              hjust=0,vjust=0,size=6,angle=-45,fontface='bold',family = "Arial")+
    # geom_segment(data=data.frame(x=c(.25),
    #                              y=if_else(isSeasonal,6.5,0.5),
    #                              xend=c(.25),
    #                              yend=c(12.5),
    #                              color=c("#ED0000FF")),
    #              aes(x=x,y=y,xend=xend,yend=yend,color=I(color)),
    #              alpha=.5,linewidth=5)+
    geom_segment(data=data.frame(x=c(.25-2),
                                 y=c(6.55),
                                 xend=c(.25-2),
                                 yend=c(12.5),
                                 color=c("#ED0000FF")),
                 aes(x=x,y=y,xend=xend,yend=yend,color=I(color)),
                 alpha=if_else(isSeasonal,0.5,0),linewidth=3)+
    theme_minimal()+
    scale_x_continuous(expand = expansion(),breaks=NULL)+
    scale_y_continuous(expand = expansion(add=c(NA,.5)),
                       breaks=if_else(isSeasonal,9,6),labels=if_else(isSeasonal,'RSV season*','Year-round'))+
    theme(axis.text.y = element_text(angle = 90,hjust = 0,face = 'bold',size=12),
          legend.position = 'bottom',legend.text=element_text(size=13))+
    labs(x='',y='',fill=NULL)+
    scale_fill_manual(values = c('1'='#87ceeb','0'=NA,'2'='#f4f4b9'),
                      labels = c('Time window of protection','Fetal period'),na.value = 'white',breaks = c('1','2'))
    # guides(fill = guide_legend(
    #   title.position = "top",
    #   title.hjust = 0.5,
    #   label.position = "bottom",  # label below the key
    #   keywidth = unit(3,"lines"),
    #   keyheight = unit(1,"lines"),
    #   direction = "vertical"
    # ))
  # labs(x='',y='',fill='Effectiveness')+
  # scale_fill_continuous(low='white',high='skyblue')+
  # guides(fill=guide_colorbar(theme = theme(
  #   legend.key.width  = unit(1, "lines"),
  #   legend.key.height = unit(10, "lines")
  # )))
}

plotVE2('hos')
ggsave('pdf/Eff_Plot_Vaccine_Hos.pdf',width = 9,height = 6)

plotVE2('hos',isSeasonal = FALSE)
ggsave('pdf/Eff_Plot_Vaccine_Hos_year_round.pdf',width = 9,height = 6)


## Proportion by Seasonality ----
df_sample_pro_by_sea<-import('model_data/df_sample_pro_by_sea_all.rds') %>%   # code_10.1_representive_site
  rename(ISOCountry=ISO3)

df_sample_pro_by_sea[2,'observed'][[1]]


## Proportion by Age ----
df_chain_pro_by_age<-import('model_data/df_chain_pro_by_age.rds') %>% #MCMC
  mutate(sample=map(data,~{
    .x %>% filter(index_seq<=1000) %>% #保留1000个
      pivot_wider(id_cols = index_seq,names_from=parameter,values_from = value) %>%
      column_to_rownames(var = 'index_seq') %>%
      as.matrix()
  }))


# Prefer results by region-income; if not converged, use results by income
df_chain_pro_by_age2<-bind_rows(
  filter(df_chain_pro_by_age,str_detect(region_income,'_'),converge==T),
  data.frame(region_income=setdiff(df_pop$region_income,df_chain_pro_by_age$region_income[df_chain_pro_by_age$converge==T])) %>%
    mutate(income=gsub('.*_(.*)$','\\1',region_income)) %>%
    left_join(filter(df_chain_pro_by_age,!str_detect(region_income,'_'),converge==T) %>%
                dplyr::select(-region_income,-converge),by='income') %>%
    mutate(replace_by=income)
)

df_chain_pro_by_age2[1,'sample'][[1]] %>% class()

# Model Function ----

# Effectiveness and Coverage
df_scenarios<-tribble(~type,~Eff,~Uptake_case,
                 'main',c(0.83,0.81,0.71,0.77),'main',
                 'sens',c(0.83,0.81,0.71,0.77),'lower',
                 'sens',c(0.83,0.81,0.71,0.77),'higher',
                 'sens',c(0.88,0.86,0.76,0.82),'main',
                 'sens',c(0.78,0.76,0.66,0.72),'main'
                 )

## Uptake assumption ----
df_type2<-df_type %>% left_join(dplyr::select(df_pop,ISOCountry,Income2019,WHORegion))

# Table 68 Country List ----
df_type2 %>%
  transmute(Country,nirsevimab=recode(nir,'NIP-included'='NIP','Licensed'='licensed',.missing='unlicensed'),
            RSVpreF=recode(abr,'NIP-included'='NIP','Licensed'='licensed',.missing='unlicensed'),
            `WHO region`=toupper(WHORegion),`Income level`=paste0(Income2019,'IC')) %>%
  mutate(nirsevimab=factor(nirsevimab,levels=c('NIP','licensed','unlicensed')),
         RSVpreF=factor(RSVpreF,levels=c('NIP','licensed','unlicensed'))) %>%
  arrange(nirsevimab,RSVpreF,`WHO region`) %>%
  export('docs/Tables_df_type2.xlsx')
  

df_type2 %>% filter(abr=='NIP-included',nir=='Licensed',Income2019=='H')

gw32_pro<-c('POL'=0.010,'SVN'=0.009,'GBR'=0.012,'URY'=0.017)

Tdap_uptake<-c('BEL'=0.64,'FRA'=0.62,'AUS'=0.79,'CHE'=0.86,'URY'=0.63,'CRI'=0.95,
               'LUX'=NA,'POL'=NA,'SVN'=NA)

df_real_uptake<-import('model_data/Real-world_uptake.csv')


get_uptake<-function(ISO,nir_abr,Income,Uptake_case){
  fmin<-function(a,b){ min(a,b,na.rm = T) }
  fmax<-function(a,b){ max(a,b,na.rm = T) }
  f_preF<-function(Uptake_case){
    if(Uptake_case %in% c('main','lower')) {
      return(fmin)
    } else if (Uptake_case =='higher') {
      return(fmax)
    }
  }
  
  mAb_base<-c('main'=0.8,'lower'=0.6,'higher'=0.9)
  coef<-c('main'=0.6,'lower'=0.5,'higher'=0.7)
  lic_H_nir <-c('main'=0.01,'lower'=0.005,'higher'=0.05)
  lic_H_abr <-c('main'=0.05,'lower'=0.010,'higher'=0.10)
  lic_UM_nir<-c('main'=0.005,'lower'=0.0005,'higher'=0.01)
  lic_UM_abr<-c('main'=0.010,'lower'=0.0050,'higher'=0.05)
  lic_L_nir<-lic_LM_nir<-c('main'=0.0005,'lower'=0.0005,'higher'=0.005)
  lic_L_abr<-lic_LM_abr<-c('main'=0.0050,'lower'=0.0005,'higher'=0.010)
  
  if (is.na(nir_abr)) {
    cov_mat_nir <- genCov(0)
    cov_mat_abr <- genCov(0)
  } else if (ISO %in% df_real_uptake$ISO) {
    if(Uptake_case=='main') {
      cov_mat_nir <- genCov(df_real_uptake[df_real_uptake$ISO==ISO,'main_nir'])
      cov_mat_abr <- genCov(df_real_uptake[df_real_uptake$ISO==ISO,'main_abr'])
    } else if(Uptake_case=='lower') {
      cov_mat_nir <- genCov(df_real_uptake[df_real_uptake$ISO==ISO,'lower_nir'])
      cov_mat_abr <- genCov(df_real_uptake[df_real_uptake$ISO==ISO,'lower_abr'])
    } else if(Uptake_case=='higher') {
      cov_mat_nir <- genCov(df_real_uptake[df_real_uptake$ISO==ISO,'higher_nir'])
      cov_mat_abr <- genCov(df_real_uptake[df_real_uptake$ISO==ISO,'higher_abr'])
    }
  } else if(ISO=='CHL') {
    cov_mat_nir<-matrix(c(rep(0.98,12*6),rep(0.90,12*6)),nrow=12,ncol=12,byrow = T)
    cov_mat_abr<-genCov(0.0005)
  } else if (nir_abr=='NIP-NIP') {
    if(ISO == 'AUS') {
      mV_c<-f_preF(Uptake_case)(coef[Uptake_case],Tdap_uptake[ISO])
      cov_mat_abr <- genCov(mV_c)
      cov_mat_nir <- genCov((1-mV_c)*mAb_base[Uptake_case])
    } else {
      mAb_c<-mAb_base[Uptake_case]
      mV_c<-f_preF(Uptake_case)((1-mAb_c)*coef[Uptake_case],Tdap_uptake[ISO])
      if(Uptake_case=='higher') {mV_c<-min(mV_c,1-mAb_c)}
      cov_mat_nir <- genCov(mAb_c)
      cov_mat_abr <- genCov(mV_c)
    }
  } else if(nir_abr =='NIP-Lic') {
    cov_mat_nir <- genCov(mAb_base[Uptake_case])
    cov_mat_abr <- genCov(0.0005)
  } else if(nir_abr =='NIP-NA') {
    cov_mat_nir <- genCov(mAb_base[Uptake_case])
    cov_mat_abr <- genCov(0)
  } else if(nir_abr =='Lic-NIP') {
    cov_mat_abr <- genCov(f_preF(Uptake_case)(coef[Uptake_case],Tdap_uptake[ISO]))
    if(Income=='H') {
      per<-recode(Uptake_case,'main'=0.9,'lower'=0.6,'higher'=1)
      cov_mat_nir<-genCov(gw32_pro[ISO]*per)
    } else if(Income=='UM'){
      cov_mat_nir <- genCov(0.005)
    } else if (Income %in% c('LM','L')) {
      cov_mat_nir <- genCov(0.0005)
    }
  } else if(nir_abr =='NA-NIP') {
    cov_mat_abr <- genCov(f_preF(Uptake_case)(coef[Uptake_case],Tdap_uptake[ISO]))
    cov_mat_nir <- genCov(0)
  } else if(nir_abr =='Lic-Lic') {
    cov_mat_nir <- genCov(get(paste0('lic_',Income,'_nir'))[Uptake_case])
    cov_mat_abr <- genCov(get(paste0('lic_',Income,'_abr'))[Uptake_case])
  } else if(nir_abr =='Lic-NA') {
    cov_mat_nir <- genCov(get(paste0('lic_',Income,'_nir'))[Uptake_case])
    cov_mat_abr <- genCov(0)
  } else if(nir_abr =='NA-Lic') {
    cov_mat_abr <- genCov(get(paste0('lic_',Income,'_abr'))[Uptake_case])
    cov_mat_nir <- genCov(0)
  }
  
  return(list(cov_mat_nir,cov_mat_abr,ISO,max(cov_mat_nir+cov_mat_abr)))
}

get_uptake('BEL','NIP-NIP','H','higher')[[1]]

df_type2 %>%
  filter(ISOCountry !='LIE') %>%
  transmute(ISO=ISOCountry,nir_abr,Income=Income2019) %>%
  pmap(get_uptake, Uptake_case = 'higher') %>%
  map_lgl(~pluck(.x,4)==1) %>%
  which()

df_type2 %>%
  filter(ISOCountry !='LIE') %>%
  slice(c(2,10,47)) %>%
  transmute(ISO=ISOCountry,nir_abr,Income=Income2019) %>%
  pmap(get_uptake, Uptake_case = 'higher')

df_type2 %>%
  filter(WHORegion =='Afr') %>%
  transmute(ISO=ISOCountry,nir_abr,Income=Income2019) %>%
  pmap(get_uptake, Uptake_case = 'higher')

df_type2 %>%
  filter(Income2019 =='LM') %>%
  transmute(ISO=ISOCountry,nir_abr,Income=Income2019) %>%
  pmap(get_uptake, Uptake_case = 'main')

# Population,HR,Mort,pro by age and season

df_pars_data<-df_pop %>%
  left_join(df_type2 %>% transmute(ISOCountry,nir,abr,nir_abr,Income2019)) %>%
  left_join(df_hr %>% transmute(region_income,hr.est=est,hr.se=se)) %>%
  left_join(df_mor_mc2 %>% transmute(ISOCountry,mort)) %>%
  left_join(df_sample_pro_by_sea %>% transmute(ISOCountry,pro_by_sea=observed)) %>%
  left_join(df_chain_pro_by_age2 %>% transmute(region_income,pro_by_age=sample)) %>%
  mutate(lack_pop=map(pop_infant_2026,~is.null(.x) | is.na(.x)),
         lack_hr=map(hr.est,~is.null(.x)),
         lack_hr=map(hr.est,~is.null(.x)),
         lack_mort=map(mort,~is.null(.x)),
         lack_pro_sea=map(pro_by_sea,~is.null(.x)),
         lack_pro_age=map(pro_by_age,~is.null(.x)))

df_pars_data %>%
  transmute(ISO=ISOCountry,nir_abr,Income=Income2019) %>%
  pmap(get_uptake, Uptake_case = 'higher') %>%
  map_lgl(~pluck(.x,4)>1)

# NIP-NIP countries primary product
df_pars_data %>%
  filter(nir_abr =='NIP-NIP') %>%
  dplyr::select(nir_abr,Country.Name,ISOCountry)

df_pars_data %>% count(nir_abr,lack_pop,lack_hr,lack_mort,lack_pro_sea,lack_pro_age)


df_pars_data %>%
  dplyr::select(ISOCountry,WHORegion,region_income,region_income,Country.Name,pop_infant_2019,pop_infant_2026,nir_abr)

df_onerow<-df_pars_data[10,]
df_one_scenario<-df_scenarios[1,]


## Run one row ----
run_onerow<-function(df_onerow,df_one_scenario){
  #one country
  stopifnot("dataframe should be one row"=nrow(df_onerow)==1)
  stopifnot("dataframe should be one row"=nrow(df_one_scenario)==1)
  
  #ISOCountry
  ISO<-df_onerow$ISOCountry
  Income<-df_onerow$Income2019
  Uptake_case<-df_one_scenario$Uptake_case
  nir_abr<-df_onerow$nir_abr # nirsevimab status
  pop_2019<-df_onerow$pop_infant_2019
  pop_2026<-df_onerow$pop_infant_2026
  hr.est<-df_onerow$hr.est
  hr.se<-df_onerow$hr.se
  mort<-df_onerow$mort
  # Sampling
  hr_vec<-exp(rnorm(n = 1000,mean = hr.est, sd = hr.se))*1000
  mort_vec<-mort[[1]]

  # 2019 hospitalisations and deaths (no-implementation baseline)
  N_hos_2019<-pop_2019*hr_vec
  N_dea_2019<-pop_2019*mort_vec
  # 2026 hospitalisations and deaths (no-implementation scenario)
  N_hos<-pop_2026*hr_vec
  N_dea<-pop_2026*mort_vec

  # Uptake
  cov_mat_nir<-get_uptake(ISO,nir_abr,Income,Uptake_case)[[1]]
  cov_mat_abr<-get_uptake(ISO,nir_abr,Income,Uptake_case)[[2]]
  
  # Effectiveness 
  effs<-df_one_scenario$Eff[[1]] # 2.mab hos 2. mort # 3. vaccine hos 4.mort
  eff_nir_hos<-genAE(effs[1])
  eff_nir_dea<-genAE(effs[2])
  
  ## is RSVPreF Seasonal
  isSeasonal=ISO %in% c('AUT','BEL','USA','ARG','URY')
  
  eff_abr_hos<-genVE(effs[3],isSeasonal)
  eff_abr_dea<-genVE(effs[4],isSeasonal)
  
  getAvt<-function(n,cov,pro,eff){
    n*cov*pro*eff
  }
  
  # Consumed Doses ----
  getDoses<-function(pop_2026,cov_mat,eff_mat){
    is_injected<-sign(eff_mat %>% rowSums())
    sum(pop_2026/12*is_injected*cov_mat[,1])
  }
  
  if(!is.na(nir_abr)){
    observed<-df_onerow$pro_by_sea[[1]]
    pro_ses<-MCMCpack::rdirichlet(1000,alpha=1+observed)# 后验分布
    pro_age<-df_onerow$pro_by_age[[1]] # MCMC
    
    # proportion by season and agegroup
    pro_mats<-map(1:1000,~outer(pro_ses[.x,],pro_age[.x,]))
    
    N_hos_avt<-map_dbl(1:1000,~{
      n_nir_hos<-getAvt(N_hos[.x],cov_mat_nir,pro_mats[[.x]],eff_nir_hos)
      n_abr_hos<-getAvt(N_hos[.x],cov_mat_abr,pro_mats[[.x]],eff_abr_hos)
      sum(n_nir_hos+n_abr_hos)
    })
    N_dea_avt<-map_dbl(1:1000,~{
      n_nir_dea<-getAvt(N_dea[.x],cov_mat_nir,pro_mats[[.x]],eff_nir_dea)
      n_abr_dea<-getAvt(N_dea[.x],cov_mat_abr,pro_mats[[.x]],eff_abr_dea)
      sum(n_nir_dea+n_abr_dea)
    })
    # Doses Consumed ----
    D_nir<-getDoses(pop_2026,cov_mat_nir,eff_nir_hos)
    D_abr<-getDoses(pop_2026,cov_mat_abr,eff_abr_hos)
    
  } else {
    N_hos_avt<-0
    N_dea_avt<-0
    D_nir<-0
    D_abr<-0
  }

  return(data.frame(ISOCountry=df_onerow$ISOCountry,
                    index=1:1000,
                    hr=hr_vec,
                    mort=mort_vec,
                    N_hos_2019=N_hos_2019,
                    N_dea_2019=N_dea_2019,
                    N_hos_ctr=N_hos, # 2026 counterfactual
                    N_dea_ctr=N_dea,
                    N_hos_avt=N_hos_avt,
                    N_dea_avt=N_dea_avt,
                    N_hos_pre=N_hos-N_hos_avt,
                    N_dea_pre=N_dea-N_dea_avt,
                    hr_pre=(N_hos-N_hos_avt)/pop_2026,
                    mt_pre=(N_dea-N_dea_avt)/pop_2026,
                    D_nir=D_nir,
                    D_abr=D_abr) %>%
           left_join(df_onerow %>% dplyr::select(ISOCountry,Income2019,WHORegion,region_income,region_ns_zone,pop_infant_2019,pop_infant_2026,nir,abr,nir_abr)) %>%
           mutate(params=map(ISOCountry,~list(eff=effs,uptake_case=Uptake_case)))
         )
}


## Main analysis----
set.seed(1234)
df_res.main<-map_dfr(split(df_pars_data,df_pars_data$ISOCountry),~run_onerow(.x,df_scenarios[1,]))

## Main analysis and sensitive analysis ----
set.seed(1234)
df_res.all<-1:nrow(df_scenarios) %>%
  map_dfr(~{
    i=.x
    map_dfr(split(df_pars_data,df_pars_data$ISOCountry),~run_onerow(.x,df_scenarios[i,]))
  })

identical(df_res.all %>% head(206000),df_res.main) # TRUE

### ➡️ export df_res.all.rds ----
rio::export(df_res.all,'model_data/df_res.all.RDS')
df_res.all<-rio::import('model_data/df_res.all.RDS')


# Result summary----

add_by<-function(df_res,bywhat){
  df_res %>%
    summarise(across(c(starts_with('pop_infant'),starts_with('N_'),starts_with('D_')),~sum(.x,na.rm=T)),.by=c(index,{{bywhat}})) %>%
    mutate(hr_ctr=N_hos_ctr/pop_infant_2026,
           hr_pre=N_hos_pre/pop_infant_2026,
           mt_ctr=N_dea_ctr/pop_infant_2026,
           mt_pre=N_dea_pre/pop_infant_2026)
}
add_by(df_res.main,WHORegion)


sum_by<-function(df,bywhat){
  df %>%
    dplyr::summarise(across(where(is.numeric) & !index,list(q500=~median(.x),q025=~quantile(.x,probs = 0.025),q975=~quantile(.x,probs = 0.975))),.by={{bywhat}})
}
 
add_by(df_res.main,WHORegion) %>% sum_by(WHORegion)

# Sensitive analysis use GBD mortality data ----

df_pars_data.GBD<-df_pop %>%
  left_join(df_type2 %>% transmute(ISOCountry,nir,abr,nir_abr,Income2019)) %>%
  left_join(df_hr %>% transmute(region_income,hr.est=est,hr.se=se)) %>%
  left_join(df_mor_mc2.GBD %>% transmute(ISOCountry,mort)) %>% # use GBD data 
  left_join(df_sample_pro_by_sea %>% transmute(ISOCountry,pro_by_sea=observed)) %>%
  left_join(df_chain_pro_by_age2 %>% transmute(region_income,pro_by_age=sample)) %>%
  mutate(lack_pop=map(pop_infant_2026,~is.null(.x) | is.na(.x)),
         lack_hr=map(hr.est,~is.null(.x)),
         lack_hr=map(hr.est,~is.null(.x)),
         lack_mort=map(mort,~is.null(.x)),
         lack_pro_sea=map(pro_by_sea,~is.null(.x)),
         lack_pro_age=map(pro_by_age,~is.null(.x)))

set.seed(1234)
df_res.all.GBD<-map_dfr(split(df_pars_data.GBD,df_pars_data.GBD$ISOCountry),~run_onerow(.x,df_scenarios[1,]))

rio::export(df_res.all.GBD,'model_data/df_res.all.GBD.RDS')



# ️➡️ rda/code_11_model.RData ----  
save.image('rda/code_11.2_model.RData')
load('rda/code_11.2_model.RData')


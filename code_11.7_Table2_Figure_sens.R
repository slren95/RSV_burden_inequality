#
# ------------------------------------------------------------------------------
# Script: code_11.7_Table2_Figure_sens.R
# Purpose:
#   Generate Table 2 and supplementary figures for sensitivity analyses, showing
#   absolute/relative reductions in RSV-associated hospitalisations and deaths
#   across scenarios (uptake/effectiveness variants) and strata (region/income).
#
# Inputs:
#   - model_data/df_res.all.RDS (scenario/sensitivity results)
#   - functions.R
#
# Outputs:
#   - `docs/Table2.xlsx` and other appendix tables.
#   - Sensitivity figures saved via `ggsave()` (see script body).
#
# Usage:
#   source("code_11.7_Table2_Figure_sens.R")
# ------------------------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(ggsci)
library(rio)
library(scales)
library(ggforce)
library(patchwork)

source('functions.R')

df_res.all<-rio::import('model_data/df_res.all.RDS') %>%
  mutate(params_str=map(params,~paste0(.x,collapse = '_')))

df_res.all %>% distinct(params_str)

df_res.main<-df_res.all %>% filter(params_str=='c(0.83, 0.81, 0.71, 0.77)_main')

# Functions
add_by2<-function(df_res,bywhat){
  df_res %>%
    summarise(across(c(starts_with('pop_infant'),starts_with('N_'),starts_with('D_')),~sum(.x,na.rm=T)),.by=c(index,{{bywhat}})) %>%
    rename("Group" := {{ bywhat }}) %>%
    mutate(hr_ctr=N_hos_ctr/pop_infant_2026,
           hr_pre=N_hos_pre/pop_infant_2026,
           mt_ctr=N_dea_ctr/pop_infant_2026,
           mt_pre=N_dea_pre/pop_infant_2026,
           dec_no_hos=N_hos_ctr-N_hos_pre,
           dec_no_dea=N_dea_ctr-N_dea_pre,
           dec_pct_hos=dec_no_hos/N_hos_ctr,
           dec_pct_dea=dec_no_dea/N_dea_ctr
           )
}

sum_by<-function(df,bywhat){
  df %>%
    dplyr::summarise(across(where(is.numeric) & !index,list(q500=~median(.x),q025=~quantile(.x,probs = 0.025),q975=~quantile(.x,probs = 0.975))),.by={{bywhat}})
}

add_by_group<-bind_rows(
  df_res.main %>% add_by2(WHORegion) %>% mutate(Group=toupper(Group)),
  df_res.main %>% add_by2(Income2019) %>% mutate(Group=paste0(Group,'IC')),
  df_res.main %>% mutate(WHORegion='Global') %>% add_by2(WHORegion)
)

sum_by_group<-sum_by(add_by_group,Group) %>%
  `[`(c(3,1,2,4,6,5,7,10,9,8,11),)

# Table 2 ----
sum_by_group %>%
  mutate(across(c(starts_with('N_hos'),starts_with('dec_no_hos')),~comma(round(.x/1000)*1000)),
         across(c(starts_with('N_dea')),~comma(round(.x/100)*100)),
         across(c(starts_with('dec_no_dea')),~comma(round(.x/10)*10)),
         across(starts_with('D_'),~comma(round(.x)*1000)),
         across(matches('(hr_)'),~round(.x,1)),
         across(matches('(mt_)'),~round(.x,2)),
         across(starts_with('dec_pct'),~sprintf('%2.1f',.x*100), .names = "{.col}_str")) %>%
  mutate(across(starts_with('dec_pct'),~str_replace_all(.x,'0\\.0','0'))) %>%
  transmute(Group,
            D_nir=D_nir_q500,
            D_abr=D_abr_q500,
            Hos_ctr=sprintf('%s (%s-%s)\n%.1f (%.1f-%.1f)',N_hos_ctr_q500,N_hos_ctr_q025,N_hos_ctr_q975,hr_ctr_q500,hr_ctr_q025,hr_ctr_q975),
            Hos_pre=sprintf('%s (%s-%s)\n%.1f (%.1f-%.1f)',N_hos_pre_q500,N_hos_pre_q025,N_hos_pre_q975,hr_pre_q500,hr_pre_q025,hr_pre_q975),
            Dec_hos=sprintf('%s (%s-%s)\n%s (%s-%s)',dec_no_hos_q500,dec_no_hos_q025,dec_no_hos_q975,dec_pct_hos_q500_str,dec_pct_hos_q025_str,dec_pct_hos_q975_str),
            Dea_ctr=sprintf('%s (%s-%s)\n%.1f (%.1f-%.1f)',N_dea_ctr_q500,N_dea_ctr_q025,N_dea_ctr_q975,mt_ctr_q500,mt_ctr_q025,mt_ctr_q975),
            Dea_pre=sprintf('%s (%s-%s)\n%.1f (%.1f-%.1f)',N_dea_pre_q500,N_dea_pre_q025,N_dea_pre_q975,mt_pre_q500,mt_pre_q025,mt_pre_q975),
            Dec_dea=sprintf('%s (%s-%s)\n%s (%s-%s)',dec_no_dea_q500,dec_no_dea_q025,dec_no_dea_q975,dec_pct_dea_q500_str,dec_pct_dea_q025_str,dec_pct_dea_q975_str)
  ) %>%
  createAppendixTable('docs/Table2.xlsx')

# Figure S13 S14 ( Main + sensitive ) ----
df_longer<-sum_by_group %>%
  dplyr::select(Group,matches('dec_.*[0-9]$')) %>%
  pivot_longer(cols = -1,names_pattern = 'dec_(.*)_(.*)_(.*)',names_to = c('type','metric','ci')) %>%
  mutate(ci=recode(ci,'q500'='median','q025'='lower','q975'='upper')) %>%
  pivot_wider(id_cols = 1:3,names_from = ci,values_from = value,names_prefix = 'dec_')

income_level=c('HIC','UMIC','LMIC','LIC')
region_level=c('AFR','AMR','EMR','EUR','SEAR','WPR')

df_longer_all<-unlist(unique(df_res.all$params_str)) %>% setNames(c('main analysis','lower uptake','higher uptake','higher effectiveness','lower effectiveness')) %>%
  imap_dfr(~{
    df_res<-df_res.all %>% filter(params_str==.x)
    add_by_group<-bind_rows(
      df_res %>% add_by2(WHORegion) %>% mutate(Group=toupper(Group)),
      df_res %>% add_by2(Income2019) %>% mutate(Group=paste0(Group,'IC')),
      df_res %>% mutate(WHORegion='Global') %>% add_by2(WHORegion)
    )
    sum_by_group<-sum_by(add_by_group,Group) %>%
      `[`(c(3,1,2,4,6,5,7,10,9,8,11),)
    df_longer<-sum_by_group %>%
      dplyr::select(Group,matches('dec_.*[0-9]$')) %>%
      pivot_longer(cols = -1,names_pattern = 'dec_(.*)_(.*)_(.*)',names_to = c('type','metric','ci')) %>%
      mutate(ci=recode(ci,'q500'='median','q025'='lower','q975'='upper')) %>%
      pivot_wider(id_cols = 1:3,names_from = ci,values_from = value,names_prefix = 'dec_')
    sens_level=c('higher uptake','higher effectiveness','main analysis','lower effectiveness','lower uptake')
    df_longer %>% mutate(sens=.y) %>%
      mutate(sens=factor(sens,levels=sens_level))
  }) %>%
  mutate(facet=case_when(Group=='Global'~'Global',
                         Group %in% income_level~'By Income level',
                         T~'By WHO region'),
         facet=factor(facet,levels=c('Global','By Income level','By WHO region'))) %>%
  mutate(Group=factor(Group,levels=c(income_level,region_level,'Global')))

shape_map<-c('main analysis'=19,'higher uptake'=15,'lower uptake'=15,
             'higher effectiveness'=21,'lower effectiveness'=21)

df_longer_all %>%
  filter(Group %in% income_level,metric=='hos',type=='no') %>%
  mutate(Group=factor(Group,levels=income_level)) %>%
  ggplot(aes(Group,dec_median,color=sens))+
  geom_point(position = position_dodge(width = .5),aes(shape=sens))+
  geom_errorbar(aes(ymin=dec_lower,ymax=dec_upper),position = position_dodge(width = .5), width = 0.2)+
  scale_colour_lancet(name=NULL,guide = guide_legend())+
  scale_shape_manual(name=NULL,values=shape_map,guide = guide_legend())+
  labs(x='Income level',y='Averted RSV-associated ALRI hospital admissions')+
  scale_y_continuous(labels = comma_format())+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(linewidth = .2),
        panel.grid.major = element_line(linewidth = .1),
        legend.position = c(0.98,0.98),
        legend.justification = c(1,1))

df_longer_all %>%
  filter(Group %in% 'Global',metric=='hos',type=='no') %>%
  ggplot(aes(Group,dec_median,color=sens))+
  geom_point(position = position_dodge(width = .5))+
  geom_errorbar(aes(ymin=dec_lower,ymax=dec_upper),position = position_dodge(width = .5), width = 0.2)+
  scale_colour_lancet(name=NULL)+
  labs(x='Income level',y='Averted RSV-associated ALRI hospital admissions')+
  scale_y_continuous(labels = comma_format())+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(linewidth = .2),
        panel.grid.major = element_line(linewidth = .1),
        legend.position = c(0.98,0.98),
        legend.justification = c(1,1))
  
p1<-df_longer_all %>%
  filter(facet!='By WHO region',metric=='hos',type=='no') %>%
  ggplot(aes(Group,dec_median,color=sens))+
  geom_point(position = position_dodge(width = .5),aes(shape=sens))+
  geom_errorbar(aes(ymin=dec_lower,ymax=dec_upper),position = position_dodge(width = .5), width = 0.2)+
  scale_colour_lancet(name=NULL,guide = guide_legend())+
  scale_shape_manual(name=NULL,values=shape_map,guide = guide_legend())+
  labs(x=NULL,y='Averted RSV-associated ALRI hospital admissions')+
  scale_y_continuous(labels = comma_format())+
  theme_bw()+
  theme(#panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=10),
        axis.line = element_line(linewidth = .2),
        panel.grid.major = element_line(linewidth = .1),
        legend.position = c(0.98,0.98),
        legend.justification = c(1,1))+
  ggforce::facet_row(vars(facet), scales = 'free_x', space = 'free')

p2<-df_longer_all %>%
  filter(facet=='By WHO region',metric=='hos',type=='no') %>%
  ggplot(aes(Group,dec_median,color=sens))+
  geom_point(position = position_dodge(width = .5),aes(shape=sens))+
  geom_errorbar(aes(ymin=dec_lower,ymax=dec_upper),position = position_dodge(width = .5), width = 0.2)+
  scale_colour_lancet(name=NULL,guide = guide_legend())+
  scale_shape_manual(name=NULL,values=shape_map,guide = guide_legend())+
  labs(x=NULL,y='Averted RSV-associated ALRI hospital admissions')+
  scale_y_continuous(labels = comma_format())+
  theme_bw()+
  theme(#panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=10),
    axis.line = element_line(linewidth = .2),
    panel.grid.major = element_line(linewidth = .1),
    legend.position = 'NULL')+
  ggforce::facet_row(vars(facet), scales = 'free_x', space = 'free')

p1/p2+plot_layout(axis_titles = 'collect_y',guides = "collect")

c('hos','dea') %>%
  walk(~{
    p1<-df_longer_all %>%
      filter(facet!='By WHO region',metric==.x,type=='no') %>%
      ggplot(aes(Group,dec_median,color=sens))+
      geom_point(position = position_dodge(width = .5),aes(shape=sens))+
      geom_errorbar(aes(ymin=dec_lower,ymax=dec_upper),position = position_dodge(width = .5), width = 0.2)+
      scale_colour_lancet(name=NULL,guide = guide_legend())+
      scale_shape_manual(name=NULL,values=shape_map,guide = guide_legend())+
      labs(x=NULL,y='Averted RSV-associated ALRI hospital admissions')+
      scale_y_continuous(labels = comma_format())+
      theme_bw()+
      theme(#panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=10),
        axis.line = element_line(linewidth = .2),
        panel.grid.major = element_line(linewidth = .1),
        legend.position = c(0.98,0.98),
        legend.justification = c(1,1))+
      ggforce::facet_row(vars(facet), scales = 'free_x', space = 'free')
    
    p2<-df_longer_all %>%
      filter(facet=='By WHO region',metric==.x,type=='no') %>%
      ggplot(aes(Group,dec_median,color=sens))+
      geom_point(position = position_dodge(width = .5),aes(shape=sens))+
      geom_errorbar(aes(ymin=dec_lower,ymax=dec_upper),position = position_dodge(width = .5), width = 0.2)+
      scale_colour_lancet(name=NULL,guide = guide_legend())+
      scale_shape_manual(name=NULL,values=shape_map,guide = guide_legend())+
      labs(x=NULL,y='Averted RSV-associated ALRI hospital admissions')+
      scale_y_continuous(labels = comma_format())+
      theme_bw()+
      theme(#panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=10),
        axis.line = element_line(linewidth = .2),
        panel.grid.major = element_line(linewidth = .1),
        legend.position = 'NULL')+
      ggforce::facet_row(vars(facet), scales = 'free_x', space = 'free')
    plot<-p1/p2+plot_layout(axis_titles = 'collect_y',guides = "collect")&
      labs(y=if_else(.x=='hos','Averted RSV-associated ALRI hospital admissions','Averted RSV-attributable deaths'))
    #ggsave(plot = plot,filename = sprintf('Figures/Averted_%s.png',.x),width=10,height=6,dpi=300)
  })

plots<-expand.grid(c('hos','dea'),c('no','pct')) %>% 
  pmap(~{
    labels_fun<-if(..2=='no') comma_format() else percent_format()
    
    ylab<-sprintf('%s %s',if_else(..2=='no','Averted','Decrease in'),
                  if_else(..1=='hos','RSV-associated ALRI hospital admissions','RSV-attributable deaths'))
    tag<-if_else(..2=='no','A','B')
    
    p1<-df_longer_all %>%
      filter(facet!='By WHO region',metric==..1,type==..2) %>%
      ggplot(aes(Group,dec_median,color=sens))+
      geom_point(position = position_dodge(width = .8),size = 1,aes(shape=sens))+
      geom_errorbar(aes(ymin=dec_lower,ymax=dec_upper),position = position_dodge(width = .8), width = 0.2)+
      scale_colour_lancet(name=NULL,guide = guide_legend())+
      scale_shape_manual(name=NULL,values=shape_map,guide = guide_legend())+
      labs(x=NULL,y='Averted RSV-associated ALRI hospital admissions',tag=tag)+
      scale_y_continuous(labels = labels_fun)+
      theme_bw()+
      theme(#panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=10),
        axis.line = element_line(linewidth = .2),
        panel.grid.major = element_line(linewidth = .1),
        legend.position = c(0.98,0.98),
        legend.justification = c(1,1))+
      ggforce::facet_row(vars(facet), scales = 'free_x', space = 'free')
    
    p2<-df_longer_all %>%
      filter(facet=='By WHO region',metric==..1,type==..2) %>%
      ggplot(aes(Group,dec_median,color=sens))+
      geom_point(position = position_dodge(width = .8),size = 1,aes(shape=sens))+
      geom_errorbar(aes(ymin=dec_lower,ymax=dec_upper),position = position_dodge(width = .8), width = 0.2)+
      scale_colour_lancet(name=NULL,guide = guide_legend())+
      scale_shape_manual(name=NULL,values=shape_map,guide = guide_legend())+
      labs(x=NULL,y='Averted RSV-associated ALRI hospital admissions')+
      scale_y_continuous(labels = labels_fun)+
      theme_bw()+
      theme(#panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=10),
        axis.line = element_line(linewidth = .2),
        panel.grid.major = element_line(linewidth = .1),
        legend.position = 'NULL')+
      ggforce::facet_row(vars(facet), scales = 'free_x', space = 'free')
    
    plot<-p1/p2+plot_layout(axis_titles = 'collect_y',guides = "collect")&
      labs(y=ylab)
    
    #ggsave(plot = plot,filename = sprintf('Figures/Averted_%s_%s.png',..1,..2),width=10,height=6,dpi=300)
    plot
  })

plots[[3]]

plots[[1]]&theme(legend.position = 'NULL')|plots[[3]]
ggsave(filename = 'Figures/Averted_Hos.tiff',width=12,height=5,dpi=300)
plots[[2]]&theme(legend.position = 'NULL')|plots[[4]]
ggsave(filename = 'Figures/Averted_Dea.tiff',width=12,height=5,dpi=300)

# Figure S15 Sensitive GBD ----
df_res.all.GBD<-rio::import('model_data/df_res.all.GBD.RDS') %>%
  mutate(params_str=map(params,~paste0(.x,collapse = '_')))

df_longer_all.GBD<-list('main'=df_res.main,'GBD'=df_res.all.GBD) %>% imap_dfr(~{
  cat(.y,'\n')
  df_res<-.x
  add_by_group<-bind_rows(
    df_res %>% add_by2(WHORegion) %>% mutate(Group=toupper(Group)),
    df_res %>% add_by2(Income2019) %>% mutate(Group=paste0(Group,'IC')),
    df_res %>% mutate(WHORegion='Global') %>% add_by2(WHORegion)
  )
  sum_by_group<-sum_by(add_by_group,Group) %>%
    `[`(c(3,1,2,4,6,5,7,10,9,8,11),)
  df_longer<-sum_by_group %>%
    dplyr::select(Group,matches('dec_.*[0-9]$')) %>%
    pivot_longer(cols = -1,names_pattern = 'dec_(.*)_(.*)_(.*)',names_to = c('type','metric','ci')) %>%
    mutate(ci=recode(ci,'q500'='median','q025'='lower','q975'='upper')) %>%
    pivot_wider(id_cols = 1:3,names_from = ci,values_from = value,names_prefix = 'dec_') %>%
    mutate(sens=.y)
}) %>%
  mutate(facet=case_when(Group=='Global'~'Global',
                         Group %in% income_level~'By Income level',
                         T~'By WHO region'),
         facet=factor(facet,levels=c('Global','By Income level','By WHO region'))) %>%
  mutate(Group=factor(Group,levels=c(income_level,region_level,'Global')))

pal <- pal_lancet()(9)  # 先拿一整套
pal
# "#00468BFF" "#ED0000FF" "#42B540FF" "#0099B4FF" "#925E9FFF" "#FDAF91FF" "#AD002AFF" "#ADB6B6FF" "#1B1919FF"

plots.GBD<-expand.grid(c('hos','dea'),c('no','pct')) %>% 
  pmap(~{
    labels_fun<-if(..2=='no') comma_format() else percent_format()
    
    ylab<-sprintf('%s %s',if_else(..2=='no','Averted','Decrease in'),
                  if_else(..1=='hos','RSV-associated ALRI hospital admissions','RSV-attributable deaths'))
    tag<-if_else(..2=='no','A','')
    
    p1<-df_longer_all.GBD %>%
      filter(facet!='By WHO region',metric==..1,type==..2) %>%
      ggplot(aes(Group,dec_median,color=sens))+
      geom_point(position = position_dodge(width = .4),size = 1)+
      geom_errorbar(aes(ymin=dec_lower,ymax=dec_upper),position = position_dodge(width = .4), width = 0.2)+
      #scale_colour_lancet(name=NULL,guide = guide_legend(),labels=c('using GBD data','main analysis'))+
      scale_colour_manual(
        values = pal[c(6, 7)],
        name = NULL,
        labels = c("using GBD data", "main analysis")
      )+
      scale_shape_manual(name=NULL,values=shape_map,guide = guide_legend())+
      labs(x=NULL,y='Averted RSV-associated ALRI hospital admissions',tag=tag)+
      scale_y_continuous(labels = labels_fun)+
      theme_bw()+
      theme(#panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=10),
        axis.line = element_line(linewidth = .2),
        panel.grid.major = element_line(linewidth = .1),
        legend.position = c(0.98,0.98),
        legend.justification = c(1,1))+
      ggforce::facet_row(vars(facet), scales = 'free_x', space = 'free')
    
    p2<-df_longer_all.GBD %>%
      filter(facet=='By WHO region',metric==..1,type==..2) %>%
      ggplot(aes(Group,dec_median,color=sens))+
      geom_point(position = position_dodge(width = .4),size = 1)+
      geom_errorbar(aes(ymin=dec_lower,ymax=dec_upper),position = position_dodge(width = .4), width = 0.2)+
      #scale_colour_lancet(name=NULL,guide = guide_legend())+
      scale_colour_manual(
        values = pal[c(6, 7)],
        name = NULL,
        labels = c("using GBD data", "main analysis")
      )+
      scale_shape_manual(name=NULL,values=shape_map,guide = guide_legend())+
      labs(x=NULL,y='Averted RSV-associated ALRI hospital admissions')+
      scale_y_continuous(labels = labels_fun)+
      theme_bw()+
      theme(#panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=10),
        axis.line = element_line(linewidth = .2),
        panel.grid.major = element_line(linewidth = .1),
        legend.position = 'NULL')+
      ggforce::facet_row(vars(facet), scales = 'free_x', space = 'free')
    
    plot<-p1/p2+plot_layout(axis_titles = 'collect_y',guides = "collect")&
      labs(y=ylab)
    
    #ggsave(plot = plot,filename = sprintf('Figures/Averted_%s_%s.png',..1,..2),width=10,height=6,dpi=300)
    plot
  })
plots.GBD[[4]]
ggsave(filename = 'Figures/Averted_Dea_GBD.tiff',width=8,height=5,dpi=300)

#
# ------------------------------------------------------------------------------
# Script: code_11.6_Gini.R
# Purpose:
#   Quantify inequality in RSV burden across countries using the Gini coefficient
#   (Lorenz curve approach) for hospital admissions and mortality outcomes.
#
# Inputs:
#   - model_data/df_res.all.RDS (model results)
#   - rda/df_pop_center_nir.rds and model_data/df_pop_2019_2026.rds (population/
#     grouping metadata; see `import()` calls below)
#   - functions.R
#
# Outputs:
#   - Inequality plots under `plot/` (e.g., `plot/Gini_plot_*.png`) and any other
#     exported summaries created in the script.
#
# Usage:
#   source("code_11.6_Gini.R")
#
# Notes:
#   Uses parallel execution via `furrr`; adjust the future plan if running on a
#   constrained machine.
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
library(ineq)
library(furrr)
library(ggtext)
library(cowplot)
library(ggplotify)
plan(multisession)

source('functions.R')

region_label<-c(
  'AFR' = 'African Region',
  'AMR' = 'Region of the Americas',
  'EMR' = 'Eastern Mediterranean Region',
  'EUR' = 'European Region',
  'SEAR' = 'South-East Asia Region',
  'WPR' = 'Western Pacific Region',
  'GLOBAL'='Global'
)

# Countries Categary ----
df_pop<-left_join(
  import('rda/df_pop_center_nir.rds') %>% 
    transmute(ISOCountry,WHORegion,region_ns_zone,Country.Name,WHOName),
  import('model_data/df_pop_2019_2026.rds')) %>%
  mutate(region_income=paste(WHORegion,Income2019,sep='_'),.after=WHORegion)

# Model result ----

df_res.all<-rio::import('model_data/df_res.all.RDS') %>%
  mutate(params_str=map(params,~paste0(.x,collapse = '_')))

unique(df_res.all$params_str)
df_res.main<-df_res.all %>% filter(params_str=="c(0.8, 0.05, 0.005)_c(0.83, 0.81, 0.71, 0.77)_c(1, 1)")

names(df_res.main)

## df_res.median (Median Rate)----
df_res.median<-df_res.main %>%
  summarise(across(c(hr,mort,hr_pre,mt_pre,starts_with('pop_')),~median(.x,na.rm=T)),.by=c(ISOCountry)) %>%
  drop_na() %>%
  rename(hr_2019=hr,hr_2026=hr_pre,
         mt_2019=mort,mt_2026=mt_pre) %>%
  left_join(dplyr::select(df_pop,ISOCountry,WHORegion))

# Function to generate x and y coordinates for a Lorenz curve plot
gen_geni_data<-function(rate,pop){
  # x rate y pop
  n<-rate*pop
  pop_ordered<-pop[order(rate)]
  n_ordered<-n[order(rate)]
  n_cumpro<-cumsum(n_ordered)/sum(n_ordered)
  pop_cumpro<-cumsum(pop_ordered)/sum(pop_ordered)
  data.frame(x=c(0,pop_cumpro),y=c(0,n_cumpro)) # add initial point 0,0 
}

# Hospital admissions ----
df_plot_hr<-bind_cols(
  gen_geni_data(df_res.median$hr_2019,df_res.median$pop_infant_2019),
  gen_geni_data(df_res.median$hr_2019,df_res.median$pop_infant_2026),
  gen_geni_data(df_res.median$hr_2026,df_res.median$pop_infant_2026)
) %>%
  setNames(c('x1','y1','x2','y2','x3','y3')) %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "type"),
               names_pattern = "([xy])(\\d+)")


ggplot(df_plot_hr,aes(x,y,color=type))+
  geom_line()+
  scale_color_lancet(name=NULL,
    labels=c('1'='2019','2'='2026 (with no implementation)','3'='2026 (with status quo implementation)')
  )+
  theme_bw()+
  labs(title='RSV-associated hospital admissions')+
  coord_equal()+
  geom_abline(slope=1)+
  scale_x_continuous(limits = c(0,1),expand = expansion())+
  scale_y_continuous(limits = c(0,1),expand = expansion())+
  theme()

ggsave('plot/Gini_plot_hr.png',width=1000,height=800,units='px',dpi=200)


# Mortality ----
df_plot_mt<-bind_cols(
  gen_geni_data(df_res.median$mt_2019,df_res.median$pop_infant_2019),
  gen_geni_data(df_res.median$mt_2019,df_res.median$pop_infant_2026),
  gen_geni_data(df_res.median$mt_2026,df_res.median$pop_infant_2026)
) %>%
  setNames(c('x1','y1','x2','y2','x3','y3')) %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "type"),
               names_pattern = "([xy])(\\d+)")


ggplot(df_plot_mt,aes(x,y,color=type))+
  geom_line()+
  scale_color_lancet(name=NULL,
                     labels=c('1'='2019','2'='2026 (with no implementation)','3'='2026 (with status quo implementation)')
  )+
  theme_bw()+
  labs(title='RSV-attributable deaths')+
  coord_equal()+
  geom_abline(slope=1)+
  scale_x_continuous(limits = c(0,1),expand = expansion())+
  scale_y_continuous(limits = c(0,1),expand = expansion())+
  theme()

ggsave('plot/Gini_plot_mt.png',width=1000,height=800,units='px',dpi=200)


# Compute the Gini coefficient
gini_from_points <- function(x, y) {
  # x: cumulative population share (increasing from 0 to 1)
  # y: corresponding cumulative share of cases (or other quantity)
  
  # Trapezoidal rule to compute area A under the Lorenz curve
  area <- sum((y[-1] + y[-length(y)]) * diff(x)) / 2
  
  # Gini = 1 - 2A, where A is the area under the Lorenz curve
  # Unit square area is 1; perfect equality gives A = 0.5 (Gini = 0)
  # Extreme inequality yields A close to 0 (Gini close to 1)
  gini <- 1 - 2 * area
  
  return(gini)
}


Gini(1:10) # 0.3 
gini_from_points(0:10/10,cumsum(c(0,1:10))/sum(c(0,1:10))) # 0.3 

all.equal(Gini(c(1,3,9)),gini_from_points(0:3/3,cumsum(c(0,1,3,9))/sum(c(0,1,3,9))))

# getGini_from_df_plot ----
getGini<-function(df_plot){
  1:3 %>%
    map_dbl(~{
      df<-df_plot %>% filter(type==!!.x)
      gini_from_points(df$x,df$y)
    }) %>%
    enframe(name='type',value = 'gini')
}

getGini(df_plot_hr)
getGini(df_plot_mt)

getGini_all<-function(df_plot_with_cases){
  suppressMessages({
    bind_rows(
      getGini(df_plot_with_cases %>% filter(cases=='hosp')) %>% mutate(cases='hosp'),
      getGini(df_plot_with_cases %>% filter(cases=='mort')) %>% mutate(cases='mort')
    )
  })
}


# Median Gini ----

# Both hosp and Mort,3 scenario
gen_df_plot_all<-function(df_res.one){
  # hospital admission
  df_plot_hr<-bind_cols(
    gen_geni_data(df_res.one$hr_2019,df_res.one$pop_infant_2019),
    gen_geni_data(df_res.one$hr_2019,df_res.one$pop_infant_2026),
    gen_geni_data(df_res.one$hr_2026,df_res.one$pop_infant_2026)
  ) %>%
    setNames(c('x1','y1','x2','y2','x3','y3')) %>%
    pivot_longer(cols = everything(),
                 names_to = c(".value", "type"),
                 names_pattern = "([xy])(\\d+)") %>%
    mutate(cases='hosp')
  # mortality
  df_plot_mt<-bind_cols(
    gen_geni_data(df_res.one$mt_2019,df_res.one$pop_infant_2019),
    gen_geni_data(df_res.one$mt_2019,df_res.one$pop_infant_2026),
    gen_geni_data(df_res.one$mt_2026,df_res.one$pop_infant_2026)
  ) %>%
    setNames(c('x1','y1','x2','y2','x3','y3')) %>%
    pivot_longer(cols = everything(),
                 names_to = c(".value", "type"),
                 names_pattern = "([xy])(\\d+)") %>%
    mutate(cases='mort')
  bind_rows(df_plot_hr,df_plot_mt)
}

gen_df_plot_all(df_res.median) %>% getGini_all()
getGini(df_plot_hr)
getGini(df_plot_mt)

# 
get_df_plot_all_byregion<-function(df_res.one){
  df_res.one_byregion<-c('.*',unique(df_res.one$WHORegion)) %>%
    map(~{
      df_res.one %>% filter(str_detect(WHORegion,.x)) %>%
        mutate(region=ifelse(.x=='.*','Global',WHORegion))
    })
  map_dfr(
    df_res.one_byregion,~{
      gen_df_plot_all(.x) %>% 
        mutate(region=.x[1,'region'])
    }
  )
}

df_plot_all_byregion<-get_df_plot_all_byregion(df_res.median)

getGini_all(df_plot_all_byregion %>% filter(region=='Global'))
gen_df_plot_all(df_res.median) %>% getGini_all()


get_gini_byregion<-function(df_plot_all_byregion){
  unique(df_plot_all_byregion$region) %>%
    map_dfr(~{
      df_plot_all<-df_plot_all_byregion %>% filter(region==!!.x)
      getGini_all(df_plot_all) %>% mutate(region=.x)
    })
}

# get Gini all, all region
get_gini_byregion(df_plot_all_byregion)


# example use median / index =1 
df_res.median %>% get_df_plot_all_byregion() %>% get_gini_byregion()

df_res.main %>% filter(index==1) %>% 
  dplyr::select(ISOCountry,hr,mort,hr_pre,mt_pre,starts_with('pop_')) %>%
  drop_na() %>%
  rename(hr_2019=hr,hr_2026=hr_pre,
         mt_2019=mort,mt_2026=mt_pre) %>%
  left_join(dplyr::select(df_pop,ISOCountry,WHORegion)) %>% # 类似 df_res.median
  get_df_plot_all_byregion() %>% get_gini_byregion() 

# Gini under median rate ----
df_gini_median<-df_res.median %>% get_df_plot_all_byregion() %>% get_gini_byregion()

# Gini 95% UR  ----

get_gini_one_scenario<-function(df_res.main,index_begin=1){
  index<-seq(from=index_begin,length=100)
  print(range(index))
  index %>%
    future_map_dfr(~{
      suppressMessages({
        df_res.one<-df_res.main %>% filter(index==.x) %>% 
          dplyr::select(ISOCountry,hr,mort,hr_pre,mt_pre,starts_with('pop_')) %>%
          drop_na() %>%
          rename(hr_2019=hr,hr_2026=hr_pre,
                 mt_2019=mort,mt_2026=mt_pre) %>%
          left_join(dplyr::select(df_pop,ISOCountry,WHORegion))
        
        df_res.one %>% get_df_plot_all_byregion() %>% get_gini_byregion() %>%
          mutate(index=.x)
      }
      )
    })
}

 # get_gini_one_scenario(df_res.main,index_begin=1)
 # 
 # for(i in c(0:9)*100+1) {
 #   df_gini_main<-get_gini_one_scenario(df_res.main,index_begin=i)
 #   export(df_gini_main,sprintf('model_data/df_gini_main_batch_%s.rds',i))
 # }

df_gini_main<-dir('model_data/',pattern='df_gini_main_batch',full.names = T) %>%
  map_dfr(~import(.x))

export(df_gini_main,'rda/df_gini_main.rds')


# df_gini_main_sum<-df_gini_main %>%
#   summarise(gini_est=median(gini),
#             gini_lower=quantile(gini,0.025),
#             gini_upper=quantile(gini,0.975),.by=c(type,cases,region)) %>%
#   mutate(gini_str=sprintf('%.4f\n(%.4f-%.4f)',gini_est,gini_lower,gini_upper),
#          type=recode(type,'1'='2019','2'='2026_no','3'='2026_status'))
#   
# 
# region_label<-c(
#   'AFR' = 'African Region',
#   'AMR' = 'Region of the Americas',
#   'EMR' = 'Eastern Mediterranean Region',
#   'EUR' = 'European Region',
#   'SEAR' = 'South-East Asia Region',
#   'WPR' = 'Western Pacific Region',
#   'GLOBAL'='Global'
# )
# 
# df_gini_main_sum %>%
#   pivot_wider(id_cols = region,names_from = c(type,cases),values_from = gini_str) %>%
#   arrange(region) %>%
#   `[`(c(1,2,3,5,6,4),) %>%
#   mutate(region=toupper(region)) %>%
#   #mutate(region=region_label[toupper(region)]) %>%
#   createAppendixTable('docs/Gini_by_region.xlsx')
  
# Gini Plot Lorenz plot----

# df_gini_median %>% pivot_wider(id_cols = region,names_from = c(cases,type),values_from = gini)

# Mort Global,EUR,AMR Lorenz Plot ----
df_gini_text<-df_gini_median %>%
  mutate(gini_str=sprintf('%.2f',gini)) %>%
  filter(region %in% c('Global','Amr','Eur','Wpr'),cases=='mort') %>%
  mutate(x=0,y=0.95-type*0.05,
         color=pal_lancet()(n=3)[type]) %>%
  mutate(md=sprintf("<span style='color:%s;'>%s</span>",color,gini_str)) %>%
  group_by(region) %>%
  mutate(label=paste0(md,collapse = ' '),
         label=paste0('Gini coefficient<br>',label)) %>%
  ungroup() %>%
  mutate(region=recode(region,"Global" = "Global",
                       "Amr" = "Region of the Americas",
                       "Eur" = "European Region",
                       "Wpr" = "Western Pacific Region"))

## plot_lorenz ----
fontbase<-4
plot_lorenz<-df_plot_all_byregion %>%
  filter(region %in% c('Global','Amr','Eur','Wpr'),
         cases=='mort') %>%
  mutate(region=recode(region,"Global" = "Global",
                       "Amr" = "Region of the Americas",
                       "Eur" = "European Region",
                       "Wpr" = "Western Pacific Region")
         ) %>%
  ggplot(aes(x,y,color=type,group=type))+
    geom_line()+
  facet_wrap(~factor(region,levels=c( "Global","Region of the Americas","European Region","Western Pacific Region")),
             nrow = 2)+
  theme_bw()+
  scale_color_lancet(name=NULL,labels=c('2019','2026 (no implementation)','2026 (status quo implementation)'))+
  coord_equal()+
  geom_abline(slope=1,linetype='dashed')+
  scale_x_continuous(limits = c(0,1),breaks=0:4/4,labels=~ifelse(.x==0,'0',.x),expand = expansion())+
  scale_y_continuous(limits = c(0,1),breaks=0:4/4,labels=~ifelse(.x==0,'0',.x),expand = expansion())+
  theme(panel.grid.major = element_line(linewidth = 0.1),
        #panel.grid = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.2, fill = NA),
        strip.background = element_blank(),
        strip.text = element_text(size=12+fontbase),
        axis.ticks = element_line(linewidth = .2),
        text=element_text(size=12+fontbase),
        legend.text = element_text(size=12+fontbase),
        axis.title = element_text(size=12+fontbase),
        panel.spacing = unit(1.2, "lines"),
        legend.position = 'top')+
  labs(x='Cumulative proportion of the infant population',
       y='Cumulative proportion of RSV-attributable deaths')+
  geom_richtext(data=df_gini_text %>% filter(type==1),aes(x=0,y=1,label=label),color='black',
          size=5,show.legend = FALSE,hjust=-.05,vjust=1.1,label.size = .1)

plot_lorenz
ggsave('pdf/Figure_gini_Mortality_Global_AMR_EUR.pdf',width=9,height=9)

# Hosp and mort all region ----
df_gini_text_all<-df_gini_median %>%
  filter(type!=1) %>% # remove 2019
  mutate(gini_str=sprintf('%.2f',gini)) %>%
  mutate(x=0,y=0.95-type*0.05,
         color=pal_lancet()(n=2)[type-1]) %>%
  mutate(md=sprintf("<span style='color:%s;padding-left:20px'>%s</span>",color,gini_str)) %>%
  group_by(region,cases) %>%
  mutate(label=paste0(md,collapse = ' '),
         label=paste0('<br>',label)) %>%
  ungroup() %>%
  mutate(region=region_label[toupper(region)])

fontbase<--4
#### hosp ----
p_hos_byregion<-df_plot_all_byregion %>%
  filter(cases=='hosp',region!='Global',type!=1) %>%
  mutate(region=region_label[toupper(region)],
         region=factor(region,levels=region_label[-7])
  ) %>%
  ggplot(aes(x,y,color=type,group=type))+
  geom_line()+
  facet_wrap(vars(region),nrow = 2)+
  theme_bw()+
  scale_color_lancet(name=NULL,labels=c('2026 (no implementation)','2026 (status quo implementation)'))+
  coord_equal()+
  geom_abline(slope=1,linetype='dashed')+
  scale_x_continuous(limits = c(0,1),breaks=0:4/4,labels=~ifelse(.x==0,'0',.x),expand = expansion())+
  scale_y_continuous(limits = c(0,1),breaks=0:4/4,labels=~ifelse(.x==0,'0',.x),expand = expansion())+
  theme(panel.grid.major = element_line(linewidth = .2),
        panel.border = element_rect(color = "black", linewidth = 0.2, fill = NA),
        strip.background = element_blank(),
        strip.text = element_text(size=12+fontbase),
        axis.ticks = element_line(linewidth = .2),
        text=element_text(size=12+fontbase),
        legend.text = element_text(size=12+fontbase),
        axis.title = element_text(size=12+fontbase),
        panel.spacing = unit(1.2, "lines"),
        legend.position = 'NULL')+
  labs(x='Cumulative proportion of the infant population',tag='B',
       y='Cumulative proportion of RSV-associated hospital admissions')+
  geom_richtext(data=df_gini_text_all %>% filter(type==2,cases=='hosp',region!='Global'),aes(x=0,y=1,label=label),color='black',
                size=3,show.legend = FALSE,hjust=-.05,vjust=1.1,lineheight = .8)

p_hos_byregion
  
p_hos_global<-df_plot_all_byregion %>%
  #filter(cases=='hosp',region=='Global') %>%
  filter(cases=='hosp',region=='Global',type!=1) %>%
  ggplot(aes(x,y,color=type,group=type))+
  geom_line()+
  facet_wrap(vars(region),nrow = 3)+
  theme_bw()+
  #scale_color_lancet(name=NULL,labels=c('2019','2026 (no implementation)','2026 (status quo implementation)'))+
  scale_color_lancet(name=NULL,labels=c('2026 (no implementation)','2026 (status quo implementation)'))+
  coord_equal()+
  geom_abline(slope=1,linetype='dashed')+
  scale_x_continuous(limits = c(0,1),breaks=0:4/4,labels=~ifelse(.x==0,'0',.x),expand = expansion())+
  scale_y_continuous(limits = c(0,1),breaks=0:4/4,labels=~ifelse(.x==0,'0',.x),expand = expansion())+
  theme(panel.grid.major = element_line(linewidth = .2),
        panel.border = element_rect(color = "black", linewidth = 0.2, fill = NA),
        strip.background = element_blank(),
        strip.text = element_text(size=12+fontbase),
        axis.ticks = element_line(linewidth = .2),
        text=element_text(size=12+fontbase),
        #axis.title.y = element_text(size=14),
        legend.text = element_text(size=12+fontbase),
        axis.title = element_text(size=12+fontbase),
        axis.title.y = element_text(size=10+fontbase),
        panel.spacing = unit(1.2, "lines"),
        legend.position = 'NULL')+
  labs(x='Cumulative proportion of the infant population',
       y='Cumulative proportion of RSV-associated hospital admissions',tag='A')+
  geom_richtext(data=df_gini_text_all %>% filter(type==2,cases=='hosp',region=='Global'),aes(x=0,y=1,label=label),color='black',
                size=3,show.legend = FALSE,hjust=-.05,vjust=1.1,lineheight = .8)



p_hos_legend<-get_legend_35(p_hos_global+theme(legend.position = 'left',
                                               legend.text = element_text(size=10))) %>% 
  ggdraw()

# Define the layout: put legend in the second cell; use a spacer for the first
area_global  <- patchwork::area(1, 1, 1, 2)  # top-left panel
area_spacer <- patchwork::area(2, 1, 2, 1)  # bottom-left spacer
area_legend <- patchwork::area(2, 2, 2, 2)  # legend in its own cell
area_byregion <- patchwork::area(1, 3, 2, 3)  # by-region panels (span two rows)

# Patchwork layout composition
p_hos_global + plot_spacer() + p_hos_legend + p_hos_byregion +
  plot_layout(design = c(area_global, area_spacer, area_legend, area_byregion),
              widths = c(0.1, 0.9, 3),  # the second column is the spacer
              heights = c(1, 1),
              axis_titles = 'collect')


ggsave('Figures/Lorenz_hosp.tiff',width = 10,height = 5)
ggsave('pdf/Lorenz_hosp.pdf',width = 10,height = 5,dpi=200)

#### mort ----
p_mort_byregion<-df_plot_all_byregion %>%
  filter(cases=='mort',region!='Global',type!=1) %>%
  mutate(region=region_label[toupper(region)],
         region=factor(region,levels=region_label[-7])
  ) %>%
  ggplot(aes(x,y,color=type,group=type))+
  geom_line()+
  facet_wrap(vars(region),nrow = 2)+
  theme_bw()+
  scale_color_lancet(name=NULL,labels=c('2026 (no implementation)','2026 (status quo implementation)'))+
  coord_equal()+
  geom_abline(slope=1,linetype='dashed')+
  scale_x_continuous(limits = c(0,1),breaks=0:4/4,labels=~ifelse(.x==0,'0',.x),expand = expansion())+
  scale_y_continuous(limits = c(0,1),breaks=0:4/4,labels=~ifelse(.x==0,'0',.x),expand = expansion())+
  theme(panel.grid.major = element_line(linewidth = .2),
        panel.border = element_rect(color = "black", linewidth = 0.2, fill = NA),
        strip.background = element_blank(),
        strip.text = element_text(size=12+fontbase),
        axis.ticks = element_line(linewidth = .2),
        text=element_text(size=12+fontbase),
        legend.text = element_text(size=12+fontbase),
        axis.title = element_text(size=12+fontbase),
        panel.spacing = unit(1.2, "lines"),
        legend.position = 'NULL')+
  labs(x='Cumulative proportion of the infant population',tag='B',
       y='Cumulative proportion of RSV-attributable deaths')+
  geom_richtext(data=df_gini_text_all %>% filter(type==2,cases=='mort',region!='Global'),aes(x=0,y=1,label=label),color='black',
                size=3,show.legend = FALSE,hjust=-.05,vjust=1.1,lineheight = .8)

p_mort_global<-df_plot_all_byregion %>%
  filter(cases=='mort',region=='Global',type!=1) %>%
  mutate(region=region_label[toupper(region)],
         region=factor(region,levels=region_label)
  ) %>%
  ggplot(aes(x,y,color=type,group=type))+
  geom_line()+
  facet_wrap(vars(region),nrow = 3)+
  theme_bw()+
  scale_color_lancet(name=NULL,labels=c('2026 (no implementation)','2026 (status quo implementation)'))+
  coord_equal()+
  geom_abline(slope=1,linetype='dashed')+
  scale_x_continuous(limits = c(0,1),breaks=0:4/4,labels=~ifelse(.x==0,'0',.x),expand = expansion())+
  scale_y_continuous(limits = c(0,1),breaks=0:4/4,labels=~ifelse(.x==0,'0',.x),expand = expansion())+
  theme(panel.grid.major = element_line(linewidth = .2),
        panel.border = element_rect(color = "black", linewidth = 0.2, fill = NA),
        strip.background = element_blank(),
        strip.text = element_text(size=12+fontbase),
        axis.ticks = element_line(linewidth = .2),
        text=element_text(size=12+fontbase),
        #axis.title.y = element_text(size=14),
        legend.text = element_text(size=12+fontbase),
        axis.title = element_text(size=12+fontbase),
        panel.spacing = unit(1.2, "lines"),
        legend.position = 'NULL')+
  labs(x='Cumulative proportion of the infant population',
       y='Cumulative proportion of RSV-attributable deaths',tag='A')+
  geom_richtext(data=df_gini_text_all %>% filter(type==2,cases=='mort',region=='Global'),aes(x=0,y=1,label=label),color='black',
                size=3,show.legend = FALSE,hjust=-.05,vjust=1.1,lineheight = .8)

p_mort_legend<-get_legend_35(p_mort_global+theme(legend.position = 'left',
                                               legend.text = element_text(size=10))) %>% 
  ggdraw()

p_mort_global + plot_spacer() + p_mort_legend + p_mort_byregion +
  plot_layout(design = c(area_global, area_spacer, area_legend, area_byregion),
              widths = c(0.1, 0.9, 3),  # 第二列就是 spacer，调宽度
              heights = c(1, 1),
              axis_titles = 'collect')


ggsave('Figures/Lorenz_mort.tiff',width = 10,height = 5,dpi=200)
ggsave('pdf/Lorenz_mort.pdf',width = 10,height = 5)

# Gini from median ----
df_res.median %>% get_df_plot_all_byregion() %>% get_gini_byregion() %>%
  pivot_wider(id_cols = region,names_from = c(cases,type),values_from = gini)

# Gini Difference/Change ----
df_gini_main_with_change<-df_gini_main %>%
  pivot_wider(id_cols = c(region,index),names_from = c(cases,type),values_from = gini) %>%
  mutate(hosp_add1=hosp_2-hosp_1,hosp_add2=hosp_3-hosp_2,
         mort_add1=hosp_2-mort_1,mort_add2=mort_3-mort_2,
         hosp_inc1=hosp_add1/hosp_1,hosp_inc2=hosp_add2/hosp_2,
         mort_inc1=mort_add1/mort_1,mort_add2=mort_add2/mort_2)

df_gini_main_with_change %>%
  reframe(across(contains('add'),~list(
    q025=quantile(.x,0.025),
    q050=quantile(.x,0.5)
  )),.by='region') %>% view()

df_gini_main_with_change %>%
  ggplot(aes(index,mort_add1))+
  geom_point(alpha=.5)+
  scale_y_continuous(limits = c(-0.5,0.25))+
  geom_abline(slope = 0,intercept = 0,color='red')+
  facet_wrap(vars(region),nrow=2,scales = 'free_y')+
  labs(y='Diffence in Gini coefficient of RSV-attributable deaths',title='2026(no implementation)-2019')

ggsave('plot/Difference_gini_mort1.tiff',width=8,height=4.5,dpi=200)

df_gini_main_with_change %>%
  ggplot(aes(index,mort_add2))+
  geom_point(alpha=.5)+
  #scale_y_continuous(limits = c(-0.05,NA))+
  geom_abline(slope = 0,intercept = 0,color='red')+
  facet_wrap(vars(region),nrow=2,scales = 'free_y')+
  theme(axis.text = element_text(size=14))+
  labs(y='Difference in Gini coefficient of RSV-attributable deaths',title='2026(status quo implementation)-2026(no implementation)')

ggsave('plot/Difference_gini_mort2.tiff',width=8,height=4.5,dpi=200)

df_gini_main_with_change %>%
  ggplot(aes(index,hosp_add1))+
  geom_point(alpha=.5)+
  scale_y_continuous(limits = c(-0.04,0.06))+
  geom_abline(slope = 0,intercept = 0,color='red')+
  facet_wrap(vars(region),nrow=2,scales = 'free_y')+
  labs(y='Difference in Gini coefficient of RSV-associated hosptital admissions',title='2026(no implementation)-2019')+
  theme(axis.title = element_text(size=10))

ggsave('plot/Difference_gini_hosp1.tiff',width=8,height=4.5,dpi=200)

df_gini_main_with_change %>%
  ggplot(aes(index,hosp_add2))+
  geom_point(alpha=.5)+
  scale_y_continuous(limits = c(-0.1,0.12))+
  geom_abline(slope = 0,intercept = 0,color='red')+
  facet_wrap(vars(region),nrow=2,scales = 'free_y')+
  labs(y='Difference in Gini coefficient of RSV-associated hosptital admissions',title='2026(status quo implementation)-2026(no implementation)')+
  theme(axis.title = element_text(size=10))


ggsave('plot/Difference_gini_hosp2.tiff',width=8,height=4.5,dpi=200)


# Gini difference violin plot ----
plot_global<-df_gini_main_with_change %>%
  filter(region=='Global') %>%
  dplyr::select(region,index,contains('add')) %>%
  pivot_longer(cols=-c(region,index),names_pattern = '(.*)_add(.)',names_to = c('type','group')) %>%
  mutate(group = factor(group, levels = c('1', '2'))) %>%
  ggplot(aes(type,value,color=group))+
  geom_violin()+
  facet_wrap(vars(region),nrow=3,scales = 'free')+
  geom_hline(aes(yintercept=0),color='red',linewidth = .5)+
  scale_x_discrete(labels=c('RSV-associated hospital admissions','RSV-attributable deaths'))+
  scale_color_lancet(name=NULL,labels=c('1'='2026(no impl) - 2019','2'='2026(status quo) - 2026(no impl)'))+
  labs(x=NULL,y='Difference in Gini coefficient')+
  theme_bw()+
  theme(legend.position = 'right',
        strip.background = element_blank())

plot_global

legend <- get_legend_35(plot_global)
# Convert to a ggplot object (cowplot::ggdraw)
legend_plot <- ggdraw(legend)+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 20))

df_gini_main_with_change %>%
  filter(region!='Global') %>%
  dplyr::select(region,index,contains('add')) %>%
  pivot_longer(cols=-c(region,index),names_pattern = '(.*)_add(.)',names_to = c('type','group')) %>%
  ggplot(aes(value,type,color=group))+
  geom_violin()+
  facet_wrap(vars(region),nrow=2,scales = 'free',labeller =as_labeller(toupper))+
  geom_hline(aes(yintercept=0))+
  scale_y_discrete(labels=c('RSV-associated hospital admissions','RSV-attributable deaths'))+
  scale_color_lancet(name=NULL,labels=c('1'='2026(no implementation)-2019 ','2'='2026(status quo implementation)-2026(no implementation)'))+
  labs(x=NULL,y='Difference in Gini coefficient')+
  theme_bw()+
  theme(legend.position = 'NULL',
        strip.background = element_blank())

df_plots_byregion<-unique(df_gini_main_with_change$region) %>% sort() %>% setdiff('Global') %>%
  map(~{
    df_gini_main_with_change %>%
      filter(region==.x) %>%
      dplyr::select(region,index,contains('add')) %>%
      pivot_longer(cols=-c(region,index),names_pattern = '(.*)_add(.)',names_to = c('type','group')) %>%
      ggplot(aes(type,value,color=group))+
      geom_violin()+
      facet_wrap(vars(region),nrow=2,scales = 'free',labeller =as_labeller(toupper))+
      geom_hline(aes(yintercept=0),color='red',linewidth = .5)+
      scale_x_discrete(labels=c('RSV-associated hospital admissions','RSV-attributable deaths'))+
      scale_color_lancet(name=NULL,labels=c('1'='2026(no implementation)-2019 ','2'='2026(status quo implementation)-2026(no implementation)'))+
      labs(x=NULL,y='Difference in Gini coefficient')+
      theme_bw()+
      theme(legend.position = 'NULL',
            strip.background = element_blank())
  })

(plot_global+theme(legend.position = 'NULL'))+legend_plot+
df_plots_byregion[[1]]+df_plots_byregion[[2]]+
  df_plots_byregion[[3]]+  df_plots_byregion[[4]]+
  df_plots_byregion[[5]]+df_plots_byregion[[6]]+plot_layout(ncol=2,axis_titles = 'collect_y')


df_gini_main_with_change %>%
  filter(region=="Global") %>%
  dplyr::select(region,index,contains('add')) %>%
  pivot_longer(cols=-c(region,index),names_pattern = '(.*)_add(.)',names_to = c('type','group')) %>%
  ggplot(aes(type,value,color=group))+
  geom_violin()+
  #geom_boxplot()+
  facet_wrap(vars(region,type),nrow=2,scales = 'free',labeller =as_labeller(toupper))+
  geom_hline(aes(yintercept=0),color='red',linewidth = .5)+
  scale_x_discrete(labels=c('RSV-associated hospital admissions','RSV-attributable deaths'))+
  scale_color_lancet(name=NULL,labels=c('1'='2026(no implementation)-2019 ','2'='2026(status quo implementation)-2026(no implementation)'))+
  labs(x=NULL,y='Difference in Gini coefficient')+
  theme_bw()+
  theme(legend.position = 'NULL',
        strip.background = element_blank())


df_gini_main_with_change %>%
  filter(region=="Global") %>%
  dplyr::select(region,index,contains('add')) %>%
  pivot_longer(cols=-c(region,index),names_pattern = '(.*)_add(.)',names_to = c('type','group')) %>%
  filter(type=='hosp') %>%
  ggplot(aes(group,value,color=group))+
  geom_violin(width=1) +
  geom_boxplot(width=0.2, alpha=0.2) +
  #geom_boxplot()+
  facet_wrap(vars(region),ncol=2,scales = 'free',labeller =as_labeller(~ifelse(.x=='Global',.x,toupper(.x))))+
  geom_hline(aes(yintercept=0),color='red',linewidth = .2,linetype='dashed')+
  #scale_x_discrete(labels=c('RSV-associated hospital admissions','RSV-attributable deaths'))+
  scale_color_lancet(name=NULL,labels=c('1'='2026(no implementation)-2019 ','2'='2026(status quo implementation)-2026(no implementation)'))+
  labs(x=NULL,y='Difference in Gini coefficient')+
  theme_bw()+
  theme(legend.position = 'NULL',
        strip.background = element_blank())

# Hosp 
plot_list_hosp<-unique(df_gini_main_with_change$region) %>% 
  sort() %>% `[`(c(5,1:3,4,6,7)) %>%
  map(~{
    df_gini_main_with_change %>%
      filter(region==.x) %>%
      dplyr::select(region,index,contains('add')) %>%
      pivot_longer(cols=-c(region,index),names_pattern = '(.*)_add(.)',names_to = c('type','group')) %>%
      filter(type=='hosp') %>%
      ggplot(aes(group,value,color=group))+
      geom_violin(width=1) +
      geom_boxplot(width=0.2, alpha=0.2,color='gray') +
      #geom_boxplot()+
      facet_wrap(vars(region),ncol=2,scales = 'free',labeller =as_labeller(~ifelse(.x=='Global',.x,region_label[toupper(.x)])))+
      geom_hline(aes(yintercept=0),color='red',linewidth = .2,linetype='dashed')+
      scale_x_discrete(labels=c('2026(no impl) \n- 2019','2026(status quo)\n- 2026(no impl)'))+
      scale_color_lancet(name=NULL,labels=c('1'='2026(no impl) - 2019','2'='2026(status quo) - 2026(no impl)'))+
      labs(x=NULL,y='Difference in Gini coefficient')+
      theme_bw()+
      theme(legend.position = 'NULL',
            axis.title = element_text(size=14),
            text = element_text(size=12),
            legend.text = element_text(size=12),
            strip.text = element_text(size=13),
            strip.background = element_blank())
  })

plot_list_hosp <- append(plot_list_hosp, list(legend_plot),after = 1)

wrap_plots(plot_list_hosp, nrow = 4,axis_titles = 'collect')

ggsave('pdf/Diff_gini_hosp.pdf',width=10,height=12)

# Mort 
plot_list_mort<-unique(df_gini_main_with_change$region) %>% 
  sort() %>% `[`(c(5,1:3,4,6,7)) %>%
  map(~{
    df_gini_main_with_change %>%
      filter(region==.x) %>%
      dplyr::select(region,index,contains('add')) %>%
      pivot_longer(cols=-c(region,index),names_pattern = '(.*)_add(.)',names_to = c('type','group')) %>%
      filter(type=='mort') %>%
      ggplot(aes(group,value,color=group))+
      geom_violin(width=1) +
      geom_boxplot(width=0.2, alpha=0.2,color='gray') +
      #geom_boxplot()+
      facet_wrap(vars(region),ncol=2,scales = 'free',labeller =as_labeller(~ifelse(.x=='Global',.x,region_label[toupper(.x)])))+
      geom_hline(aes(yintercept=0),color='red',linewidth = .2,linetype='dashed')+
      scale_x_discrete(labels=c('2026(no impl) \n- 2019','2026(status quo)\n- 2026(no impl)'))+
      scale_color_lancet(name=NULL,labels=c('1'='2026(no impl) - 2019','2'='2026(status quo) - 2026(no impl)'))+
      labs(x=NULL,y='Difference in Gini coefficient')+
      theme_bw()+
      theme(legend.position = 'NULL',
            axis.title = element_text(size=14),
            text = element_text(size=12),
            legend.text = element_text(size=12),
            strip.text = element_text(size=13),
            strip.background = element_blank())
  })

plot_list_mort <- append(plot_list_mort, list(legend_plot),after = 1)

wrap_plots(plot_list_mort, nrow = 4,ncol=2,axis_titles = 'collect')

ggsave('pdf/Diff_gini_mort.pdf',width=10,height=12)


# Gini differcene point line range plot ----
df_gini_main_with_change %>%
  filter(region!='Global') %>%
  dplyr::select(region,index,contains('add')) %>%
  pivot_longer(cols=-c(region,index),names_pattern = '(.*)_add(.)',names_to = c('type','group')) %>%
  ggplot(aes(value,type,color=group))+
  geom_violin()+
  facet_wrap(vars(region),nrow=2,scales = 'free',labeller =as_labeller(toupper))+
  geom_hline(aes(yintercept=0))+
  scale_y_discrete(labels=c('RSV-associated hospital admissions','RSV-attributable deaths'))+
  scale_color_lancet(name=NULL,labels=c('1'='2026(no implementation)-2019 ','2'='2026(status quo implementation)-2026(no implementation)'))+
  labs(x=NULL,y='Difference in Gini coefficient')+
  theme_bw()+
  theme(legend.position = 'NULL',
        strip.background = element_blank())

df_plot_gini_diff<-df_gini_main_with_change %>%
  summarise(across(contains('add'),list(lower=~quantile(.x,0.025),
                                        upper=~quantile(.x,0.975),
                                        median=~quantile(.x,0.5))),.by=region) %>%
  pivot_longer(cols=-region,names_pattern = '(.*)_add(.)_(.*)',names_to = c('type','group','est')) %>%
  pivot_wider(id_cols = c(region,type,group),names_from = est,values_from = value)

p2<-ggplot(filter(df_plot_gini_diff,region!='Global'),aes(x=group,color=type,group = type))+
  geom_point(aes(y=median),position=position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = lower,ymax=upper),position=position_dodge(width = 0.5),width = 0.1, )+
  facet_wrap(vars(region),nrow=3,scales = 'free',labeller=as_labeller(~ifelse(.x=='Global',.x,region_label[toupper(.x)])))+
  geom_hline(aes(yintercept=0),color='red',linewidth=.2,linetype='dashed')+
  theme_bw()+
  scale_x_discrete(name=NULL,labels=c('1'='2026(no imple)-2019 ','2'='2026(status quo)-2026(no imple)'))+
  scale_color_lancet(name=NULL,labels=c('hosp'='RSV-associated hospital admissions','mort'='RSV-attributable deaths'))+
  labs(y='Difference in Gini coefficient',tag='B')+
  theme(strip.background = element_blank(),
        strip.text=element_text(size=19),
        text = element_text(size = 20),          # 全局字体大小
        axis.title = element_text(size = 18),    # 坐标轴标题
        axis.text.x = element_markdown(size=16,lineheight = 0.8),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        panel.grid.major = element_line(linewidth = .2),
        legend.position = 'none')
p2

p1<-ggplot(filter(df_plot_gini_diff,region=='Global'),aes(x=group,color=type,group = type))+
  geom_point(aes(y=median),position=position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = lower,ymax=upper),position=position_dodge(width = 0.5),width = 0.1, )+
  facet_wrap(vars(region),nrow=3,scales = 'free',labeller=as_labeller(~ifelse(.x=='Global',.x,region_label[toupper(.x)])))+
  geom_hline(aes(yintercept=0),color='red',linewidth=.2,linetype='dashed')+
  theme_bw()+
  scale_x_discrete(name=NULL,labels=c('1'='2026(no imple)-2019 ','2'='2026(status quo)-2026(no imple)'))+
  scale_color_lancet(name=NULL,labels=c('hosp'='RSV-associated hospital admissions','mort'='RSV-attributable deaths'))+
  labs(y='Difference in Gini coefficient',tag='A')+
  theme(strip.background = element_blank(),
        strip.text=element_text(size=19),
        text = element_text(size = 20),          # 全局字体大小
        axis.title = element_text(size = 18),    # axis titles
        axis.text.x = element_markdown(size=16,lineheight = 0.5),    # axis tick labels
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        panel.grid.major = element_line(linewidth = .2),
        legend.position = 'right')

legend <- get_legend_35(p1)
# Convert to a ggplot object (cowplot::ggdraw)
legend_plot <- ggdraw(legend)+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 50))


((p1+theme(legend.position = 'none')+plot_layout(width=c(3.42,3)))+legend_plot)/p2+
  plot_layout(heights = c(0.9,3))

ggsave('Figures/Diff_gini.tiff',width=8,height=9,dpi=200)

#Gini difference only show 2026 ----
## plot_ginidiff----
region_label

fontbase<-4
plot_ginidiff<-df_plot_gini_diff %>%
  filter(group==2) %>%
  mutate(region=ifelse(region!='Global',toupper(region),'Global'),
         region=factor(region,levels=c('Global','AFR','AMR','EMR','EUR','SEAR','WPR'))) %>%
  ggplot(aes(x=region,color=type,group = type)) +
  geom_point(aes(y=median),position=position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = lower,ymax=upper),position=position_dodge(width = 0.5),width = 0.1)+
  theme_bw()+
  labs(x='Region',y='Difference in Gini coefficient')+
  scale_y_continuous(expand = expansion(),limits = c(-0.2,0.4),breaks = seq(-0.2,0.4,0.1))+
  scale_color_lancet(name=NULL,labels=c('RSV-associated hospital admissions','RSV-attributable deaths'))+
  geom_hline(aes(yintercept=0),linetype='dashed',linewidth=.2,color='red')+
  theme(
    legend.position = 'top',
    panel.border = element_blank(),
    panel.grid.major = element_line(linewidth = 0.1),
    axis.line = element_line(linewidth = .2),
    axis.ticks = element_line(linewidth = .2),
    text = element_text(size=12+fontbase),
    axis.title = element_text(size=12+fontbase),
    legend.text = element_text(size=12+fontbase)
  )

plot_ginidiff
ggsave('Figures/Gini_diff_linerange.tiff',width=8,height=4.5,dpi=300)
ggsave('pdf/Gini_diff_linerange.pdf',width=9,height=9)


save.image('rda/code_11.6_Gini.RData')

df_plot_gini_diff %>%
  filter(group==2) %>%
  mutate(region=ifelse(region!='Global',toupper(region),'Global'),
         region=factor(region,levels=c('Global','AFR','AMR','EMR','EUR','SEAR','WPR'))) %>%
  filter(type=='mort',region %in% c('Global','AMR','EUR','WPR'))


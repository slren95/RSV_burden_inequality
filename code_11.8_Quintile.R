#
# ------------------------------------------------------------------------------
# Script: code_11.8_Quintile.R
# Purpose:
#   Quantify inequality using the Q5/Q1 ratio (5th vs 1st population-weighted
#   quintile) for projected RSV burden metrics, overall and by WHO region.
#
# Inputs:
#   - model_data/df_res.all.RDS (model results; filtered to the main analysis)
#   - rda/df_pop_center_nir.rds and model_data/df_pop_2019_2026.rds (metadata)
#   - functions.R
#
# Outputs:
#   - Quintile tables/plots created in the script (see export/plot calls).
#
# Usage:
#   source("code_11.8_Quintile.R")
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
library(ggsci)
library(scales)

theme_set(theme_classic(base_family = "Helvetica"))

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
df_res.main<-df_res.all %>% filter(params_str=="c(0.83, 0.81, 0.71, 0.77)_main")


## Cauculate median rate ----
df_res.median<-df_res.main %>%
  summarise(across(c(hr,mort,hr_pre,mt_pre,starts_with('pop_')),~median(.x,na.rm=T)),.by=c(ISOCountry)) %>%
  drop_na() %>%
  rename(hr_2019=hr,hr_2026=hr_pre,
         mt_2019=mort,mt_2026=mt_pre) %>%
  left_join(dplyr::transmute(df_pop,ISOCountry,WHORegion,Income=Income2019))

## Cauculate Quintiles ----
get_quintile<-function(rate,pop){
  df<-data.frame(rate=rate,pop=pop) %>%
    arrange(rate)
  rate<-df$rate
  pop<-df$pop
  cases<-c(0,rate*pop)
  pops<-c(0,pop)
  cum_cases<-cumsum(cases)
  cum_pop_pro<-cumsum(pops)/sum(pops)
  xout<-c(0.2,0.4,0.6,0.8,1.0)
  result <- approx(cum_pop_pro, cum_cases, xout = xout, method = "linear")
  round(diff(c(0,result$y)))
}

sum(get_quintile(df_res.median$mt_2019,df_res.median$pop_infant_2026))
sum(df_res.median$mt_2019*df_res.median$pop_infant_2026)

sum(unique(df_res.median$WHORegion) %>%
  map_dbl(~{
    df<-df_res.median %>% filter(WHORegion==.x)
    sum(get_quintile(df$mt_2019,df$pop_infant_2026))
  }))


df_quintile_long<-unique(c('.',df_res.median$WHORegion)) %>% setNames(str_replace(.,'\\.','Global')) %>%
  imap(~df_res.median %>%
        filter(str_detect(WHORegion,.x)) %>%
         mutate(Group=.y)) %>%
  map_dfr(~{
    df<-.x
    data.frame(hos_ctr=get_quintile(df$hr_2019,df$pop_infant_2026),
               hos_pre=get_quintile(df$hr_2026,df$pop_infant_2026),
               dea_ctr=get_quintile(df$mt_2019,df$pop_infant_2026),
               dea_pre=get_quintile(df$mt_2026,df$pop_infant_2026),
               quintile=paste0('q',1:5)) %>%
      pivot_longer(cols = -quintile,names_to = 'metric') %>%
      mutate(Group=df$Group[1])
  }) %>%
  separate(col=metric,into=c('hos_dea','scenario'),remove = F) %>%
  mutate(scenario= recode(scenario,'ctr'='no implementation scenario','pre'='June-2025 product use scenario'),
         scenario = factor(scenario,levels=c('no implementation scenario','June-2025 product use scenario')),
         hos_dea=factor(hos_dea,levels=c('hos','dea')),
         Group=toupper(Group))

df_quintile_wide<-df_quintile_long %>%
  pivot_wider(id_cols = c(Group,metric,hos_dea,scenario),names_from = quintile,values_from = value) %>%
  mutate(q51_ratio=round(q5/q1,1),sum=q1+q2+q3+q4+q5,
         ratio_text=sprintf("<span style='color:%s'>%s</span>",if_else(scenario=='no implementation scenario',ggsci::pal_lancet()(4)[3],ggsci::pal_lancet()(4)[4]),q51_ratio),
         ytext=q5*(1+.1)) %>%
  group_by(Group,hos_dea) %>%
  mutate(richtext=paste0(c('5th / 1st<br>',ratio_text),collapse = '  '))



df_quintile_long %>% 
  filter(Group=='GLOBAL') %>%
  ggplot(aes(quintile,value,fill = scenario))+
  geom_col(position = position_dodge(),width = .8,color='black',size=.1)+
  facet_wrap(vars(hos_dea),scales = 'free',labeller = as_labeller(c('dea'='RSV-attributable deaths','hos'='RSV-associated ALRI hospitalisation')))+
  scale_fill_manual(values=alpha(ggsci::pal_lancet()(4)[3:4],.5),name=NULL)+
  theme_bw()+
  scale_x_discrete(labels=c('1st','2nd','3rd','4th','5th'),name='Quantile of population')+
  scale_y_continuous(expand=expansion(mult=c(0,.1),c(0,0)),labels = label_number(big.mark = ","),name='Number of cases')+
  theme(strip.background = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(hjust = -.005,face = 'bold'),
        axis.line=element_line(linewidth = .2))

# Death ----

p1_dea<-df_quintile_long %>% 
  filter(Group=='GLOBAL',hos_dea=='dea') %>%
  ggplot(aes(quintile,value,fill = scenario))+
  geom_col(position = position_dodge(),width = .8,color='black',size=.1)+
  facet_wrap(vars(Group),scales = 'free',labeller = as_labeller(region_label))+
  scale_fill_manual(values=alpha(ggsci::pal_lancet()(4)[3:4],.6),name=NULL)+
  theme_bw()+
  scale_x_discrete(labels=c('1st','2nd','3rd','4th','5th'),name='Quantile of population')+
  scale_y_continuous(expand=expansion(mult=c(0,.1),c(0,0)),labels = label_number(big.mark = ","),name='RSV-attributable deaths')+
  theme(strip.background = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=8),
        axis.line=element_line(linewidth = .2),
        axis.ticks = element_line(linewidth = .2))+
  labs(tag='A')


p1_dea

p2_dea<-df_quintile_long %>% 
  filter(Group!='GLOBAL',hos_dea=='dea') %>%
  ggplot(aes(quintile,value,fill = scenario))+
  geom_col(position = position_dodge(),width = .8,color='black',size=.1)+
  facet_wrap(vars(Group),scales = 'free',labeller = as_labeller(region_label))+
  scale_fill_manual(values=alpha(ggsci::pal_lancet()(4)[3:4],.6),name=NULL)+
  theme_bw()+
  scale_x_discrete(labels=c('1st','2nd','3rd','4th','5th'),name='Quantile of population')+
  scale_y_continuous(expand=expansion(mult=c(0,.1),c(0,0)),labels = label_number(big.mark = ","),name='RSV-attributable deaths')+
  theme(strip.background = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=8),
        axis.line=element_line(linewidth = .2),
        legend.position = 'none',
        axis.ticks = element_line(linewidth = .2))+
  labs(tag='B')


p2_dea

legend <- get_legend_35(p1_dea)
# Convert to a ggplot object (cowplot::ggdraw)
legend_plot <- ggdraw(legend)+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = -30))


area_global  <- patchwork::area(1, 1, 1, 2)  # 左上图
area_spacer <- patchwork::area(2, 1, 2, 1)  # 左下 spacer
area_legend <- patchwork::area(2, 2, 2, 2)  # legend 单独放一格
area_byregion <- patchwork::area(1, 3, 2, 3)  # byregion 图，右边跨两行

(p1_dea+theme(legend.position = 'none')) + plot_spacer() + legend_plot + p2_dea +
plot_layout(design = c(area_global, area_spacer, area_legend, area_byregion),
            widths = c(0.05, 0.9, 3),  # 第二列就是 spacer，调宽度
            heights = c(1, 1),
            axis_titles = 'collect')

## Figure 4 no errorbar ----
ggsave('pdf/Quintile_dea.pdf',width = 10,height = 5,dpi=200)

# Hospitalisation ----

p1_hos<-df_quintile_long %>% 
  filter(Group=='GLOBAL',hos_dea=='hos') %>%
  ggplot(aes(quintile,value,fill = scenario))+
  geom_col(position = position_dodge(),width = .8,color='black',size=.1)+
  facet_wrap(vars(Group),scales = 'free',labeller = as_labeller(region_label))+
  scale_fill_manual(values=alpha(ggsci::pal_lancet()(4)[3:4],.6),name=NULL)+
  theme_bw()+
  scale_x_discrete(labels=c('1st','2nd','3rd','4th','5th'),name='Quantile of population')+
  scale_y_continuous(expand=expansion(mult=c(0,.1),c(0,0)),labels = label_number(big.mark = ","),name='RSV-associated ALRI hospitalisation')+
  theme(strip.background = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=8),
        axis.line=element_line(linewidth = .2),
        axis.ticks = element_line(linewidth = .2))+
  labs(tag='A')


p1_hos

p2_hos<-df_quintile_long %>% 
  filter(Group!='GLOBAL',hos_dea=='hos') %>%
  ggplot(aes(quintile,value,fill = scenario))+
  geom_col(position = position_dodge(),width = .8,color='black',size=.1)+
  facet_wrap(vars(Group),scales = 'free',labeller = as_labeller(region_label))+
  scale_fill_manual(values=alpha(ggsci::pal_lancet()(4)[3:4],.6),name=NULL)+
  theme_bw()+
  scale_x_discrete(labels=c('1st','2nd','3rd','4th','5th'),name='Quantile of population')+
  scale_y_continuous(expand=expansion(mult=c(0,.1),c(0,0)),labels = label_number(big.mark = ","),name='RSV-associated ALRI hospitalisation')+
  theme(strip.background = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=8),
        axis.line=element_line(linewidth = .2),
        legend.position = 'none',
        axis.ticks = element_line(linewidth = .2))+
  labs(tag='B')


p2_hos

legend <- get_legend_35(p1_dea)
# Convert to a ggplot object (cowplot::ggdraw)
legend_plot <- ggdraw(legend)+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = -30))


area_global  <- patchwork::area(1, 1, 1, 2)  # 左上图
area_spacer <- patchwork::area(2, 1, 2, 1)  # 左下 spacer
area_legend <- patchwork::area(2, 2, 2, 2)  # legend 单独放一格
area_byregion <- patchwork::area(1, 3, 2, 3)  # byregion 图，右边跨两行

(p1_hos+theme(legend.position = 'none')) + plot_spacer() + legend_plot + p2_hos +
  plot_layout(design = c(area_global, area_spacer, area_legend, area_byregion),
              widths = c(0.05, 0.9, 3),  # 第二列就是 spacer，调宽度
              heights = c(1, 1),
              axis_titles = 'collect')

ggsave('pdf/Quintile_hos.pdf',width = 10,height = 5,dpi=200)


df_quintile_wide %>%
  pivot_wider(id_cols = c(Group,hos_dea),names_from = scenario,values_from = q51_ratio) %>%
  arrange(hos_dea,Group)

# 1000 Q5-Q1 ratio ----
df_list<-df_res.main %>%
  left_join(dplyr::transmute(df_pop,ISOCountry,WHORegion,Income=Income2019)) %>%
  rename(hr_2019=hr,hr_2026=hr_pre,
         mt_2019=mort,mt_2026=mt_pre) %>%
  split(.,.$index) %>%
  map(~filter(.,!is.na(.x$pop_infant_2019),!is.na(.x$pop_infant_2026)))

df_quintile_long_1000<-imap_dfr(df_list,~{
  df_res.median<-.x
  unique(c('.',df_res.median$WHORegion)) %>% setNames(str_replace(.,'\\.','Global')) %>%
    imap(~df_res.median %>%
           filter(str_detect(WHORegion,.x)) %>%
           mutate(Group=.y)) %>%
    map_dfr(~{
      df<-.x
      data.frame(hos_ctr=get_quintile(df$hr_2019,df$pop_infant_2026),
                 hos_pre=get_quintile(df$hr_2026,df$pop_infant_2026),
                 dea_ctr=get_quintile(df$mt_2019,df$pop_infant_2026),
                 dea_pre=get_quintile(df$mt_2026,df$pop_infant_2026),
                 quintile=paste0('q',1:5)) %>%
        pivot_longer(cols = -quintile,names_to = 'metric') %>%
        mutate(Group=df$Group[1])
    }) %>%
    separate(col=metric,into=c('hos_dea','scenario'),remove = F) %>%
    mutate(scenario= recode(scenario,'ctr'='no implementation scenario','pre'='June-2025 product use scenario'),
           scenario = factor(scenario,levels=c('no implementation scenario','June-2025 product use scenario')),
           hos_dea=factor(hos_dea,levels=c('hos','dea')),
           Group=toupper(Group),
           index=.y)
}
)


df_qunintile_long_sum<-df_quintile_long_1000 %>%
  summarise(q025 = quantile(value, 0.025),
            q500 = quantile(value, 0.5),
            q975 = quantile(value, 0.975),
            .by = c(quintile,metric,Group))

df_quintile_wide_1000<-df_quintile_long_1000 %>%
  pivot_wider(id_cols = c(Group,metric,hos_dea,scenario,index),names_from = quintile,values_from = value) %>%
  mutate(q51_ratio=round(q5/q1,1))

df_quintile_wide_sum<-df_quintile_wide_1000 %>%
  reframe(across(q1:q51_ratio,list(`median`=~quantile(.x,0.5),`lower`=~quantile(.x,0.025),`upper`=~quantile(.x,0.975)),
                   .names = "{.col}_{.fn}"),
            .by = c(metric,Group,scenario,hos_dea))

df_text<-df_quintile_wide_sum %>%
  dplyr::select(metric:hos_dea,matches('q51_')) %>%
  set_names(.,~str_remove(.,'_ratio')) %>%
  mutate(ratio_text=sprintf('%.1f (%.1f-%.1f)',round(q51_median,1),round(q51_lower,1),round(q51_upper,1)))


df_plot<-df_quintile_wide_sum %>%
  dplyr::select(metric:hos_dea,matches('q._')) %>%
  pivot_longer(cols=matches('q._'),names_pattern = '(.*)_(.*)',names_to = c('quintile','.value')) %>%
  left_join(df_text)


p1_dea2<-df_plot %>% 
  filter(Group=='GLOBAL',hos_dea=='dea') %>%
  ggplot(aes(quintile,median,fill = scenario))+
  geom_col(position = position_dodge(),width = .8,color='black',size=.1)+
  geom_errorbar(aes(ymin=lower,ymax=upper),position = position_dodge(width = .8),width = .2,size=.3)+
  facet_wrap(vars(Group),scales = 'free',labeller = as_labeller(region_label))+
  scale_fill_manual(values=alpha(ggsci::pal_lancet()(4)[3:4],.6),name=NULL)+
  theme_bw()+
  scale_x_discrete(labels=c('Q1','Q2','Q3','Q4','Q5'),name='Quantile of population')+
  scale_y_continuous(expand=expansion(mult=c(0,.1),c(0,0)),labels = label_number(big.mark = ","),name='RSV-attributable deaths')+
  theme(strip.background = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=8),
        axis.line=element_line(linewidth = .2),
        axis.ticks = element_line(linewidth = .2))+
  labs(tag='A')

p2_dea2<-df_plot %>% 
  filter(Group!='GLOBAL',hos_dea=='dea') %>%
  ggplot(aes(quintile,median,fill = scenario))+
  geom_col(position = position_dodge(),width = .8,color='black',size=.1)+
  geom_errorbar(aes(ymin=lower,ymax=upper),position = position_dodge(width = .8),width = .2,size=.3)+
  facet_wrap(vars(Group),scales = 'free',labeller = as_labeller(region_label))+
  scale_fill_manual(values=alpha(ggsci::pal_lancet()(4)[3:4],.6),name=NULL)+
  theme_bw()+
  scale_x_discrete(labels=c('Q1','Q2','Q3','Q4','Q5'),name='Quantile of population')+
  scale_y_continuous(expand=expansion(mult=c(0,.1),c(0,0)),labels = label_number(big.mark = ","),name='RSV-attributable deaths')+
  theme(strip.background = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=8),
        axis.line=element_line(linewidth = .2),
        axis.ticks = element_line(linewidth = .2))+
  labs(tag='B')


legend <- get_legend_35(p1_dea2)
# Convert to a ggplot object (cowplot::ggdraw)
legend_plot <- ggdraw(legend)+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = -30))


area_global  <- patchwork::area(1, 1, 1, 2)  # 左上图
area_spacer <- patchwork::area(2, 1, 2, 1)  # 左下 spacer
area_legend <- patchwork::area(2, 2, 2, 2)  # legend 单独放一格
area_byregion <- patchwork::area(1, 3, 2, 3)  # byregion 图，右边跨两行

(p1_dea2+theme(legend.position = 'none')) + plot_spacer() + legend_plot + (p2_dea2+theme(legend.position = 'none')) +
  plot_layout(design = c(area_global, area_spacer, area_legend, area_byregion),
              widths = c(0.05, 0.9, 3),  # 第二列就是 spacer，调宽度
              heights = c(1, 1),
              axis_titles = 'collect')

## Figure 4 with errorbar ----
ggsave('pdf/Quintile_dea2.pdf',width = 10,height = 5,dpi=200)

(p1_dea2+theme(legend.position = 'none',
  strip.text = element_text(size=12),
  text = element_text(size=14),
  legend.text = element_text(size=13)
  )) + plot_spacer() + legend_plot + (p2_dea2+theme(legend.position = 'none',
                                                    strip.text = element_text(size=12),
                                                    text = element_text(size=14),
                                                    legend.text = element_text(size=13))) +
  plot_layout(design = c(area_global, area_spacer, area_legend, area_byregion),
              widths = c(0.05, 0.9, 3),  # 第二列就是 spacer，调宽度
              heights = c(1, 1),
              axis_titles = 'collect')

p1_dea2+theme(legend.position = 'top',strip.text = element_blank(),
              text=element_text(size=16))+labs(tag=NULL)

ggsave('pdf/Quintile_dea2_eposter.pdf',width = 13.5,height = 5,dpi=200)

df_text %>% pivot_wider(id_cols = c(metric,hos_dea),names_from = Group,values_from = ratio_text)

# metric  hos_dea GLOBAL           AMR             EMR            AFR           EUR              WPR            SEAR          
# <chr>   <fct>   <chr>            <chr>           <chr>          <chr>         <chr>            <chr>          <chr>         
# 1 hos_ctr hos     5.7 (4.0-9.8)    3.0 (2.0-6.0)   4.3 (2.6-8.8)  3.0 (2.4-4.2) 2.0 (1.8-2.3)    4.2 (3.2-6.9)  3.9 (1.5-10.3)
# 2 hos_pre hos     5.7 (4.0-9.9)    3.9 (2.3-8.1)   4.3 (2.6-8.8)  3.0 (2.4-4.2) 2.3 (1.9-2.9)    4.2 (3.0-6.8)  3.9 (1.5-10.3)
# 3 dea_ctr dea     18.3 (11.7-27.5) 5.8 (3.4-13.6)  6.8 (4.2-11.7) 4.4 (2.9-8.1) 12.7 (8.0-20.0)  6.8 (3.6-15.3) 1.9 (1.3-3.7) 
# 4 dea_pre dea     20.8 (13.0-31.7) 10.4 (5.0-25.2) 6.8 (4.2-11.7) 4.4 (2.9-8.1) 22.1 (13.1-35.7) 7.0 (3.7-15.9) 1.9 (1.3-3.7) 

# 1000 Q5-Q1 ratio GBD ----
df_res.all.GBD<-rio::import('model_data/df_res.all.GBD.RDS')

df_list.GBD<-df_res.all.GBD %>%
  left_join(dplyr::transmute(df_pop,ISOCountry,WHORegion,Income=Income2019)) %>%
  rename(hr_2019=hr,hr_2026=hr_pre,
         mt_2019=mort,mt_2026=mt_pre) %>%
  split(.,.$index) %>%
  map(~filter(.,!is.na(.x$pop_infant_2019),!is.na(.x$pop_infant_2026)))

df_quintile_long_1000.GBD<-imap_dfr(df_list.GBD,~{
  df_res.median<-.x
  unique(c('.',df_res.median$WHORegion)) %>% setNames(str_replace(.,'\\.','Global')) %>%
    imap(~df_res.median %>%
           filter(str_detect(WHORegion,.x)) %>%
           mutate(Group=.y)) %>%
    map_dfr(~{
      df<-.x
      data.frame(hos_ctr=get_quintile(df$hr_2019,df$pop_infant_2026),
                 hos_pre=get_quintile(df$hr_2026,df$pop_infant_2026),
                 dea_ctr=get_quintile(df$mt_2019,df$pop_infant_2026),
                 dea_pre=get_quintile(df$mt_2026,df$pop_infant_2026),
                 quintile=paste0('q',1:5)) %>%
        pivot_longer(cols = -quintile,names_to = 'metric') %>%
        mutate(Group=df$Group[1])
    }) %>%
    separate(col=metric,into=c('hos_dea','scenario'),remove = F) %>%
    mutate(scenario= recode(scenario,'ctr'='no implementation scenario','pre'='June-2025 product use scenario'),
           scenario = factor(scenario,levels=c('no implementation scenario','June-2025 product use scenario')),
           hos_dea=factor(hos_dea,levels=c('hos','dea')),
           Group=toupper(Group),
           index=.y)
}
)

df_qunintile_long_sum.GBD<-df_quintile_long_1000.GBD %>%
  summarise(q025 = quantile(value, 0.025),
            q500 = quantile(value, 0.5),
            q975 = quantile(value, 0.975),
            .by = c(quintile,metric,Group))

df_quintile_wide_1000.GBD<-df_quintile_long_1000.GBD %>%
  pivot_wider(id_cols = c(Group,metric,hos_dea,scenario,index),names_from = quintile,values_from = value) %>%
  mutate(q51_ratio=round(q5/q1,1))

df_quintile_wide_sum.GBD<-df_quintile_wide_1000.GBD %>%
  reframe(across(q1:q51_ratio,list(`median`=~quantile(.x,0.5),`lower`=~quantile(.x,0.025),`upper`=~quantile(.x,0.975)),
                 .names = "{.col}_{.fn}"),
          .by = c(metric,Group,scenario,hos_dea))

df_text.GBD<-df_quintile_wide_sum.GBD %>%
  dplyr::select(metric:hos_dea,matches('q51_')) %>%
  set_names(.,~str_remove(.,'_ratio')) %>%
  mutate(ratio_text=sprintf('%.1f (%.1f-%.1f)',round(q51_median,1),round(q51_lower,1),round(q51_upper,1)))


df_plot.GBD<-df_quintile_wide_sum.GBD %>%
  dplyr::select(metric:hos_dea,matches('q._')) %>%
  pivot_longer(cols=matches('q._'),names_pattern = '(.*)_(.*)',names_to = c('quintile','.value')) %>%
  left_join(df_text.GBD)

p1_dea2.GBD<-df_plot.GBD %>% 
  filter(Group=='GLOBAL',hos_dea=='dea') %>%
  ggplot(aes(quintile,median,fill = scenario))+
  geom_col(position = position_dodge(),width = .8,color='black',size=.1)+
  geom_errorbar(aes(ymin=lower,ymax=upper),position = position_dodge(width = .8),width = .2,size=.3)+
  facet_wrap(vars(Group),scales = 'free',labeller = as_labeller(region_label))+
  scale_fill_manual(values=alpha(ggsci::pal_lancet()(4)[3:4],.6),name=NULL)+
  theme_bw()+
  scale_x_discrete(labels=c('Q1','Q2','Q3','Q4','Q5'),name='Quantile of population')+
  scale_y_continuous(expand=expansion(mult=c(0,.1),c(0,0)),labels = label_number(big.mark = ","),name='RSV-attributable deaths')+
  theme(strip.background = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=8),
        axis.line=element_line(linewidth = .2),
        axis.ticks = element_line(linewidth = .2))+
  labs(tag='A')

p2_dea2.GBD<-df_plot.GBD %>% 
  filter(Group!='GLOBAL',hos_dea=='dea') %>%
  ggplot(aes(quintile,median,fill = scenario))+
  geom_col(position = position_dodge(),width = .8,color='black',size=.1)+
  geom_errorbar(aes(ymin=lower,ymax=upper),position = position_dodge(width = .8),width = .2,size=.3)+
  facet_wrap(vars(Group),scales = 'free',labeller = as_labeller(region_label))+
  scale_fill_manual(values=alpha(ggsci::pal_lancet()(4)[3:4],.6),name=NULL)+
  theme_bw()+
  scale_x_discrete(labels=c('Q1','Q2','Q3','Q4','Q5'),name='Quantile of population')+
  scale_y_continuous(expand=expansion(mult=c(0,.1),c(0,0)),labels = label_number(big.mark = ","),name='RSV-attributable deaths')+
  theme(strip.background = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=8),
        axis.line=element_line(linewidth = .2),
        axis.ticks = element_line(linewidth = .2))+
  labs(tag='B')


legend.GBD <- get_legend_35(p1_dea2.GBD)
# Convert to a ggplot object (cowplot::ggdraw)
legend_plot.GBD <- ggdraw(legend.GBD)+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = -30))


(p1_dea2.GBD+theme(legend.position = 'none')) + plot_spacer() + legend_plot + (p2_dea2.GBD+theme(legend.position = 'none')) +
  plot_layout(design = c(area_global, area_spacer, area_legend, area_byregion),
              widths = c(0.05, 0.9, 3),  # 第二列就是 spacer，调宽度
              heights = c(1, 1),
              axis_titles = 'collect')

ggsave('pdf/Quintile_dea2.GBD.pdf',width = 10,height = 5,dpi=200)

df_text.GBD %>% pivot_wider(id_cols = c(metric,hos_dea),names_from = Group,values_from = ratio_text)
# # A tibble: 4 × 9
# metric  hos_dea GLOBAL           AMR              EMR            AFR           EUR                 WPR              SEAR          
# <chr>   <fct>   <chr>            <chr>            <chr>          <chr>         <chr>               <chr>            <chr>         
# 1 hos_ctr hos     5.7 (4.0-9.8)    3.0 (2.0-6.0)    4.3 (2.6-8.8)  3.0 (2.4-4.2) 2.0 (1.8-2.3)       4.2 (3.2-6.9)    3.9 (1.5-10.3)
# 2 hos_pre hos     5.7 (4.0-9.9)    3.9 (2.3-8.1)    4.3 (2.6-8.8)  3.0 (2.4-4.2) 2.3 (1.9-2.9)       4.2 (3.0-6.8)    3.9 (1.5-10.3)
# 3 dea_ctr dea     35.5 (27.2-46.6) 17.4 (12.2-25.5) 8.4 (5.3-13.5) 5.7 (4.0-8.9) 301.9 (225.0-383.5) 17.9 (10.3-29.4) 2.1 (1.4-3.6) 
# 4 dea_pre dea     40.5 (30.4-53.7) 30.1 (21.2-45.0) 8.4 (5.3-13.5) 5.7 (4.0-8.9) 613.0 (384.7-760.6) 18.1 (10.5-30.0) 2.1 (1.4-3.6) 



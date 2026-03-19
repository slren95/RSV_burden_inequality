#
# ------------------------------------------------------------------------------
# Script: code_07_plot_for_incidence.R
# Purpose:
#   Visualise the estimated RSV-associated burden metrics (incidence,
#   hospitalisation, mortality, and derived ratios) and export combined result
#   objects used for reporting and downstream modelling.
#
# Inputs:
#   - rda/code_01_incidence.RData
#   - rda/code_02_hospitalisation.RData
#   - rda/code_03_Inc_to_hos_ratio.RData
#   - rda/code_04_in_hos_CFR.RData
#   - rda/code_05_overall_mortality.RData
#
# Outputs:
#   - `rda/res_all_burden_2019.rds` and other exports created via `export()`.
#   - Multiple figures under `plot/` created via `ggsave()`.
#
# Usage:
#   source("code_07_plot_for_incidence.R")
#
# Notes:
#   This is a heavy plotting script; run after scripts 01–05 have produced their
#   `.RData` outputs.
# ------------------------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(ggsci)
#library(waffle)
library(ggbump)
#library(ggradar)
library(patchwork)
library(ggforce)
library(cowplot)
library(ggnewscale)
library(ggpattern)
library(grid)
library(showtext)
library(ggbreak)
library(rio)
library(sf)
library(grid)
library(openxlsx)
library(gridExtra)
library(grid)
library(ggrepel)
library(ggtext)
library(scales)

font_add_google(name = "Roboto", family = "roboto")
showtext_auto()

load('rda/code_01_incidence.RData')
load('rda/code_02_hospitalisation.RData')
load('rda/code_03_Inc_to_hos_ratio.RData')
load('rda/code_04_in_hos_CFR.RData')
load('rda/code_05_overall_mortality.RData')


# 1.All Result ----
res_all<-list(com_rate_res_WHO %>%
                filter(AGEGR=='0-<12m') %>% dplyr::select(AGEGR,Group,IR.Com=IR.est,N.Com=N.est),
              hos_rate_res_WHO %>%
                filter(AGEGR=='0-<12m') %>% dplyr::select(AGEGR,Group,IR.Hos=IR.est,N.Hos=N.est),
              hos_mor_res_WHO %>% 
                ungroup() %>% dplyr::select(AGEGR,Group,
                                     IR.Mor_hos=IR.est,
                                     N.Mor_hos=N.est,
                                     CFR=prop.est,Pop),
              # com_mor_res_WHO → com_mor_res_WHO_with_Global
              com_mor_res_WHO_with_Global %>% transmute(AGEGR='0-<12m',Group=WHORegion,
                                            IR.Mor_all=m0012_rate.P500,
                                            N.Mor_all=m0012_N.P500,
                                            PAF=m0012_PAF.P500),
              inc2hos %>% filter(AGEGR=='0-<12m') %>%
                set_names(~str_replace(.x,'ratio','inc2hos')) %>% dplyr::select(Group,contains('inc2hos'))
) %>%
  reduce(~left_join(.x,.y))

# Check SEAR N.Hos 569
# AGEGR  Group   IR.Hos    N.Hos
# 0-<12m.1 0-<12m    Afr 12.60845  463.000
# 0-<12m.2 0-<12m    Amr 21.16270  302.000
# 0-<12m.3 0-<12m    Emr 29.92810  545.000
# 0-<12m.4 0-<12m    Eur 19.22779  205.000
# 0-<12m.5 0-<12m   Sear 16.94337  569.000
# 0-<12m.6 0-<12m    Wpr 16.91153  373.000
# 0-<12m.7 0-<12m Global 18.42075 2495.483

## ➡️ rda/res_all_burden_2019.rds ----
export(res_all,'rda/res_all_burden_2019.rds') # SEAR HOs 2019 569000


# 2.Burden Plot----
col_pal<-c('#c6dbef','#6baed6','#3182bd','#08519c') %>%
  setNames(c('IR.Com','IR.Hos','IR.Mor_hos','IR.Mor_all'))

reg_map<-c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific") %>%
  setNames(c('Afr','Amr','Emr','Eur','Sear','Wpr'))

df1<-res_all[1:6,c('Group','IR.Com','IR.Hos','IR.Mor_hos','IR.Mor_all')] %>%
  mutate_if(is.numeric,sqrt) %>%
  mutate(IR.Mor_hos2=IR.Mor_hos,
         Group=reg_map[Group]) %>%
  pivot_longer(!Group,names_to = 'rate_type') %>%
  mutate(r=value,
         x0=case_when(rate_type=='IR.Mor_all'~value,
                      rate_type=='IR.Mor_hos2'~value,
                      T~-value),
         linetype=ifelse(rate_type=='IR.Mor_hos2','dashed','solid'),
         name=ifelse(rate_type=='IR.Mor_hos2','IR.Mor_hos',rate_type)) %>%
  mutate(label=paste0(ifelse(str_detect(name,'IR.Mor'),sprintf('%.2f',round(value^2,2)),
                             round(value^2,0)),'/1000'),
         label1=ifelse(str_detect(name,'IR.Mor'),NA_character_,label),
         label2=ifelse(rate_type %in% c('IR.Mor_hos','IR.Mor_all'),label,NA_character_),
         x=case_when(name=='IR.Com'~1.88*r,
                     name=='IR.Hos'~1.78*r,
                     name=='IR.Mor_hos'~r,
                     name=='IR.Mor_all'~-r,
                     T~NA_integer_))

max(df1$value)

df1 %>%
  ggplot()+
  geom_circle(aes(x0=0,y0=-x0,r=r,fill=name,linetype=I(linetype)),linewidth=.1,alpha=1)+
  #geom_text(aes(x=x,y=y0,label=label1),parse = TRUE)+
  facet_wrap(vars(Group),nrow=2,scales = 'fixed')+
  scale_fill_brewer()+
  theme_bw()+
  theme(panel.grid = element_blank())


p1<-df1 %>%
  #filter(Group=='Amr') %>%
  ggplot()+
  geom_circle(aes(x0=0,y0=-x0,r=r,fill=name,linetype=I(linetype)),linewidth=.1,alpha=1)+
  geom_text(aes(x=0,y=x,label=label1))+
  facet_wrap(vars(Group),nrow=3)+
  scale_fill_manual(values=col_pal,name='',
                    breaks=names(col_pal),
                    labels=c('Incidence Rate','Hospitalisation Rate','In-hospital Mortality','Overall Mortality'))+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = 'null',
        strip.text = element_text(size = 15),
        strip.background = element_blank())+
  scale_x_continuous(labels = NULL,breaks = NULL,name=NULL)+
  scale_y_continuous(labels = NULL,breaks = NULL,name=NULL)

p1

p2<-df1 %>%
  #filter(Group=='Amr') %>%
  ggplot()+
  geom_segment(aes(x=0,y=sign(x)*.3,xend=1.4,yend=sign(x)*.3),color='gray20',linewidth=.2)+
  geom_circle(aes(x0=0,y0=-x0,r=r,fill=name,linetype=I(linetype)),linewidth=.2,alpha=1)+
  geom_segment(data=filter(df1,name=='IR.Mor_hos'),
               aes(x=r,y=r,xend=1.4,yend=sign(x)*.3),color='gray20',linewidth=.2)+
  geom_label(aes(x=1.4,y=sign(x)*.3,label=label2,fill=name),
             color='white',fontface = "bold",
             #label.padding = unit(0.1, "lines")
             )+
  facet_wrap(vars(Group),nrow=3)+
  scale_fill_manual(values=col_pal)+
  coord_equal()+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = 'null',
        strip.text = element_text(size = 15),
        strip.background = element_blank())+
  coord_cartesian(xlim=c(-1.85,1.85),ylim=c(-1.85,1.85))+
  scale_x_continuous(labels = NULL,breaks = NULL,name=NULL)+
  scale_y_continuous(labels = NULL,breaks = NULL,name=NULL)

p2

p3<-res_all %>%
  filter(Group !='Global') %>%
  mutate(y=IR.Com/IR.Hos,r1=sqrt(IR.Com)/10,r2=sqrt(IR.Hos)/10) %>%
  #arrange(desc(y)) %>%
  ggplot()+
  geom_circle(aes(x0=4*(1:6),y0=y+r1,r=r1),fill=col_pal[1],linewidth=.2)+
  geom_circle(aes(x0=4*(1:6),y0=y-r2,r=r2),fill=col_pal[2],linewidth=.2)+
  geom_segment(aes(x=4*(1:6)-1,y=y,xend=4*(1:6)+1,yend=y),linewidth=.2)+
  theme_bw()+
  scale_x_continuous(breaks = 4*(1:6),labels = names(reg_map))+
  scale_y_continuous(expand=expansion(),limits = c(0,12),
                     breaks=seq(0,12,3),labels=seq(0,12,3))+
  labs(x='',y='Rate Ratio')+
  theme(text=element_text(size=20),
        panel.grid = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.2),
        axis.ticks = element_line(size=.25))
p3
p4<-res_all %>%
  filter(Group !='Global') %>%
  ggplot()+
  geom_col(aes(x=1:6,y=CFR/100),color='gray30',fill='gray90',width=.5,linewidth=.2)+
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(add = c(0,0.001)))+
  scale_x_continuous(breaks=1:6,labels = names(reg_map))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x='',y='In-hospital CFR')+
  theme(text=element_text(size=20),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.2),
        axis.ticks = element_line(size=.25))
p4

p5<-res_all %>%
  filter(Group !='Global') %>%
  mutate(y=IR.Mor_all/IR.Mor_hos,r1=sqrt(IR.Mor_all)*2,r2=sqrt(IR.Mor_hos)*2) %>%
  #arrange(desc(y)) %>%
  ggplot()+
  geom_circle(aes(x0=6*(1:6),y0=y+r1,r=r1),fill=col_pal[4],linewidth=.2)+
  geom_circle(aes(x0=6*(1:6),y0=y-r2,r=r2),fill=col_pal[3],linewidth=.2)+
  geom_segment(aes(x=6*(1:6)-1,y=y,xend=6*(1:6)+1,yend=y),linewidth=.2)+
  theme_bw()+
  labs(x='',y='Rate Ratio')+
  scale_x_continuous(breaks = 6*(1:6),labels = names(reg_map))+
  scale_y_continuous(expand=expansion(),limits = c(-5,15),
                     breaks=seq(0,15,5),labels=seq(0,15,5))+
  theme(text=element_text(size=20),
        panel.grid = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.2),
        axis.ticks = element_line(size=.25))

p5

# p5+scale_y_break(c(15, 25))

# break and conbine
# p5.1<-p5+coord_cartesian(ylim=c(-5,15))
# p5.2<-p5+coord_cartesian(ylim=c(25,40))+
#   theme(axis.ticks.x = element_blank(),
#         axis.text.x = element_blank())
# 
# p512<-p5.2/p5.1+plot_layout(axes = "collect",heights = c(3,4)) &
#   theme(plot.margin = margin())
# p512

# Drop SEAR (estimate considered unreliable)

# p53<-res_all %>%
#   filter(Group !='Global') %>%
#   mutate(IR.Mor_all=ifelse(Group=='Sear',NA,IR.Mor_all)) %>%
#   mutate(y=IR.Mor_all/IR.Mor_hos,r1=sqrt(IR.Mor_all)*1.2,r2=sqrt(IR.Mor_hos)*1.2) %>%
#   #arrange(desc(y)) %>%
#   ggplot()+
#   geom_circle(aes(x0=3*(1:6),y0=y+r1,r=r1),fill=col_pal[4],linewidth=.2)+
#   geom_circle(aes(x0=3*(1:6),y0=y-r2,r=r2),fill=col_pal[3],linewidth=.2)+
#   geom_segment(aes(x=3*(1:6)-1,y=y,xend=3*(1:6)+1,yend=y),linewidth=.2)+
#   theme_bw()+
#   labs(x='',y='Rate Ratio')+
#   scale_x_continuous(breaks = 3*(1:6),labels = names(reg_map))+
#   scale_y_continuous(expand=expansion(),limits = c(-0.5,8),
#                      breaks=seq(0,5,1),labels=seq(0,5,1))+
#   theme(text=element_text(size=20),
#         panel.grid = element_blank(),
#         axis.ticks.length = unit(.15, "cm"),
#         axis.line = element_line(size=.2),
#         axis.ticks = element_line(size=.25))
# p53


df_legend<-data.frame(x=1,y=4:1,col=c('#c6dbef','#6baed6','#3182bd','#08519c'),
                       label=c('Incidence Rate','Hospitalisation Rate','In-hospital Mortality','Overall Mortality')) %>%
  mutate(size=ifelse(y==1,y+4.5,y+3))

p_legend<-ggplot(data=df_legend)+
  geom_point(aes(x=x,y=y,fill=I(col),size=I(size)),shape=21,color='black')+
  geom_text(aes(x=x,y=y+.3,label=label),hjust=.5,size=5)+
  theme_void()+
  theme(plot.margin = margin(l=3,r=3))

p1+p2+(p3/p4/p5)+
  (plot_spacer()+p_legend+plot_spacer()+plot_layout(nrow=3,heights = c(1,3,1)))+
  plot_layout(widths=c(3,3.3,3,1))+
  plot_annotation(tag_levels = list(c("A", "B", "C", "D", "E"))) &
  theme(plot.tag = element_text(size=20))

ggsave('plot/Rate_all.tiff',width=8,height=4.5,dpi=300)


# 3.Bump plot ----
df_bump<-list(
  res_all %>%
    filter(Group !='Global') %>%
    dplyr::select(Group,IR.Com,IR.Hos,IR.Mor_hos,IR.Mor_all) %>%
    setNames(c('Group',1:4)) %>%
    mutate_if(is.numeric,~cut(.x,breaks=10,include.lowest = F, labels = FALSE)) %>%
    pivot_longer(-Group,values_to = 'level'),
  res_all %>%
    filter(Group !='Global') %>%
    dplyr::select(Group,IR.Com,IR.Hos,IR.Mor_hos,IR.Mor_all) %>%
    setNames(c('Group',1:4)) %>%
    pivot_longer(-Group,values_to = 'value')) %>%
  reduce(~left_join(.x,.y)) %>%
  mutate(sqrt_value=sqrt(value)) %>%
  group_by(name,level) %>%
  arrange(value) %>%
  mutate(level2=row_number(),
         name=as.integer(name)) %>%
  add_count(level,name = 'n_level') %>%
  group_by(name) %>%
  mutate(value_to_max=value/max(value),
         level3=case_when(n_level==2 & level2==1~-0.5,
                          n_level==2 & level2==2~0.5,
                          n_level==3 & level2==1~-1,
                          n_level==3 & level2==2~0,
                          n_level==3 & level2==3~1,
                          n_level==1 & level2==1~0))
# Cut
df_cut<-c('IR.Com','IR.Hos','IR.Mor_hos','IR.Mor_all') %>%
  map_dfc(~res_all[res_all$Group!='Global',.x] %>%
            cut(breaks=10,include.lowest = F) %>%
            levels()) %>%
  setNames(c('IR.Com','IR.Hos','IR.Mor_hos','IR.Mor_all')) %>%
  mutate(id=1:10) %>%
  pivot_longer(cols = -id,values_to = 'cut') %>%
  separate(cut,sep = ',',into=c('left','right'),remove = F) %>%
  mutate(left=parse_number(left),
         right=parse_number(right)) %>%
  mutate(left=round(left,ifelse(str_detect(name,'IR.Mor'),2,0)),
         right=round(right,ifelse(str_detect(name,'IR.Mor'),2,0)),
         left=ifelse(str_detect(name,'IR.Mor'),sprintf('%.2f',left),sprintf('%.0f',left)),
         right=ifelse(str_detect(name,'IR.Mor'),sprintf('%.2f',right),sprintf('%.0f',right))) %>%
  pivot_wider(id_cols = id,values_from = c(left,right),names_from = name)

df_cutPoint<-data.frame(
  IR.Com=union(df_cut$left_IR.Com,df_cut$right_IR.Com),
  IR.Hos=union(df_cut$left_IR.Hos,df_cut$right_IR.Hos),
  IR.Mor_hos=union(df_cut$left_IR.Mor_hos,df_cut$right_IR.Mor_hos),
  IR.Mor_all=union(df_cut$left_IR.Mor_all,df_cut$right_IR.Mor_all)
) %>%
  mutate(label=paste(sprintf('%3s',IR.Com),IR.Hos,IR.Mor_hos,IR.Mor_all,sep=' / ')) %>%
  mutate(label=ifelse(row_number()==1,
                      paste0(label,'*'),label))


df_bump %>%
  ggplot() +
  geom_bump(aes(x = name, y = level+level3*.3, color = Group),size = 1) +
  geom_point(aes(x = name, y = level+level3*.3, color = Group,size=value_to_max),alpha=.8) +
  scale_color_lancet(alpha = .5,name=NULL,
                     labels = c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific"))+
  scale_x_continuous(breaks=1:4,label=c('Incidence rate','Hospital admission rate',
                                        'In-hospital mortality','Overall mortality'))+
  scale_y_continuous(breaks=seq(0.5,10.5),limits = c(0.5,10.5),labels=c(1:10,''),
                     expand = expansion(add=c(0,NA)),
                     sec.axis = sec_axis(~.,breaks = seq(-0.5,10.5),name='Rate(per 1000 person-years)',
                                         labels=c('',df_cutPoint$label)))+
  guides(size='none',
         color=guide_legend(nrow=1))+
  theme_bw()+
  labs(x='',y='Decile')+
  theme(panel.grid=element_blank(),
        axis.ticks.y = element_line(linewidth = .2),
        axis.text.y.left = element_text(vjust=-2.5,size=18),
        axis.text.y.right = element_text(size=18,vjust=-.5,hjust = .8),
        legend.position = 'top',
        legend.title = element_text(size=20),
        axis.text = element_text(size=22),
        text=element_text(size=21),
        plot.margin = margin(l=.5,r=.5,unit='cm'))

ggsave('plot/bump_plot.tiff',width=8,height=4.5,dpi=300)

# 4.HR for Model ----
## All Results for Hospitalization Rate ----
hr_all<-bind_rows(hos_rate_res_WHO %>%
                filter(AGEGR=='0-<12m',Group!='Global') %>% dplyr::select(AGEGR,Group,est,se,
                                                          HR.est=IR.est,HR.lci=IR.lci,HR.uci=IR.uci),
                hos_rate_res_WHO.income %>%
                  filter(AGEGR=='0-<12m') %>% dplyr::select(AGEGR,WHO,Income,est,se,
                                                            HR.est=IR.est,HR.lci=IR.lci,HR.uci=IR.uci),
) %>% relocate(WHO,Income,.after = Group) %>%
  mutate(Group2=ifelse(!is.na(Group),Group,paste(WHO,Income,sep='_')))

hr_all %>%
  filter(is.na(Group)) %>%
  ggplot(aes(WHO,HR.est,fill=Income))+
  geom_col(position = position_dodge2(preserve = 'single'),width=.5)+
  geom_errorbar(aes(ymin=HR.lci,ymax=HR.uci),position = position_dodge2(preserve = 'single',padding = .7),width=.5)+
  scale_fill_lancet(alpha = .8)


hr_all %>%
  filter(is.na(Group)) %>%
  ggplot(aes(WHO,HR.est,fill=Income))+
  geom_col(position = position_dodge(preserve = 'single'),width=.5)+
  geom_errorbar(aes(ymin=HR.lci,ymax=HR.uci),position = position_dodge(preserve = 'single',width=.5),width=.3)+
  scale_fill_lancet(alpha = .8)



#rio::export(hr_all,'rda/hr_all.rds')

## by income
hr_by_income<-tribble(~AGEGR,~Group2,~est,~se,
                      '0~<12m','H',-3.816001,0.13032389,
                      '0~<12m','L',-4.642504,0.33966432,
                      '0~<12m','LM',-4.047018,0.21239452,
                      '0~<12m','UM',-3.977516,0.31173764)

hr_all<-bind_rows(hr_all,hr_by_income)

hr_all %>%
  ggplot(aes(WHO,HR.est,fill=Income))+
  geom_col(position = position_dodge2(preserve = 'single'),width=.5)+
  geom_errorbar(aes(ymin=HR.lci,ymax=HR.uci),position = position_dodge2(preserve = 'single',padding = .7),width=.5)+
  scale_fill_lancet(alpha = .8)

rio::export(hr_all,'rda/hr_all.rds')

# Figure 2 Bar plot ----
res_all2<-list(com_rate_res_WHO %>%
       filter(AGEGR=='0-<12m') %>% dplyr::select(AGEGR,Group,
                                                 IR.Com=IR.est,
                                                 N.Com_est=N.est,N.Com_lci=N.lci,N.Com_uci=N.uci,
                                                 ),
     hos_rate_res_WHO %>%
       filter(AGEGR=='0-<12m') %>% dplyr::select(AGEGR,Group,
                                                 IR.Hos=IR.est,
                                                 N.Hos_est=N.est,N.Hos_lci=N.lci,N.Hos_uci=N.uci,
                                                 ),
     hos_mor_res_WHO %>% 
       ungroup() %>% dplyr::select(AGEGR,Group,
                                   IR.MorHos=IR.est,
                                   N.MorHos_est=N.est,N.MorHos_lci=N.lci,N.MorHos_uci=N.uci,
                                   CFR=prop.est,Pop),
     # com_mor_res_WHO → com_mor_res_WHO_with_Global
     com_mor_res_WHO_with_Global %>% transmute(AGEGR='0-<12m',Group=WHORegion,
                                               IR.MorAll=m0012_rate.P500,
                                               N.MorAll_est=m0012_N.P500,N.MorAll_lci=m0012_N.P025,N.MorAll_uci=m0012_N.P975,
                                               PAF=m0012_PAF.P500) %>%
       mutate(across(starts_with('N.'),~.x/1000)),
     inc2hos %>% filter(AGEGR=='0-<12m') %>%
       set_names(~str_replace(.x,'ratio','inc2hos')) %>% dplyr::select(Group,contains('inc2hos'))
) %>%
  reduce(~left_join(.x,.y))

res_all2 %>% 
  filter(Group!='Global') %>%
  select(Group,starts_with('N.')) %>%
  pivot_longer(-Group,names_pattern = 'N.(.*)_(.*)',
               names_to = c('metric','ci')) %>% 
  pivot_wider(id_cols = c(Group,metric),names_from = ci,values_from = value) %>%
  mutate(metric=factor(metric,levels=c('Com','Hos','MorHos','MorAll'),
                       labels=c('Incidence','Hospital admission','In-hospital death','Overall death'))) %>%
  ggplot(aes(str_to_upper(Group),est,fill=metric))+
  geom_col(aes(),width=.5)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=.2,linewidth=.2) +
  facet_wrap(~metric,ncol = 1,scales = 'free_y',strip.position="top")+
  scale_y_continuous(expand = expansion(mult=c(0,.2)))+
  scale_fill_manual(name = NULL, 
                        values = c('#c6dbef','#6baed6','gray70','gray40'),
                        #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
                    )+
  theme_bw()+
  theme(text = element_text(size = 14),  # 全局字体
        axis.title = element_text(size = 16),  # 坐标轴标题
        axis.text = element_text(size = 12),  # 坐标轴刻度
        legend.text = element_text(size = 14),  # 图例文字
        legend.title = element_text(size = 16),  # 图例标题
        strip.background = element_blank(),
        plot.margin = margin(t=8,b=5,l=5,r=5),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  labs(y='',x='')+
  theme(legend.position ='null')

plot_filter<-function(res_all2,metric='Incidence'){
  res_all2 %>% 
    filter(Group!='Global') %>%
    select(Group,starts_with('N.')) %>%
    pivot_longer(-Group,names_pattern = 'N.(.*)_(.*)',
                 names_to = c('metric','ci')) %>% 
    pivot_wider(id_cols = c(Group,metric),names_from = ci,values_from = value) %>%
    mutate(metric=factor(metric,levels=c('Com','Hos','MorHos','MorAll'),
                         labels=c('RSV-associated ALRI incidence','RSV-associated ALRI hospital admission','RSV-associated ALRI in-hospital death','RSV-attributable overall death'))) %>%
    filter(metric==.env$metric) %>%
    ggplot(aes(str_to_upper(Group),est,fill=metric))+
    geom_col(aes(),width=.5)+
    geom_errorbar(aes(ymin=lci,ymax=uci),width=.2,linewidth=.2) +
    facet_wrap(~metric,ncol = 1,scales = 'free_y',strip.position="top")+
    scale_y_continuous(expand = expansion(mult=c(0,.2)))+
    scale_fill_manual(name = NULL, 
                      values = setNames(c('#c6dbef','#6baed6','gray70','gray40'),
                                        c('RSV-associated ALRI incidence','RSV-associated ALRI hospital admission','RSV-associated ALRI in-hospital death','RSV-attributable overall death')),
                      #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
    )+
    theme_bw()+
    theme(text = element_text(size = 14),  # 全局字体
          axis.title = element_text(size = 16),  # 坐标轴标题
          axis.text = element_text(size = 12),  # 坐标轴刻度
          legend.text = element_text(size = 14),  # 图例文字
          legend.title = element_text(size = 16),  # 图例标题
          strip.background = element_blank(),
          strip.text = element_text(size = 14),
          plot.margin = margin(t=8,b=5,l=5,r=5),
          panel.grid=element_blank(),
          panel.border = element_blank(),
          axis.ticks.length = unit(.15, "cm"),
          axis.line = element_line(size=.25),
          axis.ticks = element_line(size=.25))+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    labs(y='',x='')+
    theme(legend.position ='null')
}

res_all3<-res_all2 %>%
  mutate(across(starts_with('N.'),~1000*.x))

p1<-plot_filter(res_all3,'RSV-associated ALRI incidence')+scale_y_continuous(expand = expansion(),limits = c(0,10000000),labels = scales::comma_format())
p2<-plot_filter(res_all3,'RSV-associated ALRI hospital admission')+scale_y_continuous(expand = expansion(),limits = c(0,1200000),breaks=0:4*300000,labels = scales::comma_format())
p3<-plot_filter(res_all3,'RSV-associated ALRI in-hospital death')+scale_y_continuous(expand = expansion(add=c(0,0)),limits = c(0,25000),breaks=0:5*5000,labels = scales::comma_format())
p4<-plot_filter(res_all3,'RSV-attributable overall death')+scale_y_continuous(expand = expansion(add=c(0,0)),limits = c(0,50000),breaks=0:5*10000,labels = scales::comma_format())


p1/p2/p3/p4+plot_layout(axes='collect_x',axis_titles = 'collect')

ggsave('pdf/burder_bar.pdf',width = 5,height = 8)


# Compute distance between circle centers
get_d <- function(r1, r2, prop) {
  target_function <- function(d) {
    A <- r1^2 * acos((d^2 + r1^2 - r2^2) / (2 * d * r1)) +
      r2^2 * acos((d^2 + r2^2 - r1^2) / (2 * d * r2)) -
      0.5 * sqrt((-d + r1 + r2) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2))
    (A - prop * pi * r2^2)^2
  }
  optimize(target_function, interval = c(abs(r1 - r2), r1 + r2))$minimum
}

get_d(12,3.3,0.2)

df11<-res_all2[1:6,c('Group','N.Com_est','N.Hos_est','N.MorHos_est','N.MorAll_est')] %>%
  set_names(~str_remove_all(.x,'(N.|_est)')) %>%
  mutate(r1=sqrt(Hos/pi),
         r2=sqrt(MorAll/pi),
         prop=MorHos/MorAll,
         d=NA_integer_,
         Group=str_to_upper(Group))

for(i in 1:nrow(df11)){
  df11[i,'d']=get_d(df11[i,'r1'],df11[i,'r2'],df11[i,'prop'])
}

df12<-df11 %>%
  mutate(y_center=r1+d) %>%
  pivot_longer(cols=c(Com,Hos,MorAll)) %>%
  mutate(r=sqrt(value/pi)) %>%
  mutate(x0=0,
         y0=case_when(name %in% c('Com','Hos')~r,
                      name == 'MorAll'~r1+d),
         y0_recenter=y0-y_center
         )

# Define a helper to compute the intersection of two circles
circle_intersection_polygon <- function(x1, y1, r1, x2, y2, r2) {
  # Create geometry for the two circles
  circle1 <- st_point(c(x1, y1)) %>%
    st_buffer(r1)
  
  circle2 <- st_point(c(x2, y2)) %>%
    st_buffer(r2)
  
  # Compute the intersection geometry
  intersection <- st_intersection(circle1, circle2)
  
  # If there is no intersection, return NULL
  if (st_is_empty(intersection)) {
    return(NULL)
  }
  
  # Extract polygon vertices for the intersection area
  polygon <- st_geometry(intersection)
  coords <- st_coordinates(polygon)
  
  return(coords)
}


df_list<-split(df12,df12$Group)

df_lens<-circle_intersection_polygon(
  df_list[[1]][[2,'x0']], df_list[[1]][[2,'y0']],df_list[[1]][[2,'r']],
  df_list[[1]][[3,'x0']], df_list[[1]][[3,'y0']],df_list[[1]][[3,'r']]
)

df_lens<-map_dfr(1:6,~{
  circle_intersection_polygon(
    df_list[[.x]][[2,'x0']], df_list[[.x]][[2,'y0']],df_list[[.x]][[2,'r']],
    df_list[[.x]][[3,'x0']], df_list[[.x]][[3,'y0']],df_list[[.x]][[3,'r']]
  ) %>%
    as.data.frame() %>%
    mutate(Group=df_list[[.x]][[1,'Group']])
})

df_lens_recenter<-map_dfr(1:6,~{
  circle_intersection_polygon(
    df_list[[.x]][[2,'x0']], df_list[[.x]][[2,'y0_recenter']],df_list[[.x]][[2,'r']],
    df_list[[.x]][[3,'x0']], df_list[[.x]][[3,'y0_recenter']],df_list[[.x]][[3,'r']]
  ) %>%
    as.data.frame() %>%
    mutate(Group=df_list[[.x]][[1,'Group']])
})


p_circle_main<-df12 %>%
  #filter(Group=='Amr') %>%
  ggplot()+
  geom_circle(aes(x0=0,y0=y0,r=r,fill=name),linewidth=.2,alpha=1)+
  facet_wrap(vars(Group),nrow=3)+
  coord_equal(xlim = c(-40,40),ylim=c(0,75))+
  scale_fill_manual(name = NULL, 
                    values = c('#c6dbef','#6baed6','gray40'),
                    #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )+
  geom_polygon(data=df_lens,aes(x = X, y = Y), fill = 'gray70')+
  theme_void()+
  theme(legend.position = 'null',
        panel.border=element_rect(fill='NA',color='black',linewidth = 0))

ggsave(plot=p_circle_main,'pdf/circle_main.pdf',width = 3,height = 6)

p_circle_zoom<-ggplot(df12)+
  geom_circle(aes(x0=0,y0=y0_recenter,r=r,fill=name),linewidth=.1)+
  facet_wrap(vars(Group),nrow=3)+
  coord_equal(xlim = c(-40,40),ylim=c(0,75)-25)+
  scale_fill_manual(name = NULL, 
                    values = c('#c6dbef','#6baed6','gray40'),
                    #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )+
  geom_polygon(data=df_lens_recenter,aes(x = X, y = Y), fill = 'gray70')+
  coord_cartesian(xlim=c(-1,1)*4,,ylim=c(-1,1)*5)+
  theme_void()+
  theme(legend.position = 'null',
        panel.border=element_rect(fill='NA',color='black',linewidth = 0))


ggsave(plot=p_circle_zoom,'pdf/circle_zoom.pdf',width = 3,height = 6)

c('#c6dbef','#6baed6','#3182bd','#08519c')

# Figure 2 Circle plot----

plot_main<-df12 %>%
  filter(Group=='AFR') %>%
  ggplot()+
  geom_circle(aes(x0=0,y0=y0,r=r,fill=name),linewidth=.2,alpha=1)+
  #facet_wrap(vars(Group),nrow=3)+
  coord_equal(xlim = c(-40,40),ylim=c(0,75),expand = FALSE)+
  scale_fill_manual(name = NULL, 
                    values = c('#c6dbef','#6baed6','gray40'),
                    #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )+
  geom_polygon(data=filter(df_lens,Group=='AFR'),aes(x = X, y = Y), fill = 'gray70')+
  labs(x=NULL,y=NULL)+
  theme(legend.position = 'none',
        panel.background = element_blank(),
        #plot.title = element_text(hjust = 0.5,size = 16),
        #panel.background = element_rect(color = 'gray70',fill=NA,linewidth = .5),
        plot.background = element_blank(),
        axis.text = element_blank(),        # 移除坐标轴文本（如果仍有空白可尝试）
        axis.ticks = element_blank(),       # 移除坐标轴刻度（可能影响空白）
        axis.title = element_blank(),       # 移除坐标轴标题
        plot.margin = margin())

plot_main

plot_sub<-df12 %>%
  filter(Group=='AFR') %>%
  ggplot()+
  geom_circle(aes(x0=0,y0=y0_recenter,r=r,fill=name),linewidth=.2,alpha=1)+
  #facet_wrap(vars(Group),nrow=3)+
  coord_equal(xlim = c(-40,40),ylim=c(0,75)-25)+
  scale_fill_manual(name = NULL, 
                    values = c('#c6dbef','#6baed6','gray40'),
                    #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )+
  geom_polygon(data=filter(df_lens_recenter,Group=='AFR'),aes(x = X, y = Y), fill = 'gray70')+
  coord_cartesian(xlim=c(-1,1)*4,,ylim=c(-1,1)*5)+
  theme_void()+
  theme(legend.position = 'none',
        #plot.margin = margin(1,1,1,1),
        panel.border = element_rect(color = 'gray40',fill=NA,linewidth = .5))

plot_sub


subvp <- viewport(width = 0.3, height = 0.5, x = 0.99, y = 0.99,just = c("right", "top"))


print(plot_main)
print(plot_sub, vp = subvp)


subvp2 <- viewport(width = 0.3, height = 0.5, x = 0.99, y = 0,just = c("right", "bottom"))

print(plot_main)
print(plot_sub, vp = subvp2)

pdf("pdf/burden_AFR.pdf", width = 8, height = 6)
grid.newpage()
print(plot_main, newpage = FALSE)
print(plot_sub, vp = subvp)
#grid.rect(gp = gpar(col = "gray20", lwd = 2)) 
dev.off()


unique(df12$Group) %>%
  walk(~{
    plot_main<-df12 %>%
      filter(Group==.x) %>%
      ggplot()+
      geom_circle(aes(x0=0,y0=y0,r=r,fill=name),linewidth=.2,alpha=1)+
      #facet_wrap(vars(Group),nrow=3)+
      coord_equal(xlim = c(-40,40),ylim=c(0,75),expand = FALSE)+
      scale_fill_manual(name = NULL, 
                        values = c('#c6dbef','#6baed6','gray40'),
                        #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
      )+
      geom_polygon(data=filter(df_lens,Group==.x),aes(x = X, y = Y), fill = 'gray70')+
      labs(x=NULL,y=NULL)+
      theme(legend.position = 'none',
            panel.background = element_blank(),
            #plot.title = element_text(hjust = 0.5,size = 16),
            #panel.background = element_rect(color = 'gray70',fill=NA,linewidth = .5),
            plot.background = element_blank(),
            axis.text = element_blank(),        # 移除坐标轴文本（如果仍有空白可尝试）
            axis.ticks = element_blank(),       # 移除坐标轴刻度（可能影响空白）
            axis.title = element_blank(),       # 移除坐标轴标题
            plot.margin = margin())
    
    plot_main
    
    plot_sub<-df12 %>%
      filter(Group==.x) %>%
      ggplot()+
      geom_circle(aes(x0=0,y0=y0_recenter,r=r,fill=name),linewidth=.2,alpha=1)+
      #facet_wrap(vars(Group),nrow=3)+
      coord_equal(xlim = c(-40,40),ylim=c(0,75)-25)+
      scale_fill_manual(name = NULL, 
                        values = c('#c6dbef','#6baed6','gray40'),
                        #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
      )+
      geom_polygon(data=filter(df_lens_recenter,Group==.x),aes(x = X, y = Y), fill = 'gray70')+
      coord_cartesian(xlim=c(-1,1)*4,,ylim=c(-1,1)*5)+
      theme_void()+
      theme(legend.position = 'none',
            #plot.margin = margin(1,1,1,1),
            panel.border = element_rect(color = 'gray40',fill=NA,linewidth = .5))
    
    plot_sub
    
    pdf(sprintf("pdf/burden_%s.pdf",.x), width = 8, height = 6)
    grid.newpage()
    print(plot_main, newpage = FALSE)
    print(plot_sub, vp = subvp)
    #grid.rect(gp = gpar(col = "gray20", lwd = 2)) 
    dev.off()
  })

offset=20
unique(df12$Group) %>%
  walk(~{
    plot_main<-df12 %>%
      filter(Group==.x) %>%
      ggplot()+
      geom_circle(aes(x0=0-offset,y0=y0,r=r,fill=name),linewidth=.2,alpha=1)+
      #facet_wrap(vars(Group),nrow=3)+
      coord_equal(xlim = c(-40,40)-offset,ylim=c(0,75),expand = FALSE)+
      scale_fill_manual(name = NULL, 
                        values = c('#c6dbef','#6baed6','gray40'),
                        #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
      )+
      geom_polygon(data=filter(df_lens,Group==.x),aes(x = X-offset, y = Y), fill = 'gray70')+
      labs(x=NULL,y=NULL)+
      theme(legend.position = 'none',
            panel.background = element_blank(),
            #plot.title = element_text(hjust = 0.5,size = 16),
            #panel.background = element_rect(color = 'gray70',fill=NA,linewidth = .5),
            plot.background = element_blank(),
            axis.text = element_blank(),        # 移除坐标轴文本（如果仍有空白可尝试）
            axis.ticks = element_blank(),       # 移除坐标轴刻度（可能影响空白）
            axis.title = element_blank(),       # 移除坐标轴标题
            plot.margin = margin())
    
    plot_main
    
    plot_sub<-df12 %>%
      filter(Group==.x) %>%
      ggplot()+
      geom_circle(aes(x0=0,y0=y0_recenter,r=r,fill=name),linewidth=.2,alpha=1)+
      #facet_wrap(vars(Group),nrow=3)+
      coord_equal(xlim = c(-40,40),ylim=c(0,75)-25)+
      scale_fill_manual(name = NULL, 
                        values = c('#c6dbef','#6baed6','gray40'),
                        #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
      )+
      geom_polygon(data=filter(df_lens_recenter,Group==.x),aes(x = X, y = Y), fill = 'gray70')+
      coord_cartesian(xlim=c(-1,1)*4,,ylim=c(-1,1)*5)+
      theme_void()+
      theme(legend.position = 'none',
            #plot.margin = margin(1,1,1,1),
            panel.border = element_rect(color = 'gray40',fill=NA,linewidth = .5))
    
    plot_sub
    
    subvp2 <- viewport(width = 0.25, height = 0.5, x = 0.99, y = 0,just = c("right", "bottom"))
    
    pdf(sprintf("pdf/burden_%s_subvp2.pdf",.x), width = 8, height = 6)
    grid.newpage()
    print(plot_main, newpage = FALSE)
    print(plot_sub, vp = subvp2)
    #grid.rect(gp = gpar(col = "gray20", lwd = 2)) 
    dev.off()
  })

# Figure 2 Circle plot method 2 ----

plot_main<-df12 %>%
  #filter(Group=='AFR') %>%
  ggplot()+
  geom_circle(aes(x0=0,y0=y0,r=r,fill=name),linewidth=.2,alpha=1)+
  facet_wrap(vars(Group),nrow=3)+
  coord_equal(xlim = c(-40,40),ylim=c(0,75),expand = FALSE)+
  scale_fill_manual(name = NULL, 
                    values = c('#c6dbef','#6baed6','gray40'),
                    #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )+
  geom_polygon(data=filter(df_lens),aes(x = X, y = Y), fill = 'gray70')+
  labs(x=NULL,y=NULL)+
  theme(legend.position = 'none',
        panel.background = element_blank(),
        #plot.title = element_text(hjust = 0.5,size = 16),
        panel.border = element_rect(color = 'gray20',fill=NA,linewidth = .5,linetype = 'solid'),
        plot.background = element_blank(),
        axis.text = element_blank(),        # 移除坐标轴文本（如果仍有空白可尝试）
        axis.ticks = element_blank(),       # 移除坐标轴刻度（可能影响空白）
        axis.title = element_blank(),       # 移除坐标轴标题
        plot.margin = margin(),
        strip.text = element_text(size = 15),
        strip.background = element_blank())

plot_main

plot_main_m2<-df12 %>%
  #filter(Group=='AFR') %>%
  ggplot()+
  geom_circle(aes(x0=0,y0=y0_recenter,r=r,fill=name),linewidth=.2,alpha=1)+
  facet_wrap(vars(Group),nrow=3)+
  coord_equal(xlim = c(-40,40),ylim=c(-5,75)-25,expand = FALSE)+
  scale_fill_manual(name = NULL, 
                    values = c('#c6dbef','#6baed6','gray40'),
                    #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )+
  geom_polygon(data=filter(df_lens_recenter),aes(x = X, y = Y), fill = 'gray70')+
  labs(x=NULL,y=NULL)+
  theme(legend.position = 'none',
        panel.background = element_blank(),
        #plot.title = element_text(hjust = 0.5,size = 16),
        panel.border = element_rect(color = 'gray20',fill=NA,linewidth = .5,linetype = 'solid'),
        plot.background = element_blank(),
        axis.text = element_blank(),        # 移除坐标轴文本（如果仍有空白可尝试）
        axis.ticks = element_blank(),       # 移除坐标轴刻度（可能影响空白）
        axis.title = element_blank(),       # 移除坐标轴标题
        plot.margin = margin(),
        strip.text = element_text(size = 15),
        strip.background = element_blank())

plot_main_m2

plot_zoom<-plot_main_m2+
  coord_cartesian(xlim=c(-3.5,3.5),ylim=c(-6,6))
plot_zoom

ggsave(plot=plot_main,'pdf/plot_main.pdf',width = 8,height = 6)
ggsave(plot=plot_zoom,'pdf/plot_zoom.pdf',width = 4,height = 6)


# Figure 3 (Rate point plot) ----
## by region ----
res_all3<-list(com_rate_res_WHO %>%
                 filter(AGEGR=='0-<12m') %>% dplyr::select(AGEGR,Group,
                                                           IR.Com_est=IR.est,IR.Com_lci=IR.lci,IR.Com_uci=IR.uci,
                                                           N.Com_est=N.est,N.Com_lci=N.lci,N.Com_uci=N.uci,
                 ),
               hos_rate_res_WHO %>%
                 filter(AGEGR=='0-<12m') %>% dplyr::select(AGEGR,Group,
                                                           IR.Hos_est=IR.est,IR.Hos_lci=IR.lci,IR.Hos_uci=IR.uci,
                                                           N.Hos_est=N.est,N.Hos_lci=N.lci,N.Hos_uci=N.uci,
                 ),
               hos_mor_res_WHO %>% 
                 ungroup() %>% dplyr::select(AGEGR,Group,
                                             IR.MorHos_est=IR.est,IR.MorHos_lci=IR.lci,IR.MorHos_uci=IR.uci,
                                             N.MorHos_est=N.est,N.MorHos_lci=N.lci,N.MorHos_uci=N.uci,
                                             CFR=prop.est,Pop),
               # com_mor_res_WHO → com_mor_res_WHO_with_Global
               com_mor_res_WHO_with_Global %>% transmute(AGEGR='0-<12m',Group=WHORegion,
                                                         IR.MorAll_est=m0012_rate.P500,IR.MorAll_lci=m0012_rate.P025,IR.MorAll_uci=m0012_rate.P975,
                                                         N.MorAll_est=m0012_N.P500,N.MorAll_lci=m0012_N.P025,N.MorAll_uci=m0012_N.P975,
                                                         PAF=m0012_PAF.P500) %>%
                 mutate(across(starts_with('N.'),~.x/1000)),
               inc2hos %>% filter(AGEGR=='0-<12m') %>%
                 set_names(~str_replace(.x,'ratio','inc2hos')) %>% dplyr::select(Group,contains('inc2hos'))
) %>%
  reduce(~left_join(.x,.y))

#### 1. Inc VS Hos ----
res_all3 %>%
  filter(Group!='Global') %>%
  select(Group,starts_with('IR.')) %>%
  set_names(~str_remove(.x,'IR.')) %>%
  ggplot(aes(Com_est,Hos_est,color=Group)) +
  geom_vline(xintercept = 94.6, linetype = "dashed")+
  geom_hline(yintercept = 15.9, linetype = "dashed")+
  geom_point(shape=19,size=2)+
  geom_errorbarh(aes(xmin = Com_lci, xmax = Com_uci))+
  geom_errorbar(aes(ymin = Hos_lci, ymax = Hos_uci))+
  scale_x_continuous(name = "Incidence rate (case per 1000)",
                     expand=expansion(add=c(0,10)),limits=c(0,250),
                     breaks=seq(0,250,50),labels=seq(0,250,50))+
  scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                     expand=expansion(),limits=c(0,70),
                     breaks=seq(0,70,10),labels=seq(0,70,10))+
  scale_colour_manual(name = NULL, 
                      values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                      labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )+
  theme_bw()+
  theme(text =element_text(size=20),
        #legend.position = c(.995,.99),
        legend.position='NULL',
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25),
        axis.title = element_blank()
        )


ggsave('pdf/Inc_vs_Hos.pdf',width = 8,height = 6)

#### 2. Inhos Death VS All Death ----

res_all3 %>%
  filter(Group!='Global') %>%
  select(Group,starts_with('IR.')) %>%
  set_names(~str_remove(.x,'IR.')) %>%
  ggplot(aes(MorAll_est,MorHos_est,color=Group)) +
  geom_point(shape=19,size=2)+
  geom_linerange(aes(xmin = MorAll_lci, xmax = MorAll_uci))+
  geom_linerange(aes(ymin = MorHos_lci, ymax = MorHos_uci))+
  geom_vline(xintercept = 0.487, linetype = "dashed")+
  geom_hline(yintercept = 0.203, linetype = "dashed")+
  scale_x_continuous(name = "Incidence rate (case per 1000)",
                     expand=expansion(add=c(0,.05)),limits=c(0,1.5),
                     breaks=seq(0,1.5,.3),labels=seq(0,1.5,.3))+
  scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                     expand=expansion(),limits=c(0,1.2),
                     breaks=seq(0,1.2,.3),labels=seq(0,1.2,.3))+
  theme_bw()+
  theme(text =element_text(size=20),
        #legend.position = c(.995,.99),
        legend.position='NULL',
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25),
        axis.title = element_blank())+
  scale_colour_manual(name = NULL, 
                      values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                      #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )

ggsave('pdf/AllDeath_vs_HosDeath.pdf',width = 8,height = 6)

#### 3. Hos VS All Death ----

res_all3 %>%
  filter(Group!='Global') %>% 
  select(Group,starts_with('IR.Hos'),starts_with('IR.MorAll')) %>%
  set_names(~str_remove(.x,'IR.')) %>%
  ggplot(aes(Hos_est,MorAll_est,color=Group)) +
  geom_point(shape=19,size=2)+
  geom_linerange(aes(xmin = Hos_lci, xmax = Hos_uci))+
  geom_linerange(aes(ymin = MorAll_lci, ymax = MorAll_uci))+
  geom_vline(xintercept = 15.9, linetype = "dashed")+
  geom_hline(yintercept = 0.487, linetype = "dashed")+
  scale_x_continuous(name = "Incidence rate (case per 1000)",
                     expand=expansion(add=c(0,0)),limits=c(0,40),
                     breaks=seq(0,40,10),labels=seq(0,40,10))+
  scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                     expand=expansion(),limits=c(0,1.5),
                     breaks=seq(0,1.5,.3),labels=seq(0,1.5,.3))+
  annotate("segment",x=14.1,xend=40,y=0.56791899,yend=0.56791899,color='#E6B800',linewidth = .5,
               arrow=arrow(type="open",length =unit(0.1,"inches")))+
  theme_bw()+
  theme(text =element_text(size=20),
        #legend.position = c(.995,.99),
        legend.position='NULL',
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        plot.margin = margin(r=10,t=10),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25),
        axis.title = element_blank())+
  scale_colour_manual(name = NULL, 
                      values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                      #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )

ggsave('pdf/Hos_vs_AllDeath.pdf',width = 8,height = 6)

### K Means ----
df<-res_all3 %>%
  filter(Group!='Global') %>% 
  transmute(Region=Group,HR=IR.Hos_est,MR=IR.MorAll_est)

# Standardise data (HR and MR may be on different scales)
df_scaled <- df %>% 
  select(HR, MR) %>% 
  scale()

# Choose K (elbow method)
wss <- sapply(1:5, function(k) kmeans(df_scaled, centers = k, nstart = 10)$tot.withinss)

png("Figures/K-means-K.png", width = 800, height = 500, units = "px", res = 200)

# Adjust margins (reduce whitespace)
par(mar = c(4, 4, 2, 1))

# Plot
plot(1:5, wss, type = "b", pch = 19, frame = FALSE, col = ifelse(1:5 == 3, "red", "black"),
     xlab = "Number of Clusters (K)", ylab = "Total Within-Cluster SS",
     cex.lab = 1.5, cex.axis = 1.2)  # 增加轴标签和坐标轴字体大小

dev.off()


# Run K-means clustering
set.seed(123)
kmeans_result <- kmeans(df_scaled, centers = 3, nstart = 10)

# Add cluster labels
df$Cluster <- as.factor(kmeans_result$cluster)

# Draw cluster boundaries
ggplot(df, aes(x = HR, y = MR, color = Cluster, fill = Cluster)) +
  geom_point(size = 4) +
  stat_ellipse(geom = "polygon", alpha = 0.2) +  # 画大圈
  geom_text(aes(label = toupper(Region)), vjust = -1,show.legend = F,size=10) +  # 标注地区名称
  theme_bw() +
  labs(x='Hospital admission rate (cases per 1000)',y='Overall mortality (cases per 1000)')+
  scale_x_continuous(limits = c(0,30))+
  scale_y_continuous(limits=c(0,1.1),breaks=0:4*0.25)+
  theme(panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25),
        text=element_text(size=30))

ggsave('Figures/k-means.png',,width = 8,height = 4.5,dpi=200)


## by region income ----

res_all3_income<-list(
  com_rate_res_WHO.income %>%
    filter(AGEGR=='0-<12m') %>% dplyr::select(WHORegion=WHO,Income,
                                              IR.Com_est=IR.est,IR.Com_lci=IR.lci,IR.Com_uci=IR.uci
    ),
  hos_rate_res_WHO.income %>%
    filter(AGEGR=='0-<12m') %>% dplyr::select(WHORegion=WHO,Income,
                                              IR.Hos_est=IR.est,IR.Hos_lci=IR.lci,IR.Hos_uci=IR.uci
    ),
  hos_mor_res_WHO.income_with_IR %>% 
    separate(Group,into = c('WHORegion','Income'),sep='-') %>%
    ungroup() %>% dplyr::select(WHORegion,Income,IR.MorHos_est=IR.est,IR.MorHos_lci=IR.lci,IR.MorHos_uci=IR.uci,
                                N.MorHos_est=N.est,N.MorHos_lci=N.lci,N.MorHos_uci=N.uci,
                                CFR=prop.est,Pop),
  # com_mor_res_WHO → com_mor_res_WHO_with_Global
  com_mor_res_WHO.income %>% transmute(WHORegion,Income=Income2019,
                                       IR.MorAll_est=m0012_rate.P500,IR.MorAll_lci=m0012_rate.P025,IR.MorAll_uci=m0012_rate.P975,
                                       N.MorAll_est=m0012_N.P500,N.MorAll_lci=m0012_N.P025,N.MorAll_uci=m0012_N.P975,
                                       PAF=m0012_PAF.P500) %>%
    mutate(across(starts_with('N.'),~.x/1000))
) %>%
  reduce(~left_join(.x,.y))

#### 1. Inc_vs_Hos_income ----
res_all3_income %>%
  select(WHORegion,Income,starts_with('IR.')) %>%
  set_names(~str_remove(.x,'IR.')) %>%
  mutate(Income=paste0(Income,'ICs'),
         Income=factor(Income,levels=c('LICs','LMICs','UMICs','HICs'))) %>%
  ggplot(aes(x = Com_est, y = Hos_est,color=WHORegion))+
  geom_vline(xintercept = 94.6, linetype = "dashed")+
  geom_hline(yintercept = 15.9, linetype = "dashed")+
  geom_linerange(aes(xmin = Com_lci, xmax = Com_uci))+
  geom_linerange(aes(ymin = Hos_lci, ymax = Hos_uci))+
  geom_point(shape=21,size=5,aes(fill=Income))+
  scale_x_continuous(expand=expansion(add=c(0,NA)),limits = c(0,NA),
                     breaks=seq(0,400,100))+
  scale_y_continuous(expand=expansion(add=c(0,NA)),limits = c(0,NA),
                     breaks=seq(0,100,25))+
  # scale_x_continuous(name = "Incidence rate (case per 1000)",
  #                    expand=expansion(add=c(0,10)),limits=c(0,250),
  #                    breaks=seq(0,250,50),labels=seq(0,250,50))+
  # scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
  #                    expand=expansion(),limits=c(0,70),
  #                    breaks=seq(0,70,10),labels=seq(0,70,10))+
  scale_colour_manual(name = NULL, 
                      values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                      #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )+
  scale_fill_manual(values=c('gray0','gray30','gray80','white'),name=NULL)+
  theme_bw()+
  theme(text =element_text(size=20),
        #legend.position = c(.995,.99),
        legend.position='NULL',
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25),
        axis.title = element_blank())+
  guides(colour=guide_legend(override.aes = list(size=1)))

ggsave('pdf/Inc_vs_Hos_Income.pdf',width = 8,height = 6)


#### 2. AllDeath_vs_HosDeath_income ----
res_all3_income %>%
  select(WHORegion,Income,starts_with('IR.')) %>%
  set_names(~str_remove(.x,'IR.')) %>%
  mutate(Income=paste0(Income,'ICs'),
         Income=factor(Income,levels=c('LICs','LMICs','UMICs','HICs'))) %>%
  ggplot(aes(x = MorHos_est, y = MorAll_est,color=WHORegion))+
  geom_vline(xintercept = 0.487, linetype = "dashed")+
  geom_hline(yintercept = 0.203, linetype = "dashed")+
  geom_linerange(aes(xmin = MorHos_lci, xmax = MorHos_uci))+
  geom_linerange(aes(ymin = MorAll_lci, ymax = MorAll_uci))+
  geom_point(shape=21,size=5,aes(fill=Income))+
  scale_x_continuous(name='',breaks=seq(0,3,.5),labels=seq(0,3,.5),
                     expand=expansion())+
  scale_y_continuous(name='',
                     expand=expansion(),limits=c(0,1.2),
                     breaks=seq(0,1.2,.3),labels=seq(0,1.2,.3))+
  # scale_x_continuous(name = "Incidence rate (case per 1000)",
  #                    expand=expansion(add=c(0,.05)),limits=c(0,1.5),
  #                    breaks=seq(0,1.5,.3),labels=seq(0,1.5,.3))+
  # scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
  #                    expand=expansion(),limits=c(0,1.2),
  #                    breaks=seq(0,1.2,.3),labels=seq(0,1.2,.3))+
  scale_colour_manual(name = NULL, 
                      values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                      #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )+
  scale_fill_manual(values=c('gray0','gray30','gray80','white'),name=NULL)+
  theme_bw()+
  theme(text =element_text(size=20),
        #legend.position = c(.995,.99),
        legend.position='NULL',
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25),
        axis.title = element_blank())+
  guides(colour=guide_legend(override.aes = list(size=1)))

ggsave('pdf/AllDeath_vs_HosDeath_Income.pdf',width = 8,height = 6)

#### 3. Hos_vs_AllDeath_income ----
res_all3_income %>%
  dplyr::select(WHORegion,Income,starts_with('IR.Hos'),starts_with('IR.MorAll')) %>%
  set_names(~str_remove(.x,'IR.')) %>%
  mutate(Income=paste0(Income,'ICs'),
         Income=factor(Income,levels=c('LICs','LMICs','UMICs','HICs'))) %>%
  ggplot(aes(x = Hos_est, y = MorAll_est,color=WHORegion))+
  geom_vline(xintercept = 15.9, linetype = "dashed")+
  geom_hline(yintercept = 0.487, linetype = "dashed")+
  geom_linerange(aes(xmin = Hos_lci, xmax = Hos_uci))+
  geom_linerange(aes(ymin = MorAll_lci, ymax = MorAll_uci))+
  geom_segment(data=res_all3_income %>%
                      dplyr::select(WHORegion,Income,starts_with('IR.Hos'),starts_with('IR.MorAll')) %>%
                      set_names(~str_remove(.x,'IR.')) %>%
                  filter(Hos_uci>40),
               aes(x=Hos_lci,xend=40,y=MorAll_est,yend=MorAll_est,color=WHORegion),
               arrow=arrow(type="open",length =unit(0.1,"inches")),show.legend = F)+
  geom_point(shape=21,size=5,aes(fill=Income))+
  scale_x_continuous(name = "Incidence rate (case per 1000)",
                     expand=expansion(add=c(0,0)),limits=c(0,40),
                     breaks=seq(0,40,10),labels=seq(0,40,10))+
  scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                     expand=expansion(),limits=c(0,1.5),
                     breaks=seq(0,1.5,.3),labels=seq(0,1.5,.3))+
  # scale_x_continuous(name = "Incidence rate (case per 1000)",
  #                    expand=expansion(add=c(0,.05)),limits=c(0,1.5),
  #                    breaks=seq(0,1.5,.3),labels=seq(0,1.5,.3))+
  # scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
  #                    expand=expansion(),limits=c(0,1.2),
  #                    breaks=seq(0,1.2,.3),labels=seq(0,1.2,.3))+
  scale_colour_manual(name = NULL, 
                      values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                      #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )+
  scale_fill_manual(values=c('gray0','gray30','gray80','white'),name=NULL)+
  theme_bw()+
  theme(text =element_text(size=20),
        #legend.position = c(.995,.99),
        legend.position='NULL',
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        plot.margin = margin(r=10,t=10),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25),
        axis.title = element_blank())+
  guides(colour=guide_legend(override.aes = list(size=1)))

ggsave('pdf/Hos_vs_AllDeath_Income.pdf',width = 8,height = 6)

# Table 1 (Disease burden estimates in 2019)----
com_rate_tab_WHO2<-com_rate_res_WHO %>%
  select(AGEGR, Group, n.all, n.impute, IR.est, IR.lci, IR.uci, N.est, N.lci, N.uci) %>%
  mutate(
    AGEGR = factor(AGEGR, levels = c("0-<12m", "0-<6m", "6-<12m")),
    Studies = paste(n.all, 
                    ifelse(AGEGR != "0-<12m", 
                           "", paste(" (", n.impute, ")", sep = "")),
                    sep = ""),
    IR = paste(format(round(IR.est, 1), nsmall = 1),
               " (",
               format(round(IR.lci, 1), nsmall = 1),
               "-",
               format(round(IR.uci, 1), nsmall = 1),
               ")", sep = ""),
    N = paste(format(round(N.est, 0)*1000, nsmall = 0),
              "\n(",
              format(round(N.lci, 0)*1000, nsmall = 0),
              "-",
              format(round(N.uci, 0)*1000, nsmall = 0),
              ")", sep = "")
  ) %>%
  arrange(AGEGR) %>%
  select(all_of(c("AGEGR", "Group", "Studies", "IR", "N"))) %>%
  pivot_wider(names_from = Group, values_from = c(Studies, IR, N), values_fill = "-") %>%
  pivot_longer(!AGEGR, names_to = c("reporting", ".value"), names_pattern = "(.*)_(.*)")

hos_rate_tab_WHO2<-hos_rate_res_WHO %>%
  select(AGEGR, Group, n.all, n.impute, IR.est, IR.lci, IR.uci, N.est, N.lci, N.uci) %>%
  mutate(
    AGEGR = factor(AGEGR, levels = c("0-<12m", "0-<6m", "6-<12m")),
    Studies = paste(n.all, 
                    ifelse(AGEGR != "0-<12m", 
                           "", paste(" (", n.impute, ")", sep = "")),
                    sep = ""),
    IR = paste(format(round(IR.est, 1), nsmall = 1),
               " (",
               format(round(IR.lci, 1), nsmall = 1),
               "-",
               format(round(IR.uci, 1), nsmall = 1),
               ")", sep = ""),
    N = paste(format(round(N.est, 0)*1000, nsmall = 0),
              "\n(",
              format(round(N.lci, 0)*1000, nsmall = 0),
              "-",
              format(round(N.uci, 0)*1000, nsmall = 0),
              ")", sep = "")
  ) %>%
  arrange(AGEGR) %>%
  select(all_of(c("AGEGR", "Group", "Studies", "IR", "N"))) %>%
  pivot_wider(names_from = Group, values_from = c(Studies, IR, N), values_fill = "-") %>%
  pivot_longer(!AGEGR, names_to = c("reporting", ".value"), names_pattern = "(.*)_(.*)")


bind_rows(
  com_rate_tab_WHO2,hos_rate_tab_WHO2,
  hos_mor_tab_WHO,com_mor_tab_WHO
) %>%
  filter(!AGEGR %in% c('0-<6m','6-<12m')) %>%
  write.xlsx('docs/Burden-by-region-2019.xlsx')

# Figure 3 new ----
# df_long<-res_all3 %>%
#   filter(Group!='Global') %>%
#   select(Group,starts_with('IR.')) %>%
#   set_names(~str_remove(.x,'IR.'))
# 
# df_wide<-res_all3 %>%
#   filter(Group!='Global') %>%
#   select(Group,starts_with('IR.')) %>%
#   set_names(~str_remove(.x,'IR.')) %>%
#   pivot_longer(cols=c(starts_with('Com_'),starts_with('Hos_')),
#                names_to = c('com_hr','est_ci'),
#                names_sep = "_",  # split columns using underscore
#                values_to = "value")
# 
# ggplot() +
#   geom_point(data=filter(df_wide,est_ci=='est'),aes(MorAll_est,value,color=com_hr),shape=19,size=2)+
#   geom_errorbar(data=df_long,aes(ymin = Hos_lci, ymax = Hos_uci))+
#   geom_errorbar(data=df_long,aes(ymin = Com_lci, ymax = Com_uci))+
#   scale_x_continuous(name = "Incidence rate (case per 1000)",
#                      expand=expansion(add=c(0,10)),limits=c(0,250),
#                      breaks=seq(0,250,50),labels=seq(0,250,50))+
#   scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
#                      expand=expansion(),limits=c(0,70),
#                      breaks=seq(0,70,10),labels=seq(0,70,10))+
#   scale_colour_manual(name = NULL, 
#                       values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
#                       labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
#   )+
#   theme_bw()+
#   theme(text =element_text(size=20),
#         #legend.position = c(.995,.99),
#         legend.position='NULL',
#         legend.justification = c(1,1),
#         legend.background = element_rect(fill='transparent'),
#         panel.grid=element_blank(),
#         panel.border = element_blank(),
#         axis.ticks.length = unit(.15, "cm"),
#         axis.line = element_line(size=.25),
#         axis.ticks = element_line(size=.25),
#         axis.title = element_blank()
#   )



# Figure 3 new2 (mortality vs Inc and hosp,point plot with loess) ----
plot_loess <- function(res_all3, res_all3_income, span=0.9) {
  df_plot<-res_all3 %>%
    filter(Group!='Global') %>%
    select(Group,starts_with('IR.')) %>%
    set_names(~str_remove(.x,'IR.')) %>%
    mutate(across(starts_with('Hos_'),~.x*3),
           Group=toupper(Group))
  
  df_plot_outlier<-df_plot %>% 
    filter(if_any(matches('(Com)_'),~.x>250) | if_any(matches('(Hos)_'),~.x>150)) %>%
    mutate(across(matches('(Com)_'),~ifelse(.x>250,250,.x))) %>%
    mutate(across(matches('(Hos)_'),~ifelse(.x>150,150,.x)))
  
  p1<-ggplot(df_plot) +
    geom_point(aes(MorAll_est,-Hos_est,color='hos'),shape=19,size=2)+
    geom_smooth(aes(MorAll_est,-Hos_est,color='hos'),method = 'loess',linewidth=.5,se=F,linetype='dashed',span=span)+
    geom_text(aes(MorAll_est,-Hos_lci,label=Group),hjust=0,vjust =-.2,size=3,lineheight = .7,fill = NA, label.color = NA,angle=30,nudge_y = .01)+
    geom_linerange(aes(ymin = -Hos_lci, ymax = -Hos_uci,x=MorAll_est,color='hos'))+
    geom_point(aes(MorAll_est,Com_est,color='Com'),shape=19,size=2)+
    geom_smooth(aes(MorAll_est,Com_est,color='Com'),method = 'loess',linewidth=.5,se=F,linetype='dashed',span=span)+
    geom_text(aes(MorAll_est,Com_lci,label=Group),,hjust=0,vjust =-.2,size=3,lineheight = .7,fill = NA, label.color = NA,angle=-30,nudge_y = .01)+
    geom_linerange(aes(ymin = Com_lci, ymax = Com_uci,x=MorAll_est,color='Com'))+
    scale_x_continuous(name = "Incidence rate (case per 1000)",
                       limits=c(0,1),expand = expansion(add=c(NA,.05)),
                       breaks=c(0,0.25,0.50,0.75,1),
                       labels = c(0,0.25,'0.50',0.75,'1.00'))+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                       expand=expansion(),limits=c(-150,250),
                       breaks=c(-150,-120,-90,-60,-30,0,1:5*50),
                       labels=c(50,40,30,20,10,0,1:5*50))+
    scale_colour_manual(name = NULL, 
                        values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                        labels = c("Incidence","Hospital admission","EMR","EUR","SEAR","WPR"))+
    geom_hline(yintercept = 0, color = "black", linewidth = 0.2)+
    geom_segment(data = data.frame(x = seq(0, 1, by = 0.2)) ,
                 aes(x = x, xend = x, y =-3, yend = 3),  # tick向下延伸0.02
                 linewidth = 0.2,
                 color = "black")+
    geom_text(data = data.frame(x = seq(0, 1, by = 0.2)) ,
              aes(x = x, y = 0,label=c('',0.2,0.4,0.6,0.8,'1.0')),vjust=1.6,size=5,color='#4f4f4f')+
    theme_bw()+
    theme(text =element_text(size=20),
          legend.position = 'top',
          #legend.position='NULL',
          #legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          panel.grid=element_blank(),
          panel.border = element_blank(),
          axis.ticks.length = unit(.15, "cm"),
          axis.line = element_line(size=.25),
          axis.ticks = element_line(size=.25),
          axis.title = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.length.x = unit(0,'cm')
    )+
    geom_segment(data=df_plot_outlier %>% filter(if_any(starts_with('Hos'),~.x==150)),arrow = arrow(length = unit(0.15, "cm")),
                 aes(y = -Hos_lci, yend = -Hos_uci,x=MorAll_est,xend=MorAll_est,color='hos'),show.legend = F)+
    geom_segment(data=df_plot_outlier %>% filter(if_any(starts_with('Com'),~.x==250)),arrow = arrow(length = unit(0.15, "cm")),
                 aes(y = Com_lci, yend = Com_uci,x=MorAll_est,xend=MorAll_est,color='Com'),show.legend = F)
    
  p1
  
  ggsave('pdf/loess_by_region.pdf',width=10,height=6)
  
  df_plot_income<-res_all3_income %>%
    select(WHORegion,Income,starts_with('IR.')) %>%
    set_names(~str_remove(.x,'IR.')) %>%
    mutate(Income=paste0(Income,'ICs'),
           Income=factor(Income,levels=c('LICs','LMICs','UMICs','HICs'))) %>%
    mutate(Group=paste0(toupper(WHORegion),'/',Income)) %>%
    mutate(across(starts_with('Hos_'),~.x*3)) %>%
    na.omit()
  
  summary(df_plot_income)
  
  df_plot_income_outlier<-df_plot_income %>%
    filter(if_any(matches('(Hos|Com)_'),~.x>250)) %>%
    mutate(across(matches('(Hos|Com)_'),~ifelse(.x>250,250,.x)))
  
  p2<-ggplot(df_plot_income) +
    geom_point(aes(MorAll_est,-Hos_est,color='hos'),shape=19,size=2)+
    geom_smooth(aes(MorAll_est,-Hos_est,color='hos'),method = 'loess',linewidth=.5,se=F,linetype='dashed',span=span)+
    #geom_text(aes(MorAll_est,-Hos_lci,label=Group),hjust=0,vjust =-.2,size=4,lineheight = .7,fill = NA, label.color = NA,angle=60)+
    geom_text(aes(MorAll_est,-Hos_lci,label=Group),hjust=0,vjust =-.2,size=3,lineheight = .7,fill = NA, label.color = NA,angle=30)+
    geom_linerange(aes(ymin = -Hos_lci, ymax = -Hos_uci,x=MorAll_est,color='hos'))+
    geom_point(aes(MorAll_est,Com_est,color='Com'),shape=19,size=2)+
    geom_smooth(aes(MorAll_est,Com_est,color='Com'),method = 'loess',linewidth=.5,se=F,linetype='dashed',span=span)+
    #geom_text(aes(MorAll_est,Com_lci,label=Group),hjust=0,vjust =-.2,size=4,lineheight = .7,fill = NA, label.color = NA,angle=-60)+
    geom_text(aes(MorAll_est,Com_lci,label=Group),hjust=0,vjust =-.2,size=3,lineheight = .7,fill = NA, label.color = NA,angle=-30)+
    geom_linerange(aes(ymin = Com_lci, ymax = Com_uci,x=MorAll_est,color='Com'))+
    scale_x_continuous(name = "Incidence rate (case per 1000)",
                       limits=c(0,1),expand = expansion(add=c(NA,.05)),
                       breaks=c(0,0.25,0.50,0.75,1),
                       labels = c(0,0.25,'0.50',0.75,'1.00'))+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                       expand=expansion(add=c(0,0)),#limits=c(-150,150),
                       breaks=c(-150,-120,-90,-60,-30,0,1:5*50),
                       labels=c(50,40,30,20,10,0,1:5*50))+
    coord_cartesian(ylim = c(-150,250))+
    scale_colour_manual(name = NULL, 
                        values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                        labels = c("Incidence","Hospital admission","EMR","EUR","SEAR","WPR"))+
    geom_hline(yintercept = 0, color = "black", linewidth = 0.2)+
    geom_segment(data = data.frame(x = seq(0, 1, by = 0.2)) ,
                 aes(x = x, xend = x, y =-3, yend = 3),  # tick向下延伸0.02
                 linewidth = 0.2,
                 color = "black")+
    geom_text(data = data.frame(x = seq(0, 1, by = 0.2)) ,
              aes(x = x, y = 0,label=c('',0.2,0.4,0.6,0.8,'1.0')),vjust=1.6,size=5,color='#4f4f4f')+
    theme_bw()+
    theme(text =element_text(size=20),
          legend.position = 'top',
          #legend.position='NULL',
          #legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          panel.grid=element_blank(),
          panel.border = element_blank(),
          axis.ticks.length = unit(.15, "cm"),
          axis.line = element_line(size=.25),
          axis.ticks = element_line(size=.25),
          axis.title = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.length.x = unit(0,'cm')
    )+
    geom_segment(data=df_plot_income_outlier %>% filter(if_any(starts_with('Hos'),~.x==250)),arrow = arrow(length = unit(0.15, "cm")),
                 aes(y = -Hos_lci, yend = -Hos_uci,x=MorAll_est,xend=MorAll_est,color='hos'),show.legend = F)+
    geom_segment(data=df_plot_income_outlier %>% filter(if_any(starts_with('Com'),~.x==250)),arrow = arrow(length = unit(0.15, "cm")),
                 aes(y = Com_lci, yend = Com_uci,x=MorAll_est,xend=MorAll_est,color='Com'),show.legend = F)
  
  p2
  
  ggsave('pdf/loess_by_income.pdf',width=10,height=6)
  
  
  p1+p2+plot_layout(guides = 'collect',axis_titles = 'collect_x',widths = c(3,5)) &
    theme(legend.position = 'top',
          axis.text = element_text(size=12),
          legend.text = element_text(size=14))
}

plot_loess(res_all3, res_all3_income, span=0.8)

ggsave('pdf/loess.pdf',width=12,height=8)

# Figure 3 new3 circle size color ----

df_plot1<-res_all3 %>%
  filter(Group!='Global') %>%
  dplyr::select(Group,starts_with('IR.')) %>%
  set_names(~str_remove(.x,'IR.')) %>%
  mutate(hjust=case_when(
    Group=='Afr'~0-.4,
    Group=='Sear'~0-.2,
    Group=='Amr'~0-.1,
    Group=='Emr'~1+.3,
    Group=='Eur'~1+.1,
    Group=='Wpr'~1+.2
  ),
  vjust=case_when(
    Group=='Afr'~0-.9,
    Group=='Sear'~0-.5,
    Group=='Amr'~0-.5,
    Group=='Emr'~0-.3,
    Group=='Eur'~0-.3,
    Group=='Wpr'~1+.3,
  ))

df_plot1_outlier<-df_plot1 %>% 
  filter(if_any(matches('(Com)_'),~.x>150) | if_any(matches('(Hos)_'),~.x>50)) %>%
  mutate(across(matches('(Com)_'),~ifelse(.x>150,150,.x))) %>%
  mutate(across(matches('(Hos)_'),~ifelse(.x>50,50,.x)))

df_plot2<-res_all3_income %>%
  dplyr::select(WHORegion,Income,starts_with('IR.')) %>%
  set_names(~str_remove(.x,'IR.')) %>%
  mutate(Income=paste0(Income,'IC'),
         Income=factor(Income,levels=c('LIC','LMIC','UMIC','HIC'))) %>%
  mutate(Group=paste0(toupper(WHORegion),'/',Income)) %>%
  mutate(hjust=case_when(
    Group=='AMR/UMIC'~1+.1,Group=='EMR/LMIC'~1+.15,Group=='AMR/HIC'~1+.1,#left top
    Group=='SEAR/LMIC'~0-.1,Group=='SEAR/UMIC'~0-.1,Group=='AFR/LIC'~0-.2, #right top
    Group=='AFR/UMIC'~0-.1,Group=='AMR/LMIC'~0-.1,Group=='WPR/LMIC'~0-.1,#right bottom
    Group=='EUR/HIC'~1+.1,Group=='SEAR/LIC'~1+.1,#left bottom
  ),
  vjust=case_when(
    Group=='AMR/UMIC'~0-.5,Group=='EMR/LMIC'~0-.55,Group=='AMR/HIC'~0-.5,#left top
    Group=='SEAR/LMIC'~0-.5,Group=='SEAR/UMIC'~0-.5,Group=='AFR/LIC'~0-.5, #right top
    Group=='AFR/UMIC'~1+.5,Group=='AMR/LMIC'~1+.5,Group=='WPR/LMIC'~1+.5,#right bottom
    Group=='EUR/HIC'~1+.5,Group=='SEAR/LIC'~1+.5,#left bottom
  ))

df_plot2_outlier<-df_plot2 %>% 
  filter(if_any(matches('(Com)_'),~.x>250) | if_any(matches('(Hos)_'),~.x>50)) %>%
  mutate(across(matches('(Com)_'),~ifelse(.x>250,250,.x))) %>%
  mutate(across(matches('(Hos)_'),~ifelse(.x>50,50,.x)))


p1<-ggplot(df_plot1,aes(Com_est,Hos_est)) +
  geom_vline(xintercept = 94.6, linetype = "dashed")+
  geom_hline(yintercept = 15.9, linetype = "dashed")+
  geom_linerange(aes(xmin = Com_lci, xmax = Com_uci),color='gray',linewidth=.4)+
  geom_linerange(aes(ymin = Hos_lci, ymax = Hos_uci),color='gray',linewidth=.4)+
  geom_segment(data=df_plot1_outlier %>% filter(if_any(starts_with('Hos'),~.x==50)),arrow = arrow(length = unit(0.15, "cm")),
               aes(y = Hos_lci, yend = Hos_uci,x=Com_est,xend=Com_est),color='gray',linewidth=.4)+
  geom_segment(data=df_plot1_outlier %>% filter(if_any(starts_with('Com'),~.x==150)),arrow = arrow(length = unit(0.15, "cm")),
               aes(x = Com_lci, xend = Com_uci,y=Hos_est,yend=Hos_est),color='gray',linewidth=.4)+
  geom_point(shape=21,aes(size=MorAll_est,fill=MorAll_est),stroke=.2)+
  #geom_text_repel(aes(label=toupper(Group)))+
  geom_text(aes(label=toupper(Group),hjust=I(hjust),vjust=I(vjust),colour=Group),
                  size=3,show.legend = FALSE 
                  )+
  scale_x_continuous(name = "Incidence rate (case per 1000)",
                     expand=expansion(add=c(0,0)),limits=c(0,150),
                     breaks=seq(0,150,50),labels=seq(0,150,50))+
  scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                     expand=expansion(),limits=c(0,50),
                     breaks=seq(0,50,10),labels=seq(0,50,10))+
  # scale_fill_gradientn(
  #   colors = c("#08306B", "white", "yellow"),  # dark blue → green → light yellow
  #   values = rescale(c(min(df_plot2$MorAll_est, na.rm=TRUE),
  #                      0.487,  # global reference
  #                      max(df_plot2$MorAll_est, na.rm=TRUE))),
  #   name = NULL
  # )+
  scale_fill_gradientn(
    colors = c("#4575B4", "white", "#D73027"),  # 蓝 → 白 → 红
    values = rescale(c(min(df_plot2$MorAll_est, na.rm=TRUE),
                       0.487,
                       max(df_plot2$MorAll_est, na.rm=TRUE))),
    name = NULL
  )+
  scale_color_manual(
    values = c(
      "#1b9e77",  # greenish (保留)
      "#0f0f0f",  # purple
      "#440154",  # olive green
      "#e7298a",  # magenta / pink
      "#a6761d",  # brownish
      "#0072B2"   # neutral gray
    ),
    name = NULL
  )+
  scale_size(name = NULL)+
  theme_bw()+
  theme(#text =element_text(size=20),
        #legend.position = c(.995,.99),
        legend.position='NULL',
        #legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25),
        axis.title = element_blank()
  )
p1


p2<-ggplot(df_plot2,aes(Com_est,Hos_est)) +
  geom_vline(xintercept = 94.6, linetype = "dashed")+
  geom_hline(yintercept = 15.9, linetype = "dashed")+
  geom_linerange(aes(xmin = Com_lci, xmax = Com_uci),color='gray80',linewidth = .4)+
  geom_linerange(aes(ymin = Hos_lci, ymax = Hos_uci),color='gray80',linewidth = .4)+
  geom_segment(data=df_plot2_outlier %>% filter(if_any(starts_with('Hos'),~.x==50)),arrow = arrow(length = unit(0.15, "cm")),
             aes(y = Hos_lci, yend = Hos_uci,x=Com_est,xend=Com_est),color='gray80',linewidth = .4)+
  geom_segment(data=df_plot2_outlier %>% filter(if_any(starts_with('Com'),~.x==250)),arrow = arrow(length = unit(0.15, "cm")),
               aes(x = Com_lci, xend = Com_uci,y=Hos_est,yend=Hos_est),color='gray80',linewidth = .4)+
  geom_point(shape=21,aes(size=MorAll_est,fill=MorAll_est),color='black',stroke=.2)+
  #geom_text_repel(aes(label=toupper(Group)))+
  geom_text(aes(label=toupper(Group),hjust=I(hjust),vjust=I(vjust),color=WHORegion),
            size=3,show.legend = FALSE 
  )+
  scale_x_continuous(name = "Incidence rate (case per 1000)",
                     expand=expansion(add=c(0,0)),limits=c(0,250),
                     breaks=seq(0,250,50),labels=seq(0,250,50))+
  scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                     expand=expansion(add=c(0,0)),limits=c(0,50),
                     breaks=seq(0,50,10),labels=seq(0,50,10))+
  # scale_fill_gradientn(
  #   colors = c("#08306B", "white", "yellow"),  # dark blue → green → light yellow
  #   values = rescale(c(min(df_plot2$MorAll_est, na.rm=TRUE),
  #                      0.487,  # global reference
  #                      max(df_plot2$MorAll_est, na.rm=TRUE))),
  #   name = NULL
  # )+
  scale_fill_gradientn(
    colors = c("#4575B4", "white", "#D73027"),  # 蓝 → 白 → 红
    values = rescale(c(min(df_plot2$MorAll_est, na.rm=TRUE),
                       0.487,
                       max(df_plot2$MorAll_est, na.rm=TRUE))),
    name = NULL,
    limits = c(0, 1),                           # ✅ 限定填充数值范围，不溢出
    breaks = c(0, 0.25, 0.5, 0.75, 1),       # ✅ 图例刻度固定
    labels = scales::number_format(accuracy = 0.01),  # 可选，美化标签格式
    oob = scales::squish,
  )+
  scale_color_manual(
    values = c(
      "#1b9e77",  # greenish (保留)
      "#0f0f0f",  # purple
      "#440154",  # olive green
      "#e7298a",  # magenta / pink
      "#a6761d",  # brownish
      "#0072B2"   # neutral gray
    ),
    name = NULL
  )+
  scale_size(name=NULL)+
  theme_bw()+
  theme(#text =element_text(size=20),
    #legend.position = c(.995,.99),
    #legend.position='NULL',
    #legend.justification = c(1,1),
    legend.background = element_rect(fill='transparent'),
    panel.grid=element_blank(),
    panel.border = element_blank(),
    axis.ticks.length = unit(.15, "cm"),
    axis.line = element_line(size=.25),
    axis.ticks = element_line(size=.25),
    axis.title = element_blank()
  )+
  guides(
    fill = guide_colorbar(order = 1),  # gradientn 图例在上
    size  = guide_legend(order = 2)    # 尺寸图例在最下
  )

p2



p1+p2+plot_layout(widths = c(4,4))&
  theme(axis.text = element_text(size=12),
        legend.text = element_text(size=14))

ggsave('pdf/Com_hos_mor_point.pdf',width=10,height=6)


#save image ----
save.image('rda/code_07_plot_for_incidence.RData')

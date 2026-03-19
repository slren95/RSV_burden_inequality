#
# ------------------------------------------------------------------------------
# Script: code_09_nirsevimab_tracker.R
# Purpose:
#   Clean and curate the product introduction tracker (e.g., nirsevimab and
#   RSVpreF/Abrysvo), harmonise country identifiers, derive implementation
#   categories (licensed vs NIP-included), and export datasets for modelling and
#   reporting.
#
# Inputs:
#   - data/<product rollout tracker workbook>.xlsx (multiple sheets)
#   - rda/pop_mor.raw_2023pop.rds (population + country metadata)
#   - Upstream burden objects (`rda/code_01_incidence.RData`, etc.) for context
#
# Outputs:
#   - `rda/df_nirsevimab.csv`, `rda/df_abrysvo.csv`
#   - `model_data/df_type.rds`
#   - `rda/code_09_nirsevimab_tracker.RData` (workspace snapshot)
#
# Usage:
#   source("code_09_nirsevimab_tracker.R")
#
# Notes:
#   File names and sheet indices are hard-coded; keep the tracker workbook
#   structure stable or update the import lines accordingly.
# ------------------------------------------------------------------------------

library(tidyverse)
library(rio)
library(openxlsx)
library(timevis)
library(countrycode)
library(reactable)
library(ggsci)
library(patchwork)


source('functions.R')
pop_mor.raw<-import('rda/pop_mor.raw_2023pop.rds')

load('rda/code_01_incidence.RData')
load('rda/code_02_hospitalisation.RData')
load('rda/code_04_in_hos_CFR.RData')
load('rda/code_05_overall_mortality.RData')

df_product_nir<-import('data/RSV单抗疫苗推广情况.xlsx',which = 1)
df_product_abr<-import('data/RSV单抗疫苗推广情况.xlsx',which = 2)

# Shimo Document----
df_product_nir<-df_product_nir %>%
  relocate(ISOCountry,Country,ChineseName,EU,Product) %>%
  arrange(EU,Country,desc(Product),Licensed_Date,NIP) %>%
  mutate_if(is.POSIXct,~as.character(.x))
  

df_product_abr<-df_product_abr %>%
  relocate(ISOCountry,Country,ChineseName,EU,Product) %>%
  arrange(EU,Country,desc(Product),Licensed_Date,NIP) %>%
  mutate_if(is.POSIXct,~as.character(.x))

write.xlsx(df_product_nir,'data/df_product_nir.xlsx')
write.xlsx(df_product_abr,'data/df_product_abr.xlsx')


# Nirsevimab ----
tem<-df_product_nir[which(df_product_nir$ISOCountry=='NLD'),c('Licensed_Date','Source1')]
tem
# fill EU
df_product_nir[which(df_product_nir$EU=='y'),c('Licensed_Date','Source1')]<-tem

# add Type
df_nirsevimab<-df_product_nir %>%
  mutate(type=case_when(
    NIP=='y'~'NIP-included',
    NIP=='y-WHO'~'NIP-included',
    T~'Licensed'
  )) %>%
  mutate(ISO2=countrycode(ISOCountry, "iso3c", "iso2c"),.after = ISOCountry) %>%
  left_join(pop_mor.raw %>% transmute(ISOCountry,WHORegion=toupper(WHORegion),Income2019)) %>%
  relocate()

df_nirsevimab %>% count(type)

# fix Liechtenstein ---
df_nirsevimab<-df_nirsevimab %>%
  mutate(WHORegion=ifelse(Country=='Liechtenstein','EUR',WHORegion),
         Income2019=ifelse(Country=='Liechtenstein','H',Income2019))

## ➡️ export df_nirsevimab.csv ----
rio::export(df_nirsevimab,'rda/df_nirsevimab.csv')

# Abrysvo ----
tem<-df_product_abr[which(df_product_abr$ISOCountry=='AUT'),c('Licensed_Date','Source1')]
tem
# fill EU
df_product_abr[which(df_product_abr$EU=='y'),c('Licensed_Date','Source1')]<-tem

# add Type
df_abrysvo<-df_product_abr %>%
  mutate(type=case_when(
    NIP=='y'~'NIP-included',
    NIP=='y-WHO'~'NIP-included',
    T~'Licensed'
  )) %>%
  mutate(ISO2=countrycode(ISOCountry, "iso3c", "iso2c"),.after = ISOCountry)  %>%
  left_join(pop_mor.raw %>% transmute(ISOCountry,WHORegion=toupper(WHORegion),Income2019))

df_abrysvo %>% count(type)

# fix Liechtenstein ---
df_abrysvo <-df_abrysvo  %>%
  mutate(WHORegion=ifelse(Country=='Liechtenstein','EUR',WHORegion),
         Income2019=ifelse(Country=='Liechtenstein','H',Income2019))

##➡️export df_abrysvo.csv----
rio::export(df_abrysvo,'rda/df_abrysvo.csv')

# Nirsevimab and Abrysvo Type ----

df_type<-full_join(
  df_nirsevimab %>%
    dplyr::select(ISOCountry,nir=type),
  df_abrysvo %>%
    dplyr::select(ISOCountry,abr=type)
) %>%
  left_join(bind_rows(df_nirsevimab,df_abrysvo) %>%
              distinct(ISOCountry,.keep_all = T) %>%
              dplyr::select(ISOCountry,Country,ChineseName,EU)) %>%
  mutate(nir_abr=paste0(
    case_when(nir=='NIP-included'~'NIP',nir=='Licensed'~'Lic',is.na(nir)~'NA'),'-',
    case_when(abr=='NIP-included'~'NIP',abr=='Licensed'~'Lic',is.na(abr)~'NA')
  ))

##➡️model_data/df_type.rds ----

export(df_type,'model_data/df_type.rds')

save.image('rda/code_09_nirsevimab_tracker.RData')

df_type %>% count(nir_abr)


# 💹 Plot(Licensed and NIP bar plot) ----
plot_nir_p1<-df_nirsevimab %>%
  filter(!is.na(type)) %>%
  select(Income2019,type) %>% 
  mutate(Cluster=paste0(Income2019,'IC'),
         Cluster=factor(Cluster,levels=c('LIC','LMIC','UMIC','HIC')),
         type=factor(type)) %>% 
  count(Cluster,type,.drop = F) %>%
  mutate(n2=ifelse(n==0,0.1,n)) %>%
  ggplot(aes(Cluster,n2,fill=type))+
  geom_col(position = 'dodge',width=.8,colour = "black",linewidth = .3)+
  geom_text(aes(label=n,y=n2+1),position = position_dodge(width=.8))+
  scale_fill_lancet(alpha = .5,name=NULL)+
  scale_fill_manual(values = c('#80a3c3','#f68080'),name=NULL,
                    labels=c('Licensed but not included in NIP','Licensed and included in NIP'))+
  scale_x_discrete(expand = expansion(add=c(.5,.5)))+
  scale_y_continuous(expand = expansion(add=c(0,0)),limits = c(0,40),breaks = 0:8*5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.tag = element_text(size = 14),
        legend.position = c(.88,.95))+
  labs(x='',y='',tag='')

plot_nir_p2<-df_nirsevimab %>%
  filter(!is.na(type)) %>%
  select(WHORegion,type) %>%
  mutate(WHORegion=factor(WHORegion,levels=c('AFR','AMR','EMR','EUR','SEAR','WPR')),
         type=factor(type)) %>%
  count(WHORegion,type,.drop = F) %>%
  mutate(n2=ifelse(n==0,0.1,n)) %>%
  ggplot(aes(WHORegion,n2,fill=type))+
  geom_col(position = 'dodge',width=.8,colour = "black",linewidth = .3)+
  geom_text(aes(label=n,y=n2+1),position = position_dodge(width=.8))+
  scale_fill_lancet(alpha = .5,name=NULL)+
  scale_fill_manual(values = c('#80a3c3','#f68080'),name=NULL,
                    labels=c('Licensed but not included in NIP','Licensed and included in NIP'))+
  scale_x_discrete(expand = expansion(add=c(.5,.5)))+
  scale_y_continuous(expand = expansion(add=c(0,0)),limits = c(0,40),breaks = 0:8*5)+
  theme_bw()+
  theme(legend.position = 'null',
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.tag = element_text(size = 14))+
  labs(x='',y='',tag='')


plot_abr_p1<-df_abrysvo %>%
  filter(!is.na(type)) %>%
  select(Income2019,type) %>% 
  mutate(Cluster=paste0(Income2019,'IC'),
         Cluster=factor(Cluster,levels=c('LIC','LMIC','UMIC','HIC')),
         type=factor(type)) %>% 
  count(Cluster,type,.drop = F) %>%
  mutate(n2=ifelse(n==0,0.1,n)) %>%
  ggplot(aes(Cluster,n2,fill=type))+
  geom_col(position = 'dodge',width=.8,colour = "black",linewidth = .3)+
  geom_text(aes(label=n,y=n2+1),position = position_dodge(width=.8))+
  scale_fill_lancet(alpha = .5,name=NULL)+
  scale_fill_manual(values = c('#80a3c3','#f68080'),name=NULL,
                    labels=c('Licensed but not included in NIP','Licensed and included in NIP'))+
  scale_x_discrete(expand = expansion(add=c(.5,.5)))+
  scale_y_continuous(expand = expansion(add=c(0,0)),,limits = c(0,40),breaks = 0:8*5)+
  theme_bw()+
  theme(legend.position = 'null',
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.tag = element_text(size = 14))+
  labs(x='',y='',tag='')

plot_abr_p2<-df_abrysvo %>%
  filter(!is.na(type)) %>%
  select(WHORegion,type) %>%
  mutate(WHORegion=factor(WHORegion,c('AFR','AMR','EMR','EUR','SEAR','WPR')),
         type=factor(type)) %>%
  count(WHORegion,type,.drop = F) %>%
  mutate(n2=ifelse(n==0,0.1,n)) %>%
  ggplot(aes(WHORegion,n2,fill=type))+
  geom_col(position = 'dodge',width=.8,colour = "black",linewidth = .3)+
  geom_text(aes(label=n,y=n2+1),position = position_dodge(width = .8))+
  scale_fill_lancet(alpha = .5,name=NULL)+
  scale_fill_manual(values = c('#80a3c3','#f68080'),name=NULL,
                    labels=c('Licensed but not included in NIP','Licensed and included in NIP'))+
  scale_x_discrete(expand = expansion(add=c(.5,.5)))+
  scale_y_continuous(expand = expansion(add=c(0,0)),,limits = c(0,40),breaks = 0:8*5)+
  theme_bw()+
  theme(legend.position = 'null',
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.tag = element_text(size = 14))+
  labs(x='',y='',tag='')


plot_nir_p2+plot_nir_p1+
  plot_abr_p2+plot_abr_p1+
  plot_layout(ncol=2,nrow=2,widths = c(3,2),guides = 'collect') &
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size=16),
        text=element_text(size=14),
        axis.line = element_line(linewidth = .3),
        legend.position = "top",
        legend.text = element_text(size=14))

ggsave('pdf/Nir-Abr.pdf',width = 12,height = 9)


df_nirsevimab %>%
  filter(!is.na(type)) %>%
  select(WHORegion,type) %>%
  mutate(WHORegion=factor(WHORegion,levels=c('AFR','AMR','EUR','SEAR','WPR','EMR')),
         type=factor(type)) %>%
  count(WHORegion,type,.drop = F) %>%
  mutate(n2=ifelse(n==0,0.1,n)) %>%
  ggplot(aes(WHORegion,n2,fill=type))+
  geom_col(position = 'dodge',width=.8,colour = "black",linewidth = .3)+
  geom_text(aes(label=n,y=n2+.8),position = position_dodge(width=.8),size = 5)+
  scale_fill_lancet(alpha = .5,name=NULL)+
  scale_fill_manual(values = c('#80a3c3','#f68080'),name=NULL,
                    labels=c('licensed but not included in NIP','included in NIP'))+
  scale_x_discrete(expand = expansion(add=c(.5,.5)))+
  scale_y_continuous(expand = expansion(add=c(0,0)),limits = c(0,30))+
  theme_bw()+
  theme(legend.position = c(.75,.95),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(size=24),
        plot.tag = element_text(size = 14),
        plot.margin = margin(t=2,b=2))+
  labs(x=NULL,y='Number of countries',tag='')

ggsave('Figures/MSD-Nirsevimab.tiff',width=1000,height=600,unit='px',dpi=200)

## Table Countries ----

df_type %>% filter(EU=='y') %>%
  arrange(Country) %>%
  transmute(Country,nir=recode(nir,'Licensed'='licensed','NIP-included'='NIP'),
            abr=recode(abr,'Licensed'='licensed','NIP-included'='NIP')) %>%
  createAppendixTable('docs/Nir-Abr-EU.xlsx')

df_nirsevimab %>%
  select(3,1:2,4:6,type) %>%
  arrange(type,Country) %>%
  createAppendixTable('docs/df_nir.xlsx')

df_abrysvo %>%
  select(3,1:2,4:6,type) %>%
  arrange(type,Country) %>%
  createAppendixTable('docs/df_abr.xlsx')


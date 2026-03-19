#
# ------------------------------------------------------------------------------
# Script: code_map.R
# Purpose:
#   Visualise the implementation status of nirsevimab and RSVpreF (Abrysvo) using
#   world maps (licensed vs NIP-included vs unlicensed) for reporting materials.
#
# Inputs:
#   - shiny/df_nirsevimab.csv
#   - shiny/df_abrysvo.csv
#
# Outputs:
#   - `plot/map_nir.png`, `plot/map_abr.png` (and optional zoomed variants)
#
# Usage:
#   source("code_map.R")
#
# Notes:
#   Country name harmonisation is handled via `recode()` to match `map_data()`
#   region naming conventions.
# ------------------------------------------------------------------------------

library(tidyverse)
library(rio)

df_nirsevimab <- import('shiny/df_nirsevimab.csv') %>% 
  transmute(region=Country,type) %>%
  mutate(region=recode(region,'Czechia'='Czech Republic','United States'='USA',
                       'Indian'='India','United Kingdom'='UK'))

df_abrysvo <- import('shiny/df_abrysvo.csv') %>% 
  transmute(region=Country,type) %>%
  mutate(region=recode(region,'Czechia'='Czech Republic','United States'='USA',
                       'Indian'='India','United Kingdom'='UK'))

world_map<-map_data('world') %>% filter(region!='Antarctica') %>%
  mutate(region=recode(region,'Taiwan'='China'))

df_nirsevimab %>% count(type)

world_map_nir<-left_join(world_map,df_nirsevimab,by='region') %>%
  replace_na(list(type='Unlicensed')) %>%
  mutate(type=factor(type,levels=c('NIP-included','Licensed','Unlicensed'),
                     #labels=c('Included in NIP (16)','Approved only (38)','Not yet approved'),
                     labels=c('Included in NIP (16)', 'Approved only (38)', 'Not yet approved')
                     ))


df_abrysvo %>% count(type)

world_map_abr<-left_join(world_map,df_abrysvo,by='region') %>%
  replace_na(list(type='Unlicensed')) %>%
  mutate(type=factor(type,levels=c('NIP-included','Licensed','Unlicensed'),
                     #labels=c('Included in NIP (12)','Approved only (52)','Not yet approved'),
                     labels=c('Included in NIP (12)', 'Approved only (52)', 'Not yet approved')
                     ))

# nirsevimab 
ggplot() +
  geom_map(
    data = world_map_nir, map = world_map_nir,
    aes(x=long, y=lat, map_id = region,fill=type),
    color = "black",  linewidth = 0.1,alpha=0.9) +
  scale_fill_manual(values = c("#d61e62","#0092d4","gray90"),na.value = "",name='')+
  theme_void() +
  theme(legend.position = c(0.64,0.02),
    #legend.position = 'NULL',
        legend.key.width = unit(1,'cm'),
        legend.key.height = unit(1,'cm'),
        plot.margin = margin(b=9),
        legend.direction = 'horizontal',
        text=element_text(size=22))

ggsave('plot/map_nir.png',width = 950,height = 500,units = 'px',bg = 'white',dpi=200)

ggplot() +
  geom_map(
    data = world_map_nir, map = world_map_nir,
    aes(x=long, y=lat, map_id = region,fill=type),
    color = "black",  linewidth = 0.1,alpha=0.9) +
  scale_fill_manual(values = c("#d61e62","#0092d4","gray90"),na.value = "",name='')+
  theme_void() +
  theme(legend.position = 'null')+
  coord_cartesian(xlim=c(-12,18),ylim=c(36,55))

# abrysvo
ggplot() +
  geom_map(
    data = world_map_abr, map = world_map_abr,
    aes(x=long, y=lat, map_id = region,fill=type),
    color = "black",  linewidth = 0.1,alpha=0.9) +
  scale_fill_manual(values = c("#d61e62","#0092d4","gray90"),na.value = "",name='')+
  theme_void() +
  theme(legend.position = c(0.64,0.02),
        #legend.position = 'NULL',
        legend.key.width = unit(1,'cm'),
        legend.key.height = unit(1,'cm'),
        plot.margin = margin(b=9),
        legend.direction = 'horizontal',
        text=element_text(size=22))

ggsave('plot/map_abr.png',width = 950,height = 450,units = 'px',bg = 'white',dpi=200)

ggplot() +
  geom_map(
    data = world_map_abr, map = world_map_abr,
    aes(x=long, y=lat, map_id = region,fill=type),
    color = "black",  linewidth = 0.1,alpha=0.9) +
  scale_fill_manual(values = c("#d61e62","#0092d4","gray90"),na.value = "",name='')+
  theme_void() +
  theme(legend.position = 'null')+
  coord_cartesian(xlim=c(-12,23),ylim=c(36,60))

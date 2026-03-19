#
# ------------------------------------------------------------------------------
# Script: code_10.1_country_center_and_category.R
# Purpose:
#   Derive country centroid coordinates and categorise countries into latitude/
#   climate bands (e.g., N/S hemisphere and tropical/subtropical/temperate) for
#   stratified analyses and visualisations.
#
# Inputs:
#   - rda/pop_by_country_2026.rds (population-by-country dataset)
#   - data/df_center.csv (country centroids; ISO + lat/lon)
#   - rda/df_nirsevimab.csv, rda/df_abrysvo.csv (product status, optional)
#
# Outputs:
#   - Plot(s) under `plot/` (e.g., `plot/map_region_ns_zone.tiff`)
#   - Intermediate joined datasets saved later in the script (if exported)
#
# Usage:
#   source("code_10.1_country_center_and_category.R")
#
# Notes:
#   Some manual fixes for missing centroids (e.g., NAM/ESH) are applied inline.
# ------------------------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(rio)
library(countrycode)
library(ggsci)
library(sf)
library(rnaturalearth)
library(leaflet)
library(openxlsx)

# ⬇️Countries Population----
df_pop<-import('rda/pop_by_country_2026.rds',trust=T)

## Categorize by latitude ---- 
# https://github.com/gavinr/world-countries-centroids?tab=readme-ov-file

# df_center<-read_csv('https://cdn.jsdelivr.net/gh/gavinr/world-countries-centroids@v1/dist/countries.csv') #249
# rio::export(df_center,'data/df_center.csv')

# ⬇️ df_center
df_center<-import('data/df_center.csv')


df_center<-mutate(df_center,ISO3=countrycode(ISO, "iso2c", "iso3c"))

setdiff(df_pop$ISOCountry,df_center$ISO3)
# "NAM"  "ESH"

df_pop %>% filter(ISOCountry %in% c( "NAM","ESH"))

# Join center_lon,center_lat
df_pop$ISOCountry[duplicated(df_pop$ISOCountry)]

tem1<-left_join(df_pop,dplyr::select(df_center,longitude,latitude,ISO3),by=c('ISOCountry'='ISO3'))
tem1 %>%
  group_by(ISOCountry) %>%
  filter(n()>1)

df_center %>% filter(ISO3 %in% c('ESP'))

# df pop center ----
df_pop_center<-tem1 %>% filter(ISOCountry!='ESP'|latitude>40)

df_pop_center %>% filter(is.na(latitude))

df_center %>% filter(str_detect(COUNTRY,'(amibia)|(saha)')) %>% pull(longitude)
# Nambia long 18.16451 lat: -21.908582
#https://api.map.baidu.com/lbsapi/getpoint/index.html
# Western Sahara -13.484314,24.005409
df_pop_center %>% filter(ISOCountry %in% c( "NAM","ESH"))
df_pop_center[which(df_pop_center$ISOCountry=='NAM'),c('longitude','latitude')]<-c(18.16451,-21.908582)
df_pop_center[which(df_pop_center$ISOCountry=='ESH'),c('longitude','latitude')]<-c(-13.484314,24.005409)
head(df_pop_center)

df_pop_center<-df_pop_center %>%
  mutate(NS=if_else(latitude>0,'N','S'),
         Zone=case_when(abs(latitude)<23.5~'Tropical',
                        abs(latitude)<35~'Subropical',
                        #abs(latitude)<60~'Temperate',
                        #T~'Code'，
                        T~'Temperate and Code'),
         ns_zone=paste(NS,str_sub(Zone,1,3),sep='-'),
         region_ns_zone=paste(WHORegion,ns_zone,sep='-')
  )

df_pop_center %>% count(NS,Zone)
df_pop_center %>% count(ns_zone,region_ns_zone)

# ggplot map_data ----
df_map<-map_data('world')

with(df_map,{
  with(df_pop_center,{
    setdiff(unique(WHOName),unique(region))
    setdiff(unique(region),unique(WHOName))
  })
})

map_df<-left_join(df_map,df_pop_center,by=c('region'='WHOName'))

ggplot()+
  geom_map(data=map_df,map=map_df,
             aes(x=long,y=lat,map_id=region,fill=region_ns_zone),
           na.value = "#EEEEEE",na.translate=FALSE)+
  geom_point(data=map_df,aes(x=longitude,y=latitude),shape=3,colour='brown')+
  geom_hline(yintercept = c(23.5,-23.5), lty = 3, color = "darkgrey") +
  geom_hline(yintercept = c(35,-35), lty = 2, color = "darkgrey") +
  geom_hline(yintercept = 0, lty = 1, color = "darkgrey") +
  geom_text(aes(y = c(35,23.5,0,-23.5,-35), label =c("35°","23.5°","0°","-23.5°","-35°")), 
            x = -Inf, hjust = -0.1, vjust = -1, size = 4,color = "darkgrey")+
  theme_void()
ggsave('plot/map_region_ns_zone.tiff',width = 12,height = 5)

# sf map data by country ----
df_ne<-ne_countries(type = "countries",scale = 'small')

df_ne_type_country<- df_ne %>% filter(str_detect(type,'ountry'))
unique(df_ne$iso_a3)

class(df_ne_type_country) # sf

## ⬇️Nirsevimab and Abrysvo status data ----
df_nirsevimab<-import('rda/df_nirsevimab.csv') # 49
df_abrysvo<-import('rda/df_abrysvo.csv') #43

df_nir_abr<-bind_rows(df_nirsevimab,df_abrysvo)

# fix missing iso_a3 
which(!df_nir_abr$ISOCountry %in% df_ne$iso_a3)


df_nir_abr %>% filter(!ISOCountry %in% df_ne$iso_a3) %>% select(ISOCountry,ChineseName,Product)
# 
# ISOCountry ChineseName    Product
# 1        MLT      马其他 Nirsevimab
# 2        LIE  列支敦士登 Nirsevimab
# 3        SGP      新加坡 Nirsevimab
# 4        MLT      马其他    Abrysvo
# 5        LIE  列支敦士登    Abrysvo
# 6        SGP      新加坡    Abrysvo


df_nir_abr$ISOCountry[c(22,40,45,68,83,87)] # France MLT Malta LIE SGP


df_ne %>% dplyr::select(iso_a3,name) %>% filter(name %in% c('Taiwan','France','China','Norway'))
df_ne[which(df_ne$name=='France'),'iso_a3']='FRA'
df_ne[which(df_ne$name=='Norway'),'iso_a3']='NOR'
df_ne[which(df_ne$name=='Taiwan'),'iso_a3']='CHN'
df_ne %>% dplyr::select(iso_a3,name) %>% filter(name %in% c('Taiwan','France','China','Norway'))

df_ne2 <- df_ne %>%
  mutate(name2=case_when(name=='Taiwan'~'China(Taiwan)',T~name),
         name3=case_when(name=='Taiwan'~'China',T~name))

which(!df_nir_abr$ISOCountry %in% df_ne2$iso_a3)
df_nir_abr$ISOCountry[c(22,60)]

class(df_ne2)
## sf map data for country with Nirsevimab or Abrysvo Licensed ----
df_nirsevimab_sf<-df_ne2 %>%
  filter(iso_a3 %in% df_nirsevimab$ISOCountry) %>% 
  dplyr::select(iso_a3,name,name2,name3) %>% 
  left_join(df_nirsevimab,.,by=c('ISOCountry'='iso_a3')) %>%
  st_as_sf()


df_abrysvo_sf<-df_ne2 %>%
  filter(iso_a3 %in% df_abrysvo$ISOCountry) %>% 
  dplyr::select(iso_a3,name,name2,name3) %>% 
  left_join(df_abrysvo,.,by=c('ISOCountry'='iso_a3')) %>%
  st_as_sf()


df_nir_abr_sf<-bind_rows(df_nirsevimab_sf,df_abrysvo_sf)

class(df_nir_abr_sf)

## ➡️ Export df_nir_abr_sf.rds----
saveRDS(df_nir_abr_sf,'rda/df_nir_abr_sf.rds')



# Join df_pop_center with df_nirsevimab ----
countries<-import('rda/countries.json') # chinese name by ISO3

intersect(names(df_pop_center),names(df_nirsevimab))

df_pop_center_nir<-left_join(df_pop_center,
                             df_nirsevimab %>% dplyr::select(-WHORegion),by='ISOCountry') %>%
  left_join(dplyr::select(countries,ISO3,China),by=c('ISOCountry'='ISO3'))

df_pop_center_abr<-left_join(df_pop_center,
                             df_abrysvo %>% dplyr::select(-WHORegion),by='ISOCountry') %>%
  left_join(dplyr::select(countries,ISO3,China),by=c('ISOCountry'='ISO3'))

df_pop_center_nir %>% count(type)
# type   n
# 1     Licensed  37
# 2 NIP-included  16
# 3         <NA> 153
df_pop_center_abr %>% count(type)
# type   n
# 1     Licensed  51
# 2 NIP-included  12
# 3         <NA> 143

df_nirsevimab %>%
  filter(!ISOCountry %in% df_pop_center_nir$ISOCountry) # Liechtenstein  列支敦士登 

df_abrysvo %>%
  filter(!ISOCountry %in% df_pop_center_nir$ISOCountry) # Liechtenstein  列支敦士登 

## ➡️ Export df_pop_center_nir----
rio::export(df_pop_center_nir,'rda/df_pop_center_nir.rds')
rio::export(df_pop_center_abr,'rda/df_pop_center_abr.rds')

# EXCEL categorization of countries by region_ns_zone----
with(df_pop_center_nir,{
  table(region_ns_zone,type)
})

df_tem_nir<-reduce(list(df_pop_center_nir %>%
                      group_by(region_ns_zone) %>%
                      summarise(ISOs_All=paste0(ISOCountry,collapse = ','),
                                names_All=paste0(WHOName,collapse = ','),
                                cnames_All=paste0(China,collapse = ',')),
                    df_pop_center_nir %>%
                      filter(type=='NIP-included') %>%
                      group_by(region_ns_zone) %>%
                      summarise(ISOs_NIP=paste0(ISOCountry,collapse = ','),
                                names_NIP=paste0(WHOName,collapse = ','),
                                cnames_NIP=paste0(China,collapse = ',')),
                    df_pop_center_nir %>%
                      filter(type=='Licensed') %>%
                      group_by(region_ns_zone) %>%
                      summarise(ISOs_Lic=paste0(ISOCountry,collapse = ','),
                                names_Lic=paste0(WHOName,collapse = ','),
                                cnames_Lic=paste0(China,collapse = ','))
            ),left_join)

df_tem_abr<-reduce(list(df_pop_center_abr %>%
                          group_by(region_ns_zone) %>%
                          summarise(ISOs_All=paste0(ISOCountry,collapse = ','),
                                    names_All=paste0(WHOName,collapse = ','),
                                    cnames_All=paste0(China,collapse = ',')),
                        df_pop_center_abr %>%
                          filter(type=='NIP-included') %>%
                          group_by(region_ns_zone) %>%
                          summarise(ISOs_NIP=paste0(ISOCountry,collapse = ','),
                                    names_NIP=paste0(WHOName,collapse = ','),
                                    cnames_NIP=paste0(China,collapse = ',')),
                        df_pop_center_abr %>%
                          filter(type=='Licensed') %>%
                          group_by(region_ns_zone) %>%
                          summarise(ISOs_Lic=paste0(ISOCountry,collapse = ','),
                                    names_Lic=paste0(WHOName,collapse = ','),
                                    cnames_Lic=paste0(China,collapse = ','))
),left_join)

# Create workbook and worksheets
wb <- createWorkbook()
addWorksheet(wb, "Nirsevimab")
addWorksheet(wb, "Abrysvo")

# Write data
writeData(wb, "Nirsevimab", df_tem_nir)
writeData(wb, "Abrysvo", df_tem_abr)

# Create header style
header_style <- createStyle(
  fontSize = 12, fontColour = "#FFFFFF", fgFill = "#4F81BD", halign = "CENTER", 
  textDecoration = "bold"
)

# Create body style: wrap text, borders, and background fill
content_style <- createStyle(
  wrapText = TRUE, border = "TopBottomLeftRight", fgFill = "#DCE6F1"
)

# Apply header style
addStyle(wb, 1, style = header_style, rows = 1, cols = 1:ncol(df_tem_nir), gridExpand = TRUE)
addStyle(wb, 2, style = header_style, rows = 1, cols = 1:ncol(df_tem_abr), gridExpand = TRUE)

# Apply body style
addStyle(wb, 1, style = content_style, rows = 2:(nrow(df_tem_nir) + 1), cols = 1:ncol(df_tem_nir), gridExpand = TRUE)
addStyle(wb, 2, style = content_style, rows = 2:(nrow(df_tem_abr) + 1), cols = 1:ncol(df_tem_abr), gridExpand = TRUE)
# Set column widths
setColWidths(wb, 1, cols = 1:10, widths = c(15,rep(50,3),rep(20,6)))
setColWidths(wb, 2, cols = 1:10, widths = c(15,rep(50,3),rep(20,6)))

# Save workbook
saveWorkbook(wb, "docs/Region zoon countries_副本.xlsx", overwrite = TRUE)
#saveWorkbook(wb, "docs/Region zoon countries.xlsx", overwrite = TRUE)

# Paper export table (Countries zoon) ----
full_join(
  df_tem_nir %>%
    dplyr::select(region_ns_zone,names_NIP,names_Lic) %>% 
    filter(if_any(-region_ns_zone,~!is.na(.))) %>%
    transmute(Cou_mAb_NIP=names_NIP,
              Cou_mAb_Lic=names_Lic,region_ns_zone),
  df_tem_abr %>%
    dplyr::select(region_ns_zone,names_NIP,names_Lic) %>% 
    filter(if_any(-region_ns_zone,~!is.na(.))) %>%
    transmute(Cou_mv_NIP=names_NIP,
              Cou_mv_Lic=names_Lic,region_ns_zone)
) %>%
  relocate(region_ns_zone,.after = last_col()) %>%
  rio::export('rda/countires-nip-lic.rds')


save.image('rda/code_10.1_country_center_and_category.RData')

df_tem_nir %>%
  dplyr::select(region_ns_zone,names_NIP,names_Lic) %>% 
  filter(if_any(-region_ns_zone,~!is.na(.))) %>%
  transmute(Cou_mAb_NIP=names_NIP,
            Cou_mAb_Lic=names_Lic,region_ns_zone)

df_nirsevimab %>% count(WHORegion,type)
df_abrysvo %>% count(type,WHORegion)


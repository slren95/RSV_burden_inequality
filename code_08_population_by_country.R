#
# ------------------------------------------------------------------------------
# Script: code_08_population_by_country.R
# Purpose:
#   Clean and harmonise population inputs (infant population by country/region),
#   fix known issues in legacy population tables, and export standardised
#   datasets used throughout the project.
#
# Inputs:
#   - workspaceToBegin.RData (contains `pop_mor.raw`, `pop_WHO.raw`, `map.data*`)
#   - UN WPP spreadsheet(s) under `data/` (see `read.xlsx()` calls in script)
#
# Outputs:
#   - `rda/pop_mor.raw.rds`, `rda/pop_WHO.raw.rds`, `rda/map.data*.rds`
#   - Additional population comparison objects exported later in the script
#
# Usage:
#   source("code_08_population_by_country.R")
#
# Notes:
#   Run from project root; the script writes canonical population files under
#   `rda/` that many other scripts assume exist.
# ------------------------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(rio)
library(openxlsx)
library(janitor)

load("workspaceToBegin.RData")

# Fix pop_mor.raw bug ----
table(str_count(pop_mor.raw$ISOCountry))
pop_mor.raw$ISOCountry<-str_trim(pop_mor.raw$ISOCountry)
table(str_count(pop_mor.raw$ISOCountry))

pop_mor.raw %>% add_count(WHOName) %>% filter(n==2) %>% view()

pop_mor.raw<-pop_mor.raw %>% filter(ISOCountry!='CUW') # remove CUW CUba duplicated

# Export pop_mor.raw ----
export(pop_mor.raw,'rda/pop_mor.raw.rds')
export(pop_WHO.raw,'rda/pop_WHO.raw.rds')
export(map.data,'rda/map.data.rds')
export(map.data2,'rda/map.data2.rds')

# population before(from Professor Li) ----
df_pop_li_2019<-pop_mor.raw %>%
  dplyr::select(ISOCountry,Country.Name,WHOName,WHORegion,Income2019,Y0,Y1,Y2,Y3,Y4)


# pop data in 2019 (from U.N.)----
df_li<-read.xlsx("data/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx",sheet=1)
names(df_li)<-df_li[10,]
df_li<-df_li[-(1:10),]

df_li <-df_li %>% clean_names()

df_pop_un_2019<-df_li %>%
  filter(year==2019,type=='Country/Area') %>%
  dplyr::select(year,region_subregion_country_or_area,type,x0,x1,x2,x3,x4,x5) %>%
  mutate(across(starts_with('x'),~round(as.numeric(.x))))

# pop data in 2024-2030(from U.N.)----
df_li2<-read.xlsx("data/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx",sheet=2)
names(df_li2)<-df_li2[10,]
df_li2<-df_li2[-(1:10),]
df_li2 <-df_li2 %>% clean_names()
df_pop_un_2024_2030<-df_li2 %>%
  filter(year %in% 2024:2030,type=='Country/Area') %>%
  dplyr::select(year,region_subregion_country_or_area,type,x0,x1,x2,x3,x4,x5) %>%
  mutate(across(starts_with('x'),~round(as.numeric(.x))))
nrow(df_pop_un_2024_2030)/7 # 237country

# compare 2019 data (from Professor and U.N.)----
df_pop_2019_lj<-left_join(df_pop_li_2019,df_pop_un_2019,
                          by=c('Country.Name'='region_subregion_country_or_area'))

df_pop_2019_lj.x0_is_na<- df_pop_2019_lj %>%
  filter(is.na(x0))

lj2<-df_pop_2019_lj.x0_is_na %>%
  dplyr::select(1:9) %>%
  left_join(df_pop_un_2019,
            by=c('WHOName'='region_subregion_country_or_area'))

lj2 %>%
  filter(is.na(x0)) %>% view()
# [1] "Micronesia (Federated States of)"         
# [2] "The former Yugoslav Republic of Macedonia"
# [3] "Democratic People's Republic of Korea"    
# [4] "Reunion"                                  
# [5] "Turkey"  

df_pop_special<-df_pop_un_2019 %>% filter(
  str_detect(region_subregion_country_or_area,'Micro') |
  str_detect(region_subregion_country_or_area,'Macedonia') |
  str_detect(region_subregion_country_or_area,'Dem..*Korea') |
  str_detect(region_subregion_country_or_area,'R.union') |
  str_detect(region_subregion_country_or_area,'T.rkiye')
)

df_pop_special %>%
  mutate(region_subregion_country_or_area=
           case_when(str_detect(region_subregion_country_or_area,
                                'Micro')~"Micronesia (Federated States of)",
                     str_detect(region_subregion_country_or_area,
                                'Macedonia')~"The former Yugoslav Republic of Macedonia",
                     str_detect(region_subregion_country_or_area,
                                'Dem..*Korea')~"Democratic People's Republic of Korea",
                     str_detect(region_subregion_country_or_area,
                                'R.union')~"Reunion",
                     str_detect(region_subregion_country_or_area,
                                'T.rkiye')~"Turkey"))

# left join pop 2019
lj1<-df_pop_li_2019 %>% left_join(df_pop_un_2019,
                               by=c('Country.Name'='region_subregion_country_or_area'))

lj2<-lj1 %>% filter(is.na(x0)) %>% dplyr::select(1:9) %>%
  left_join(df_pop_un_2019,
            by=c('WHOName'='region_subregion_country_or_area'))

lj3<-lj2 %>% filter(is.na(x0)) %>% dplyr::select(1:9) %>%
  left_join(df_pop_un_2019 %>%
              mutate(region_subregion_country_or_area=
                       case_when(str_detect(region_subregion_country_or_area,
                                            'Micro')~"Micronesia (Federated States of)",
                                 str_detect(region_subregion_country_or_area,
                                            'Macedonia')~"The former Yugoslav Republic of Macedonia",
                                 str_detect(region_subregion_country_or_area,
                                            'Dem..*Korea')~"Democratic People's Republic of Korea",
                                 str_detect(region_subregion_country_or_area,
                                            'R.union')~"Reunion",
                                 str_detect(region_subregion_country_or_area,
                                            'T.rkiye')~"Turkey",
                                 T~region_subregion_country_or_area)),
            by=c('WHOName'='region_subregion_country_or_area'))

df_pop_2019_check<-bind_rows(list(
  'Country.Name'=lj1 %>% filter(!is.na(x0)),
  'WHOName'=lj2 %>% filter(!is.na(x0)),
  'WHOName after revised'=lj3
),.id='Join_by') %>%
  mutate(y0_x0=Y0/x0)

plot(df_pop_2019_check$y0_x0)
df_pop_2019_check %>%
  summarise(across(c(Y0,x0),~sum(.x,na.rm=T)))
# Y0     x0
# 136062 136112

df_pop_2019_check %>%
  summarise(across(c(Y0,x0),~sum(.x,na.rm=T)),.by=WHORegion)

# left join pop 2024 ----
get_pop_by_year<-function(df_pop_li_2019=df_pop_li_2019,
                          df_pop_un_2024_2030=df_pop_un_2024_2030,
                          y=2024){
  lj1<-df_pop_li_2019 %>% left_join(filter(df_pop_un_2024_2030,year==y),
                                    by=c('Country.Name'='region_subregion_country_or_area'))
  
  lj2<-lj1 %>% filter(is.na(x0)) %>% dplyr::select(1:9) %>%
    left_join(filter(df_pop_un_2024_2030,year==y),
              by=c('WHOName'='region_subregion_country_or_area'))
  
  lj3<-lj2 %>% filter(is.na(x0)) %>% dplyr::select(1:9) %>%
    left_join(filter(df_pop_un_2024_2030,year==y) %>%
                mutate(region_subregion_country_or_area=
                         case_when(str_detect(region_subregion_country_or_area,
                                              'Micro')~"Micronesia (Federated States of)",
                                   str_detect(region_subregion_country_or_area,
                                              'Macedonia')~"The former Yugoslav Republic of Macedonia",
                                   str_detect(region_subregion_country_or_area,
                                              'Dem..*Korea')~"Democratic People's Republic of Korea",
                                   str_detect(region_subregion_country_or_area,
                                              'R.union')~"Reunion",
                                   str_detect(region_subregion_country_or_area,
                                              'T.rkiye')~"Turkey",
                                   T~region_subregion_country_or_area)),
              by=c('WHOName'='region_subregion_country_or_area'))
  bind_rows(list(
    'Country.Name'=lj1 %>% filter(!is.na(x0)),
    'WHOName'=lj2 %>% filter(!is.na(x0)),
    'WHOName after revised'=lj3
  ),.id='Join_by')
}

df_pop_2024<-get_pop_by_year(df_pop_li_2019,df_pop_un_2024_2030,2024)

pop_by_country_2024<-df_pop_2024 %>%
  dplyr::select(Join_by,ISOCountry,Country.Name,WHOName,WHORegion,year,x0,x1,x2,x3,x4)

# left join pop 2026 ----

df_pop_2026<-get_pop_by_year(df_pop_li_2019,df_pop_un_2024_2030,2026)

## Replace pop 2026 with pop 2025 ----
df_pop_2025<-get_pop_by_year(df_pop_li_2019,df_pop_un_2024_2030,2025)

df_pop_2025 %>%
  select(x0:x5) %>%
  summarise(across(everything(),~sum(.x,na.rm=T)))

df_pop_2026 %>%
  select(x0:x5) %>%
  summarise(across(everything(),~sum(.x,na.rm=T)))

df_pop_2026<-df_pop_2025

## !!! Replace end --------------
pop_by_country_2026<-df_pop_2026 %>%
  dplyr::select(Join_by,ISOCountry,Country.Name,WHOName,WHORegion,year,x0,x1,x2,x3,x4)


# Export Data----
export(df_pop_2019_check,'rda/df_pop_2019_check.rds')
export(pop_by_country_2024,'rda/pop_by_country_2024.rds')
export(df_pop_un_2019,'rda/df_pop_un_2019.rds')
export(df_pop_un_2024_2030,'rda/df_pop_un_2024_2030.rds')

# Countries added ----
setdiff(df_pop_un_2019$region_subregion_country_or_area,pop_mor.raw$Country.Name)
setdiff(pop_mor.raw$Country.Name,df_pop_un_2019$region_subregion_country_or_area)

df_pop_un_2019 %>% filter(region_subregion_country_or_area %in% 
                            setdiff(df_pop_un_2019$region_subregion_country_or_area,pop_mor.raw$Country.Name)
) %>% view()

pop_mor.raw

# Pop 2026----

export(pop_by_country_2026,'rda/pop_by_country_2026.rds')

# Population data from model ----
df_pop_2019_2026<-left_join(
  pop_mor.raw %>%
    dplyr::select(ISOCountry,Country.Name,WHORegion,Income2019,pop_infant_2019=Y0),
  pop_by_country_2026 %>%
    dplyr::select(ISOCountry,pop_infant_2026=x0)
)

df_pop_2019_2026 %>%
  summarise(across(starts_with('pop_infant'),~sum(.,na.rm = T)),.by=WHORegion)

df_pop_2019_2026 %>%
  summarise(across(starts_with('pop_infant'),~sum(.,na.rm = T)))

export(df_pop_2019_2026,'model_data/df_pop_2019_2026.rds')


sum(df_pop_2019_2026$pop_infant_2026)


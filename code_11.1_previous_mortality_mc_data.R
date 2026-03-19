#
# ------------------------------------------------------------------------------
# Script: code_11.1_previous_mortality_mc_data.R
# Purpose:
#   Prepare country-level community mortality inputs (Monte Carlo draws or
#   summaries) to be used in the final static model, including handling missing
#   countries by imputation/replacement rules.
#
# Inputs:
#   - data/mor_all_DeCoDe.predict.rds (mortality prediction draws)
#   - workspaceToBegin.RData (legacy objects for consistency checks)
#   - rda/pop_mor.raw.rds and rda/df_pop_center_nir.rds (country metadata)
#
# Outputs:
#   - `model_data/mr_mc_all.rds` (prepared mortality dataset for modelling)
#
# Usage:
#   source("code_11.1_previous_mortality_mc_data.R")
#
# Notes:
#   Replacement for missing countries is done via income-level medians; review
#   the logic if the country list or hierarchies change.
# ------------------------------------------------------------------------------

library(rio)
library(tidyverse)

## All Results for Community Mortality Rate ----
mort_mc<-import('data/mor_all_DeCoDe.predict.rds')

mort_mc %>%
  filter(Country.Name=='China') %>% view()

load("workspaceToBegin.RData")

all.equal(mor_all_DeCoDe.predict,mort_mc)

mor_all_DeCoDe.predict %>% filter(is.na(Y0)) %>% distinct(ISOCountry)

## Countries
# Countries Categary 
df_pop<-left_join(
  import('rda/pop_mor.raw.rds') %>% # code_08_
    dplyr::select(ISOCountry,Income2019),
  import('rda/df_pop_center_nir.rds') %>% 
    transmute(ISOCountry,WHORegion,region_ns_zone,Country.Name,WHOName,type,pop_infant_2026=x0)) %>%
  mutate(region_income=paste(WHORegion,Income2019,sep='_'),.after=WHORegion)

setdiff(df_pop$ISOCountry,mort_mc$ISOCountry) #206-195=11

setdiff(mort_mc$ISOCountry,df_pop$ISOCountry)

df_pop %>%
  filter(ISOCountry %in% setdiff(df_pop$ISOCountry,mort_mc$ISOCountry)) %>%
  View()

df_pop %>%
  filter(Country.Name=='China') %>% pull(pop_infant_2026)


## MC 195 ----
mort_mc %>%
  dplyr::filter(Country.Name=='China') %>% dplyr::select(m0001_N,m0106_N,m0612_N,Y0)

# China Y0 16720

mr_mc_195<-mort_mc %>%
  transmute(ISOCountry,Country.Name,WHORegion,Income2019,index,mor_all=(m0001_N+m0106_N+m0612_N)/Y0) # mortality

summary(mr_mc_195$mor_all)

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.003   0.072   0.206   0.370   0.511   6.204   11000 

mr_mc_195 %>% filter(is.na(mor_all)) %>% distinct(ISOCountry) %>% pull() # 11

mort_mc %>%
  filter(ISOCountry %in% (mr_mc_195 %>% filter(is.na(mor_all)) %>% pull(ISOCountry))) %>% 
  distinct(ISOCountry,.keep_all = T) %>%
  View()

mr_mc_195.2<-mr_mc_195 %>%
  mutate(mor_all=case_when(ISOCountry %in% (mr_mc_195 %>% filter(is.na(mor_all)) %>% distinct(ISOCountry) %>% pull())~0,
                           T~mor_all))

mr_mc_195.2 %>% filter(is.na(mor_all)) %>% distinct(ISOCountry) %>% pull()

# median by income and index
mr_mc_195.2_sum_by_income<-mr_mc_195.2 %>%
  dplyr::summarise(across(mor_all,median),.by=c(Income2019,index))

# MC 11 ----

# 206-195=11 countries ----
df_pop %>%
  filter(ISOCountry %in% setdiff(df_pop$ISOCountry,mort_mc$ISOCountry)) %>%
  distinct(ISOCountry,.keep_all = T) %>%
  View()

# replace with income median
mr_mc_11<-df_pop %>%
  filter(ISOCountry %in% setdiff(df_pop$ISOCountry,mort_mc$ISOCountry)) %>%
  dplyr::select(ISOCountry,Country.Name,WHORegion,Income2019,WHOName) %>%
  left_join(mr_mc_195.2_sum_by_income,by='Income2019') %>%
  mutate(replace_by='Median_by_income_index')

# MC 206 ----

mr_mc_all<-bind_rows(
  mr_mc_195.2,
  mr_mc_11
)

hist(mr_mc_all$mor_all,breaks = 50)
summary(mr_mc_all$mor_all)

mr_mc_all %>%
  ggplot(aes(mor_all,fill=WHORegion)) +
  geom_density()

rio::export(mr_mc_all,'model_data/mr_mc_all.rds')


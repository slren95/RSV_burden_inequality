#
# ------------------------------------------------------------------------------
# Script: code_uptake.R
# Purpose:
#   Clean and extract WHO vaccine coverage data (e.g., Tdap proxies) as reference
#   inputs for modelling RSVpreF maternal vaccine uptake assumptions.
#
# Inputs:
#   - data/WHO-uptake *.xlsx (WHO coverage export; see `import()` call below)
#   - rda/df_abrysvo.csv (country list and metadata)
#   - model_data/df_type.rds (product status/type by country)
#
# Outputs:
#   - docs/df_type.xlsx
#   - docs/Tdap.xlsx
#
# Usage:
#   source("code_uptake.R")
# ------------------------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(rio)

data<-import('data/WHO-uptake 2025-24-10 14-0.xlsx')

df_abrysvo <- import('rda/df_abrysvo.csv')%>%
  relocate(WHORegion,Income2019,.after=ISO2)

df_type<-import('model_data/df_type.rds')

export(df_type,'docs/df_type.xlsx')


data %>% 
  filter(CODE %in% (df_abrysvo %>% filter(type=='NIP-included') %>% pull(ISOCountry))) %>%
  count(ANTIGEN,ANTIGEN_DESCRIPTION)

bind_rows(
  data %>% 
    filter(CODE %in% (df_abrysvo %>% filter(type=='NIP-included') %>% pull(ISOCountry)),
           ANTIGEN=='PERCV_PW',!is.na(COVERAGE)) %>%
    slice_max(YEAR,by=CODE),
  data %>% 
    filter(CODE %in% (df_abrysvo %>% filter(type=='NIP-included') %>% pull(ISOCountry)),
           ANTIGEN=='TT2PLUS',!is.na(COVERAGE)) %>%
    slice_max(YEAR,by=CODE) 
) %>%
  export('docs/Tdap.xlsx')


#
# ------------------------------------------------------------------------------
# Script: code_06_study_inclusion.R
# Purpose:
#   Curate and export study-level inclusion lists (study sites / references) used
#   in the GLMM/meta-analysis, and generate supporting appendix tables and plots.
#
# Inputs:
#   - workspaceToBegin.RData
#   - rda/code_01_incidence.RData
#   - rda/code_02_hospitalisation.RData
#   - rda/code_03_Inc_to_hos_ratio.RData
#   - rda/code_04_in_hos_CFR.RData
#   - rda/code_05_overall_mortality.RData
#   - functions.R
#
# Outputs:
#   - Appendix tables under `docs/` (e.g., `Studies Included_*.xlsx`).
#   - Figures under `plot/` generated via `ggsave()`.
#
# Usage:
#   source("code_06_study_inclusion.R")
#
# Notes:
#   This script is mainly for documentation/reporting rather than core modelling.
# ------------------------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(ggsci)
library(janitor)
library(openxlsx)
library(showtext)
source('functions.R')
font_add_google(name = "Amatic SC", family = "amatic-sc")
showtext_auto()
load("workspaceToBegin.RData")
# 6. Study-level----
load('rda/code_01_incidence.RData')
load('rda/code_02_hospitalisation.RData')
load('rda/code_03_Inc_to_hos_ratio.RData')
load('rda/code_04_in_hos_CFR.RData')
load('rda/code_05_overall_mortality.RData')

region_label<-c(
  'AFR' = 'African Region',
  'AMR' = 'Region of the Americas',
  'EMR' = 'Eastern Mediterranean Region',
  'EUR' = 'European Region',
  'SEAR' = 'South-East Asia Region',
  'WPR' = 'Western Pacific Region'
)



# 6.1 Community incidence----
WHO_inclusion_com <- left_join(com_rate_average.minimal[com_rate_average.minimal$AGEGR %in% c("0-<6m", "6-<12m", "0-<12m", "0-<24m", "0-<36m", "0-<60m"),
                                                        c("AGEGR", "SID")] %>% group_by(SID) %>%
                                 dplyr::summarise(method = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "0-<12m"), "direct", "indirect")[1]),
                               all.raw) # 31
WHO_inclusion_com1 <- left_join(com_rate_average.minimal[com_rate_average.minimal$AGEGR %in% c("0-<6m", "6-<12m", "0-<12m", "0-<24m", "0-<36m", "0-<60m") &
                                                          !is.na(com_rate_average.minimal$ALRI_N) & !is.na(com_rate_average.minimal$ALRI_Deno),
                                                        c("AGEGR", "SID")] %>% group_by(SID) %>%
                                 dplyr::summarise(method = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "0-<12m"), "direct", "indirect")[1]),
                               all.raw) # 29

WHO_inclusion_com2<-com_rate_average.minimal[com_rate_average.minimal$AGEGR %in% c("0-<6m", "6-<12m", "0-<12m", "0-<24m", "0-<36m", "0-<60m") &
                                               !is.na(com_rate_average.minimal$ALRI_N),
                         c("AGEGR", "SID")] %>% 
  mutate(method = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "0-<12m"), "direct", "indirect")) %>% 
  group_by(SID) %>%
  summarise(method=paste(unique(method),collapse=',')) %>% 
  mutate(method=ifelse(method=='indirect','indirect','direct')) %>%
  left_join(all.raw)

identical(WHO_inclusion_com1,WHO_inclusion_com2)

## only 0-<12m ----

WHO_inclusion_com_only012<-com_rate_average.minimal[com_rate_average.minimal$AGEGR %in% c("0-<12m", "0-<24m", "0-<36m", "0-<60m") &
                           !is.na(com_rate_average.minimal$ALRI_N),
                         c("AGEGR", "SID")] %>% 
  mutate(method = ifelse(AGEGR %in% c("0-<12m"), "direct", "indirect")) %>% 
  group_by(SID) %>%
  summarise(method=paste(unique(method),collapse=',')) %>% 
  mutate(method=ifelse(method=='indirect','indirect','direct')) %>%
  left_join(all.raw)

nrow(WHO_inclusion_com_only012)

## Imputed Data Point ----
WHO_inclusion_com_only012 %>% filter(method=='indirect') %>%  pull(SID)
# [1] "L314" "T061" "T074" "T143" "T162"
com_rate_average.minimal %>%
  filter(SID %in% (WHO_inclusion_com_only012 %>% filter(method=='indirect') %>%  pull(SID))) %>% 
  select(SID,AGEGR)

ggsave(
  ggplot(data=map.data2,
       aes(x = long, y = lat, group = group, fill = whoreg)) +
  geom_polygon(color = "black",  linewidth = 0.1,alpha=0.7)+
  scale_fill_manual(name = NULL, 
                    values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                    labels = region_label[c("AFR","AMR","EMR","EUR","SEAR","WPR")],
                    na.translate=FALSE)+
  geom_jitter(data = WHO_inclusion_com_only012,
              aes(x = LON, y = LAT, shape = method,
                  group = method), fill = "red2",colour = "white",
              size=2, alpha = 0.7)+
  scale_shape_manual(name = NULL, values = c(21,22)) +
  ggthemes::theme_map()+
  theme(text = element_text(size = 30),
        legend.key.spacing.y = unit(.1, "cm"),
        legend.position = c(.05,.05))+ 
  guides(shape = guide_legend(override.aes = list(size = 5),order=2),
         fill = guide_legend(override.aes = list(colour = 'black',
                                                 linewidth=.3),
                             keywidth = unit(.5, "cm"),  
                             keyheight = unit(.5, "cm"),
                             order=1)),
  filename = "plot/map_com_rate.tiff", width = 10, height = 5,dpi=300)

wb_com<-WHO_inclusion_com_only012 %>%
  transmute(SID,References=paste0(Author0,' et al. ',PubYear),
            Country,Location=Location0,`WHO Region`=WHO,`Income level`=paste0(Income,'IC'),`Study period`=StudyPeriod,
            Specimen,`Diagnostic test`=ViralTest,
            Indirect=ifelse(method=='indirect','Yes',''),
            `QA score`=format(round(QA_all,2),nsmall=2)
            ) %>%
  mutate(Specimen=case_when(str_detect(Specimen,'Oro-pharyngeal in children less than one years ')~'OPS or NPS',T~Specimen)) %>%
  arrange(Indirect,desc(`QA score`)) %>%
  left_join(com_rate_average.minimal %>%
              filter(SID %in% (WHO_inclusion_com_only012 %>% filter(method=='indirect') %>%  pull(SID))) %>% 
              select(SID,AGEGR)) %>%
  mutate(AGEGR=ifelse(Indirect=='Yes',gsub('m',' months',AGEGR),'0-<12 months'),
         `WHO Region`=toupper(`WHO Region`))

nrow(wb_com)
createAppendixTable(wb_com, "docs/Studies Included_Com.xlsx")



# 6.2 Hospitalisation----
WHO_inclusion_hos <- left_join(hos_rate_average.minimal[hos_rate_average.minimal$AGEGR %in% 
                                                          c("0-<6m", "6-<12m", "0-<12m", "0-<24m", "0-<36m", "0-<60m") &
                                                          !is.na(hos_rate_average.minimal$HosALRI_N),
                                                        c("AGEGR", "SID")] %>% group_by(SID) %>%
                                 dplyr::summarise(method = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "0-<12m"), "direct", "indirect")[1]),
                               all.raw)

WHO_inclusion_hos1<-hos_rate_average.minimal[hos_rate_average.minimal$AGEGR %in% 
                                               c("0-<6m", "6-<12m", "0-<12m", "0-<24m", "0-<36m", "0-<60m") &
                                               !is.na(hos_rate_average.minimal$HosALRI_N),
                                             c("AGEGR", "SID")] %>% 
  mutate(method = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "0-<12m"), "direct", "indirect")) %>% 
  group_by(SID) %>%
  summarise(method=paste(unique(method),collapse=',')) %>% 
  mutate(method=ifelse(method=='indirect','indirect','direct')) %>%
  left_join(all.raw)

identical(WHO_inclusion_hos,WHO_inclusion_hos1)
nrow(WHO_inclusion_hos)

## only 0-<12m ----

WHO_inclusion_hos_only012<-hos_rate_average.minimal[hos_rate_average.minimal$AGEGR %in% c("0-<12m", "0-<24m", "0-<36m", "0-<60m") &
                                                                          !is.na(hos_rate_average.minimal$HosALRI_N),
                                                                        c("AGEGR", "SID")] %>%
  mutate(method = ifelse(AGEGR %in% c("0-<12m"), "direct", "indirect")) %>% 
  group_by(SID) %>%
  summarise(method=paste(unique(method),collapse=',')) %>% 
  mutate(method=ifelse(method=='indirect','indirect','direct')) %>%
  left_join(all.raw)

nrow(WHO_inclusion_hos_only012) # 105

#check double study one SID two studies
hos_rate_average.minimal[hos_rate_average.minimal$AGEGR %in% c("0-<12m", "0-<24m", "0-<36m", "0-<60m") &
                           !is.na(hos_rate_average.minimal$HosALRI_N),] %>%
  filter(SID %in% c('T004','T079')) %>% view()
#check end

## Imputed Data Point ----
WHO_inclusion_hos_only012 %>% filter(method=='indirect') %>%  pull(SID)
# [1] "L088" "L090" "L157" "L314" "T028" "T042" "T043" "T044" "T061" "T069" "T077" "T080" "T087" "T091" "T136" "T142" "T163" "U014"
hos_rate_average.minimal %>%
  filter(SID %in% (WHO_inclusion_hos_only012 %>% filter(method=='indirect') %>%  pull(SID))) %>% 
  select(SID,AGEGR) %>%
  filter(grepl('^0-',AGEGR)) %>%
  filter(!(SID=='L088' & AGEGR !='0-<60m'))

#L088

wb_hos<-WHO_inclusion_hos_only012 %>%
  transmute(SID,References=paste0(Author0,' et al. ',PubYear),
            Country,Location=Location0,`WHO Region`=WHO,`Income level`=paste0(Income,'IC'),`Study period`=StudyPeriod,
            Specimen,`Diagnostic test`=ViralTest,
            Indirect=ifelse(method=='indirect','Yes',''),
            `QA score`=format(round(QA_all,2),nsmall=2)) %>%
  arrange(Indirect,desc(`QA score`)) %>%
  left_join(hos_rate_average.minimal %>%
              filter(SID %in% (WHO_inclusion_hos_only012 %>% filter(method=='indirect') %>%  pull(SID))) %>% 
              select(SID,AGEGR) %>%
              filter(grepl('^0-',AGEGR)) %>%
              filter(!(SID=='L088' & AGEGR !='0-<60m'))) %>%
  mutate(AGEGR=ifelse(Indirect=='Yes',gsub('m',' months',AGEGR),'0-<12 months'),
         `WHO Region`=toupper(`WHO Region`))


nrow(wb_hos)

createAppendixTable(wb_hos, "docs/Studies Included_Hos.xlsx")

ggsave(
  ggplot(data=map.data2,
         aes(x = long, y = lat, group = group, fill = whoreg)) +
    geom_polygon(color = "black",  linewidth = 0.1,alpha=0.7)+
    scale_fill_manual(name = NULL, 
                      values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                      labels = region_label[c("AFR","AMR","EMR","EUR","SEAR","WPR")],
                      na.translate=FALSE)+
    geom_jitter(data =  WHO_inclusion_hos_only012,
                aes(x = LON, y = LAT, shape = method,
                    group = method), fill = "red2",colour = "white",
                size=2, alpha = 0.7)+
    scale_shape_manual(name = NULL, values = c(21,22)) +
    ggthemes::theme_map()+
    theme(text = element_text(size = 30),
          legend.key.spacing.y = unit(.1, "cm"),
          legend.position = c(.05,.05))+ 
    guides(shape = guide_legend(override.aes = list(size = 5),order=2),
           fill = guide_legend(override.aes = list(colour = 'black',linewidth=.3),
                               keywidth = unit(.5, "cm"),  
                               keyheight = unit(.5, "cm"),
                               order=1)),
  filename = "plot/map_hos_rate.tiff", width = 10, height = 5,dpi=300)


#6.3 CFR----
# code_04
WHO_inclusion_mor1<-hos_mor_average.minimal[hos_mor_average.minimal$AGEGR %in%  c("0-<12m"),
                                           c("AGEGR", "SID")] %>%
  distinct(SID,.keep_all = T) %>%
  left_join(all.raw)

identical(WHO_inclusion_mor,WHO_inclusion_mor1)
nrow(WHO_inclusion_mor)

wb_mor<-WHO_inclusion_mor1 %>%
  arrange(PubYear,-QA_all) %>%
  transmute(References=paste0(Author0,' et al. ',PubYear),
            Country,Location=Location0,`WHO Region`=toupper(WHO),`Income level`=paste0(Income,'IC'),`Study period`=StudyPeriod,
            Specimen,`Diagnostic test`=ViralTest,
            `QA score`=format(round(QA_all,2),nsmall=2))

nrow(wb_mor)

createAppendixTable(wb_mor, "docs/Studies Included_CFR.xlsx")

ggsave(
  ggplot() +
    geom_polygon(data = map.data2, 
                 aes(x = long, y = lat, group = group, fill = whoreg), colour = "grey50", size = 0.05)+
    scale_fill_manual(name = NULL, 
                      values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                      labels = region_label[c("AFR","AMR","EMR","EUR","SEAR","WPR")],
                      na.translate=FALSE)+
    geom_jitter(data = WHO_inclusion_mor,
                aes(x = LON, y = LAT), shape = 21, fill = "red2",colour = "white",
                size=2, alpha = 0.7)+
    ggthemes::theme_map()+
    theme(text = element_text(size = 30),
          legend.key.spacing.y = unit(.1, "cm"),
          legend.position = c(.05,.05))+ 
    guides(
      fill = guide_legend(override.aes = list(colour = 'black',linewidth=.3),
                          keywidth = unit(.5, "cm"),  
                          keyheight = unit(.5, "cm"))),
  filename = "plot/map_hos_mor.tiff", width = 10, height = 5,dpi=300
)

#6.4 Inclusion_overall----
WHO_inclusion_overall <- data.frame(
  SID = unique(c(WHO_inclusion_com_only012$SID, WHO_inclusion_hos_only012$SID, WHO_inclusion_mor$SID))
)
WHO_inclusion_overall$com <- WHO_inclusion_overall$SID %in% WHO_inclusion_com_only012$SID
WHO_inclusion_overall$hos <- WHO_inclusion_overall$SID %in% WHO_inclusion_hos_only012$SID
WHO_inclusion_overall$mor <- WHO_inclusion_overall$SID %in% WHO_inclusion_mor$SID
WHO_inclusion_overall <- left_join(WHO_inclusion_overall, 
                                   all.raw[c("SID","Author0","PubYear","Country", "WHO", "StudyPeriod", "StudyMY", "Specimen", "ViralTest", "QA_all")]) %>%
  mutate(QA_all=round(QA_all,2))

WHO_inclusion_overall %>% 
  summarise(across(c(com,hos,mor),~sum(.x))) # 29 105 96

WHO_inclusion_overall  %>% 
  count(PubYear=='Unpub')

write.xlsx(WHO_inclusion_overall, file = "WHO region/inclusion.xlsx", asTable = T)

save.image('rda/code_06_study_inclusion.RData')

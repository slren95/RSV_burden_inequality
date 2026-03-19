#
# ------------------------------------------------------------------------------
# Script: code_02_hospitalisation.R
# Purpose:
#   Estimate RSV-associated ALRI hospital admission rates by WHO region and by
#   World Bank income level, including imputation and meta-analytic pooling.
#
# Inputs:
#   - workspaceToBegin.RData: legacy data objects/functions from prior work.
#   - functions.R: shared helper functions used across analysis scripts.
#
# Outputs:
#   - Tables under `WHO region/` (CSV/XLSX) created by `write.*()` calls.
#   - Intermediate `.RData` objects if saved later in the script.
#   - Figures saved via `ggsave()` (see file names in the script body).
#
# Usage:
#   source("code_02_hospitalisation.R")
#
# Notes:
#   Execute after `workspaceToBegin.RData` is available and from project root.
# ------------------------------------------------------------------------------

# Load packages
rm(list=ls())
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(metafor)
library(stringr)
library(readxl)
library(mapdata)
library(maps)
library(maptools)
library(rgdal)
library(colorspace)
library(scales)
library(Amelia)
library(epiR)
library(openxlsx)
load("workspaceToBegin.RData")
#library(sjPlot)
library(showtext)
font_add_google(name = "Amatic SC", family = "amatic-sc")
showtext_auto()
source('functions.R')


# 2. Hos_ALRI----
# 2.1 Imputation----
hos_rate.impute_WHO <- genHosImputeTab_WHO(hos_rate_average.minimal)
hos_rate_impute_WHO.data <- do.call(
  rbind,
  by(hos_rate_average.minimal[
    with(hos_rate_average.minimal, 
         SID %in% intersect(setdiff(SID[AGEGR != "0-<12m" & !is.na(HosALRI_N)],SID[AGEGR == "0-<12m" & !is.na(HosALRI_N)]),
                            SID[AGEGR %in% c("0-<24m", "0-<36m", "0-<60m") & !is.na(HosALRI_N)]) 
         & AGEGR %in% c("0-<24m", "0-<36m", "0-<60m")),], 
    hos_rate_average.minimal[
      with(hos_rate_average.minimal, 
           SID %in% intersect(setdiff(SID[AGEGR != "0-<12m" & !is.na(HosALRI_N)],SID[AGEGR == "0-<12m" & !is.na(HosALRI_N)]),
                              SID[AGEGR %in% c("0-<24m", "0-<36m", "0-<60m") & !is.na(HosALRI_N)]) 
           & AGEGR %in% c("0-<24m", "0-<36m", "0-<60m")),]$SID,
    FUN = genHosImputeEach_WHO
  )
)
hos_rate_combined_WHO <- rbind(hos_rate_impute_WHO.data, hos_rate_average.minimal)

## Check L088 Imputed by which agegroup ----
df_split<-split(hos_rate_average.minimal[
  with(hos_rate_average.minimal, 
       SID %in% intersect(setdiff(SID[AGEGR != "0-<12m" & !is.na(HosALRI_N)],SID[AGEGR == "0-<12m" & !is.na(HosALRI_N)]),
                          SID[AGEGR %in% c("0-<24m", "0-<36m", "0-<60m") & !is.na(HosALRI_N)]) 
       & AGEGR %in% c("0-<24m", "0-<36m", "0-<60m")),], 
  hos_rate_average.minimal[
    with(hos_rate_average.minimal, 
         SID %in% intersect(setdiff(SID[AGEGR != "0-<12m" & !is.na(HosALRI_N)],SID[AGEGR == "0-<12m" & !is.na(HosALRI_N)]),
                            SID[AGEGR %in% c("0-<24m", "0-<36m", "0-<60m") & !is.na(HosALRI_N)]) 
         & AGEGR %in% c("0-<24m", "0-<36m", "0-<60m")),]$SID)

df.each<-df_split[[1]] # L088
# ref <- left_join(df.each[nrow(df.each),],hos_rate.impute_WHO) 0-60

## !!!check 107 ---- 
hos_rate_combined_WHO %>% filter(!(is.na(HosALRI_N)|is.na(HosALRI_Deno)))  %>% distinct(SID) %>% nrow() # 107
hos_rate_combined_WHO %>% filter(AGEGR=='0-<12m',!(is.na(HosALRI_N)|is.na(HosALRI_Deno)))  %>% distinct(SID) %>% nrow() # 105

setdiff(hos_rate_combined_WHO %>% filter(!(is.na(HosALRI_N)|is.na(HosALRI_Deno)))  %>% pull(SID),
        hos_rate_combined_WHO %>% filter(AGEGR=='0-<12m',!(is.na(HosALRI_N)|is.na(HosALRI_Deno)))  %>% pull(SID)
        ) # U142 U247

hos_rate_impute_WHO.data %>% distinct(AGEGR)
hos_rate_impute_WHO.data %>% 
  distinct(SID) %>% nrow() # 18  number of imputed (indirect)

SIDs_18<-hos_rate_impute_WHO.data %>% distinct(SID) %>% pull()

hos_rate_average.minimal %>% distinct(AGEGR)

hos_rate_average.minimal %>%  filter(AGEGR=='0-<12m',!is.na(HosALRI_N)) %>% distinct(SID) %>% nrow() # 87 number of direct

SIDs_105<-c(hos_rate_impute_WHO.data %>% distinct(SID) %>% pull(),
            hos_rate_average.minimal %>%  filter(AGEGR=='0-<12m',!is.na(HosALRI_N)) %>% distinct(SID) %>% pull()
)
c('U142','U247') %in% SIDs_105 # FALSE FALSE
c('U142','U247') %in% SIDs_18 # FALSE FALSE

# code_02_Hosp_Check


# 2.2 Main----
hos_rate_res_WHO <- rbind(
  do.call(rbind, 
          by(hos_rate_combined_WHO[hos_rate_combined_WHO$AGEGR=="0-<12m",],
             hos_rate_combined_WHO[hos_rate_combined_WHO$AGEGR=="0-<12m", "WHO"],
             genMetaRateEach.Impute, prefix = "HosALRI", varToKeep = c("AGEGR","WHO","Impute"))),
  do.call(rbind, 
          by(hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="0-<6m",],
             hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="0-<6m", "WHO"],
             genMetaRateEach, prefix = "HosALRI", varToKeep = c("AGEGR","WHO","Impute"))),
  do.call(rbind, 
          by(hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="6-<12m",],
             hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="6-<12m", "WHO"],
             genMetaRateEach, prefix = "HosALRI", varToKeep = c("AGEGR","WHO","Impute")))
)
hos_rate_res_WHO <- hos_rate_res_WHO[hos_rate_res_WHO$n.all>0,]
names(hos_rate_res_WHO)[2] <- "Group"
hos_rate_res_WHO <- hos_rate_res_WHO[names(hos_rate_res_WHO)!="I2"]
hos_rate_res_WHO <- left_join(hos_rate_res_WHO, pop_WHO.raw, by = c("AGEGR" = "AGEGR", "Group" = "WHO"))

# N.est /1000
hos_rate_res_WHO$N.est <- with(hos_rate_res_WHO, round(IR.est * Pop/1000),0) 
hos_rate_res_WHO$N.lci <- with(hos_rate_res_WHO, round(IR.lci * Pop/1000),0)
hos_rate_res_WHO$N.uci <- with(hos_rate_res_WHO, round(IR.uci * Pop/1000),0)
hos_rate_res_WHO <- do.call(rbind,
                            by(hos_rate_res_WHO, hos_rate_res_WHO[c("AGEGR")],
                               FUN = genRateGlobal, n.level = 6))
hos_rate_tab_WHO <- hos_rate_res_WHO[c("AGEGR", "Group", "n.all", "n.impute", "IR.est", "IR.lci", "IR.uci", "N.est", "N.lci", "N.uci")]
hos_rate_tab_WHO$AGEGR <- factor(hos_rate_tab_WHO$AGEGR, levels = c("0-<12m", "0-<6m", "6-<12m"))
hos_rate_tab_WHO$AGEGR <- factor(hos_rate_tab_WHO$AGEGR, levels = c("0-<12m", "0-<6m", "6-<12m"))
hos_rate_tab_WHO <- hos_rate_tab_WHO[order(hos_rate_tab_WHO$AGEGR),]
hos_rate_tab_WHO$Studies <- with(hos_rate_tab_WHO,
                                 paste(n.all, 
                                       ifelse(AGEGR!="0-<12m", 
                                              "", paste("(",n.impute,")", sep = "")),
                                       sep = ""))
hos_rate_tab_WHO$IR <- with(hos_rate_tab_WHO, paste(format(round(IR.est,1), nsmall=1),
                                                    "\n(",
                                                    format(round(IR.lci,1), nsmall=1),
                                                    "-",
                                                    format(round(IR.uci,1), nsmall=1),
                                                    ")", sep = ""))
hos_rate_tab_WHO$N <- with(hos_rate_tab_WHO, paste(format(round(N.est,0), nsmall=0),
                                                   "\n(",
                                                   format(round(N.lci,0), nsmall=0),
                                                   "-",
                                                   format(round(N.uci,0), nsmall=0),
                                                   ")", sep = ""))
hos_rate_tab_WHO <- (hos_rate_tab_WHO[c("AGEGR", "Group", "Studies", "IR", "N")] %>%
                       pivot_wider(names_from = Group,values_from = c(Studies,IR, N), values_fill = "-")
) %>% pivot_longer(!AGEGR,names_to = c("reporting", ".value"),
                   names_pattern = "(.*)_(.*)")
write.csv(hos_rate_tab_WHO, file = "WHO region/hos_rate_main.csv", row.names = FALSE)
write.xlsx(hos_rate_tab_WHO,file = 'WHO region/hos_rate_main.xlsx',asTable = T)
#tab_df(hos_rate_tab_WHO)

# !!!Check SEAR ----
hos_rate_res_WHO %>% filter(AGEGR=='0-<12m')

# exp(-4.077879)*1000*33607=569
# AGEGR  Group Impute       est        se n.all n.impute   IR.est    IR.lci   IR.uci       Pop    N.est    N.lci    N.uci
# 0-<12m.1 0-<12m    Afr      1 -4.373388 0.2678798    20        1 12.60845  7.458263 21.31500  36701.49  463.000  274.000  782.000
# 0-<12m.2 0-<12m    Amr      1 -3.855515 0.2087325    30        3 21.16270 14.057055 31.86014  14259.03  302.000  200.000  454.000
# 0-<12m.3 0-<12m    Emr      0 -3.508957 0.3816657     2        0 29.92810 14.164420 63.23531  18203.08  545.000  258.000 1151.000
# 0-<12m.4 0-<12m    Eur      1 -3.951399 0.1258763    27        7 19.22779 15.023851 24.60806  10655.06  205.000  160.000  262.000
# 0-<12m.5 0-<12m   Sear      1 -4.077879 0.2810306    12        3 16.94337  9.767460 29.39124  33607.61  569.000  328.000  988.000
# 0-<12m.6 0-<12m    Wpr      1 -4.079760 0.2075154    16        4 16.91153 11.260099 25.39941  22045.06  373.000  248.000  560.000
# 0-<12m.7 0-<12m Global      1        NA        NA   107       18 18.42075 14.556327 24.05349 135471.33 2495.483 1971.965 3258.558

# 2.3 Sensitivity analysis----
hos_rate_res_WHO.sens <- rbind(
  data.frame(
    do.call(rbind, 
            by(hos_rate_combined_WHO[hos_rate_combined_WHO$AGEGR=="0-<12m",],
               hos_rate_combined_WHO[hos_rate_combined_WHO$AGEGR=="0-<12m", "WHO"],
               genMetaRateEach.Impute, prefix = "HosALRI", varToKeep = c("AGEGR","WHO","Impute"))),
    method = "Main analysis"
  ),
  data.frame(
    do.call(rbind, 
            by(hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="0-<12m" & 
                                          hos_rate_average.minimal$WHO %in% (hos_rate_res_WHO$Group[hos_rate_res_WHO$Impute==1]),],
               hos_rate_average.minimal[hos_rate_average.minimal$AGEGR=="0-<12m"& 
                                          hos_rate_average.minimal$WHO %in% (hos_rate_res_WHO$Group[hos_rate_res_WHO$Impute==1]), "WHO"],
               genMetaRateEach, prefix = "HosALRI", varToKeep = c("AGEGR","WHO","Impute"))),
    method = "No imputation"
  ),
  
  data.frame(
    do.call(rbind, 
            by(hos_rate_combined_WHO[hos_rate_combined_WHO$AGEGR=="0-<12m" & hos_rate_combined_WHO$QA_all>=0.6,],
               hos_rate_combined_WHO[hos_rate_combined_WHO$AGEGR=="0-<12m"& hos_rate_combined_WHO$QA_all>=0.6, "WHO"],
               genMetaRateEach.Impute, prefix = "HosALRI", varToKeep = c("AGEGR","WHO", "Impute"))),
    method = "High-quality studies only"
  )
)

# 2.4 By income----
hos_rate_res_WHO.income <- do.call(rbind, 
                                   by(hos_rate_combined_WHO[hos_rate_combined_WHO$AGEGR=="0-<12m",],
                                      hos_rate_combined_WHO[hos_rate_combined_WHO$AGEGR=="0-<12m", c("WHO", "Income")],
                                      genMetaRateEach.Impute, prefix = "HosALRI", varToKeep = c("AGEGR","WHO","Income","Impute")))
hos_rate_res_WHO.income <- hos_rate_res_WHO.income[hos_rate_res_WHO.income$n.all>0,]

# 2.5 Map showing inclusion----
# Inclusion
hos_rate_combined_WHO[hos_rate_combined_WHO$AGEGR=="0-<12m",] %>% 
  distinct(SID,.keep_all = T) %>% 
  filter(!is.na(HosALRI_Deno),!is.na(HosALRI_N)) %>% 
  count(WHO)


WHO_inclusion_hos <- left_join(hos_rate_average.minimal[hos_rate_average.minimal$AGEGR %in% 
                                                          c("0-<6m", "6-<12m", "0-<12m", "0-<24m", "0-<36m", "0-<60m") &
                                                          !is.na(hos_rate_average.minimal$HosALRI_N),
                                                        c("AGEGR", "SID")] %>% group_by(SID) %>%
                                 dplyr::summarise(method = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "0-<12m"), "direct", "indirect")[1]),
                               all.raw)
ggsave(
  ggplot(data = map.data2, 
         aes(x = long, y = lat, group = group, fill = whoreg)) +
    geom_polygon(colour = "grey50", size = 0.05)+
    scale_fill_manual(name = NULL, values = hsv(c(210,0,60,150,288,20)/360, c(0.3), c(0.9)), na.translate=FALSE)+
    geom_jitter(data = WHO_inclusion_hos,
                aes(x = LON, y = LAT, shape = method,
                    group = method), fill = "red2",colour = "white",
                width = 1, height =1, alpha = 0.7)+
    scale_shape_manual(name = NULL, values = c(21,22,24)) +
    ggthemes::theme_map()+
    theme(text = element_text(size = 20))+ 
    guides(shape = guide_legend(override.aes = list(size = 5)),
           fill = guide_legend(override.aes = list(size = 3.5, colour = NA))),
  filename = "plot/map_hos_rate.tiff", width = 10, height = 5,dpi=300
)

#2.6 Plot ----
# For the roadshow

ggsave(
  ggplot(data = hos_rate_res_WHO[hos_rate_res_WHO$Group!="Global",] %>%
           add_count(AGEGR),
         aes(x = AGEGR, y = IR.est, colour = Group,width=0.05*n))+
    geom_point(size=2,position = position_dodge(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci),position = position_dodge(width = 0.5))+
    scale_colour_lancet(name = NULL,labels = c("AFR","AMR","EMR","EUR","SEAR","WPR"))+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                       expand=expansion(),limits = c(0,70),
                       breaks=seq(0,70,by=10),labels = seq(0,70,by=10))+
    scale_x_discrete(name = NULL)+
    theme_bw()+
    theme(text =element_text(size=30),
          legend.title = element_text(size=28),
          legend.position = c(.995,.99),
          legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          plot.margin = margin(t=5,b=5,l=5,r=5),
          panel.grid=element_blank(),
          panel.border = element_blank(),
          axis.ticks.length = unit(.15, "cm"),
          axis.line = element_line(size=.25),
          axis.ticks = element_line(size=.25)),
  filename = "plot/hos_rate.tiff", width = 8, height = 4.5,dpi=300
)

ggsave(
  ggplot(data = hos_rate_res_WHO[hos_rate_res_WHO$Group!="Global" & hos_rate_res_WHO$AGEGR=="0-<12m",],
         aes(x = "", y = N.est, fill = Group))+
    geom_bar(stat = "identity", width = 1, colour = "white", alpha = 0.6)+
    geom_label(aes(label = paste(N.est, " (",scales::percent(N.est/sum(N.est), accuracy = 1), ")",sep = ""),
                   group = Group,
                   x = 1.38), show.legend = FALSE, fill = "white",alpha = 0.7,size=10,
               position = position_stack(vjust = 0.5))+
    scale_fill_lancet(name = NULL, labels = c("AFR","AMR","EMR","EUR","SEAR","WPR"))+
    coord_polar("y", start = 0, direction = -1)+
    theme_void()+
    theme(legend.text=element_text(size=20)),
  filename = "plot/hos_N_pie.tiff", width = 6, height = 4,dpi=300
)

ggsave(
  hos_rate_res_WHO.sens[!duplicated(hos_rate_res_WHO.sens[c("WHO", "n.all")]),] %>%
    mutate(method=factor(method,levels=c('Main analysis','No imputation','High-quality studies only'))) %>%
    add_count(WHO) %>%
    ggplot(aes(x = WHO, y = IR.est, colour = method,width=0.1*n))+
    geom_point(size=2,position = position_dodge(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci),position = position_dodge(width = 0.5))+
    scale_x_discrete(name = NULL, labels = c("AFR","AMR","EMR","EUR","SEAR","WPR"))+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                       expand=expansion(),limits = c(0,70),
                       breaks=seq(0,70,by=10),labels = seq(0,70,by=10))+
    scale_colour_lancet(name = NULL)+
    theme_bw()+
    theme(text =element_text(size=30),
          legend.title = element_text(size=28),
          legend.position = c(.995,.99),
          legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          plot.margin = margin(t=5,b=5,l=5,r=5),
          panel.grid=element_blank(),
          panel.border = element_blank(),
          axis.ticks.length = unit(.15, "cm"),
          axis.line = element_line(size=.25),
          axis.ticks = element_line(size=.25)),
  filename = "plot/hos_rate_sens.tiff",  width = 9, height = 4,dpi=200
)

ggsave(
  ggplot(data = hos_rate_res_WHO.income %>%
           add_count(WHO), 
         aes(x = WHO, y = IR.est,width=0.1*n,
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low income", "Lower-middle income", "Upper-middle income", "High income"))))+
    geom_point(size=2,position = position_dodge(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), position = position_dodge(width = 0.5))+
    scale_x_discrete(name = NULL, labels = c("AFR","AMR","EMR","EUR","SEAR","WPR"))+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                       expand=expansion(),limits = c(0,110),
                       breaks=seq(0,110,by=10),labels = seq(0,110,by=10))+
    scale_colour_lancet(name = "Income level")+
    theme_bw()+
    theme(text =element_text(size=30),
          legend.title = element_text(size=28),
          legend.position = c(.998,.99),
          legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          plot.margin = margin(t=5,b=5,l=5,r=5),
          panel.grid=element_blank(),
          panel.border = element_blank(),
          axis.ticks.length = unit(.15, "cm"),
          axis.line = element_line(size=.25),
          axis.ticks = element_line(size=.25)),
  filename = "plot/hos_rate_income.tiff", width = 7, height = 4.5,dpi=300,device = 'tiff'
)

save(hos_rate.impute_WHO,
     hos_rate_impute_WHO.data,
     hos_rate_combined_WHO,
     hos_rate_res_WHO,
     hos_rate_tab_WHO,
     hos_rate_res_WHO.sens,
     hos_rate_res_WHO.income,
     WHO_inclusion_hos,
     file='rda/code_02_hospitalisation.RData'
)

# for paper ----
createAppendixTable(hos_rate_tab_WHO,'docs/hos_rate_tab_WHO.xlsx')


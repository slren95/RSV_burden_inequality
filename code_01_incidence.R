#
# ------------------------------------------------------------------------------
# Script: code_01_incidence.R
# Purpose:
#   Estimate RSV-associated ALRI incidence rates (community incidence) by WHO
#   region and by World Bank income level, including imputation for missing
#   age-strata and meta-analytic pooling.
#
# Inputs:
#   - workspaceToBegin.RData: legacy data objects/functions from Li et al. (2022).
#   - functions.R: shared helper functions (table export, themes, utilities).
#
# Outputs:
#   - CSV/XLSX tables under `WHO region/` (e.g., `com_rate_main.csv/.xlsx`).
#   - Optional formatted tables under `docs/` (see `createAppendixTable()` calls).
#   - Figures saved via `ggsave()` (see file names in the script body).
#
# Usage:
#   source("code_01_incidence.R")
#
# Notes:
#   Run from the project root so relative paths resolve correctly.
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
library(sjPlot)
library(officer)
library(flextable)
load("workspaceToBegin.RData") # Raw data and functions from our previous study (Li et al. Lancet 2022)
library(openxlsx)
library(showtext)
font_add_google(name = "Amatic SC", family = "amatic-sc")
showtext_auto()
source('functions.R')

# 1. ALRI incidence ----
# 1.1 Imputation----
com_rate.impute_WHO <- genComImputeTab_WHO(com_rate_average.minimal)
com_rate_impute_WHO.data <- do.call(
  rbind,
  by(com_rate_average.minimal[
    with(com_rate_average.minimal, 
         SID %in% intersect(setdiff(SID[AGEGR != "0-<12m" & !is.na(ALRI_N)],SID[AGEGR == "0-<12m" & !is.na(ALRI_N)]),
                            SID[AGEGR %in% c("0-<24m", "0-<36m", "0-<60m") & !is.na(ALRI_N)]) 
         & AGEGR %in% c("0-<24m", "0-<36m", "0-<60m")),], 
    com_rate_average.minimal[
      with(com_rate_average.minimal, 
           SID %in% intersect(setdiff(SID[AGEGR != "0-<12m" & !is.na(ALRI_N)],SID[AGEGR == "0-<12m" & !is.na(ALRI_N)]),
                              SID[AGEGR %in% c("0-<24m", "0-<36m", "0-<60m") & !is.na(ALRI_N)]) 
           & AGEGR %in% c("0-<24m", "0-<36m", "0-<60m")),]$SID,
    FUN = genComImputeEach_WHO
  )
)
com_rate_combined_WHO <- rbind(com_rate_impute_WHO.data, com_rate_average.minimal)
# 1.2 Main----
com_rate_res_WHO <- rbind(
  do.call(rbind, 
          by(com_rate_combined_WHO[com_rate_combined_WHO$AGEGR=="0-<12m",],
             com_rate_combined_WHO[com_rate_combined_WHO$AGEGR=="0-<12m", "WHO"],
             genMetaRateEach.Impute, prefix = "ALRI", varToKeep = c("AGEGR","WHO","Impute"))),
  do.call(rbind, 
          by(com_rate_average.minimal[com_rate_average.minimal$AGEGR=="0-<6m",],
             com_rate_average.minimal[com_rate_average.minimal$AGEGR=="0-<6m", "WHO"],
             genMetaRateEach, prefix = "ALRI", varToKeep = c("AGEGR","WHO","Impute"))),
  do.call(rbind, 
          by(com_rate_average.minimal[com_rate_average.minimal$AGEGR=="6-<12m",],
             com_rate_average.minimal[com_rate_average.minimal$AGEGR=="6-<12m", "WHO"],
             genMetaRateEach, prefix = "ALRI", varToKeep = c("AGEGR","WHO","Impute")))
)
names(com_rate_res_WHO)[2] <- "Group"
com_rate_res_WHO <- com_rate_res_WHO[names(com_rate_res_WHO)!="I2"]
com_rate_res_WHO <- left_join(com_rate_res_WHO, pop_WHO.raw, by = c("AGEGR" = "AGEGR", "Group" = "WHO"))
com_rate_res_WHO$N.est <- with(com_rate_res_WHO, round(IR.est * Pop/1000),0)
com_rate_res_WHO$N.lci <- with(com_rate_res_WHO, round(IR.lci * Pop/1000),0)
com_rate_res_WHO$N.uci <- with(com_rate_res_WHO, round(IR.uci * Pop/1000),0)
com_rate_res_WHO <- do.call(rbind,
                            by(com_rate_res_WHO, com_rate_res_WHO[c("AGEGR")],
                               FUN = genRateGlobal, n.level = 6))
com_rate_tab_WHO <- com_rate_res_WHO[c("AGEGR", "Group", "n.all", "n.impute", "IR.est", "IR.lci", "IR.uci", "N.est", "N.lci", "N.uci")]
com_rate_tab_WHO$AGEGR <- factor(com_rate_tab_WHO$AGEGR, levels = c("0-<12m", "0-<6m", "6-<12m"))
com_rate_tab_WHO <- com_rate_tab_WHO[order(com_rate_tab_WHO$AGEGR),]
com_rate_tab_WHO$Studies <- with(com_rate_tab_WHO,
                                 paste(n.all, 
                                       ifelse(AGEGR!="0-<12m", 
                                              "", paste("(",n.impute,")", sep = "")),
                                       sep = ""))
com_rate_tab_WHO$IR <- with(com_rate_tab_WHO, paste(format(round(IR.est,1), nsmall=1),
                                                    "\n(",
                                                    format(round(IR.lci,1), nsmall=1),
                                                    "-",
                                                    format(round(IR.uci,1), nsmall=1),
                                                    ")", sep = ""))
com_rate_tab_WHO$N <- with(com_rate_tab_WHO, paste(format(round(N.est,0), nsmall=0),
                                                   "\n(",
                                                   format(round(N.lci,0), nsmall=0),
                                                   "-",
                                                   format(round(N.uci,0), nsmall=0),
                                                   ")", sep = ""))
com_rate_tab_WHO <- (com_rate_tab_WHO[c("AGEGR", "Group", "Studies", "IR", "N")] %>%
                       pivot_wider(names_from = Group,values_from = c(Studies,IR, N), values_fill = "-")
) %>% pivot_longer(!AGEGR,names_to = c("reporting", ".value"),
                   names_pattern = "(.*)_(.*)")
write.csv(com_rate_tab_WHO, file = "WHO region/com_rate_main.csv", row.names = FALSE)
write.xlsx(com_rate_tab_WHO,"WHO region/com_rate_main.xlsx",asTable = T)
tab_df(com_rate_tab_WHO)



createAppendixTable(com_rate_tab_WHO,'docs/com_rate_tab_WHO.xlsx')

# 1.3 Sensitivity analysis----
com_rate_res_WHO.sens <- rbind(
  data.frame(
    do.call(rbind, 
            by(com_rate_combined_WHO[com_rate_combined_WHO$AGEGR=="0-<12m",],
               com_rate_combined_WHO[com_rate_combined_WHO$AGEGR=="0-<12m", "WHO"],
               genMetaRateEach.Impute, prefix = "ALRI", varToKeep = c("AGEGR","WHO","Impute"))),
    method = "Main analysis"
  ),
  data.frame(
    do.call(rbind, 
            by(com_rate_average.minimal[com_rate_average.minimal$AGEGR=="0-<12m" & 
                                          com_rate_average.minimal$WHO %in% (com_rate_res_WHO$Group[com_rate_res_WHO$Impute==1]),],
               com_rate_average.minimal[com_rate_average.minimal$AGEGR=="0-<12m"& 
                                          com_rate_average.minimal$WHO %in% (com_rate_res_WHO$Group[com_rate_res_WHO$Impute==1]), "WHO"],
               genMetaRateEach, prefix = "ALRI", varToKeep = c("AGEGR","WHO","Impute"))),
    method = "No imputation"
  ),
  
  data.frame(
    do.call(rbind, 
            by(com_rate_combined_WHO[com_rate_combined_WHO$AGEGR=="0-<12m" & com_rate_combined_WHO$QA_all>=0.6,],
               com_rate_combined_WHO[com_rate_combined_WHO$AGEGR=="0-<12m"& com_rate_combined_WHO$QA_all>=0.6, "WHO"],
               genMetaRateEach.Impute, prefix = "ALRI", varToKeep = c("AGEGR","WHO", "Impute"))),
    method = "High-quality studies only"
  )
)

# 1.4 By income----
com_rate_res_WHO.income <- do.call(rbind, 
                                   by(com_rate_combined_WHO[com_rate_combined_WHO$AGEGR=="0-<12m",],
                                      com_rate_combined_WHO[com_rate_combined_WHO$AGEGR=="0-<12m", c("WHO", "Income")],
                                      genMetaRateEach.Impute, prefix = "ALRI", varToKeep = c("AGEGR","WHO","Income","Impute")))


# 1.5 Map showing inclusion----
# Inclusion
WHO_inclusion_com <- left_join(com_rate_average.minimal[com_rate_average.minimal$AGEGR %in% c("0-<6m", "6-<12m", "0-<12m", "0-<24m", "0-<36m", "0-<60m"),
                                                        c("AGEGR", "SID")] %>% group_by(SID) %>%
                                 dplyr::summarise(method = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "0-<12m"), "direct", "indirect")[1]),
                               all.raw)
ggplot(data=map.data2,
       aes(x = long, y = lat, group = group, fill = whoreg)) +
  geom_polygon(colour = "grey50", size = 0.05)+
  scale_fill_manual(name = NULL, values = hsv(c(210,0,60,150,288,20)/360, c(0.3), c(0.9)), na.translate=FALSE)+
  geom_jitter(data = WHO_inclusion_com,
              aes(x = LON, y = LAT, shape = method,
                  group = method), fill = "red2",colour = "white",
              width = 1, height =1, alpha = 0.7)+
  scale_shape_manual(name = NULL, values = c(21,22)) +
  ggthemes::theme_map()+
  theme(text = element_text(size = 12),
        panel.grid=element_blank())+ 
  guides(shape = guide_legend(override.aes = list(size = 5)),
         fill = guide_legend(override.aes = list(size = 3.5, colour = NA)))

ggsave(filename = "plot/map_com_rate.tiff", width = 10, height = 5,dpi=300)


# 1.6 Plot ----
# "Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific",
# "AFR","AMR","EMR","EUR","SEAR","WPR"
com_rate_res_WHO %>%
  filter(Group != "Global") %>%
  mutate(AGEGR=factor(AGEGR,levels=c('0-<6m','6-<12m','0-<12m'))) %>%
  add_count(AGEGR) %>%
  ggplot(aes(x = AGEGR, y = IR.est, colour = Group,width=0.05*n))+
  geom_point(size=2,position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), position = position_dodge(width = 0.5))+
  scale_colour_lancet(name = NULL, labels = c("AFR","AMR","EMR","EUR","SEAR","WPR","Global"))+
  scale_y_continuous(name = "Incidence rate (case per 1000)",
                     expand=expansion(mult=c(0,0.05)),limits = c(0,NA),
                     breaks=seq(0,350,by=50),labels = seq(0,350,by=50))+
  scale_x_discrete(name = NULL)+
  theme_bw()+
  theme(text =element_text(size=30),
        legend.position = c(.995,.99),
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        plot.margin = margin(t=5,b=5,l=5,r=5),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25))
ggsave(filename = "plot/com_rate.tiff", width = 8, height = 4.6,dpi = 300)

com_rate_res_WHO %>%
  filter(Group != "Global",AGEGR=="0-<12m") %>%
  ggplot(aes(x = "", y = N.est, fill = Group))+
  geom_bar(stat = "identity", width = 1, colour = "white", alpha = 0.6)+
  geom_label(aes(label = paste(N.est, " (",scales::percent(N.est/sum(N.est), accuracy = 1), ")",sep = ""),
                 group = Group,
                 x = 1.38), show.legend = FALSE, fill = "white",alpha = 0.7,size=10,
             position = position_stack(vjust = 0.5))+
  scale_fill_lancet(name = NULL, labels = c("AFR","AMR","EMR","EUR","SEAR","WPR"))+
  coord_polar("y", start = 0, direction = -1)+
  theme_void()+
  theme(legend.text=element_text(size=20),
        panel.grid=element_blank())
ggsave(filename = "plot/com_N_pie.tiff", width = 8, height = 4.6,dpi = 300)

com_rate_res_WHO.sens[!duplicated(com_rate_res_WHO.sens[c("WHO", "n.all")]),] %>%
  mutate(method=factor(method,levels=c('Main analysis','No imputation','High-quality studies only'))) %>%
  add_count(WHO) %>%
  ggplot(aes(x = WHO, width=0.1*n,
             y = IR.est, colour = method))+
  geom_point(size=2,position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), position = position_dodge(width = 0.5))+
  scale_x_discrete(name = NULL, labels = c("AFR","AMR","EMR","EUR","SEAR","WPR"))+
  scale_y_continuous(name = "Incidence rate (case per 1000)")+
  scale_colour_lancet(name = NULL)+
  theme_bw()+
  theme(text =element_text(size=30),
        legend.position = c(.995,.99),
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25))
ggsave(filename = "plot/com_rate_sens.tiff", width = 9, height = 4,dpi = 200)

ggplot(data = com_rate_res_WHO.income %>%
         add_count(WHO), 
       aes(x = WHO, y = IR.est, width=0.1*n,
           colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low income", "Lower-middle income", "Upper-middle income", "High income"))))+
  geom_point(size=2,position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci),position = position_dodge(width = 0.5))+
  scale_x_discrete(name = NULL, labels = c("AFR","AMR","EMR","EUR","SEAR","WPR"))+
  scale_y_continuous(name = "Incidence rate (case per 1000)",
                     expand=expansion(mult=c(0,0)),limits = c(0,450),
                     breaks=seq(0,450,by=50),labels = seq(0,450,by=50))+
  scale_colour_lancet(name = "Income level")+
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
        axis.ticks = element_line(size=.25))

ggsave(filename = "plot/com_rate_income.tiff", width = 7.5, height = 5,dpi = 300)

# Imputed Data point ----
com_rate_impute_WHO.data %>%
  distinct(SID,.keep_all = T) %>% pull(SID)

com_rate_average.minimal %>%
  filter(SID %in% (com_rate_impute_WHO.data %>%
           distinct(SID,.keep_all = T) %>% pull(SID))) %>%
  arrange(QA_all) %>%
  select(SID,Author0,WHO,AGEGR)

save(com_rate.impute_WHO,
     com_rate_impute_WHO.data,
     com_rate_combined_WHO,
     com_rate_res_WHO,
     com_rate_tab_WHO,
     com_rate_res_WHO.sens,
     com_rate_res_WHO.income,
     WHO_inclusion_com,
     file='rda/code_01_incidence.RData'
     )

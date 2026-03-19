#
# ------------------------------------------------------------------------------
# Script: mainScript.R
# Purpose:
#   End-to-end (legacy) analysis script for estimating RSV-associated burden and
#   generating key intermediate outputs (tables/figures) used across the project.
#
# Inputs:
#   - workspaceToBegin.RData: pre-processed data objects and helper functions
#     from the previous study (Li et al., Lancet 2022).
#
# Outputs:
#   - Various CSV/Excel exports and figure files written by downstream sections
#     (see `write.*()` / `ggsave()` calls within the script).
#
# Usage:
#   Run from the project root so relative paths resolve correctly:
#     source("mainScript.R")
#
# Notes:
#   - This script assumes many objects already exist after loading
#     `workspaceToBegin.RData`.
#   - Sections are organized by outcome (incidence, hospitalisation, mortality).
# ------------------------------------------------------------------------------

# Library----
rm(list=ls())
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(metafor)
library(stringr)
library(readxl)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(colorspace)
#library(gt)
library(scales)
library(Amelia)
library(epiR)
load("workspaceToBegin.RData")
# 1. ALRI----
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
# For the roadshow
ggsave(
  ggplot(data = com_rate_res_WHO[com_rate_res_WHO$Group!="Global",],
         aes(x = AGEGR, y = IR.est, colour = Group))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), width =0.5, position = position_dodge2(width = 0.5))+
    scale_colour_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    scale_y_continuous(name = "Incidence rate (case per 1000)")+
    scale_x_discrete(name = NULL)+
    theme_bw(),
  filename = "WHO region/roadshow/com_rate.tiff", width = 8, height = 4.5
)
ggsave(
  ggplot(data = com_rate_res_WHO[com_rate_res_WHO$Group!="Global" & com_rate_res_WHO$AGEGR=="0-<12m",],
         aes(x = "", y = N.est, fill = Group))+
    geom_bar(stat = "identity", width = 1, colour = "white", alpha = 0.6)+
    geom_label(aes(label = paste(N.est, " (",scales::percent(N.est/sum(N.est), accuracy = 1), ")",sep = ""),
                   group = Group,
                   x = 1.38), show.legend = FALSE, fill = "white",alpha = 0.7,
               position = position_stack(vjust = 0.5))+
    scale_fill_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    coord_polar("y", start = 0, direction = -1)+
    theme_void(),
  filename = "WHO region/roadshow/com_N_pie.tiff", width = 6, height = 4
)
ggsave(
  ggplot(data = com_rate_res_WHO[com_rate_res_WHO$Group!="Global",],
         aes(x = AGEGR, y = IR.est, colour = Group))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), width =0.5, position = position_dodge2(width = 0.5))+
    scale_colour_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    scale_y_continuous(name = "Incidence rate (case per 1000)")+
    scale_x_discrete(name = NULL)+
    theme_bw(),
  filename = "WHO region/roadshow/com_rate.pdf", width = 8, height = 4.5
)
ggsave(
  ggplot(data = com_rate_res_WHO[com_rate_res_WHO$Group!="Global" & com_rate_res_WHO$AGEGR=="0-<12m",],
         aes(x = "", y = N.est, fill = Group))+
    geom_bar(stat = "identity", width = 1, colour = "white", alpha = 0.6)+
    geom_label(aes(label = paste(N.est, " (",scales::percent(N.est/sum(N.est), accuracy = 1), ")",sep = ""),
                   group = Group,
                   x = 1.38), show.legend = FALSE, fill = "white",alpha = 0.7,
               position = position_stack(vjust = 0.5))+
    scale_fill_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    coord_polar("y", start = 0, direction = -1)+
    theme_void(),
  filename = "WHO region/roadshow/com_N_pie.pdf", width = 6, height = 4
)
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
ggsave(
  ggplot(data = com_rate_res_WHO.sens[!duplicated(com_rate_res_WHO.sens[c("WHO", "n.all")]),], 
         aes(x = WHO, 
             y = IR.est, colour = method))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), width = 0.5, position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    scale_y_continuous(name = "Incidence rate (case per 1000)")+
    scale_colour_lancet(name = NULL)+
    theme_bw()+
    theme(text = element_text(size = 14)),
  filename = "WHO region/com_rate_sens.pdf", width = 7.5, height = 5
)
# 1.4 By income----
com_rate_res_WHO.income <- do.call(rbind, 
                                   by(com_rate_combined_WHO[com_rate_combined_WHO$AGEGR=="0-<12m",],
                                      com_rate_combined_WHO[com_rate_combined_WHO$AGEGR=="0-<12m", c("WHO", "Income")],
                                      genMetaRateEach.Impute, prefix = "ALRI", varToKeep = c("AGEGR","WHO","Income","Impute")))
ggsave(
  ggplot(data = com_rate_res_WHO.income, 
         aes(x = WHO, y = IR.est, 
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), width = 0.5, position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific"))+
    scale_y_continuous(name = "Incidence rate (case per 1000)")+
    scale_colour_lancet(name = "Income group")+
    theme_bw(),
  filename = "WHO region/com_rate_income.pdf", width = 7, height = 4.5
)
ggsave(
  ggplot(data = com_rate_res_WHO.income, 
         aes(x = WHO, y = IR.est, 
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), width = 0.5, position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific"))+
    scale_y_continuous(name = "Incidence rate (case per 1000)")+
    scale_colour_lancet(name = "Income group")+
    theme_bw(),
  filename = "WHO region/com_rate_income.tiff", width = 7, height = 4.5
)
# 1.5 Map showing inclusion----
# Inclusion
WHO_inclusion_com <- left_join(com_rate_average.minimal[com_rate_average.minimal$AGEGR %in% c("0-<6m", "6-<12m", "0-<12m", "0-<24m", "0-<36m", "0-<60m"),
                                                        c("AGEGR", "SID")] %>% group_by(SID) %>%
                                 dplyr::summarise(method = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "0-<12m"), "direct", "indirect")[1]),
                               all.raw)
ggsave(
  ggplot(data=map.data2,
    #data = rbind(map.data2[map.data2$COUNTRY!="Sudan",], map.data2[map.data2$COUNTRY=="Sudan",]), 
         aes(x = long, y = lat, group = group, fill = whoreg)) +
    geom_polygon(colour = "grey50", size = 0.05)+
    scale_fill_manual(name = NULL, values = hsv(c(210,0,60,150,288,20)/360, c(0.3), c(0.9)), na.translate=FALSE)+
    geom_jitter(data = WHO_inclusion_com,
                aes(x = LON, y = LAT, shape = method,
                    group = method), fill = "red2",colour = "white",
                width = 1, height =1, alpha = 0.7)+
    scale_shape_manual(name = NULL, values = c(21,22)) +
    ggthemes::theme_map()+
    theme(text = element_text(size = 12))+ 
    guides(shape = guide_legend(override.aes = list(size = 5)),
           fill = guide_legend(override.aes = list(size = 3.5, colour = NA))),
  filename = "WHO region/map_com_rate.pdf", width = 10, height = 5
)
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
hos_rate_res_WHO$N.est <- with(hos_rate_res_WHO, round(IR.est * Pop/1000),0)
hos_rate_res_WHO$N.lci <- with(hos_rate_res_WHO, round(IR.lci * Pop/1000),0)
hos_rate_res_WHO$N.uci <- with(hos_rate_res_WHO, round(IR.uci * Pop/1000),0)
hos_rate_res_WHO <- do.call(rbind,
                            by(hos_rate_res_WHO, hos_rate_res_WHO[c("AGEGR")],
                               FUN = genRateGlobal, n.level = 6))
hos_rate_tab_WHO <- hos_rate_res_WHO[c("AGEGR", "Group", "n.all", "n.impute", "IR.est", "IR.lci", "IR.uci", "N.est", "N.lci", "N.uci")]
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
write.xlsx(hosp)
# For the roadshow
ggsave(
  ggplot(data = hos_rate_res_WHO[hos_rate_res_WHO$Group!="Global",],
         aes(x = AGEGR, y = IR.est, colour = Group))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), width =0.5, position = position_dodge2(width = 0.5))+
    scale_colour_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    scale_y_continuous(name = "Incidence rate (case per 1000)")+
    scale_x_discrete(name = NULL)+
    theme_bw(),
  filename = "WHO region/roadshow/hos_rate.pdf", width = 8, height = 4.5
)
ggsave(
  ggplot(data = hos_rate_res_WHO[hos_rate_res_WHO$Group!="Global" & hos_rate_res_WHO$AGEGR=="0-<12m",],
         aes(x = "", y = N.est, fill = Group))+
    geom_bar(stat = "identity", width = 1, colour = "white", alpha = 0.6)+
    geom_label(aes(label = paste(N.est, " (",scales::percent(N.est/sum(N.est), accuracy = 1), ")",sep = ""),
                   group = Group,
                   x = 1.38), show.legend = FALSE, fill = "white",alpha = 0.7,
               position = position_stack(vjust = 0.5))+
    scale_fill_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    coord_polar("y", start = 0, direction = -1)+
    theme_void(),
  filename = "WHO region/roadshow/hos_N_pie.pdf", width = 6, height = 4
)
ggsave(
  ggplot(data = hos_rate_res_WHO[hos_rate_res_WHO$Group!="Global",],
         aes(x = AGEGR, y = IR.est, colour = Group))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), width =0.5, position = position_dodge2(width = 0.5))+
    scale_colour_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    scale_y_continuous(name = "Incidence rate (case per 1000)")+
    scale_x_discrete(name = NULL)+
    theme_bw(),
  filename = "WHO region/roadshow/hos_rate.tiff", width = 8, height = 4.5
)
ggsave(
  ggplot(data = hos_rate_res_WHO[hos_rate_res_WHO$Group!="Global" & hos_rate_res_WHO$AGEGR=="0-<12m",],
         aes(x = "", y = N.est, fill = Group))+
    geom_bar(stat = "identity", width = 1, colour = "white", alpha = 0.6)+
    geom_label(aes(label = paste(N.est, " (",scales::percent(N.est/sum(N.est), accuracy = 1), ")",sep = ""),
                   group = Group,
                   x = 1.38), show.legend = FALSE, fill = "white",alpha = 0.7,
               position = position_stack(vjust = 0.5))+
    scale_fill_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    coord_polar("y", start = 0, direction = -1)+
    theme_void(),
  filename = "WHO region/roadshow/hos_N_pie.tiff", width = 6, height = 4
)
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
ggsave(
  ggplot(data = hos_rate_res_WHO.sens[!duplicated(hos_rate_res_WHO.sens[c("WHO", "n.all")]),], 
         aes(x = WHO, y = IR.est, colour = method))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), width = 0.5, position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)")+
    scale_colour_lancet(name = NULL)+
    theme_bw()+
    theme(text = element_text(size = 14)),
  filename = "WHO region/hos_rate_sens.pdf", width = 7.5, height = 5
)
# 2.4 By income----
hos_rate_res_WHO.income <- do.call(rbind, 
                                   by(hos_rate_combined_WHO[hos_rate_combined_WHO$AGEGR=="0-<12m",],
                                      hos_rate_combined_WHO[hos_rate_combined_WHO$AGEGR=="0-<12m", c("WHO", "Income")],
                                      genMetaRateEach.Impute, prefix = "HosALRI", varToKeep = c("AGEGR","WHO","Income","Impute")))
hos_rate_res_WHO.income <- hos_rate_res_WHO.income[hos_rate_res_WHO.income$n.all>0,]
ggsave(
  ggplot(data = hos_rate_res_WHO.income, 
         aes(x = WHO, y = IR.est, 
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), width = 0.5, position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific"))+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)")+
    scale_colour_lancet(name = "Income group")+
    theme_bw(),
  filename = "WHO region/hos_rate_income.pdf", width = 7, height = 4.5
)
ggsave(
  ggplot(data = hos_rate_res_WHO.income, 
         aes(x = WHO, y = IR.est, 
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = IR.lci, ymax = IR.uci), width = 0.5, position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific"))+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)")+
    scale_colour_lancet(name = "Income group")+
    theme_bw(),
  filename = "WHO region/hos_rate_income.tiff", width = 7, height = 4.5
)
# 2.5 Map showing inclusion----
# Inclusion
WHO_inclusion_hos <- left_join(hos_rate_average.minimal[hos_rate_average.minimal$AGEGR %in% 
                                                          c("0-<6m", "6-<12m", "0-<12m", "0-<24m", "0-<36m", "0-<60m") &
                                                          !is.na(hos_rate_average.minimal$HosALRI_N),
                                                        c("AGEGR", "SID")] %>% group_by(SID) %>%
                                 dplyr::summarise(method = ifelse(AGEGR %in% c("0-<6m", "6-<12m", "0-<12m"), "direct", "indirect")[1]),
                               all.raw)
ggsave(
  ggplot(data = rbind(map.data2[map.data2$COUNTRY!="Sudan",], map.data2[map.data2$COUNTRY=="Sudan",]), 
         aes(x = long, y = lat, group = group, fill = whoreg)) +
    geom_polygon(colour = "grey50", size = 0.05)+
    scale_fill_manual(name = NULL, values = hsv(c(210,0,60,150,288,20)/360, c(0.3), c(0.9)), na.translate=FALSE)+
    geom_jitter(data = WHO_inclusion_hos,
                aes(x = LON, y = LAT, shape = method,
                    group = method), fill = "red2",colour = "white",
                width = 1, height =1, alpha = 0.7)+
    scale_shape_manual(name = NULL, values = c(21,22,24)) +
    ggthemes::theme_map()+
    theme(text = element_text(size = 12))+ 
    guides(shape = guide_legend(override.aes = list(size = 5)),
           fill = guide_legend(override.aes = list(size = 3.5, colour = NA))),
  filename = "WHO region/map_hos_rate.pdf", width = 10, height = 5
)
# 3. Incidence-to-hospitalisation ratio----
# 3.1 Main----
inc2hos.MC <- inner_join(
  rbind(
    data.frame(
      genMC(df = com_rate_res_WHO[com_rate_res_WHO$AGEGR=="0-<12m" & com_rate_res_WHO$Group!="Global",], id = "Group", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "inc.rate", seed = 4488),
      AGEGR = "0-<12m"
    ),
    data.frame(
      genMC(df = com_rate_res_WHO[com_rate_res_WHO$AGEGR=="0-<6m"& com_rate_res_WHO$Group!="Global",], id = "Group", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "inc.rate", seed = 5354),
      AGEGR = "0-<6m"
    ),
    data.frame(
      genMC(df = com_rate_res_WHO[com_rate_res_WHO$AGEGR=="6-<12m"& com_rate_res_WHO$Group!="Global",], id = "Group", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "inc.rate", seed = 2675),
      AGEGR = "6-<12m"
    )
  ),
  rbind(
    data.frame(
      genMC(df = hos_rate_res_WHO[hos_rate_res_WHO$AGEGR=="0-<12m"& hos_rate_res_WHO$Group!="Global",], id = "Group", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "hos.rate", seed = 7799),
      AGEGR = "0-<12m"
    ),
    data.frame(
      genMC(df = hos_rate_res_WHO[hos_rate_res_WHO$AGEGR=="0-<6m"& hos_rate_res_WHO$Group!="Global",], id = "Group", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "hos.rate", seed = 4811),
      AGEGR = "0-<6m"
    ),
    data.frame(
      genMC(df = hos_rate_res_WHO[hos_rate_res_WHO$AGEGR=="6-<12m"& hos_rate_res_WHO$Group!="Global",], id = "Group", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "hos.rate", seed = 1311),
      AGEGR = "6-<12m"
    )
  )
)
inc2hos.MC$ratio <- inc2hos.MC$inc.rate / inc2hos.MC$hos.rate
inc2hos <- inc2hos.MC %>% group_by(AGEGR, Group) %>% dplyr::summarise(
  ratio.est = median(ratio),
  ratio.lci = quantile(ratio, 0.025),
  ratio.uci = quantile(ratio, 0.975),
  inc.est = median(inc.rate)*1000,
  inc.lci = quantile(inc.rate, 0.025)*1000,
  inc.uci = quantile(inc.rate, 0.975)*1000,
  hos.est = median(hos.rate)*1000,
  hos.lci = quantile(hos.rate, 0.025)*1000,
  hos.uci = quantile(hos.rate, 0.975)*1000
)
inc2hos$Ratio <- with(inc2hos, paste(format(round(ratio.est,1), nsmall=1),
                                     "\n(",
                                     format(round(ratio.lci,1), nsmall=1),
                                     "-",
                                     format(round(ratio.uci,1), nsmall=1),
                                     ")", sep = ""))
inc2hos$AGEGR <-factor(inc2hos$AGEGR, levels = c("0-<12m", "0-<6m", "6-<12m"))
inc2hos_tab <- inc2hos[c("AGEGR", "Group", "Ratio")] %>%
  pivot_wider(names_from = Group,values_from = c(Ratio), values_fill = "-")
write.csv(inc2hos_tab, file = "WHO region/inc2hos_main.csv", row.names = FALSE)
ggsave(
  ggplot(data = inc2hos[inc2hos$AGEGR=="0-<12m",], 
         aes(x = inc.est, y = hos.est, 
             colour = factor(Group, 
                             labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific"))))+
    geom_point()+
    geom_errorbarh(aes(xmin = inc.lci, xmax = inc.uci))+
    geom_errorbar(aes(ymin = hos.lci, ymax = hos.uci))+
    geom_vline(xintercept = 94.6, linetype = "dashed")+
    geom_hline(yintercept = 15.9, linetype = "dashed")+
    scale_x_continuous(name = "Incidence rate (case per 1000)")+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)")+
    scale_colour_lancet(name = "WHO Region")+
    theme_bw()+
    theme(text = element_text(size = 14)),
  filename = "WHO region/inchos_0012.pdf", width = 7, height = 5.5
)
ggsave(
  ggplot(data = inc2hos[inc2hos$AGEGR=="0-<12m",], 
         aes(x = inc.est, y = hos.est, 
             colour = factor(Group, 
                             labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific"))))+
    geom_point()+
    geom_errorbarh(aes(xmin = inc.lci, xmax = inc.uci))+
    geom_errorbar(aes(ymin = hos.lci, ymax = hos.uci))+
    geom_vline(xintercept = 94.6, linetype = "dashed")+
    geom_hline(yintercept = 15.9, linetype = "dashed")+
    scale_x_continuous(name = "Incidence rate (case per 1000)")+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)")+
    scale_colour_lancet(name = "WHO Region")+
    theme_bw()+
    theme(text = element_text(size = 14)),
  filename = "WHO region/inchos_0012.tiff", width = 7, height = 5.5
)
ggsave(
  ggplot(data = inc2hos[inc2hos$AGEGR=="0-<6m",], 
         aes(x = inc.est, y = hos.est, 
             colour = factor(Group, levels = c("Afr", "Amr", "Emr", "Eur", "Sear", "Wpr"),
                             labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific"))))+
    geom_point()+
    geom_errorbarh(aes(xmin = inc.lci, xmax = inc.uci))+
    geom_errorbar(aes(ymin = hos.lci, ymax = hos.uci))+
    geom_vline(xintercept = 96.3, linetype = "dashed")+
    geom_hline(yintercept = 20.2, linetype = "dashed")+
    scale_x_continuous(name = "Incidence rate (case per 1000)")+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)")+
    scale_colour_lancet(name = "WHO Region", drop = FALSE)+
    theme_bw()+
    theme(text = element_text(size = 14)),
  filename = "WHO region/inchos_0006.pdf", width = 7, height = 5.5
)
ggsave(
  ggplot(data = inc2hos[inc2hos$AGEGR=="6-<12m",], 
         aes(x = inc.est, y = hos.est, 
             colour = factor(Group, levels = c("Afr", "Amr", "Emr", "Eur", "Sear", "Wpr"),
                             labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific"))))+
    geom_point()+
    geom_errorbarh(aes(xmin = inc.lci, xmax = inc.uci))+
    geom_errorbar(aes(ymin = hos.lci, ymax = hos.uci))+
    geom_vline(xintercept = 82.6, linetype = "dashed")+
    geom_hline(yintercept = 10, linetype = "dashed")+
    scale_x_continuous(name = "Incidence rate (case per 1000)")+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)")+
    scale_colour_lancet(name = "WHO Region", drop = FALSE)+
    theme_bw()+
    theme(text = element_text(size = 14)),
  filename = "WHO region/inchos_0612.pdf", width = 7, height = 5.5
)
# 3.2 Income----
inc2hos.income.MC <- inner_join(
  rbind(
    data.frame(
      genMC(df = com_rate_res_WHO.income[com_rate_res_WHO.income$Income=="L",], id = "WHO", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "inc.rate", seed = 1029),
      Income = "L"
    ),
    data.frame(
      genMC(df = com_rate_res_WHO.income[com_rate_res_WHO.income$Income=="LM",], id = "WHO", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "inc.rate", seed = 9658),
      Income = "LM"
    ),
    data.frame(
      genMC(df = com_rate_res_WHO.income[com_rate_res_WHO.income$Income=="UM",], id = "WHO", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "inc.rate", seed = 3419),
      Income = "UM"
    ),
    data.frame(
      genMC(df = com_rate_res_WHO.income[com_rate_res_WHO.income$Income=="H",], id = "WHO", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "inc.rate", seed = 1841),
      Income = "H"
    )
  ),
  rbind(
    data.frame(
      genMC(df = hos_rate_res_WHO.income[hos_rate_res_WHO.income$Income=="L",], id = "WHO", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "hos.rate", seed = 3238),
      Income = "L"
    ),
    data.frame(
      genMC(df = hos_rate_res_WHO.income[hos_rate_res_WHO.income$Income=="LM",], id = "WHO", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "hos.rate", seed = 5880),
      Income = "LM"    
    ),
    data.frame(
      genMC(df = hos_rate_res_WHO.income[hos_rate_res_WHO.income$Income=="UM",], id = "WHO", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "hos.rate", seed = 7462),
      Income = "UM"   
    ),
    data.frame(
      genMC(df = hos_rate_res_WHO.income[hos_rate_res_WHO.income$Income=="H",], id = "WHO", input.mean = "est", input.se = "se", 
            n = N.MC, transFUN = exp, output.name = "hos.rate", seed = 1039),
      Income = "H"   
    )
  )
)
inc2hos.income.MC$ratio <- inc2hos.income.MC$inc.rate / inc2hos.income.MC$hos.rate
inc2hos.income <- inc2hos.income.MC %>% group_by(Income, WHO) %>% dplyr::summarise(
  ratio.est = median(ratio),
  ratio.lci = quantile(ratio, 0.025),
  ratio.uci = quantile(ratio, 0.975),
  inc.est = median(inc.rate)*1000,
  inc.lci = quantile(inc.rate, 0.025)*1000,
  inc.uci = quantile(inc.rate, 0.975)*1000,
  hos.est = median(hos.rate)*1000,
  hos.lci = quantile(hos.rate, 0.025)*1000,
  hos.uci = quantile(hos.rate, 0.975)*1000
)
ggsave(
  ggplot(data = inc2hos.income, 
         aes(x = WHO, y = ratio.est, 
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = ratio.lci, ymax = ratio.uci), width = 0.5, position = position_dodge2(width = 0.5))+
    geom_segment(data = inc2hos.income[inc2hos.income$WHO=="Sear" & inc2hos.income$Income=="UM",],
                 aes(x = 5+1/6, y = ratio.lci, xend = 5+1/6, yend = 39.9), arrow = arrow(ends = "last", length = unit(0.03, "npc")),
                 position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific"))+
    scale_y_continuous(name = "Incidence to hospitalisation ratio", limits = c(0,40))+
    scale_colour_lancet(name = "Income group")+
    theme_bw(),
  filename = "WHO region/inc2hos_income.pdf", width = 7, height = 4.5
)
ggsave(
  ggplot(data = inc2hos.income, 
         aes(x = inc.est, y = hos.est, 
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High")
             )))+
    geom_point()+
    geom_errorbarh(aes(xmin = inc.lci, xmax = inc.uci))+
    geom_errorbar(aes(ymin = hos.lci, ymax = hos.uci))+
    geom_vline(xintercept = 94.6, linetype = "dashed")+
    geom_hline(yintercept = 15.9, linetype = "dashed")+
    scale_x_continuous(name = "Incidence rate (case per 1000)")+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)")+
    scale_colour_lancet(name = "WHO Region")+
    facet_wrap(~factor(WHO,labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific")), nrow = 2)+
    theme_bw()+
    theme(text = element_text(size = 14)),
  filename = "WHO region/inchos_0012_income.pdf", width = 14, height = 8.5
)
ggsave(
  ggplot(data = inc2hos.income, 
         aes(x = inc.est, y = hos.est, 
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High")
             )))+
    geom_point()+
    geom_errorbarh(aes(xmin = inc.lci, xmax = inc.uci))+
    geom_errorbar(aes(ymin = hos.lci, ymax = hos.uci))+
    geom_vline(xintercept = 94.6, linetype = "dashed")+
    geom_hline(yintercept = 15.9, linetype = "dashed")+
    scale_x_continuous(name = "Incidence rate (case per 1000)")+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)")+
    scale_colour_lancet(name = "WHO Region")+
    facet_wrap(~factor(WHO,labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific")), nrow = 2)+
    theme_bw()+
    theme(text = element_text(size = 14)),
  filename = "WHO region/inchos_0012_income.tiff", width = 14, height = 8.5
)
# 4. In-hospital case fatality ratio----
# 4.1 Main----
hos_mor_res_WHO <-   rbind(
  do.call(rbind, 
          by(hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m",],
             hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m", "WHO"],
             genMetaPropEach.flex_WHO, case.text = "HosALRI_Deaths", deno.text = "HosALRI_N",varToKeep = c("AGEGR","WHO","Impute")))
)
#do.call(rbind, 
#       by(hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<6m",],
#           hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<6m", "WHO"],
#          genMetaPropEach.flex_WHO, case.text = "HosALRI_Deaths", deno.text = "HosALRI_N",varToKeep = c("AGEGR","WHO","Impute")))
#do.call(rbind, 
#        by(hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="6-<12m",],
#           hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="6-<12m", "WHO"],
#           genMetaPropEach.flex_WHO, case.text = "HosALRI_Deaths", deno.text = "HosALRI_N",varToKeep = c("AGEGR","WHO","Impute")))
hos_mor_res_WHO <- left_join(hos_mor_res_WHO,hos_rate_res_WHO[1:5],by = c("AGEGR" = "AGEGR", "WHO" = "Group"))
hos_mor_res_WHO$Group <- hos_mor_res_WHO$WHO
hos_mor_res_WHO <- genRateGlobal.fromProp_WHO(hos_mor_res_WHO, n.level = 6)
hos_mor_tab_WHO <- hos_mor_res_WHO[c("Group", "n.all", "N.est", "N.lci", "N.uci", "prop.est", "prop.lci", "prop.uci")]
hos_mor_tab_WHO$Studies <- with(hos_mor_tab_WHO,
                                paste(n.all, sep = ""))
hos_mor_tab_WHO$prop <- with(hos_mor_tab_WHO, paste(format(round(prop.est,2), nsmall=2),
                                                    "\n(",
                                                    format(round(prop.lci,2), nsmall=2),
                                                    "-",
                                                    format(round(prop.uci,2), nsmall=2),
                                                    ")", sep = ""))
hos_mor_tab_WHO$N <- with(hos_mor_tab_WHO, paste(format(round(N.est,1)*1000, nsmall=0),
                                                 "\n(",
                                                 format(round(N.lci,1)*1000, nsmall=0),
                                                 "-",
                                                 format(round(N.uci,1)*1000, nsmall=0),
                                                 ")", sep = ""))
hos_mor_tab_WHO <- (hos_mor_tab_WHO[c("Group", "Studies", "prop", "N")] %>%
                      pivot_wider(names_from = Group,values_from = c(Studies,prop, N))
) %>% pivot_longer(everything(),names_to = c("reporting", ".value"),
                   names_pattern = "(.*)_(.*)")
write.csv(hos_mor_tab_WHO, file = "WHO region/hos_mor_main.csv", row.names = FALSE)
# Roadshow
ggsave(
  ggplot(data = hos_mor_res_WHO[!hos_mor_res_WHO$Group %in% c("Global", "Sear"),],
         aes(x = AGEGR, y = prop.est, colour = factor(Group, levels = c("Afr", "Amr", "Emr", "Eur", "Sear", "Wpr"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci), width =0.5, position = position_dodge2(width = 0.5))+
    scale_colour_lancet(name = NULL, 
                        labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"),
                        drop = FALSE)+
    scale_y_continuous(name = "In-hospital case fatality ratio (%)")+
    scale_x_discrete(name = NULL)+
    theme_bw(),
  filename = "WHO region/roadshow/hos_mor.pdf", width = 4.5, height = 4.5
)
ggsave(
  ggplot(data = hos_mor_res_WHO[!hos_mor_res_WHO$Group %in% c("Global", "Sear"),],
         aes(x = AGEGR, y = prop.est, colour = factor(Group, levels = c("Afr", "Amr", "Emr", "Eur", "Sear", "Wpr"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci), width =0.5, position = position_dodge2(width = 0.5))+
    scale_colour_lancet(name = NULL, 
                        labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"),
                        drop = FALSE)+
    scale_y_continuous(name = "In-hospital case fatality ratio (%)")+
    scale_x_discrete(name = NULL)+
    theme_bw(),
  filename = "WHO region/roadshow/hos_mor.tiff", width = 4.5, height = 4.5
)
# 4.2 Sensitivity----
hos_mor_res_WHO.sens <- rbind(
  data.frame(
    do.call(rbind, 
            by(hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m",],
               hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m", "WHO"],
               genMetaPropEach.flex_WHO, case.text = "HosALRI_Deaths", deno.text = "HosALRI_N",varToKeep = c("AGEGR","WHO","Impute"))),
    method = "Main analysis"
  ),
  
  data.frame(
    do.call(rbind, 
            by(hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m" & hos_mor_average.minimal$QA_all >= 0.6,],
               hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m"& hos_mor_average.minimal$QA_all >= 0.6, "WHO"],
               genMetaPropEach.flex_WHO, case.text = "HosALRI_Deaths", deno.text = "HosALRI_N",varToKeep = c("AGEGR","WHO","Impute"))),
    method = "High-quality studies only"
  )
)
ggsave(
  ggplot(data = hos_mor_res_WHO.sens[!duplicated(hos_mor_res_WHO.sens[c("WHO", "n.all")]),], 
         aes(x = WHO, y = prop.est, colour = method))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci), width = 0.5, position = position_dodge2(width = 0.5))+
    geom_segment(data = hos_mor_res_WHO.sens[hos_mor_res_WHO.sens$WHO=="Sear" & hos_mor_res_WHO.sens$method=="Main analysis",],
                 aes(x = 5, y = prop.lci, xend = 5, yend = 4.9), arrow = arrow(ends = "last", length = unit(0.03, "npc")),
                 position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    scale_y_continuous(name = "In-hospital case fatality ratio (%)", limits = c(0,5))+
    scale_colour_lancet(name = NULL)+
    theme_bw()+
    theme(text = element_text(size = 14)),
  filename = "WHO region/hos_mor_sens.pdf", width = 7.5, height = 5
)
# 4.3 By income----
hos_mor_res_WHO.income <-   rbind(
  do.call(rbind, 
          by(hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m",],
             hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m", c("WHO", "Income")],
             genMetaPropEach.flex_WHO, case.text = "HosALRI_Deaths", deno.text = "HosALRI_N",varToKeep = c("AGEGR","WHO","Income", "Impute")))
)
ggsave(
  ggplot(data = hos_mor_res_WHO.income, 
         aes(x = factor(WHO,labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific")), y = prop.est, 
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci), width = 0.5, position = position_dodge2(width = 0.5))+
    geom_segment(data = hos_mor_res_WHO.income[hos_mor_res_WHO.income$WHO=="Amr" & hos_mor_res_WHO.income$Income=="LM",],
                 aes(x = 2-1/6, y = prop.lci, xend = 2-1/6, yend = 9.9), arrow = arrow(ends = "last", length = unit(0.03, "npc")),
                 position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL)+
    scale_y_continuous(name = "In-hospital case fatality ratio (%)", limits = c(0,10))+
    scale_colour_lancet(name = "Income group")+
    theme_bw(),
  filename = "WHO region/hos_mor_income.pdf", width = 7.5, height = 5
)
ggsave(
  ggplot(data = hos_mor_res_WHO.income, 
         aes(x = factor(WHO,labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific")), y = prop.est, 
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci), width = 0.5, position = position_dodge2(width = 0.5))+
    geom_segment(data = hos_mor_res_WHO.income[hos_mor_res_WHO.income$WHO=="Amr" & hos_mor_res_WHO.income$Income=="LM",],
                 aes(x = 2-1/6, y = prop.lci, xend = 2-1/6, yend = 9.9), arrow = arrow(ends = "last", length = unit(0.03, "npc")),
                 position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL)+
    scale_y_continuous(name = "In-hospital case fatality ratio (%)", limits = c(0,10))+
    scale_colour_lancet(name = "Income group")+
    theme_bw(),
  filename = "WHO region/hos_mor_income.tiff", width = 7.5, height = 5
)
# 4.4 Map showing inclusion----
# Inclusion
WHO_inclusion_mor <- left_join(hos_mor_average.minimal[hos_mor_average.minimal$AGEGR %in% 
                                                         c("0-<12m"),
                                                       c("AGEGR", "SID")] %>% group_by(SID),
                               all.raw)
ggsave(
  ggplot() +
    geom_polygon(data = rbind(map.data2[map.data2$COUNTRY!="Sudan",], map.data2[map.data2$COUNTRY=="Sudan",]), 
                 aes(x = long, y = lat, group = group, fill = whoreg), colour = "grey50", size = 0.05)+
    scale_fill_manual(name = NULL, values = hsv(c(210,0,60,150,288,20)/360, c(0.3), c(0.9)), na.translate=FALSE)+
    geom_jitter(data = WHO_inclusion_mor,
                aes(x = LON, y = LAT), shape = 21, fill = "red2",colour = "white",
                width = 1, height =1, alpha = 0.7)+
    ggthemes::theme_map()+
    theme(text = element_text(size = 12))+ 
    guides(
      fill = guide_legend(override.aes = list(size = 3.5, colour = NA))),
  filename = "WHO region/map_hos_mor.pdf", width = 10, height = 5
)
# 5. Overall mortality----
# 5.1 Main----
com_mor_res_WHO <- (mor_all_DeCoDe.predict %>% group_by(WHORegion, index) %>%
                      dplyr::summarise(
                        #                              Country.Name = paste0(Country.Name, collapse = ", "),
                        m0001_N = sum(m0001_N),
                        m0106_N = sum(m0106_N),
                        m0006_N = sum(m0001_N + m0106_N),
                        m0612_N = sum(m0612_N),
                        m0012_N = sum(m0001_N + m0106_N + m0612_N),
                        m1260_N = sum(m1260_N),
                        m0060_N = sum(m0001_N + m0106_N + m0612_N +m1260_N),
                        m0001_deno = sum(m0001_deno),
                        m0106_deno = sum(m0106_deno),
                        m0006_deno = sum(m0006_deno),
                        m0612_deno = sum(m0612_deno),
                        m0012_deno = sum(m0012_deno),
                        m1260_deno = sum(m1260_deno),
                        m0060_deno = sum(m0060_deno),
                        Y0 = sum(Y0, na.rm = TRUE)
                      )) %>% group_by(WHORegion) %>%
  dplyr::summarise(
    #    Country = Country.Name[1],
    m0012_N.P500 = round(quantile(m0012_N, 0.5),0),
    m0012_N.P025 = round(quantile(m0012_N, 0.025),0),
    m0012_N.P975 = round(quantile(m0012_N, 0.975),0),
    m0012_PAF.P500 = round(quantile(m0012_N/m0012_deno, 0.5),3),
    m0012_PAF.P025 = round(quantile(m0012_N/m0012_deno, 0.025),3),
    m0012_PAF.P975 = round(quantile(m0012_N/m0012_deno, 0.975),3),
    m0012_rate.P500 = round(quantile(m0012_N, 0.5),0)/Y0[1],
    m0012_rate.P025 = round(quantile(m0012_N, 0.025),0)/Y0[1],
    m0012_rate.P975 = round(quantile(m0012_N, 0.975),0)/Y0[1]
  )
com_mor_res_WHO.income <- (mor_all_DeCoDe.predict %>% group_by(WHORegion, Income2019, index) %>%
                             dplyr::summarise(
                               #                            Country.Name = paste0(Country.Name, collapse = ", "),
                               m0001_N = sum(m0001_N),
                               m0106_N = sum(m0106_N),
                               m0006_N = sum(m0001_N + m0106_N),
                               m0612_N = sum(m0612_N),
                               m0012_N = sum(m0001_N + m0106_N + m0612_N),
                               m1260_N = sum(m1260_N),
                               m0060_N = sum(m0001_N + m0106_N + m0612_N +m1260_N),
                               m0001_deno = sum(m0001_deno),
                               m0106_deno = sum(m0106_deno),
                               m0006_deno = sum(m0006_deno),
                               m0612_deno = sum(m0612_deno),
                               m0012_deno = sum(m0012_deno),
                               m1260_deno = sum(m1260_deno),
                               m0060_deno = sum(m0060_deno),
                               Y0 = sum(Y0, na.rm = TRUE)
                             )) %>% group_by(WHORegion, Income2019) %>%
  dplyr::summarise(
    #    Country = Country.Name[1],
    m0012_N.P500 = round(quantile(m0012_N, 0.5),0),
    m0012_N.P025 = round(quantile(m0012_N, 0.025),0),
    m0012_N.P975 = round(quantile(m0012_N, 0.975),0),
    m0012_PAF.P500 = round(quantile(m0012_N/m0012_deno, 0.5),3),
    m0012_PAF.P025 = round(quantile(m0012_N/m0012_deno, 0.025),3),
    m0012_PAF.P975 = round(quantile(m0012_N/m0012_deno, 0.975),3),
    m0012_rate.P500 = round(quantile(m0012_N, 0.5),0)/Y0[1],
    m0012_rate.P025 = round(quantile(m0012_N, 0.025),0)/Y0[1],
    m0012_rate.P975 = round(quantile(m0012_N, 0.975),0)/Y0[1]
  )
# Reporting
com_mor_tab_WHO <- com_mor_res_WHO
com_mor_tab_WHO <- com_mor_tab_WHO %>% pivot_longer(
  !WHORegion,
  names_to = c("AGEGR", ".value"),
  names_pattern = "(.*)_(.*)"
)
com_mor_tab_WHO $N <- with(com_mor_tab_WHO , paste(round(N.P500,-2), "\n(", 
                                                   round(N.P025,-2),"-", 
                                                   round(N.P975,-2), ")", 
                                                   sep = ""))
com_mor_tab_WHO $PAF <- with(com_mor_tab_WHO , paste(format(PAF.P500*100, nsmall=1),
                                                     "\n(",
                                                     format(PAF.P025*100, nsmall=1),
                                                     "-",
                                                     format(PAF.P975*100, nsmall=1),
                                                     ")", sep = ""))
com_mor_tab_WHO$rate <- with(com_mor_tab_WHO , paste(format(round(rate.P500,2), nsmall=2),
                                                     "\n(",
                                                     format(round(rate.P025,2), nsmall=2),
                                                     "-",
                                                     format(round(rate.P975,2), nsmall=2),
                                                     ")", sep = ""))
com_mor_tab_WHO <- (com_mor_tab_WHO[c("WHORegion", "AGEGR", "N", "PAF", "rate")] %>%
                      pivot_wider(names_from = WHORegion,values_from = c(PAF,N, rate))
) %>% pivot_longer(!AGEGR,names_to = c("reporting", ".value"),
                   names_pattern = "(.*)_(.*)")
write.csv(com_mor_tab_WHO, file = "WHO region/com_mor_main.csv", row.names = FALSE)
ggsave(
  ggplot(data = com_mor_res_WHO.income, 
         aes(x = factor(WHORegion, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific")), y = m0012_rate.P500, 
             colour = factor(Income2019, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = m0012_rate.P025, ymax = m0012_rate.P975), width = 0.5, position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL)+
    scale_y_continuous(name = "Overall mortality rate (case per 1000)")+
    scale_colour_lancet(name = "Income group")+
    theme_bw(),
  filename = "WHO region/com_mor_income.pdf", width = 7.5, height = 5
)
ggsave(
  ggplot(data = com_mor_res_WHO.income, 
         aes(x = factor(WHORegion, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific")), y = m0012_rate.P500, 
             colour = factor(Income2019, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = m0012_rate.P025, ymax = m0012_rate.P975), width = 0.5, position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL)+
    scale_y_continuous(name = "Overall mortality rate (case per 1000)")+
    scale_colour_lancet(name = "Income group")+
    theme_bw(),
  filename = "WHO region/com_mor_income.tiff", width = 7.5, height = 5
)
# 5.2 CFR to mortality rate----
ggsave(
  ggplot(data = left_join(com_mor_res_WHO, hos_mor_res_WHO, by = c("WHORegion" = "Group")),
         aes(x = m0012_rate.P500, y = prop.est, 
             colour = factor(WHORegion, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific"))))+
    geom_point()+
    geom_errorbarh(aes(xmin = m0012_rate.P025, xmax = m0012_rate.P975))+
    geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci))+
    geom_vline(xintercept = 0.4875, linetype = "dashed")+
    geom_hline(yintercept = 0.9, linetype = "dashed")+
    geom_segment(aes(x = com_mor_res_WHO[com_mor_res_WHO$WHORegion=="Sear",]$m0012_rate.P500, 
                     y = hos_mor_res_WHO[hos_mor_res_WHO$Group=="Sear",]$prop.lci, 
                     xend = com_mor_res_WHO[com_mor_res_WHO$WHORegion=="Sear",]$m0012_rate.P500, 
                     yend = 4.9), arrow = arrow(ends = "last", length = unit(0.03, "npc")), colour = pal_lancet("lanonc")(6)[5])+
    scale_x_continuous(name = "Overall mortality rate (case per 1000)")+
    scale_y_continuous(name = "In-hospital case fatality ratio (%)", limits = c(0,5))+
    scale_colour_lancet(name = "WHO Region")+
    theme_bw()+
    theme(text = element_text(size = 14)),
  filename = "WHO region/cfr_mor.pdf", width = 7, height = 5.5
)
ggsave(
  ggplot(data = inner_join(com_mor_res_WHO.income, hos_mor_res_WHO.income, by = c("WHORegion" = "WHO", "Income2019" ="Income")),
         aes(x = m0012_rate.P500, y = prop.est, 
             colour = factor(Income2019, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High")
             )))+
    geom_point()+
    geom_errorbarh(aes(xmin = m0012_rate.P025, xmax = m0012_rate.P975))+
    geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci))+
    geom_vline(xintercept = 0.4875, linetype = "dashed")+
    geom_hline(yintercept = 0.9, linetype = "dashed")+
    scale_x_continuous(name = "Overall mortality rate (case per 1000)")+
    scale_y_continuous(name = "In-hospital case fatality ratio (%)", limits = c(0,7))+
    geom_segment(aes(x = ifelse(WHORegion == "Amr" & Income2019 == "LM", m0012_rate.P500, NA), 
                     y = ifelse(WHORegion == "Amr" & Income2019 == "LM", prop.lci, NA), 
                     xend = ifelse(WHORegion == "Amr" & Income2019 == "LM", m0012_rate.P500, NA),
                     yend = ifelse(WHORegion == "Amr" & Income2019 == "LM", 6.9, NA)), arrow = arrow(ends = "last", length = unit(0.03, "npc")))+
    scale_colour_lancet(name = "WHO Region")+
    facet_wrap(~factor(WHORegion,labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast Asia", "Western\nPacific")), nrow = 2)+
    theme_bw()+
    theme(text = element_text(size = 14)),
  filename = "WHO region/cfr_mor_income.pdf", width = 14, height = 8.5
)
# Roadshow
ggsave(
  ggplot(data = com_mor_res_WHO,
         aes(x = "0-<12m", y = m0012_rate.P500, colour = WHORegion))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = m0012_rate.P025, ymax = m0012_rate.P975), width =0.5, position = position_dodge2(width = 0.5))+
    scale_colour_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    scale_y_continuous(name = "Mortality rate (case per 1000)")+
    scale_x_discrete(name = NULL)+
    theme_bw(),
  filename = "WHO region/roadshow/com_mor.pdf", width = 4.5, height = 4.5
)
ggsave(
  ggplot(data = com_mor_res_WHO,
         aes(x = "", y = m0012_N.P500, fill = WHORegion))+
    geom_bar(stat = "identity", width = 1, colour = "white", alpha = 0.6)+
    geom_label(aes(label = paste(round(m0012_N.P500/10,0)*10, " (",scales::percent(m0012_N.P500/sum(m0012_N.P500), accuracy = 1), ")",sep = ""),
                   group = WHORegion,
                   x = 1.38), show.legend = FALSE, fill = "white",alpha = 0.7,
               position = position_stack(vjust = 0.5))+
    scale_fill_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    coord_polar("y", start = 0, direction = -1)+
    theme_void(),
  filename = "WHO region/roadshow/com_mor_pie.pdf", width = 6, height = 4
)
ggsave(
  ggplot(data = com_mor_res_WHO,
         aes(x = "0-<12m", y = m0012_rate.P500, colour = WHORegion))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = m0012_rate.P025, ymax = m0012_rate.P975), width =0.5, position = position_dodge2(width = 0.5))+
    scale_colour_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    scale_y_continuous(name = "Mortality rate (case per 1000)")+
    scale_x_discrete(name = NULL)+
    theme_bw(),
  filename = "WHO region/roadshow/com_mor.tiff", width = 4.5, height = 4.5
)
ggsave(
  ggplot(data = com_mor_res_WHO,
         aes(x = "", y = m0012_N.P500, fill = WHORegion))+
    geom_bar(stat = "identity", width = 1, colour = "white", alpha = 0.6)+
    geom_label(aes(label = paste(round(m0012_N.P500/10,0)*10, " (",scales::percent(m0012_N.P500/sum(m0012_N.P500), accuracy = 1), ")",sep = ""),
                   group = WHORegion,
                   x = 1.38), show.legend = FALSE, fill = "white",alpha = 0.7,
               position = position_stack(vjust = 0.5))+
    scale_fill_lancet(name = NULL, labels = c("Africa", "America", "Eastern\nMediterranean", "European", "Southeast\nAsia", "Western\nPacific"))+
    coord_polar("y", start = 0, direction = -1)+
    theme_void(),
  filename = "WHO region/roadshow/com_mor_pie.tiff", width = 6, height = 4
)
# 6. Study-level----
WHO_inclusion_overall <- data.frame(
  SID = unique(c(WHO_inclusion_com$SID, WHO_inclusion_hos$SID), WHO_inclusion_mor$SID)
)
WHO_inclusion_overall$com <- WHO_inclusion_overall$SID %in% WHO_inclusion_com$SID
WHO_inclusion_overall$hos <- WHO_inclusion_overall$SID %in% WHO_inclusion_hos$SID
WHO_inclusion_overall$mor <- WHO_inclusion_overall$SID %in% WHO_inclusion_mor$SID
WHO_inclusion_overall <- left_join(WHO_inclusion_overall, 
                                   all.raw[c("SID","Country", "WHO", "StudyPeriod", "StudyMY", "Specimen", "ViralTest", "QA_all")])
write.csv(WHO_inclusion_overall, file = "inclusion.csv", row.names = FALSE)

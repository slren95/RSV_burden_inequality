#
# ------------------------------------------------------------------------------
# Script: code_05_overall_mortality.R
# Purpose:
#   Estimate RSV-attributable all-cause mortality (overall deaths) by aggregating
#   model-based predictions and summarising results by WHO region (and globally).
#
# Inputs:
#   - workspaceToBegin.RData
#   - rda/code_04_in_hos_CFR.RData
#   - functions.R
#
# Outputs:
#   - Derived mortality summaries/tables written via `write.*()` calls.
#   - Intermediate objects saved to `rda/` if the script calls `save.image()` or
#     similar later on.
#
# Usage:
#   source("code_05_overall_mortality.R")
#
# Notes:
#   Run from the project root and after upstream CFR inputs are available.
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
#library(gt)
library(scales)
library(Amelia)
library(epiR)
library(openxlsx)
library(sjPlot)
load("workspaceToBegin.RData")
load('rda/code_04_in_hos_CFR.RData')

source('functions.R')

# 5. Overall mortality----
# 5.1 Main----
mor_all_DeCoDe.predict %>% dplyr::count(WHORegion,Country.Name) #195

## check ----
mor_all_DeCoDe.predict %>%
  mutate(mort=(m0001_N + m0106_N + m0612_N)/Y0) %>%
  pull(mort) %>%
  summary()
  #hist(breaks = 30)

## check ----

com_mor_res_WHO <- (mor_all_DeCoDe.predict %>% group_by(WHORegion, index) %>%
                      dplyr::summarise(
                        # Country.Name = paste0(Country.Name, collapse = ", "),
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

# Global ----
com_mor_res_WHO_Global<-(mor_all_DeCoDe.predict %>% group_by(index) %>%
    dplyr::summarise(
      WHORegion='Global',
      # Country.Name = paste0(Country.Name, collapse = ", "),
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
                               #  Country.Name = paste0(Country.Name, collapse = ", "),
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

# rbind global
com_mor_tab_WHO <- rbind(com_mor_res_WHO,com_mor_res_WHO_Global)

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
                                                     " (",
                                                     format(PAF.P025*100, nsmall=1),
                                                     "-",
                                                     format(PAF.P975*100, nsmall=1),
                                                     ")", sep = ""))
com_mor_tab_WHO$rate <- with(com_mor_tab_WHO , paste(format(round(rate.P500,2), nsmall=2),
                                                     " (",
                                                     format(round(rate.P025,2), nsmall=2),
                                                     "-",
                                                     format(round(rate.P975,2), nsmall=2),
                                                     ")", sep = ""))
com_mor_tab_WHO <- (com_mor_tab_WHO[c("WHORegion", "AGEGR", "N", "PAF", "rate")] %>%
                      pivot_wider(names_from = WHORegion,values_from = c(PAF,N, rate))
) %>% pivot_longer(!AGEGR,names_to = c("reporting", ".value"),
                   names_pattern = "(.*)_(.*)") %>%
  `[`(c(2,3,1),)

# write.csv(com_mor_tab_WHO, file = "WHO region/com_mor_main.csv", row.names = FALSE)
# write.xlsx(com_mor_tab_WHO, file = "WHO region/com_mor_main.xlsx", asTable = T)
tab_df(com_mor_tab_WHO)

## docs/com_mor_tab_WHO.xlsx
createAppendixTable(com_mor_tab_WHO, "docs/com_mor_tab_WHO.xlsx")


com_mor_res_WHO.income %>%
  add_count(WHORegion) %>% 
  mutate(
  Income2019=factor(Income2019, levels = c("L", "LM", "UM", "H"), labels = c("LICs", "LMICs", "UMICs", "HICs")),
  WHORegion=str_to_upper(WHORegion)
  ) %>%
  ggplot(aes(width=0.05*n,x = WHORegion, y = m0012_rate.P500, 
           colour = WHORegion))+
  geom_errorbar(aes(ymin = m0012_rate.P025, ymax = m0012_rate.P975, group = Income2019), 
                position = position_dodge(width = 0.5)) +
  geom_point(aes(fill=Income2019, group = Income2019), 
             position = position_dodge(width = 0.5), shape=21, size=3)+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name = "Overall mortality rate (cases per 1000)",
                     expand=expansion(add=c(0,0)),limits=c(0,2),
                     breaks=seq(0,2,.5),labels=seq(0,2,.5))+
  scale_colour_manual(name = NULL, 
                      values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                      labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
  )+
  scale_fill_manual(values=c('gray0','gray30','gray80','white'),name = NULL)+
  theme_bw()+
  theme(text = element_text(size = 14),  # 全局字体
        axis.title = element_text(size = 16),  # 坐标轴标题
        axis.text = element_text(size = 14),  # 坐标轴刻度
        legend.text = element_text(size = 14),  # 图例文字
        legend.title = element_text(size = 16),  # 图例标题
        legend.position = c(.995,.99),
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        plot.margin = margin(t=8,b=5,l=5,r=5),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25))+
  guides(colour=guide_legend(order=1))

ggsave(filename = "plot/com_mor_income.tiff", width = 7.5, height = 5,dpi=300)

# 5.2 CFR to mortality rate----
# 5.3 Plot----
left_join(com_mor_res_WHO, hos_mor_res_WHO, by = c("WHORegion" = "Group")) %>%
  ggplot(aes(x = m0012_rate.P500, y = prop.est, 
             colour = factor(WHORegion, labels = c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific"))))+
  geom_point()+
  geom_errorbarh(aes(xmin = m0012_rate.P025, xmax = m0012_rate.P975))+
  geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci))+
  geom_vline(xintercept = 0.4875, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  # geom_segment(aes(x = com_mor_res_WHO[com_mor_res_WHO$WHORegion=="Sear",]$m0012_rate.P500, 
  #                  y = hos_mor_res_WHO[hos_mor_res_WHO$Group=="Sear",]$prop.lci, 
  #                  xend = com_mor_res_WHO[com_mor_res_WHO$WHORegion=="Sear",]$m0012_rate.P500, 
  #                  yend = 4.9), arrow = arrow(ends = "last", length = unit(0.03, "npc")), colour = pal_lancet("lanonc")(6)[5])+
  scale_x_continuous(name = "Overall mortality rate (case per 1000)")+
  scale_y_continuous(name = "In-hospital case fatality ratio (%)", limits = c(0,3.2))+
  scale_colour_lancet(name = "WHO Region")+
  theme_bw()+
  theme(text=element_text(size=30),
        legend.position = c(.995,.99),
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        panel.grid=element_blank())

ggsave(filename = "plot/cfr_mor.tiff", width = 8, height = 4.5,dpi=300)


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
    facet_wrap(~factor(WHORegion,labels = c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific")), nrow = 2)+
    theme_bw()+
    theme(text = element_text(size = 28),
          legend.title = element_text(size=25),
          legend.text = element_text(size=25),
          legend.position = 'top',
          panel.grid = element_blank()),
  filename = "plot/cfr_mor_income.tiff", width = 8, height = 4.5,dpi=300
)
# Roadshow
ggsave(
  ggplot(data = com_mor_res_WHO,
         aes(x = "", y = m0012_N.P500, fill = WHORegion))+
    geom_bar(stat = "identity", width = 1, colour = "white", alpha = 0.6)+
    geom_label(aes(label = paste(round(m0012_N.P500/10,0)*10, " (",scales::percent(m0012_N.P500/sum(m0012_N.P500), accuracy = 1), ")",sep = ""),
                   group = WHORegion,
                   x = 1.38), show.legend = FALSE, fill = "white",alpha = 0.7,size=10,
               position = position_stack(vjust = 0.5))+
    scale_fill_lancet(name = NULL, labels = c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific"))+
    coord_polar("y", start = 0, direction = -1)+
    theme_void()+
    theme(legend.text=element_text(size=20)),
  filename = "plot/com_mor_pie.tiff", width = 6, height = 4,dpi=300
)
ggsave(
  ggplot(data = com_mor_res_WHO,
         aes(x = "0-<12m", y = m0012_rate.P500, colour = WHORegion))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = m0012_rate.P025, ymax = m0012_rate.P975), width =0.5, position = position_dodge2(width = 0.5))+
    scale_colour_lancet(name = NULL, labels = c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific"))+
    scale_y_continuous(name = "Mortality rate (case per 1000)")+
    scale_x_discrete(name = NULL)+
    theme_bw()+
    theme(text=element_text(size=30),
          legend.position = c(.995,.99),
          legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          panel.grid=element_blank()),
  filename = "plot/com_mor.tiff", width = 8, height = 4.5
)

# with Global
com_mor_res_WHO_with_Global<-rbind(com_mor_res_WHO,com_mor_res_WHO_Global)

save(com_mor_res_WHO,
     com_mor_res_WHO_with_Global,
     com_mor_tab_WHO,
     com_mor_res_WHO.income,
     file='rda/code_05_overall_mortality.RData')


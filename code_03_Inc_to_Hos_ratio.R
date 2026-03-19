#
# ------------------------------------------------------------------------------
# Script: code_03_Inc_to_Hos_ratio.R
# Purpose:
#   Quantify the incidence-to-hospitalisation ratio (IHR) for RSV-associated ALRI
#   by combining incidence and hospital admission estimates via Monte Carlo
#   simulation and summarising uncertainty.
#
# Inputs:
#   - workspaceToBegin.RData
#   - rda/code_01_incidence.RData
#   - rda/code_02_hospitalisation.RData
#
# Outputs:
#   - `WHO region/inc2hos_main.csv/.xlsx` and other derived outputs written by
#     `write.*()` calls.
#
# Usage:
#   source("code_03_Inc_to_Hos_ratio.R")
#
# Notes:
#   This script expects upstream scripts (01 and 02) to have been run and their
#   `.RData` outputs to exist under `rda/`.
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
library(showtext)
library(sjPlot)
library(rio)
font_add_google(name = "Amatic SC", family = "amatic-sc")
showtext_auto()
load("workspaceToBegin.RData")

load('rda/code_01_incidence.RData')
load('rda/code_02_hospitalisation.RData')
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
write.xlsx(inc2hos_tab, file = "WHO region/inc2hos_main.xlsx", asTable = T)
tab_df(inc2hos_tab)

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

# 3.3 Plot
com_rate_res_WHO %>% filter(Group=='Global')
hos_rate_res_WHO %>% filter(Group=='Global')

ggsave(
  ggplot(data = inc2hos[inc2hos$AGEGR=="0-<12m",], 
         aes(x = inc.est, y = hos.est, 
             colour = factor(Group, 
                             labels = c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific"))))+
    geom_point()+
    geom_errorbarh(aes(xmin = inc.lci, xmax = inc.uci))+
    geom_errorbar(aes(ymin = hos.lci, ymax = hos.uci))+
    geom_vline(xintercept = 85.0, linetype = "dashed")+
    geom_hline(yintercept = 18.4, linetype = "dashed")+
    scale_x_continuous(name = "Incidence rate (case per 1000)",
                       expand=expansion(),limits=c(0,250),
                       breaks=seq(0,250,50),labels=seq(0,250,50))+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                       expand=expansion(),limits=c(0,70),
                       breaks=seq(0,70,10),labels=seq(0,70,10))+
    scale_colour_lancet(name = NULL)+
    theme_bw()+
    theme(text =element_text(size=30),
          legend.position = c(.995,.99),
          legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          plot.margin = margin(t=5,b=5,l=8,r=8),
          panel.grid=element_blank(),
          panel.border = element_blank(),
          axis.ticks.length = unit(.15, "cm"),
          axis.line = element_line(size=.25),
          axis.ticks = element_line(size=.25)),
  filename = "plot/inchos_0012.tiff", width = 7, height = 5.5,dpi=300
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

ggsave(
  ggplot(data = inc2hos.income, 
         aes(x = WHO, y = ratio.est, 
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High"))))+
    geom_point(position = position_dodge2(width = 0.5))+
    geom_errorbar(aes(ymin = ratio.lci, ymax = ratio.uci), width = 0.5, position = position_dodge2(width = 0.5))+
    geom_segment(data = inc2hos.income[inc2hos.income$WHO=="Sear" & inc2hos.income$Income=="UM",],
                 aes(x = 5+1/6, y = ratio.lci, xend = 5+1/6, yend = 39.9), arrow = arrow(ends = "last", length = unit(0.02, "npc")),
                 position = position_dodge2(width = 0.5))+
    scale_x_discrete(name = NULL, labels = c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific"))+
    scale_y_continuous(name = "Incidence to hospitalisation ratio", limits = c(0,40))+
    scale_colour_lancet(name = "Income group")+
    theme_bw()+
    theme(text=element_text(size=30),
          legend.position = c(.995,.99),
          legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          panel.grid=element_blank()),
  filename = "plot/inc2hos_income.tiff", width = 7, height = 4.5
)
ggsave(
  ggplot(data = inc2hos.income, 
         aes(x = inc.est, y = hos.est, 
             colour = factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("Low", "Lower-middle", "Upper-middle", "High")
             )))+
    geom_point()+
    geom_errorbarh(aes(xmin = inc.lci, xmax = inc.uci))+
    geom_errorbar(aes(ymin = hos.lci, ymax = hos.uci))+
    geom_vline(xintercept = 85.0, linetype = "dashed")+
    geom_hline(yintercept = 18.4, linetype = "dashed")+
    facet_wrap(~factor(WHO,labels = c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific")), nrow = 2)+
    scale_x_continuous(name = "Incidence rate (case per 1000)",
                       expand=expansion(),limits=c(0,450),
                       breaks=seq(0,450,50),labels=seq(0,450,50))+
    scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                       expand=expansion(),limits=c(0,100),
                       breaks=seq(0,100,10),labels=seq(0,100,10))+
    scale_colour_lancet(name = 'Income level',labels=c('Low income','Lower-middle income','Upper-middle income','High Income'))+
    theme_bw()+
    theme(text = element_text(size = 28),
          legend.title = element_text(size=25),
          legend.text = element_text(size=25),
          legend.position = 'top',
          panel.grid = element_blank(),
          strip.background = element_blank(),
          panel.spacing = unit(1,'lines'),
          axis.line = element_line(size=.25),
          axis.ticks = element_line(size=.25)),
  filename = "plot/inchos_0012_income.tiff", width = 8, height = 4.5,dpi=300
)

# Plot Ratio
inc2hos %>%
  #filter(AGEGR=='0-<12m') %>%
  ggplot(aes(x=Group,y=ratio.est,color=Group))+
  geom_point(shape=21,size=5,)+
  geom_errorbar(aes(ymin=ratio.lci,ymax=ratio.uci))+
  scale_colour_lancet(name = NULL, labels = c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific","Global"))+
  facet_wrap(vars(AGEGR))

ggplot(data = inc2hos, 
       aes(x = inc.est, y = hos.est, 
           colour = factor(Group, 
                           labels = c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific"))))+
  geom_point()+
  geom_errorbarh(aes(xmin = inc.lci, xmax = inc.uci))+
  geom_errorbar(aes(ymin = hos.lci, ymax = hos.uci))+
  geom_vline(xintercept = 85.0, linetype = "dashed")+
  geom_hline(yintercept = 18.4, linetype = "dashed")+
  facet_wrap(vars(AGEGR))+
  scale_x_log10()+
  scale_y_log10()

save(inc2hos,
     inc2hos.MC,
     inc2hos_tab,
     inc2hos.income,
     inc2hos.income.MC,
     file='rda/code_03_Inc_to_Hos_ratio.RData')

# Plot-for-paper ----
ggplot(data = inc2hos[inc2hos$AGEGR=="0-<12m",], 
       aes(x = inc.est, y = hos.est,color=Group))+
  geom_point(shape=19,size=2,aes(color=Group))+
  geom_errorbarh(aes(xmin = inc.lci, xmax = inc.uci))+
  geom_errorbar(aes(ymin = hos.lci, ymax = hos.uci))+
  geom_vline(xintercept = 94.6, linetype = "dashed")+
  geom_hline(yintercept = 15.9, linetype = "dashed")+
  scale_x_continuous(name = "Incidence rate (case per 1000)",
                     expand=expansion(add=c(0,10)),limits=c(0,250),
                     breaks=seq(0,250,50),labels=seq(0,250,50))+
  scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                     expand=expansion(),limits=c(0,70),
                     breaks=seq(0,70,10),labels=seq(0,70,10))+
  scale_colour_lancet(name = NULL,labels=~str_to_upper(.x))+
  theme_bw()+
  theme(text =element_text(size=20),
        legend.position = c(.995,.99),
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25))

ggsave(filename = 'pdf/inc2hos.pdf', width = 8, height = 4.5)

inc2hos.income %>%
  mutate(Income=paste0(Income,'ICs'),
    Income=factor(Income,levels=c('HICs','UMICs','LMICs','LICs'))) %>%
  ggplot(aes(x = inc.est, y = hos.est,color=WHO))+
  # geom_errorbarh(aes(xmin = inc.lci, xmax = inc.uci))+
  # geom_errorbar(aes(ymin = hos.lci, ymax = hos.uci))+
  geom_linerange(aes(xmin = inc.lci, xmax = inc.uci))+
  geom_linerange(aes(ymin = hos.lci, ymax = hos.uci))+
  geom_point(shape=21,size=5,aes(fill=Income))+
  geom_vline(xintercept = 94.6, linetype = "dashed")+
  geom_hline(yintercept = 15.9, linetype = "dashed")+
  scale_x_continuous(name = "Incidence rate (case per 1000)",
                     expand=expansion(add=c(0,10)),limits=c(0,250),
                     breaks=seq(0,250,50),labels=seq(0,250,50))+
  scale_y_continuous(name = "Hospitalisation rate (case per 1000)",
                     expand=expansion(),limits=c(0,70),
                     breaks=seq(0,70,10),labels=seq(0,70,10))+
  scale_colour_manual(name = NULL, 
                    values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                    #labels = c("AFR","AMR","EMR","EUR","SEAR","WPR")
                    )+
  #scale_colour_lancet(name = NULL,labels=~str_to_upper(.x))+
  scale_fill_manual(values=c('gray0','gray30','gray80','white'),name='')+
  theme_bw()+
  theme(text =element_text(size=20),
        legend.position = c(.995,.99),
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        panel.grid=element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25))+
  guides(colour=guide_legend(override.aes = list(size=1)))

ggsave(filename = 'pdf/inc2hos_income.pdf', width = 8, height = 4.5)

  

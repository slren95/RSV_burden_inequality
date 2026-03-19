#
# ------------------------------------------------------------------------------
# Script: code_04_in_hos_CFR.R
# Purpose:
#   Estimate the in-hospital case fatality ratio (CFR) for RSV-associated ALRI,
#   including pooled estimates and any adjustment steps implemented in the script.
#
# Inputs:
#   - workspaceToBegin.RData
#   - rda/code_02_hospitalisation.RData (hospitalisation inputs used downstream)
#   - functions.R
#
# Outputs:
#   - Intermediate objects and any figures/tables written by this script (see
#     `ggsave()` / `write.*()` calls).
#   - Optional `.RData` saved at the end of the script (if present).
#
# Usage:
#   source("code_04_in_hos_CFR.R")
#
# Notes:
#   Run after `code_02_hospitalisation.R` has generated the referenced `.RData`.
# ------------------------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(ggsci)
library(metafor)
library(stringr)
library(readxl)
library(mapdata)
library(maps)
#library(maptools)
#library(rgdal)
library(colorspace)
#library(gt)
library(scales)
library(Amelia)
library(epiR)
library(openxlsx)
library(sjPlot)
load("workspaceToBegin.RData")
load('rda/code_02_hospitalisation.RData')
source('functions.R')

# 4. In-hospital case fatality ratio----
# 4.1 Main----
hos_mor_res_WHO_old <-   rbind(
  do.call(rbind, 
          by(hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m",],
             hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m", "WHO"],
             genMetaPropEach.flex_WHO, case.text = "HosALRI_Deaths", deno.text = "HosALRI_N",varToKeep = c("AGEGR","WHO","Impute")))
)
hos_mor_res_WHO_old
# SEAR 0.0907865 0.0003359743 19.7281827

# !!! ️Estimating SEAR CFR by weighting method ----
# data/CFR_by_income.png
df.cfr<-tribble(~Income,~cfr.est,~cfr.se,
                'H',-6.536178,0.3525089,
                'L',-4.189924,0.3096978,
                'LM',-4.170855,0.4053296,
                'UM',-4.800118,0.2187833)

df.cfr %>%
  rowwise() %>%
  mutate(cfr=transf.ilogit(cfr.est)*100) # LM 1.52 %

              
pop_mor.raw %>% filter(WHORegion=='Sear') %>%
  dplyr::count(Income2019)

# Sear sample size ----
hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m",] %>%
  filter(WHO %in% c('Sear','Emr')) %>%
  transmute(WHO,SID,HosALRI_Deaths,HosALRI_N) %>%
  arrange(WHO)

#population

pop_mor.raw %>% filter(WHORegion=='Sear') %>% summarise(sum(Y0))
pop_WHO.raw %>% filter(WHO=='Sear',AGEGR=='0-<12m') %>% pull(Pop)
pop_mor.raw %>% filter(WHORegion=='Sear') %>%
  dplyr::summarise(pop=sum(Y0),.by=Income2019) %>%
  dplyr::rename(Income=Income2019)

# hr for sear
hos_rate_res_WHO.income %>%
  filter(WHO=='Sear') %>% transmute(Income,hr.est=est,hr.se=se)

df.sear<-left_join(pop_mor.raw %>% filter(WHORegion=='Sear') %>%
                     dplyr::summarise(pop=sum(Y0),.by=Income2019) %>%
                     dplyr::rename(Income=Income2019),
                   hos_rate_res_WHO.income %>%
                     filter(WHO=='Sear') %>% transmute(Income,hr.est=est,hr.se=se)) %>%
  left_join(df.cfr)

df.sear_bycountry<-left_join(pop_mor.raw %>% filter(WHORegion=='Sear') %>%
                     transmute(pop=Y0,Income2019) %>%
                       dplyr::rename(Income=Income2019),
                   hos_rate_res_WHO.income %>%
                     filter(WHO=='Sear') %>% transmute(Income,hr.est=est,hr.se=se)) %>%
  left_join(df.cfr)


# generate mc for CFR of Sear
set.seed(1234)
df.sear.mc<-1:1000 %>%
  map_dfr(
    ~df.sear %>% 
      rowwise() %>%
      mutate(index=.x,
             hr=exp(rnorm(1,hr.est,hr.se))*1000,
             cfr=transf.ilogit(rnorm(1,cfr.est,cfr.se)))
  ) %>%
  ungroup()

df.sear.mc_bycountry<-1:1000 %>%
  map_dfr(
    ~df.sear_bycountry %>% 
      rowwise() %>%
      mutate(index=.x,
             hr=exp(rnorm(1,hr.est,hr.se))*1000,
             cfr=transf.ilogit(rnorm(1,cfr.est,cfr.se)))
  ) %>%
  ungroup()

df.sear.mc2<-df.sear.mc %>%
  mutate(N_hos=pop*hr) %>%
  group_by(index) %>%
  mutate(N_hos_sum=sum(N_hos),
         N_hos_pro=N_hos/N_hos_sum,
         cfr.part=cfr*N_hos_pro,
         cfr.adj=sum(cfr.part))

df.sear.mc2_bycountry<-df.sear.mc_bycountry %>%
  mutate(N_hos=pop*hr) %>%
  group_by(index) %>%
  mutate(N_hos_sum=sum(N_hos),
         N_hos_pro=N_hos/N_hos_sum,
         cfr.part=cfr*N_hos_pro,
         cfr.adj=sum(cfr.part))

df.mc_cfr_sear<-df.sear.mc2 %>% ungroup() %>%
  slice_head(n=1,by=index) %>%
  transmute(Group='Sear',index,prop=cfr.adj)

df.mc_cfr_sear_bycountry<-df.sear.mc2_bycountry %>% ungroup() %>%
  slice_head(n=1,by=index) %>%
  transmute(Group='Sear',index,prop=cfr.adj)

quantile(df.mc_cfr_sear$prop,c(0.025,0.5,0.975))*100
quantile(df.mc_cfr_sear_bycountry$prop,c(0.025,0.5,0.975))*100


# update hos_mor_res_WHO_old with weighted data

# Gen Global----

# hos_mor_res_WHO: CFR meta-analysis
# hos_rate_res_WHO[1:5]: hospitalisation rate meta-analysis

## Original method and results ----
hos_mor_res_WHO_old_Global<-left_join(hos_mor_res_WHO_old,hos_rate_res_WHO[1:5],by = c("AGEGR" = "AGEGR", "WHO" = "Group")) %>%
  mutate(Group=WHO) %>%
  genRateGlobal.fromProp_WHO(n.level = 6)

## Replace SEAR data ----
# df<-hos_mor_res_WHO_old
# n.level = 6

genRateGlobal.fromProp_WHO_new <- function(df, n.level, returnMC = FALSE,df.mc_cfr_sear) {
  df <- df[!(is.na(df$est.x)|is.na(df$est.y)),]
  df <- suppressMessages(left_join(df, pop_WHO.raw))
  # Original CFR
  df.mc_cfr<-genMC(df =df, id = "Group", input.mean = "est.x", input.se = "se.x", 
                   n =N.MC, transFUN = transf.ilogit, output.name = "prop", seed = 3902)
  # Replace SEAR CFR
  df.mc_cfr_new<-bind_rows(df.mc_cfr %>% filter(Group!='Sear'),
                           df.mc_cfr_sear)
  
  # Compare before vs after replacement
  df.mc_cfr %>% summarise(mean(prop),.by=Group)
  df.mc_cfr_new %>% summarise(mean(prop),.by=Group)
  hist(df.mc_cfr %>% filter(Group=='Sear') %>% pull(prop),main='',breaks = 30)
  hist(df.mc_cfr_new %>% filter(Group=='Sear') %>% pull(prop),main='',breaks = 30)
  #
  ggplot(df.mc_cfr)+
    geom_histogram(aes(prop,fill=Group))+
    facet_wrap(~Group,scales = 'free')
  ggplot(df.mc_cfr_new)+
    geom_histogram(aes(prop,fill=Group))+
    facet_wrap(~Group,scales = 'free')
  
  df.MC <-   suppressMessages({
    left_join(
      df,
      left_join(
        df.mc_cfr_new, # data after SEAR replacement
        genMC(df =df, id = "Group", input.mean = "est.y", input.se = "se.y", 
              n =N.MC, transFUN = exp, output.name = "rate", seed = 9930)
      )
    )
  })
  df.MC$N <- df.MC$Pop *df.MC$rate
  df.MC$N_prop <- df.MC$Pop *df.MC$rate * df.MC$prop
  df1<- df.MC %>% group_by(AGEGR, Group, Impute.x, Impute.y, n.all, n.impute) %>%
    dplyr::summarise(N.est = round(median(N_prop),3),
                     N.lci = round(quantile(N_prop, 0.025),3),
                     N.uci = round(quantile(N_prop, 0.975),3),
                     Pop = Pop[1],
                     prop.est = median(N_prop/N)*100,
                     prop.lci = quantile(N_prop/N, 0.025)*100,
                     prop.uci = quantile(N_prop/N, 0.975)*100
    )
  df1$IR.est <- with(df1, N.est/Pop*1000)
  df1$IR.lci <- with(df1, N.lci/Pop*1000)
  df1$IR.uci <- with(df1, N.uci/Pop*1000)
  if(nrow(df1) == n.level){
    df2<- df.MC %>% group_by(index) %>%
      dplyr::summarise(AGEGR = AGEGR[1],
                       Group = "Global",
                       Impute.x = Impute.x[1],
                       Impute.y = Impute.y[1],
                       n.all = sum(n.all),
                       n.impute = sum(n.impute),
                       N = sum(N),
                       N_prop = sum(N_prop),
                       Pop = sum(Pop)
      )
    if(returnMC) {
      return(rbind(df.MC[names(df2)],df2))
    }
    df2 <- df2 %>% group_by(AGEGR, Group, Impute.x, Impute.y, n.all, n.impute) %>%
      dplyr::summarise(N.est = round(median(N_prop),3),
                       N.lci = round(quantile(N_prop, 0.025),3),
                       N.uci = round(quantile(N_prop, 0.975),3),
                       Pop = Pop[1],
                       prop.est = median(N_prop/N)*100,#CFR
                       prop.lci = quantile(N_prop/N, 0.025)*100,
                       prop.uci = quantile(N_prop/N, 0.975)*100
      )
    df2$IR.est <- with(df2, N.est/Pop*1000)
    df2$IR.lci <- with(df2, N.lci/Pop*1000)
    df2$IR.uci <- with(df2, N.uci/Pop*1000)
    return(rbind(df1, df2))
  }else{
    return(df1)
  }
}

hos_mor_res_WHO_new_Global<-left_join(hos_mor_res_WHO_old,hos_rate_res_WHO[1:5],by = c("AGEGR" = "AGEGR", "WHO" = "Group")) %>%
  mutate(Group=WHO) %>%
  genRateGlobal.fromProp_WHO_new(n.level = 6,df.mc_cfr_sear=df.mc_cfr_sear)

# Replacement completed

# Original
hos_mor_res_WHO_old_Global
hos_mor_res_WHO_new_Global

hos_mor_tab_WHO <- hos_mor_res_WHO_new_Global[c("Group", "n.all", "N.est", "N.lci", "N.uci", "prop.est", "prop.lci", "prop.uci")]
hos_mor_tab_WHO$Studies <- with(hos_mor_tab_WHO,
                                paste(n.all, sep = ""))
hos_mor_tab_WHO$prop <- with(hos_mor_tab_WHO, paste(format(round(prop.est,2), nsmall=2),
                                                    " (",
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

# write.csv(hos_mor_tab_WHO, file = "WHO region/hos_mor_main.csv", row.names = FALSE)
# write.xlsx(hos_mor_tab_WHO, file = "WHO region/hos_mor_main.xlsx",asTable = T)
tab_df(hos_mor_tab_WHO) #

createAppendixTable(hos_mor_tab_WHO,'docs/hos_mor_tab_WHO.xlsx')


# 4.2 Sensitivity----

df.main<- data.frame(
  do.call(rbind, 
          by(hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m",],
             hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m", "WHO"],
             genMetaPropEach.flex_WHO, case.text = "HosALRI_Deaths", deno.text = "HosALRI_N",varToKeep = c("AGEGR","WHO","Impute"))),
  method = "Main analysis"
)

# sear replace
quantile(df.mc_cfr_sear$prop,probs = c(0.025,0.5,0.975))*100
df.main[df.main$WHO=='Sear',c('est','se','prop.lci','prop.est','prop.uci')]<-c(NA,NA,quantile(df.mc_cfr_sear$prop,probs = c(0.025,0.5,0.975))*100)
df.main

hos_mor_res_WHO.sens <- rbind(
  df.main,
  data.frame(
    do.call(rbind, 
            by(hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m" & hos_mor_average.minimal$QA_all >= 0.6,],
               hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m"& hos_mor_average.minimal$QA_all >= 0.6, "WHO"],
               genMetaPropEach.flex_WHO, case.text = "HosALRI_Deaths", deno.text = "HosALRI_N",varToKeep = c("AGEGR","WHO","Impute"))),
    method = "High-quality studies only"
  )
)

# 4.3 By income----
hos_mor_res_WHO.income <-   rbind(
  do.call(rbind, 
          by(hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m",],
             hos_mor_average.minimal[hos_mor_average.minimal$AGEGR=="0-<12m", c("WHO", "Income")],
             genMetaPropEach.flex_WHO, case.text = "HosALRI_Deaths", deno.text = "HosALRI_N",varToKeep = c("AGEGR","WHO","Income", "Impute")))
)

# 4.4 Map showing inclusion----
# Inclusion
WHO_inclusion_mor<-hos_mor_average.minimal[hos_mor_average.minimal$AGEGR %in%  c("0-<12m"),
                        c("AGEGR", "SID")] %>%
  distinct(SID,.keep_all = T) %>%
  left_join(all.raw)


ggplot() +
  geom_polygon(data = map.data2, 
               aes(x = long, y = lat, group = group, fill = whoreg), colour = "grey50", size = 0.05)+
  scale_fill_manual(name = NULL, values = hsv(c(210,0,60,150,288,20)/360, c(0.3), c(0.9)), na.translate=FALSE)+
  geom_jitter(data = WHO_inclusion_mor,
              aes(x = LON, y = LAT), shape = 21, fill = "red2",colour = "white",
              width = 1, height =1, alpha = 0.7)+
  ggthemes::theme_map()+
  theme(text = element_text(size = 30))+ 
  guides(
    fill = guide_legend(override.aes = list(size = 3.5, colour = NA)))

# ggsave(filename = "plot/map_hos_mor.tiff", width = 10, height = 5,dpi=300)

# 4.5 Plot ----

ggplot(df.main,
       aes(x = factor(WHO, levels = c("Afr", "Amr", "Emr", "Eur", "Sear", "Wpr"),
                      labels = c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")),
           y = prop.est, 
           colour = factor(WHO, levels = c("Afr", "Amr", "Emr", "Eur", "Sear", "Wpr"))))+
  geom_point(position = position_dodge2(width = 0.5),size=3)+
  geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci), width =0.1, position = position_dodge2(width = 0.5))+
  scale_colour_manual(name = NULL, values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"),
                      labels = c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR"),
                      drop = FALSE)+
  scale_y_continuous(name = "In-hospital case fatality ratio (%)")+
  scale_x_discrete(name = NULL)+
  theme_bw()+
  theme(text=element_text(size=20),
        legend.position = 'none',
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        panel.grid=element_blank())

ggsave(filename = "plot/hos_mor.tiff", width = 8, height = 4.5,dpi=300)

ggplot(data = hos_mor_res_WHO.sens[!duplicated(hos_mor_res_WHO.sens[c("WHO", "n.all")]),] %>%
         mutate(method=factor(method,levels=c('Main analysis','High-quality studies only'))) %>%
         add_count(WHO), 
       aes(x = WHO, y = prop.est, colour = method,width=0.1*n))+
  geom_point(size=2,position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci),position = position_dodge(width = 0.5))+
  # geom_segment(data = hos_mor_res_WHO.sens[hos_mor_res_WHO.sens$WHO=="Sear" & hos_mor_res_WHO.sens$method=="Main analysis",],
  #              aes(x = 5, y = prop.lci, xend = 5, yend = 3.48,width=.5), arrow = arrow(ends = "last", length = unit(0.03, "npc")),
  #              position = position_dodge2(width = 0.5))+
  scale_x_discrete(name = NULL, labels = c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR"))+
  scale_y_continuous(name = "In-hospital case fatality ratio (%)",
                     expand=expansion(),limits=c(0,3.5),
                     breaks=seq(0,3.5,.5),labels=seq(0,3.5,.5))+
  scale_colour_lancet(name = NULL)+
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

ggsave(filename = "plot/hos_mor_sens.tiff", width = 9, height = 4,dpi=200)

hos_mor_res_WHO.income %>%
  add_count(WHO) %>%
  mutate(Income=factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("LICs", "LMICs", "UMICs", "HICs")),
         WHO=str_to_upper(WHO)
  ) %>%
  ggplot(aes(x = WHO, y = prop.est))+
  geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci,group = Income,colour=WHO,width=0.05*n), 
                position = position_dodge(width = 0.5))+
  geom_segment(data = hos_mor_res_WHO.income[hos_mor_res_WHO.income$WHO=="Amr" & hos_mor_res_WHO.income$Income=="LM",],
               aes(x = 2-1/6, y = prop.lci, xend = 2-1/6, yend = 7.99,width=.5),
               arrow = arrow(ends = "last", length = unit(0.03, "npc")),color='#ffad60',
               position = position_dodge2(width = 0.5))+
  geom_point(data=hos_mor_res_WHO.income %>%
               add_count(WHO) %>%
               mutate(Income=factor(Income, levels = c("L", "LM", "UM", "H"), labels = c("LICs", "LMICs", "UMICs", "HICs")),
                      WHO=str_to_upper(WHO)
               ),
             aes(group = Income,fill=Income,colour=WHO),
             shape=21,size=3,position = position_dodge(width = 0.5))+
  scale_colour_manual(name = NULL, 
                      values = c("#0392cf", "#ffad60", "#E6B800", "#7bc043", "#d81159", "#6a4c93"))+
  scale_fill_manual(values=c('gray0','gray30','gray80','white'),name=NULL)+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name = "In-hospital case fatality ratio (%)",
                     expand=expansion(add=c(0,0)),limits=c(0,8),
                     breaks=seq(0,8,1),labels=seq(0,8,1))+
  theme_bw()+
  theme(text =element_text(size=30),
        legend.position = c(.995,.99),
        legend.justification = c(1,1),
        legend.background = element_rect(fill='transparent'),
        legend.spacing.y = unit(0.1, "cm"),  # 调整图例项间距
        legend.key.size = unit(0.4, "cm"),  # 缩小图例项的大小
        plot.margin = margin(t=8,b=5,l=5,r=5),
        panel.grid=element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(size=.25),
        axis.ticks = element_line(size=.25))+
  guides(colour=guide_legend(order=1))

ggsave(filename = "plot/hos_mor_income.tiff", width = 8, height = 4.5,dpi=300)

# Plot ----
ggplot(data=hos_mor_res_WHO_new_Global,
       aes(x = factor(Group),
           y = prop.est, 
           colour = factor(Group)))+
  geom_point(position = position_dodge2(width = 0.5))+
  geom_errorbar(aes(ymin = prop.lci, ymax = prop.uci), width =0.5, position = position_dodge2(width = 0.5))+
  scale_colour_lancet(name = NULL, 
                      labels = c("Africa", "America", "Eastern Mediterranean", "European", "Southeast Asia", "Western Pacific"),
                      drop = FALSE)+
  scale_y_continuous(name = "In-hospital case fatality ratio (%)")+
  scale_x_discrete(name = NULL)

# replace with new result ----
# hos_mor_res_WHO<-hos_mor_res_WHO_new_Global
# 
# save(hos_mor_res_WHO,
#      hos_mor_tab_WHO,
#      hos_mor_res_WHO.sens,
#      hos_mor_res_WHO.income,
#      WHO_inclusion_mor,
#      file='rda/code_04_in_hos_CFR.RData')

# Data for Figure 3, In-hospital mortality by income ----
# genRateGlobal.fromProp_WHO()
# genRateGlobal.fromProp_WHO_new()
# hos_mor_res_WHO_old_Global<-left_join(hos_mor_res_WHO_old,hos_rate_res_WHO[1:5],by = c("AGEGR" = "AGEGR", "WHO" = "Group")) %>%
#   mutate(Group=WHO) %>%
#   genRateGlobal.fromProp_WHO(n.level = 6)

df<-left_join(
  hos_mor_res_WHO.income %>%
  mutate(Group=paste(WHO,Income,sep='-')),
  hos_rate_res_WHO.income %>%
  mutate(Group=paste(WHO,Income,sep='-'),.before = 1) %>%
  select(1:7),by="Group") %>% 
  filter(!is.na(est.x) & !is.na(est.y))

# Join Population
pop_mor.raw %>% filter(WHORegion=='Sear') %>% summarise(sum(Y0))
pop_WHO.raw %>% filter(WHO=='Sear',AGEGR=='0-<12m') %>% pull(Pop)
pop_mor.raw %>% 
  dplyr::summarise(pop=sum(Y0,na.rm = T),.by=c(WHORegion,Income2019)) %>%
  dplyr::rename(Income=Income2019) %>%
  transmute(Group=paste(WHORegion,Income,sep='-'),Pop=pop)

df<-left_join(df,pop_mor.raw %>% 
                dplyr::summarise(pop=sum(Y0,na.rm = T),.by=c(WHORegion,Income2019)) %>%
                dplyr::rename(Income=Income2019) %>%
                transmute(Group=paste(WHORegion,Income,sep='-'),Pop=pop),
              by='Group')


df.MC <-   suppressMessages({
  left_join(
    df,
    left_join(
      genMC(df =df, id = "Group", input.mean = "est.x", input.se = "se.x", 
            n =N.MC, transFUN = transf.ilogit, output.name = "prop", seed = 3902),
      genMC(df =df, id = "Group", input.mean = "est.y", input.se = "se.y", 
            n =N.MC, transFUN = exp, output.name = "rate", seed = 9930)
    )
  )
})

df.mc_cfr<-genMC(df =df, id = "Group", input.mean = "est.x", input.se = "se.x", 
                 n =N.MC, transFUN = transf.ilogit, output.name = "prop", seed = 3902)
# Replace SEAR CFR
df.mc_cfr_new<-left_join(df.mc_cfr %>% mutate(WHO=str_remove_all(Group,'-.*')),
                         df.mc_cfr_sear %>% rename(WHO=Group),
                         by=c('WHO','index'))

df.mc_cfr_new %>%
  count(Group,WHO,is.na(prop.y),is.na(prop.x))

df.mc_cfr_new2<-df.mc_cfr_new %>%
  transmute(Group,index,
            prop=ifelse(WHO=='Sear',prop.y,prop.x))

df.mc_cfr %>% summarise(mean(prop),.by=Group)
df.mc_cfr_new2 %>% summarise(mean(prop),.by=Group)

ggplot(df.mc_cfr)+
  geom_histogram(aes(prop,fill=Group))+
  facet_wrap(~Group,scales = 'free')
ggplot(df.mc_cfr_new2)+
  geom_histogram(aes(prop,fill=Group))+
  facet_wrap(~Group,scales = 'free')

df.MC <-   suppressMessages({
  left_join(
    df,
    left_join(
      df.mc_cfr_new2,#替换sear后的数据
      genMC(df =df, id = "Group", input.mean = "est.y", input.se = "se.y", 
            n =N.MC, transFUN = exp, output.name = "rate", seed = 9930)
    )
  )
})
df.MC$N <- df.MC$Pop *df.MC$rate
df.MC$N_prop <- df.MC$Pop *df.MC$rate * df.MC$prop
df1<- df.MC %>% group_by(Group, Impute.x, Impute.y, n.all, n.impute) %>%
  dplyr::summarise(N.est = round(median(N_prop),3),
                   N.lci = round(quantile(N_prop, 0.025),3),
                   N.uci = round(quantile(N_prop, 0.975),3),
                   Pop = Pop[1],
                   prop.est = median(N_prop/N)*100,
                   prop.lci = quantile(N_prop/N, 0.025)*100,
                   prop.uci = quantile(N_prop/N, 0.975)*100
  )
df1$IR.est <- with(df1, N.est/Pop*1000)
df1$IR.lci <- with(df1, N.lci/Pop*1000)
df1$IR.uci <- with(df1, N.uci/Pop*1000)

hos_mor_res_WHO.income_with_IR<-df1

# replace with new result ----
hos_mor_res_WHO<-hos_mor_res_WHO_new_Global

save(hos_mor_res_WHO,
     hos_mor_tab_WHO,
     hos_mor_res_WHO.sens,
     hos_mor_res_WHO.income,
     hos_mor_res_WHO.income_with_IR,# In-Hos Mortality Rate by region income
     WHO_inclusion_mor,
     file='rda/code_04_in_hos_CFR.RData')

